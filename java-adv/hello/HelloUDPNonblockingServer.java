package info.kgeorgiy.ja.leontev.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.*;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.Queue;
import java.util.concurrent.*;

/**
 * @author Leontev Taras
 */
public class HelloUDPNonblockingServer implements HelloServer {

    private ExecutorService connectionHandler;
    private ExecutorService executor;
    private DatagramChannel channel;
    private Selector selector;
    private Queue<EchoInfo> queue;
    private static final int CAPACITY = 1024; // :NOTE: не очень

    private void init(int port, int threads) {
        connectionHandler = Executors.newSingleThreadExecutor();
        executor = Executors.newFixedThreadPool(threads);
        try {
            selector = Selector.open();

            if (Objects.isNull(selector)) {
                return;
            }

            channel = DatagramChannel.open();
            channel.configureBlocking(false);
            channel.bind(new InetSocketAddress(port));
            channel.register(selector, SelectionKey.OP_READ);
            queue = new ConcurrentLinkedQueue<>(); // :NOTE: лучше ограничить размер
        } catch (final ClosedChannelException e) {
            System.err.println("Closed Channel " + e.getMessage());
        } catch (final IOException ignored) {
        }
    }

    private void sendToChannel(SelectionKey key) {
        try {
            if (!queue.isEmpty()) {
                EchoInfo info = queue.poll();
                channel.send(info.data(), info.address());
            }
        } catch (final IOException ignored) {

        }
        key.interestOps(SelectionKey.OP_READ | SelectionKey.OP_WRITE);
    }

    private void receiveFromChannel(SelectionKey key) {
        try {
            ByteBuffer buffer = ByteBuffer.allocate(CAPACITY);
            final SocketAddress address = channel.receive(buffer);
            executor.submit(() -> {
                String sendData = "Hello, ".concat(HelloUDPUtility.getMessageFromBuffer(buffer));
                queue.add(new EchoInfo(address, ByteBuffer.wrap(sendData.getBytes(StandardCharsets.UTF_8))));
                key.interestOps(SelectionKey.OP_WRITE);
                selector.wakeup();
            });
        } catch (final IOException ignored) {

        }
    }

    private record EchoInfo(SocketAddress address, ByteBuffer data) {

    }

    /**
     * Starts Server, listens on {@code port}, has {@code threads} workers.
     * One is delegated for I/O operations, others are used for processing
     *
     * @param port    server port.
     * @param threads number of working threads.
     */
    @Override
    public void start(int port, int threads) {
        init(port, threads);
        connectionHandler.submit(() -> {
            while (!Thread.interrupted()) {
                try {
                    selector.select(key -> {
                        if (!key.isValid()) {
                        }
                        if (key.isReadable()) {
                            receiveFromChannel(key);
                        } else {
                            sendToChannel(key);

                        }
                    });
                } catch (final IOException ignored) {
                }
            }
        });
    }

    /**
     * closes Selector, Channel and Executors
     */
    @Override
    public void close() {
        try {
            if (channel != null && channel.isOpen()) {
                channel.close();
            }
            if (selector != null && selector.isOpen()) {
                selector.close();
            }
        } catch (final IOException ignored) {

        }
        HelloUDPUtility.closeThreads(executor);
        HelloUDPUtility.closeThreads(connectionHandler);
    }

    public static void main(String[] args) {
        if (!HelloUDPUtility.checkArguments(args)) {
            try (HelloServer server = new HelloUDPNonblockingServer()) {
                HelloUDPUtility.runServer(server, Integer.parseInt(args[0]),
                        Integer.parseInt(args[1]));
            } catch (NumberFormatException e) {
                System.err.println(e.getMessage());
            }
        }
    }
}
