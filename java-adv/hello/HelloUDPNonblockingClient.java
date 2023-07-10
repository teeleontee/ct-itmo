package info.kgeorgiy.ja.leontev.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.*;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

public class HelloUDPNonblockingClient implements HelloClient {

    private String prefix;
    private InetSocketAddress socketAddress;
    private Selector selector;
    private List<DatagramChannel> dChannels;
    private static final int SOCKET_TIMEOUT = 100;

    private void init(String host, String prefix, int threads, int port) throws IOException {
        InetAddress hostAddress = InetAddress.getByName(host);
        this.prefix = prefix;
        this.selector = Selector.open();
        this.socketAddress = new InetSocketAddress(hostAddress, port);
        this.dChannels = new ArrayList<>();

        if (Objects.isNull(selector)) {
            return;
        }

        for (int i = 1; i < threads + 1; i++) {
            DatagramChannel channel = DatagramChannel.open();
            channel.configureBlocking(false);
            channel.register(selector, SelectionKey.OP_WRITE, new SendInfo(i, 1, null));
            dChannels.add(channel);
        }
    }


    private void close() {
        try {
            selector.close();
            for (DatagramChannel channel : dChannels) {
                channel.close();
            }
        } catch (final IOException e) {
            System.err.println(e.getMessage());
        }
    }

    private record SendInfo(int threadNumber, int request, String message) {

    }

    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        try {
            init(host, prefix, threads, port);
            int finishedCount = 0;
            while (!Thread.interrupted()) {
                if (selector.select(SOCKET_TIMEOUT) == 0) {
                    selector.keys().forEach(key -> key.interestOps(SelectionKey.OP_WRITE));
                }
                for (Iterator<SelectionKey> i = selector.selectedKeys().iterator(); i.hasNext(); ) {
                    final SelectionKey key = i.next();
                    try {
                        if (key.isValid()) {
                            DatagramChannel channel = (DatagramChannel) key.channel();
                            SendInfo info = (SendInfo) key.attachment();
                            if (key.isWritable()) {
                                writeToChannel(key, channel, info);
                            } else {
                                ByteBuffer buffer = ByteBuffer.allocate(1024);
                                channel.receive(buffer);
                                if (HelloUDPUtility.getMessageFromBuffer(buffer).contains(info.message())) {
                                    if (info.request() == requests) {
                                        finishedCount++;
                                        key.cancel();
                                        continue;
                                    }
                                    int nextRequest = info.request() + 1;
                                    key.attach(new SendInfo(info.threadNumber(), nextRequest, info.message()));
                                }
                                key.interestOps(SelectionKey.OP_WRITE);
                            }
                        }
                    } finally {
                        i.remove();
                    }
                }
                if (finishedCount == threads) {
                    break;
                }
            }
        } catch (UnknownHostException e) {
            System.err.println("Unknown host: "+ e.getMessage());
        } catch (IOException e) {
            System.err.println(e.getMessage());
        } finally {
            close();
        }
    }

    private void writeToChannel(SelectionKey key, DatagramChannel channel, SendInfo info) {
        ByteBuffer message = HelloUDPUtility.createMessage(this.prefix, info.threadNumber(), info.request());
        try {
            channel.send(message, socketAddress);
        } catch (final IOException e) {
            System.err.println("IOException" + e.getMessage());
        }
        key.attach(new SendInfo(info.threadNumber(), info.request(),
                HelloUDPUtility.getMessageFromBuffer(message)));
        key.interestOps(SelectionKey.OP_READ);
    }

    public static void main(String[] args) {
        if (HelloUDPUtility.checkArguments(args)) {
            try {
                HelloClient client = new HelloUDPNonblockingClient();
                client.run(args[0], Integer.parseInt(args[1]), args[2], Integer.parseInt(args[3]), Integer.parseInt(args[4]));
            } catch (NumberFormatException e) {
                System.err.println(e.getMessage());
            }
        }
    }
}
