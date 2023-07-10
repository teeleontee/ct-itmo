package info.kgeorgiy.ja.leontev.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketException;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;


/**
 * Implementation of {@link HelloServer}
 *
 * @author Leontev Taras
 */
public class HelloUDPServer implements HelloServer {
    private ExecutorService threadsPool;
    private DatagramSocket socket;

    /**
     * Starts Server, listens on {@code port}, has {@code threads} workers.
     * One is delegated for I/O operations, others are used for processing
     * @param port server port.
     * @param threads number of working threads.
     */
    @Override
    public void start(int port, int threads) {
        try {
            this.socket = new DatagramSocket(port);
            this.threadsPool = Executors.newFixedThreadPool(threads);
            for (int i = 0; i < threads; i++) {
                threadsPool.submit(() -> {
                    while (!socket.isClosed()) {
                        try {
                            DatagramPacket packet = HelloUDPUtility.createAckDatagram(socket);
                            socket.receive(packet);
                            InetAddress address = packet.getAddress();
                            int packetPort = packet.getPort();
                            String received = new String(packet.getData(), 0,
                                    packet.getLength(), StandardCharsets.UTF_8);
                            byte[] sendMessage = ("Hello, " + received).getBytes(StandardCharsets.UTF_8);
                            DatagramPacket echoHello = HelloUDPUtility.createReqDatagram(sendMessage, address, packetPort);
                            socket.send(echoHello);
                        } catch (IOException ignored) {
                        }
                    }
                });
            }
        } catch (SocketException | SecurityException | IllegalArgumentException e) {
            System.err.println(e.getMessage());
        }
    }


    /**
     * Closes threads and Socket
     */
    @Override
    public void close() {
        socket.close();
        HelloUDPUtility.closeThreads(threadsPool);
    }

    public static void main(String[] args) {
        if (!HelloUDPUtility.checkArguments(args)) {
            try (HelloServer server = new HelloUDPServer()) {
                HelloUDPUtility.runServer(server, Integer.parseInt(args[0]),
                        Integer.parseInt(args[1]));
            } catch (NumberFormatException e) {
                System.err.println(e.getMessage());
            }
        }
    }
}
