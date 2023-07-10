package info.kgeorgiy.ja.leontev.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.IntStream;


/**
 * Implementation of {@link HelloClient}
 * @author Leontev Taras
 */
public class HelloUDPClient implements HelloClient {

    private static class ClientHelper {
        private final ExecutorService threadPool;
        private final InetAddress inetAddress;
        private final String prefix;
        private final int port;
        private final int threads;
        private final int requests;
        private static final int SOCKET_TIMEOUT = 100;

        private ClientHelper(InetAddress host, String prefix, int port, int threads, int requests) {
            this.threadPool = Executors.newFixedThreadPool(threads);
            this.inetAddress = host;
            this.threads = threads;
            this.prefix = prefix;
            this.port = port;
            this.requests = requests;
        }

        /**
         * Calls clientHelper constructor
         * @param host server host
         * @param prefix request prefix
         * @param port server port
         * @param threads number of request threads
         * @param requests number of requests per thread
         * @throws UnknownHostException if unable to resolve ip or host name
         */
        public static ClientHelper create(String host, String prefix, int port, int threads, int requests) throws UnknownHostException {
            return new ClientHelper(InetAddress.getByName(host), prefix, port, threads, requests);
        }

        /**
         * Sends out requests on {@code threads} threads
         */
        public void send() {
            IntStream.range(1, threads + 1).forEach(current -> threadPool.submit(() -> runTask(current)));
            HelloUDPUtility.closeThreads(threadPool);
        }

        private void runTask(int curThread) {
            try (DatagramSocket socket = new DatagramSocket()) {
                socket.setSoTimeout(SOCKET_TIMEOUT);
                IntStream.range(1, requests + 1).forEach(request -> {
                    try {
                        final String reqName = prefix + curThread + "_" + request;
                        final DatagramPacket snd =  HelloUDPUtility.createReqDatagram(reqName.getBytes(StandardCharsets.UTF_8),
                                inetAddress, port);
                        final DatagramPacket ack = HelloUDPUtility.createAckDatagram(socket);
                        boolean fin = false;
                        while (!fin) {
                            try {
                                socket.send(snd);
                                socket.receive(ack);
                                String data = new String(ack.getData(), ack.getOffset(),
                                        ack.getLength(), StandardCharsets.UTF_8);
                                if (data.contains(reqName)) {
                                    fin = true;
                                }
                            } catch (IOException ignored) {
                            }
                        }
                    } catch (SocketException ignored) {
                    }
                });
            } catch (SocketException e) {
                System.err.println(e.getMessage());
            }
        }
    }

    /**
     * Runs Hello client.
     * This method should return when all requests are completed.
     *
     * @param host server host
     * @param port server port
     * @param prefix request prefix
     * @param threads number of request threads
     * @param requests number of requests per thread.
     */
    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        try {
            ClientHelper clientHelper = ClientHelper.create(host, prefix, port, threads, requests);
            clientHelper.send();
        } catch (UnknownHostException e) {
            System.err.println("Invalid host: " + e.getMessage());
        }
    }

    public static void main(String[] args) {
        if (HelloUDPUtility.checkArguments(args)) {
            try {
                HelloClient client = new HelloUDPClient();
                client.run(args[0], Integer.parseInt(args[1]), args[2], Integer.parseInt(args[3]), Integer.parseInt(args[4]));
            } catch (NumberFormatException e) {
                System.err.println(e.getMessage());
            }
        }
    }
}
