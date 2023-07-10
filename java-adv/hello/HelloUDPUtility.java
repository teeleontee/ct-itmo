package info.kgeorgiy.ja.leontev.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Objects;
import java.util.Scanner;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

public class HelloUDPUtility {
    public static DatagramPacket createAckDatagram(DatagramSocket socket) throws SocketException {
        return new DatagramPacket(new byte[socket.getReceiveBufferSize()], socket.getReceiveBufferSize());
    }

    public static DatagramPacket createReqDatagram(byte[] name, InetAddress inetAddress, int port) {
        return new DatagramPacket(name, name.length, inetAddress, port);
    }

    public static DatagramPacket convert(String s) {
        byte[] bytes = s.getBytes(StandardCharsets.UTF_8);
        return new DatagramPacket(bytes, bytes.length);
    }

    public static String convert(DatagramPacket p) {
        return new String(p.getData(), p.getOffset(), p.getLength(), StandardCharsets.UTF_8);
    }

    public static String fromBuffer(ByteBuffer buffer) {
        return StandardCharsets.UTF_8.decode(buffer).toString();
    }

    static String getMessageFromBuffer(ByteBuffer buffer) {
        buffer.flip();
        byte[] bytes = new byte[buffer.remaining()];
        buffer.get(bytes);
        return new String(bytes);
    }

    public static ByteBuffer createMessage(String prefix, int curThread, int request) {
        String toSend = prefix + curThread + "_" + request;
        return ByteBuffer.wrap(toSend.getBytes(StandardCharsets.UTF_8));
    }

    public static boolean checkArguments(String[] args) {
        if (args == null || args.length == 0 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("bad arguments given");
            return false;
        }
        return true;
    }

    public static void runServer(HelloServer server, int port, int threads) {
        server.start(port, threads);
        Scanner scanner = new Scanner(System.in);
        while (true) {
            String command = scanner.nextLine();
            if (command.equals("close")) {
                break;
            }
        }
    }
    public static void closeThreads(ExecutorService threadsPool) {
        boolean terminated = threadsPool.isTerminated();
        if (!terminated) {
            threadsPool.shutdown();
            boolean interrupted = false;
            while (!terminated) {
                try {
                    terminated = threadsPool.awaitTermination(10, TimeUnit.SECONDS);
                } catch (InterruptedException e) {
                    if (!interrupted) {
                        threadsPool.shutdownNow();
                        interrupted = true;
                    }
                }
            }
            if (interrupted) {
                Thread.currentThread().interrupt();
            }
        }
    }
}
