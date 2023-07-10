package info.kgeorgiy.ja.leontev.walk;

import java.io.*;
import java.nio.file.*;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;

public class Walk {
    private static final String zeroesString = "0".repeat(64);
    private static String makeZeroesString(String path) {
        return zeroesString + " " + path + System.lineSeparator();
    }
    private static String makeString(byte[] bytes, String path) {
        return HexFormat.of().formatHex(bytes) +
                " " +
                path +
                System.lineSeparator();
    }

    private static String createHash(Path curFilePath) {
        try (InputStream reader = Files.newInputStream(curFilePath)) {
            try {
                MessageDigest hash = MessageDigest.getInstance("SHA-256");
                int c;
                byte[] bytes = new byte[1 << 10];
                while ((c = reader.read(bytes)) >= 0) {
                    hash.update(bytes, 0, c);
                }
                return makeString(hash.digest(), curFilePath.toString());
            } catch (NoSuchAlgorithmException e) {
                System.err.println(e.getMessage());
            }
        } catch (IOException  e) {
            System.err.println(e.getMessage());
        } catch (UnsupportedOperationException e) {
            System.err.println(e.getMessage());
        } catch (SecurityException e) {
            System.err.println(e.getMessage());
        } catch (IllegalArgumentException e) {
            System.err.println(e.getMessage());
        }
        return makeZeroesString(curFilePath.toString());
    }
    private static void walk(String inputFile, String outputFile) {
        try {
            Path input = Path.of(inputFile);
            Path output = Path.of(outputFile);
            try {
                if (output.getParent() != null) {
                    Files.createDirectories(output.getParent());
                }
                String filePath;
                try (BufferedReader readerInput = Files.newBufferedReader(input)) {
                    try (BufferedWriter writer = Files.newBufferedWriter(output)) {
                        while ((filePath = readerInput.readLine()) != null) {
                            String appended = makeZeroesString(filePath);
                            try {
                                Path curFilePath = Path.of(filePath);
                                appended = createHash(curFilePath);
                            } catch (InvalidPathException e) {
                                System.err.println(e.getMessage());
                            }
                            writer.write(appended);
                        }
                    } catch (IOException e) {
                        System.err.println(e.getMessage());
                    }
                } catch (IOException e) {
                    System.err.println(e.getMessage());
                }
            } catch (IOException e) {
                System.err.println(e.getMessage());
            } catch (UnsupportedOperationException e) {
                System.err.println(e.getMessage());
            } catch (SecurityException e) {
                System.err.println(e.getMessage());
            } catch (FileSystemNotFoundException e) {
                System.err.println(e.getMessage());
            }
        }
        catch (InvalidPathException e) {
            System.err.println(e.getMessage());
        }
    }

    public static void main(String[] args) {
        if (args == null || args.length != 2 || args[0] == null || args[1] == null) {
            System.err.println("Invalid arguments given");
        } else {
            walk(args[0], args[1]);
        }
    }
}
