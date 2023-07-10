package info.kgeorgiy.ja.leontev.implementor;

import info.kgeorgiy.java.advanced.implementor.BaseImplementorTest;
import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.*;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.jar.Attributes;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;

import static info.kgeorgiy.ja.leontev.implementor.ImplUtils.*;
import static java.util.Objects.*;

/**
 * Implementation of {@link JarImpler} interface
 *
 * @author teeleontee
 */
public class Implementor implements JarImpler  {


    /**
     * Just a default constructor
     */
    public Implementor() {
    }

    /**
     * Creates a String of the new Class name that is to be generated
     * @param token class token is the type token that we are creating an implementation of
     *
     * @return The name of the new class that is to be generated
     */
    private static String getClassName(Class<?> token){
        return token.getSimpleName() + IMPL_STRING;
    }


    /**
     * Gets the path to a class
     * @return Path to the class in a String
     */
    private static String getClassPath() {
        try {
            return Path.of(BaseImplementorTest.class.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        } catch (final URISyntaxException e) {
            throw new AssertionError(e);
        }
    }

    /**
     * Generates the full name of the Implementation
     *
     * @param token class token is the type token that we are creating an implementation of
     * @return The full name of the new class that we are generating
     */
    private static String getImplName(final Class<?> token) {
        return token.getPackageName() + "." + token.getSimpleName() + "Impl";
    }

    /**
     * Encapsulates a string in curly braces, this method is one of the methods
     * used for generating the interface implementation
     *
     * @param a String representation of code that we want to surround with curly braces
     * @return Code that is in a string that is now surrounded in curly braces
     */
    private static String encapsulateInCurlyBraces(String a) {
        return OPEN_CURLY_BRACE + LINE_SEPARATOR + a + LINE_SEPARATOR + TAB
                + CLOSE_CURLY_BRACE
                + LINE_SEPARATOR;
    }

    /**
     * Returns the default type for some method we are generating,
     * for {@code boolean - false}, for {@code void - nothing} for any other primitive type we return {@code 0},
     * if the type is not primitive, we return {@code null}
     *
     * @param rc The return type of some method
     * @return A String, i.e. the default implementation for the return type
     */
    private static String returnType(Class<?> rc) {
        if (rc.isPrimitive()) {
            if (rc.equals(Void.TYPE)) { return NOTHING; }
            else if (rc.equals(Boolean.TYPE)) { return FALSE; }
            else { return ZERO;}
        }
        return "null";
    }

    /**
     * Adds java syntax to the returned type of some method, i.e. the semicolon, tabulations,
     * the word "return". By passing in what we want to return we make a valid Java line of code
     *
     * @param returned What is returned by the method
     * @return A string that is a valid line of Java code, i.e. the return statement
     */
    private static String buildReturnStatement(String returned) {
        if (!Objects.equals(returned, NOTHING)) {
            return TAB.repeat(2) + RETURN + returned + SEMICOLON;
        }
        return NOTHING;
    }

    /**
     * Generates a String that is to be used in the method implementations, i.e. in the
     * parameters of some method
     *
     * @param parameters An array of parameters of some method in the interface
     * @return A String that is to be used when building the method parameters in the implementation
     */
    private static String buildParams(Parameter[] parameters) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < parameters.length; i++) {
            if (i != parameters.length - 1) {
                sb.append(parameters[i].getType().getCanonicalName()).append(SPACE).append(parameters[i].getName())
                        .append(COMMA)
                        .append(SPACE);
            } else {
                sb.append(parameters[i].getType().getCanonicalName()).append(SPACE).append(parameters[i].getName());
            }
        }
        return sb.toString();
    }

    /**
     * Creates a full implementation of some method in the interface
     *
     * @param method The Method we are currently implementing
     * @return Valid lines of Java code, that are the implementation of the method in a String
     */
    private static String getMethodString(Method method) {
        StringBuilder stringBuilder = new StringBuilder();
        Parameter[] parameters = method.getParameters();
        int mod = method.getModifiers();
        if (Modifier.isPublic(mod)) { stringBuilder.append(PUBLIC); }
        stringBuilder.append(method.getReturnType().getTypeName()).append(SPACE).append(method.getName())
                .append(OPEN_BRACE).append(buildParams(parameters)).append(CLOSE_BRACE)
                .append(SPACE)
                .append(encapsulateInCurlyBraces(buildReturnStatement(returnType(method.getReturnType()))));
        return stringBuilder.toString();
    }


    /**
     * Returns the path to the Implementations
     * @param root the given root
     * @param token Class token
     * @return path to implementation
     */
    private static Path getPath(Path root, Class<?> token) {
        return root.resolve((token.getPackageName() + "." + token.getSimpleName() + IMPL_STRING).
                replace(".", File.separator) + DOT_JAVA).toAbsolutePath();
    }

    /**
     * Generates an Implementation of an interface that compiles,
     * by being provided a class {@code token}
     * <p>The token given to this method has to be valid</p>
     *
     * @param token type token to create implementation for.
     * @param root root directory.
     * @throws ImplerException if the token or root is {@code null} or if the token is primitive
     * or has the private modifier
     */
    @Override
    public void implement(Class<?> token, Path root) throws ImplerException {
        if (isNull(token) || isNull(root)) {
            throw new ImplerException("Interface is NULL");
        }
        if (token.isPrimitive() || Modifier.isPrivate(token.getModifiers()) || !token.isInterface()) {
            throw new ImplerException("Invalid interface given");
        }
        Path path = getPath(root, token);
        if (path.getParent() != null) {
            try {
                Files.createDirectories(path.getParent());
            } catch (IOException e) {
                throw new ImplerException(e.getMessage());
            }
        }
        Method[] methods = token.getMethods();
        StringBuilder sb = new StringBuilder();
        sb.append(PACKAGE).append(token.getPackageName()).append(SEMICOLON)
                .append(LINE_SEPARATOR.repeat(2)).append(PUBLIC).append(CLASS)
                .append(getClassName(token)).append(SPACE).append(IMPLEMENTS)
                .append(SPACE).append(token.getName().replace('$', DOT))
                .append(SPACE).append(OPEN_CURLY_BRACE).append(LINE_SEPARATOR.repeat(2));

        for (Method method : methods) {
            sb.append(TAB);
            sb.append(getMethodString(method));
            sb.append(LINE_SEPARATOR);
        }
        sb.append(CLOSE_CURLY_BRACE).append(LINE_SEPARATOR);
        try (BufferedWriter writer = Files.newBufferedWriter(path)) {
            writer.write(sb.toString());
        } catch (final IOException e) {
            System.err.println(e.getMessage());
        }
    }


    /**
     * Resolves the path to some classes and then compiles them
     * using {@link Implementor#compileFiles(Path, List)}
     * @param root The root directory
     * @param classes Some arbitrary amount of tokens that are to be compiled
     * @throws ImplerException if the files are unable to be compiled
     */
    public static void compile(final Path root, final Class<?>... classes) throws ImplerException {
        final List<String> files = new ArrayList<>();
        for (final Class<?> token : classes) {
            String someFile = root.resolve(getImplName(token)
                    .replace(".", File.separator) + ".java")
                    .toAbsolutePath()
                    .toString();
            files.add(someFile);
        }
        compileFiles(root, files);
    }

    /**
     * Compiles given files using {@link ToolProvider#getSystemJavaCompiler()}
     * @param root The root directory
     * @param files The List of files that are to be compiled
     * @throws ImplerException if there is no Java Compiler available
     * or if {@link JavaCompiler#run(InputStream, OutputStream, OutputStream, String...)} didn't succeed,
     * i.e. return nonzero
     */
    public static void compileFiles(final Path root, final List<String> files) throws ImplerException {
        final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        if (compiler == null) {
            throw new ImplerException("No java compiler available");
        }

        final String classpath = root + File.pathSeparator + getClassPath();
        final String[] args = Stream.concat(files.stream(),
                Stream.of("-cp", classpath, "-encoding", "UTF-8")).toArray(String[]::new);

        final int exitCode = compiler.run(null, null, null, args);
        if (exitCode != 0) {
            throw new ImplerException("Unable to compile files");
        }
    }

    private Path createTempDirs(Path jarFile) {
        try {
            Files.createDirectories(jarFile.getParent());
            return Files.createTempDirectory(jarFile.toAbsolutePath().getParent(), "aboba");
        } catch (IOException e) {
            System.err.println("Unable to create Dir");
        }
        return null;
    }

    /**
     * Creates a jar file containing the compiled implementation that is generated
     * using {@link Implementor#implement(Class, Path)}
     * @param token type token to create implementation for.
     * @param jarFile target <var>.jar</var> file.
     * @throws ImplerException if the arguments are incorrect,
     * token is not supported, or unable to compile files
     */
    @Override
    public void implementJar(Class<?> token, Path jarFile) throws ImplerException {
        try {
            Path tmpPath = createTempDirs(jarFile);
            if (tmpPath == null) {
                System.err.println("unable to create dir");
                return;
            }
            implement(token, tmpPath);
            compile(tmpPath, token);
            final Manifest manifest = new Manifest();
            final Attributes attr = manifest.getMainAttributes();
            attr.put(Attributes.Name.MANIFEST_VERSION, "1.0");
            try (JarOutputStream outputStream = new JarOutputStream(Files.newOutputStream(jarFile), manifest)) {
                final String name = token.getPackageName()
                        .replace('.', '/') + "/" + token.getSimpleName()
                        + IMPL_STRING + ".class";
                outputStream.putNextEntry(new ZipEntry(name));
                Files.copy(Paths.get(tmpPath.toString(), name), outputStream);
            } catch (final IOException e) {
                System.err.println(e.getMessage());
            }
        } catch (final ImplerException e) {
            throw new ImplerException("Unable to implement interface");
        }
    }


    /**
     * Entry for {@link Implementor}
     *
     * <p>All arguments have to be non null</p>
     * <p>There can be 2 arguments if you want to only implement some class, {@code token} and {@code root}</p>
     * <p>There can be 3 arguments if you want to create a jar file containg the compiled implementation,
     * {@code -jar} then {@code token} and {@code root}</p>
     *
     * @param args given arguments
     */
    public static void main(String[] args) {
        try {
            Implementor implementor = new Implementor();
            if (args.length == 2) {
                Path root = Path.of(args[1]);
                Class<?> token = Class.forName(args[0]);
                implementor.implement(token, root);
            }
            else if (args.length == 3 && Objects.equals(args[0], "-jar")) {
                Path root = Path.of(args[2]);
                Class<?> token = Class.forName(args[1]);
                implementor.implementJar(token, root);
            }
            else {
                System.err.println("Bad args given");
            }
        } catch (final ImplerException e) {
            System.err.println(e.getMessage());
        } catch (ClassNotFoundException e) {
            System.err.println("class not found " + e.getMessage());
        }
    }
}
