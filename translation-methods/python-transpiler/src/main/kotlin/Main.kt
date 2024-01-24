import org.antlr.v4.runtime.*
import java.io.File
import java.nio.charset.Charset

fun readFileAsText(fp: String): String
  = File(fp).readText(Charset.defaultCharset())

fun writeToFile(fp: String, string: String) {
    val f = File(fp)
    f.createNewFile()
    f.printWriter().use {
        out -> out.println(string)
    }
}

fun main() {
    val input = readFileAsText("in/1.py")
    val inputStream = CharStreams.fromString(input)

    val lexer = PythonLexer(inputStream)
    val tokens = CommonTokenStream(lexer)

    val parser = PythonParser(tokens)
    val tree = parser.fileStatement()

    val visitor = PyVisitor()
    val program = visitor.visitFileStatement(tree)
    writeToFile("out/3.c", program)

    val printableTree = DebugTree(tree)
    println(printableTree)
}
