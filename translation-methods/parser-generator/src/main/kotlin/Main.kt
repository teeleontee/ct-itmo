
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import java.io.File
import java.nio.charset.Charset

fun readFileAsText(fp: String): String
        = File(fp).readText(Charset.defaultCharset())


fun generateGrammar(fp: String) {
    val input = readFileAsText(fp)
    val inputStream = CharStreams.fromString(input)

    val lexer = GrammarFileLexer(inputStream)
    val tokens = CommonTokenStream(lexer)

    val parser = GrammarFileParser(tokens)
    val tree = parser.grammarFile()

    val visitor = GrammarVisitor()
    visitor.visitGrammarFile(tree)

    val tokenGenerator = TokenGenerator(tree)
    tokenGenerator.generate()

    val lexerGenerator = LexerGenerator(tree)
    lexerGenerator.generate()

    val parserGenerator = ParserGenerator(tree)
    parserGenerator.generateParser()
}



fun main() {
    generateGrammar("calc/calc.gr")
//    generateGrammar("pylambda/pyLambda.gr")
}