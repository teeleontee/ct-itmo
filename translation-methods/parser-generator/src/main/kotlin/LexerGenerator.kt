
import com.squareup.kotlinpoet.*
import java.io.File

class LexerGenerator(tree: GrammarFileParser.GrammarFileContext) {
    private val grammarVisitor = GrammarVisitor().also { it.visitGrammarFile(tree) }
    private val name = grammarVisitor.grammarName + "Lexer"
    private val grName = grammarVisitor.grammarName

    private fun writeToFile(fp: String, name: String, string: String) {
        val f = File(fp)
        if (!f.exists()) {
            f.mkdirs()
        }
        val file = File(f, name)
        file.writeText(string)
    }

    private fun getListOfTerminals(): String {
        val ml = mutableListOf<String>()
        for ((_, term) in grammarVisitor.tokens) {
            if (!term.isRegex && term.name != "EPS") {
                ml.add(term.name)
            }
        }
        val res = StringBuilder("listOf(\"$\", ")
        ml.forEach {
            res.append("$it, ")
        }
        res.setLength(res.length - 1)
        res.append(")")
        return res.toString()
    }

    private fun generateScan(): FunSpec {
        val tokenType = ClassName("","${grName}Token")
        val listString = getListOfTerminals()
        val codeBlock = """
            val list = $listString
            skipWhitespace()
            val sb = StringBuilder()
            
        """.trimIndent()
        val funSpec = FunSpec.builder("scan")
            .returns(tokenType)
            .addCode(codeBlock)
            .beginControlFlow("while (!curSymbol.isWhitespace())")
            .addStatement("sb.append(curSymbol)")
        funSpec.beginControlFlow("when (val str = sb.toString())")
        for ((str, term) in grammarVisitor.tokens) {
            if (term.isRegex || str == "EPS") {
                continue
            }
            funSpec.addStatement("${term.name} -> return ${grName}Token(${grName}TokenId.$str, str)")
        }
        funSpec.addStatement("\"$\" -> return ${grName}Token(${grName}TokenId.END, str)")
        funSpec.beginControlFlow("else ->")
            .beginControlFlow("when")
        for ((str, term) in grammarVisitor.tokens) {
            if (!term.isRegex || str == "EPS") {
                continue
            }
            funSpec.beginControlFlow("\"${term.name}\".toRegex().matches(str) " +
                    "-> ").endControlFlow()
        }
        val codeBlock2 = """
            if (!list.any { it.startsWith(str) }) {
                index--
                sb.setLength(sb.length - 1)
                break
            }
            
        """.trimIndent()
        funSpec.beginControlFlow("else -> ")
            .addCode(codeBlock2).endControlFlow()
            .endControlFlow().endControlFlow()
            .endControlFlow()
            .addStatement("curSymbol = getNextSymbol(index)").endControlFlow()
        funSpec.addStatement("val lexeme = if (sb.isEmpty()) \"\$\" else sb.toString()")
        funSpec.beginControlFlow("return when")
        for ((str, term) in grammarVisitor.tokens) {
            if (!term.isRegex || str == "EPS") {
                continue
            }
            funSpec.addStatement("\"${term.name}\".toRegex().matches(lexeme) " +
                    "-> ${grName}Token(${grName}TokenId.$str, lexeme)")
        }
        funSpec.addStatement("else -> throw Exception(\"Parse Error, got symbol \$curSymbol at index \$index\")")
            .endControlFlow()
        return funSpec.build()
    }

    private fun generateGetNextSymbol(): FunSpec {
        val codeBlock = """
            index++
            return if (i >= str.length) {
              '$'
            } else {
              str[i]
            }
        """.trimIndent()
        return FunSpec.builder("getNextSymbol")
            .addModifiers(KModifier.PRIVATE)
            .returns(Char::class)
            .addParameter(
                ParameterSpec.builder("i", Int::class).build())
            .addCode(codeBlock)
            .build()
    }

    private fun generateSkipWs(): FunSpec {
        val codeBlock = """
        while (true) {
          curSymbol = getNextSymbol(index)
          if (!curSymbol.isWhitespace()) {
            break
          }
        }
        """.trimIndent()
        return FunSpec.builder("skipWhitespace")
            .addModifiers(KModifier.PRIVATE)
            .addCode(codeBlock)
            .build()
    }

    fun generate() {
        val lexerConstructor = FunSpec.constructorBuilder()
            .addParameter("str", String::class)
            .build()
        val lexerClass = TypeSpec.classBuilder(name)
            .primaryConstructor(lexerConstructor)
            .addProperty(
                PropertySpec.builder("str", String::class)
                    .mutable()
                    .addModifiers(KModifier.PRIVATE)
                    .initializer("\"\$str$\"")
                    .build())
            .addProperty(
                PropertySpec.builder("curSymbol", Char::class)
                    .mutable()
                    .addModifiers(KModifier.PRIVATE)
                    .initializer("%L", "' '")
                    .build())
            .addProperty(
                PropertySpec.builder("index", Int::class)
                    .mutable()
                    .addModifiers(KModifier.PRIVATE)
                    .initializer("%L", 0)
                    .build())
            .addFunction(generateGetNextSymbol())
            .addFunction(generateSkipWs())
            .addFunction(generateScan())
        val file = FileSpec.builder("", "${name}TokenId")
            .addType(lexerClass.build())
            .build()
        writeToFile("build/generated/$grName/kotlin", "$name.kt", file.toString())
    }
}