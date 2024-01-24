
import com.squareup.kotlinpoet.*
import java.io.File

class TokenGenerator(tree: GrammarFileParser.GrammarFileContext) {
    private val grammarVisitor = GrammarVisitor().also { it.visitGrammarFile(tree) }
    private val name = grammarVisitor.grammarName

    private fun writeToFile(fp: String, name: String, string: String) {
        val f = File(fp)
        if (!f.exists()) {
            f.mkdirs()
        }
        val file = File(f, name)
        file.writeText(string)
    }

    fun generate() {
        val tokenEnumClass = generateTokenEnum()
        val tokenClass = generateTokenClass()
        writeToFile("build/generated/$name/kotlin",
            "${name}TokenId.kt", tokenEnumClass)
        writeToFile("build/generated/$name/kotlin",
            "${name}Token.kt", tokenClass)
    }

    private fun generateTokenEnum(): String {
        val tokenEnumClass = TypeSpec.enumBuilder("${grammarVisitor.grammarName}TokenId")
        for ((k, v) in grammarVisitor.tokens) {
            tokenEnumClass.addEnumConstant(k, TypeSpec.anonymousClassBuilder()
                .addProperty(
                    PropertySpec.builder("isRegex", Boolean::class)
                        .initializer("%L", v.isRegex)
                        .build())
                .build())
        }
        tokenEnumClass.addEnumConstant("END")
        val file = FileSpec.builder("", "${grammarVisitor.grammarName}TokenId")
            .addType(tokenEnumClass.build())
            .build()
        return file.toString()
    }

    private fun generateTokenClass(): String {
        val futureType = ClassName("", "${name}TokenId")
        val tokenConstructor = FunSpec.constructorBuilder()
            .addParameter("tokenId", futureType)
            .addParameter("lexeme", String::class)
            .build()
        val tokenClass = TypeSpec.classBuilder("${name}Token")
            .primaryConstructor(tokenConstructor)
            .addProperty(
                PropertySpec.builder("tokenId", futureType)
                    .initializer("tokenId")
                    .build())
            .addProperty(
                PropertySpec.builder("lexeme", String::class)
                    .initializer("lexeme")
                    .build())
            .addFunction(
                FunSpec.builder("toString")
                    .addModifiers(KModifier.OVERRIDE)
                    .returns(String::class)
                    .addStatement("return \"\$tokenId : \$lexeme\"")
                    .build()
            )
        val file = FileSpec.builder("", "${name}TokenId")
            .addType(tokenClass.build())
            .build()
        return file.toString()
    }
}
