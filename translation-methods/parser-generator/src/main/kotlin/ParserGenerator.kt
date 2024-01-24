
import com.squareup.kotlinpoet.*
import com.squareup.kotlinpoet.ParameterizedTypeName.Companion.parameterizedBy
import java.io.File
import java.util.*

class ParserGenerator(tree: GrammarFileParser.GrammarFileContext) {
    private val grammarVisitor = GrammarVisitor().also { it.visitGrammarFile(tree) }
    private val grName = grammarVisitor.grammarName
    private val nonTerminals = grammarVisitor.nonTerminals
    private val tokens = grammarVisitor.tokens
    private var first = mutableMapOf<String, MutableSet<String>>()
    private val follow = mutableMapOf<String, MutableSet<String>>()

    private fun String.cutArgs(): String {
        val sb = StringBuilder(this)
        if (sb.last() == ')') {
            while (sb.last() != '(') {
                sb.setLength(sb.length - 1)
            }
            sb.setLength(sb.length - 1)
        }
        return sb.toString()
    }

    private fun writeToFile(fp: String, name: String, string: String) {
        val f = File(fp)
        if (!f.exists()) {
            f.mkdirs()
        }
        val file = File(f, name)
        file.writeText(string)
    }

    fun generateParser() {
        generateAllNodes()
        constructFirstSet()
        constructFollowSet()
        val lexerClassType = ClassName("", "${grName}Lexer")
        val tokenClassType = ClassName("", "${grName}Token")
        val consume = generateConsume()
        val functions = generateFunctions()
        val parseFunction = generateParseFunction()
        val typeSpec = TypeSpec.classBuilder("${grName}Parser")
            .addProperty(
                PropertySpec.builder("lexer", lexerClassType)
                    .mutable()
                    .addModifiers(KModifier.PRIVATE)
                    .initializer("${grName}Lexer(\"\")")
                    .build())
            .addProperty(
                PropertySpec.builder("curToken", tokenClassType)
                    .mutable()
                    .addModifiers(KModifier.PRIVATE)
                    .initializer("lexer.scan()")
                    .build())
            .addFunction(consume)
            .addFunction(parseFunction)
       functions.forEach {
           typeSpec.addFunction(it)
       }
        val file = FileSpec.builder("", "${grName.capitalize()}Parser")
            .addType(typeSpec.build())
            .build()
        writeToFile("build/generated/$grName/kotlin", "${grName.capitalize()}Parser.kt", file.toString())
    }

    private fun generateParseFunction(): FunSpec {
        val returnStmt = "return visit${grammarVisitor.startNonTerminal.capitalize()}()"
        val codeBlock = """
            lexer = ${grName.capitalize()}Lexer(strToParse)
            curToken = lexer.scan()
            $returnStmt
        """.trimIndent()
        val returns = ClassName("",
            "${grammarVisitor.startNonTerminal.capitalize()}Node")
        val funSpec = FunSpec.builder("parse")
            .addParameter("strToParse", String::class)
            .addCode(codeBlock)
            .returns(returns)
        return funSpec.build()
    }

    private fun String.capitalize(): String {
        return this.replaceFirstChar {
            if (it.isLowerCase()) it.titlecase(Locale.getDefault()) else it.toString()
        }
    }

    private fun getCorrectProduction(c: String, nt: NonTerminal): Production {
        for (rule in nt.productions) {
            val ruleStr = rule.production
            val firstSet = first[ruleStr[0]]
            if (firstSet != null && firstSet.contains(c)) {
                return rule
            }
        }
        return nt.productions[0]
    }

    private fun generateConsume(): FunSpec {
        val tokenIdClass = ClassName("", "${grName.capitalize()}TokenId")
        val expected = "\$c"
        val got = "\${curToken.tokenId}"
        val codeBlock = """
            if (curToken.tokenId == c) {
                curToken = lexer.scan()
            } else throw Exception("Error parsing: Expected $expected but got $got")
        """.trimIndent()
        val funSpec = FunSpec.builder("consume")
            .addModifiers(KModifier.PRIVATE)
            .addParameter(
                ParameterSpec.builder("c", tokenIdClass)
                .build())
            .addCode(codeBlock)
            .build()
        return funSpec
    }

    private fun generateFunctions(): List<FunSpec> {
        val res = mutableListOf<FunSpec>()
        for ((str, nt) in nonTerminals) {
            val nameClass = "visit" + str.capitalize()
            val returnClassName = ClassName("", "${str.capitalize()}Node")
            val funSpec = FunSpec.builder(nameClass)
                .returns(returnClassName)
            if (nt.params != null) {
                for (param in nt.params) {
                    val typeClass = ClassName("", param.type)
                    funSpec.addParameter(
                        ParameterSpec.builder(param.name, typeClass).build())
                }
            }
            funSpec.addStatement("val node = ${str.capitalize()}Node(\"$str\")")
                .beginControlFlow("when (curToken.tokenId)")
            val firstSet = first[str]
            var flag = false
            if (firstSet != null) {
                for (c in firstSet) {
                    val production = getCorrectProduction(c, nt)
                    if (c == "EPS") {
                        funSpec.beginControlFlow("else ->")
                        flag = true
                    } else {
                        funSpec.beginControlFlow("${grName.capitalize()}TokenId.$c -> ")
                    }
                    for (elem in production.production) {
                        if (tokens[elem] != null && elem != "EPS") {
                            funSpec.addStatement("consume(${grName.capitalize()}TokenId.${elem})")
                            funSpec.addStatement("node.addChild(${grName.capitalize()}Node(\"$elem\"))")
                        } else {
                            if (elem.first() == '{' && elem.last() == '}') {
                                val codeBlock = elem.substring(1, elem.length - 1)
                                    .trimIndent()
                                    .replace("it", "node")
                                    .replace("[A-Z]+.value".toRegex(), "curToken.lexeme")
                                funSpec.addStatement(codeBlock)
                            } else {
                                when {
                                    elem.cutArgs() == elem -> {
                                        val ntt = nonTerminals[elem] // no args
                                        if (ntt != null) {
                                            funSpec.addStatement("val ${ntt.name} = visit${ntt.name.capitalize()}()")
                                            funSpec.addStatement("node.addChild(${ntt.name})")
                                        }
                                    }
                                    else -> {
                                        val ntt = nonTerminals[elem.cutArgs()]
                                        if (ntt != null) {
                                            funSpec.addStatement("val ${ntt.name} = visit${elem.capitalize()}")
                                            funSpec.addStatement("node.addChild(${ntt.name})")
                                        }
                                    }
                                }
                            }
                        }
                    }
                    funSpec.endControlFlow()
                }
                if (!flag) {
                    funSpec.addStatement("else -> throw Exception(\"Unexpected Char\")")
                }
                funSpec.endControlFlow()
            }
            funSpec.addStatement("return node")
            res.add(funSpec.addModifiers(KModifier.PRIVATE).build())
        }
        return res
    }

    private fun generateAllNodes() {
        val classes = generateNonTermNodeClass()
        val nodeClass = generateNodeClass()
        writeToFile("build/generated/$grName/kotlin", "${grName.capitalize()}Nodes.kt", classes)
        writeToFile("build/generated/$grName/kotlin", "${grName.capitalize()}Node.kt", nodeClass)
    }

    private fun generateNonTermNodeClass(): String {
        val file = FileSpec.builder("", "${grName}Nodes")
        for ((str, nt) in nonTerminals) {
            val nameClass = str.replaceFirstChar {
                if (it.isLowerCase()) it.titlecase(Locale.getDefault()) else it.toString()
            } + "Node"
            val nodeName = ClassName("", "${grName.capitalize()}Node")
            val constructor = FunSpec.constructorBuilder()
                .addParameter("name", String::class)
                .build()
            val nodeClass = TypeSpec.classBuilder(nameClass)
                .primaryConstructor(constructor)
                .superclass(nodeName)
                .addSuperclassConstructorParameter("name")
            if (nt.returns != null && nt.returns != "") {
                val ret = ClassName("", nt.returns)
                nodeClass.addProperty(
                    PropertySpec.builder("attr", ret.copy(nullable = true))
                        .mutable()
                        .initializer("null")
                        .build())
            }
            file.addType(nodeClass.build())
        }
        return file.build().toString()
    }

    private fun generateNodeClass(): String {
        val nodeListType = ClassName("", "${grName.capitalize()}Node")
        val nodeType = ClassName("", "${grName.capitalize()}Node")
        val codeBlock = """
            children += node
        """.trimIndent()
        val constructor = FunSpec.constructorBuilder()
            .addParameter("name", String::class)
            .build()
        val nodeClass = TypeSpec.classBuilder("${grName.capitalize()}Node")
            .primaryConstructor(constructor)
            .addModifiers(KModifier.OPEN)
            .addProperty(
                PropertySpec.builder("name", String::class)
                    .initializer("name")
                    .build())
            .addProperty(
                PropertySpec.builder("children", MUTABLE_LIST.parameterizedBy(nodeListType))
                    .initializer("mutableListOf()")
                    .build())
            .addFunction(
                FunSpec.builder("addChild")
                    .addParameter(
                        ParameterSpec.builder("node", nodeType).build())
                    .addCode(codeBlock)
                    .build())
            .build()
        val file = FileSpec.builder("", "${grName.capitalize()}Node")
            .addType(nodeClass)
            .build()
        return file.toString()
    }

    private fun constructFirstSet() {
        for ((str, _) in tokens) {
            first[str] = mutableSetOf(str)
        }
        for ((str, _) in nonTerminals) {
            first[str] = mutableSetOf()
        }
        var changed = true
        while (changed) {
            changed = false
            for ((str, nt) in nonTerminals) {
                for (rule in nt.productions) {
                    val prod = rule.production
                    var j = 0
                    while (prod[j].contains("{")) {
                        j++
                    }
                    var i = j
                    while (prod.size != 1 && first[prod[i].cutArgs()]?.contains("EPS") == true ) {
                        i++
                    }
                    if (prod.contains("EPS")) {
                        first[str]?.add("EPS")
                    }
                    val firstAlpha = first[prod[i].cutArgs()]
                    val beforeLength = first[str]?.size
                    if (firstAlpha != null) {
                        first[str]?.addAll(firstAlpha)
                    }
                    val afterLength = first[str]?.size
                    if (beforeLength != afterLength) {
                        changed = true
                    }
                }
            }
        }
    }

    private fun constructFollowSet() {
        for ((str, _) in tokens) {
            follow[str] = mutableSetOf()
        }
        for ((str, _) in nonTerminals) {
            follow[str] = mutableSetOf()
        }
        follow[grammarVisitor.startNonTerminal] = mutableSetOf("$")
        var changed = true
        while (changed) {
            changed = false
            for ((str, nt) in nonTerminals) {
                for (rule in nt.productions) {
                    val prod = rule.production
                    for (i in 1..prod.size) {
                        val followStr = follow[str]
                        if (i == prod.size && followStr != null) {
                            follow[prod[i - 1].cutArgs()]?.addAll(followStr)
                            continue
                        }
                        val b = prod[i - 1].cutArgs(); val c = prod[i].cutArgs()
                        if (tokens.contains(b)) {
                            continue
                        }
                        val gamma = first[c]
                        val before = follow[b]?.size
                        if (gamma != null) {
                            follow[b]?.addAll(gamma)
                            follow[b]?.remove("EPS")
                            if (gamma.contains("EPS") && followStr != null) {
                                follow[b]?.addAll(followStr)
                            }
                        }
                        val after = follow[b]?.size
                        if (before != after) {
                            changed = true
                        }
                    }
                }
            }
        }
    }
}