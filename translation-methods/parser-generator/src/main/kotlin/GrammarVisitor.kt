class GrammarVisitor :
    GrammarFileBaseVisitor<String>(), GrammarFileVisitor<String> {

    var tokens = mutableMapOf("EPS" to Terminal("EPS", isRegex = false))
        private set

    var grammarName: String = ""
        private set

    val nonTerminals = mutableMapOf<String, NonTerminal>()

    var startNonTerminal: String = ""

    override fun visitName(ctx: GrammarFileParser.NameContext?): String {
        if (ctx == null) {
            return defaultResult()
        }
        grammarName = ctx.VARNAME().toString()
        return super.visitName(ctx)
    }

    override fun visitStart(ctx: GrammarFileParser.StartContext?): String {
        if (ctx == null) {
            return defaultResult()
        }
        startNonTerminal = visitNonTerm(ctx.nonTerm())
        return super.visitStart(ctx)
    }

    override fun visitRule(ctx: GrammarFileParser.RuleContext?): String {
        if (ctx != null) {
            val name = visitNonTerm(ctx.nonTerm())
            var returns = ""
            if (ctx.attrSynParam() != null) {
                returns = visitType(ctx.attrSynParam().type())
            }
            val params = mutableListOf<Parameter>()
            if (ctx.attrInhParam() != null) {
                val varNames = ctx.attrInhParam().VARNAME().map { it.toString() }
                val varTypes = ctx.attrInhParam().type().map { visitType(it) }
                for (i in varNames.indices) {
                    params.add(Parameter(varNames[i], varTypes[i]))
                }
            }
            val productions = mutableListOf<Production>()
            ctx.productions().product().forEach {
                val elem = it.productElement().map { it2 -> visitProductElement(it2) }
                val ret = if (it.returnElement() != null) {
                    val retEl = it.returnElement()
                    when {
                        retEl.nonTerm() != null -> visitNonTerm(retEl.nonTerm())
                        retEl.VARNAME() != null -> retEl.VARNAME().toString()
                        retEl.CODE() != null -> retEl.CODE().toString()
                        else -> retEl.CODEPLUS().toString()
                    }
                } else null
                productions.add(Production(elem, ret))
            }
            nonTerminals[name] = NonTerminal(name, returns, params, productions)
        }
        return super.visitRule(ctx)
    }

    override fun visitProductElement(ctx: GrammarFileParser.ProductElementContext?): String {
        if (ctx == null) {
            return defaultResult()
        }
        return when {
            ctx.element() != null -> visitElement(ctx.element())
            ctx.CODE() != null -> ctx.CODE().toString()
            ctx.EPS() != null -> ctx.EPS().toString()
            else -> defaultResult()
        }
    }

    override fun visitNonTerm(ctx: GrammarFileParser.NonTermContext?): String {
        if (ctx == null) {
            return defaultResult()
        }
        return when {
            ctx.VARNAME() != null -> ctx.VARNAME().toString()
            else -> defaultResult()
        }
    }

    override fun visitElement(ctx: GrammarFileParser.ElementContext?): String {
        if (ctx == null) {
            return defaultResult()
        }
        return when {
            ctx.nonTerm() != null -> {
                val sb = StringBuilder()
                sb.append(visitNonTerm(ctx.nonTerm()))
                if (ctx.arguments() != null) {
                    sb.append("(")
                    ctx.arguments().argument().forEach {
                        sb.append(it.VARNAME().toString())
                        sb.append(", ")
                    }
                    sb.setLength(sb.length - 2)
                    sb.append(")")
                }
                return sb.toString()
            }
            ctx.LEXEME() != null -> ctx.LEXEME().toString()
            else -> defaultResult()
        }
    }

    override fun visitType(ctx: GrammarFileParser.TypeContext?): String {
        if (ctx == null) {
            return defaultResult()
        }
        return when {
            ctx.INT() != null -> ctx.INT().toString()
            ctx.STRING() != null -> ctx.STRING().toString()
            else -> super.visitType(ctx)
        }
    }

    override fun visitLexeme(ctx: GrammarFileParser.LexemeContext?): String {
        if (ctx != null) {
            val valto = ctx.STR()
            val regex = ctx.regex()
            val lexeme = ctx.LEXEME().toString()
            if (valto != null) {
                tokens[lexeme] = Terminal(valto.toString())
            }
            if (regex != null) {
                tokens[lexeme] = Terminal(regex.REG().toString().removeSurrounding("@").trim(), isRegex = true)
            }
        }
        return super.visitLexeme(ctx)
    }

    override fun defaultResult(): String = ""
}