class PyVisitor : PythonBaseVisitor<String>(), PythonVisitor<String> {

    private fun isNumericToX(toCheck: String): Boolean {
        return toCheck.toDoubleOrNull() != null
    }

    private var variables = HashMap<String, Type>()

    private var prefix = "\t"

    override fun defaultResult(): String {
        return ""
    }

    override fun aggregateResult(aggregate: String?, nextResult: String?): String {
        return aggregate + nextResult
    }

    override fun visitFileStatement(ctx: PythonParser.FileStatementContext?): String {
        return "#include <stdio.h>\n\n" +
                "int main() " +
                "{\n${visitChildren(ctx)}\treturn 0;\n}\n"
    }

    override fun visitStmt(ctx: PythonParser.StmtContext?): String {
        return visitChildren(ctx)
    }

    override fun visitForStmt(ctx: PythonParser.ForStmtContext?): String {
        val variable = ctx?.VAR()
        val factors = ctx?.expr()?.map { visitExpr(it) }
        prefix += "\t"
        val suite = visitSuite(ctx?.suite())
        prefix = prefix.dropLast(1)
        return prefix + "for " + when (factors?.size) {
            1 -> "(int $variable = 0; $variable < ${factors[0]}; $variable++) "
            2 -> "(int $variable = ${factors[0]}; $variable < ${factors[1]}; $variable++) "
            3 -> "(int $variable = ${factors[0]}; $variable < ${factors[1]}; $variable += ${factors[2]}) "
            else -> {}
        } + "{\n$suite$prefix}\n"
    }

    override fun visitIfStmt(ctx: PythonParser.IfStmtContext?): String {
        val boolExpr = ctx?.boolExpr()?.map { visitBoolExpr(it) }
        var result = ""
        ctx?.suite()?.forEachIndexed { index, suiteContext ->
            if (index == 0) {
                prefix += "\t"
                val ifSuite = visitSuite(suiteContext)
                prefix = prefix.dropLast(1)
                result += "if (${boolExpr?.get(index)}) {\n$ifSuite$prefix}\n"
            }
            if (ctx.suite().size > 1 && index != 0) {
                if (ctx.suite().size - 1 == index) {
                    prefix += "\t"
                    val elseSuite = visitSuite(suiteContext)
                    prefix = prefix.dropLast(1)
                    result += "${prefix}else {\n$elseSuite$prefix}\n"
                } else {
                    prefix += "\t"
                    val elifSuite = visitSuite(suiteContext)
                    prefix = prefix.dropLast(1)
                    result += "${prefix}else if (${boolExpr?.get(index)}) " +
                            "{\n$elifSuite$prefix}\n"
                }
            }
        }
        return prefix + result
    }

    override fun visitExpr(ctx: PythonParser.ExprContext?): String {
        if (ctx?.PLUS() == null && ctx?.MINUS() == null) {
            return visitChildren(ctx)
        }
        val lhs = visitExpr(ctx.expr())
        val rhs = visitTerm(ctx.term())
        return if (ctx.PLUS() != null) {
            "$lhs + $rhs"
        } else {
            "$lhs - $rhs"
        }
    }

    override fun visitTerm(ctx: PythonParser.TermContext?): String {
        if (ctx?.TIMES() == null && ctx?.DIVIDE() == null) {
            return visitChildren(ctx)
        }
        val lhs = visitTerm(ctx.term())
        val rhs = visitFactor(ctx.factor())
        return if (ctx.TIMES() != null) {
            "$lhs * $rhs"
        } else {
            "$lhs / $rhs"
        }
    }

    override fun visitFactor(ctx: PythonParser.FactorContext?): String {
        if (ctx?.NUM() != null) {
            return ctx.NUM().toString()
        }
        if (ctx?.VAR() != null) {
            val size = ctx.VAR().size
            return when (size) {
                1 -> {
                    ctx.VAR()[0].toString()
                }
                2 -> {
                    val fst = ctx.VAR()[0].toString()
                    val snd = ctx.VAR()[1].toString()
                    val type = variables[fst]
                    when (type) {
                        Type.STRING -> variables["$fst[$snd]"] = Type.CHAR
                        Type.INTARR -> variables["$fst[$snd]"] = Type.INT
                        Type.STRARR -> {
                            variables["$fst[$snd]"] = Type.STRING
                        }
                        Type.INT -> println("undefined")
                        Type.CHAR -> println("undefined")
                        null -> println("undefined")
                    }
                    "$fst[$snd]"
                }
                else -> ""
            }
        }
        return visitChildren(ctx)
    }

    override fun visitBoolExpr(ctx: PythonParser.BoolExprContext?): String {
        return if (ctx?.BOOL() != null) {
            ctx.BOOL().toString()
        } else {
            val op = ctx?.BOOLOP()
            val vis = ctx?.VAR()
            val num = ctx?.NUM()
            return when {
                num?.size == 2 -> "${num[0]} $op ${num[1]}"
                vis?.size == 2 -> "${vis[0]} $op ${vis[1]}"
                else -> "${num?.get(0)} $op ${vis?.get(0)}"
            }
        }
    }

    override fun visitSuite(ctx: PythonParser.SuiteContext?): String {
        return visitChildren(ctx)
    }

    override fun visitPrintStmt(ctx: PythonParser.PrintStmtContext?): String {
        val str = ctx?.STR()
        val expr = ctx?.expr()
        return if (str != null) {
            "${prefix}printf(\"%s\", $str);\n"
        } else {
            val expression = visitExpr(expr) // !
            when {
                isNumericToX(expression) -> {
                    "${prefix}printf(\"%d\", $expression);\n"
                }
                else -> {
                    val type = variables[expression]
                    "${prefix}printf(\"" + when (type) {
                        Type.INT -> "%d"
                        Type.STRING -> "%s"
                        Type.CHAR -> "%c"
                        Type.INTARR -> "%d"
                        Type.STRARR -> "%p"
                        null -> "%s"
                    } + "\", $expression);\n"
                }
            }
        }
    }

    override fun visitAssignStmt(ctx: PythonParser.AssignStmtContext?): String {
        val variable = ctx?.VAR().toString()
        val expr = ctx?.expr()
        val strCtx = ctx?.STR()
        val inp = ctx?.inputStmt()
        val assign = ctx?.ASSIGN().toString()
        println("in assignment $variable $strCtx")
        if (expr != null) {
            val rightVar = visitExpr(expr)
            if (variables[rightVar] == Type.STRING) {
                return if (!variables.containsKey(variable)) {
                    variables[variable] = Type.STRING
                    prefix + "char* $variable $assign $rightVar;\n"
                } else {
                    "$prefix$variable $assign $rightVar;\n"
                }
            }
            return if (!variables.containsKey(variable)) {
                variables[variable] = Type.INT
                prefix + "int $variable $assign $rightVar;\n"
            } else {
                "$prefix$variable $assign $rightVar;\n"
            }
        } else if (strCtx != null && strCtx.size == 1) {
            val str = ctx.STR().toString()
            return if (!variables.containsKey(variable)) {
                variables[variable] = Type.STRING
                prefix + "char* $variable $assign $str;\n"
            } else {
                "$prefix$variable $assign $str;\n"
            }
        } else if (strCtx != null && strCtx.size != 0) {
            var res = prefix
            if (!variables.containsKey(variable)) {
                variables[variable] = Type.STRARR
                println(variable)
                println("zzzz")
                res += "char* $variable[${strCtx.size}] = { "
            } else {
                res += "$variable = { "
            }
            strCtx.forEachIndexed { index, node ->
                res += if (index == strCtx.size - 1) {
                    "$node };\n"
                } else {
                    "$node ,"
                }
            }
            return res
        }
        else if (inp != null) {
            val isInt = ctx.INT()
            return if (isInt == null) {
                "${prefix}char $variable[];\n" +
                        "${prefix}scanf(\"%s\", $variable);\n"
            } else {
                "${prefix}int $variable;\n" +
                        "${prefix}scanf(\"%d\", &$variable);\n"
            }
        } else {
            var res = prefix
            if (!variables.containsKey(variable)) {
                println("inarrayasdasd")
                variables[variable] = Type.INTARR
                res += "int $variable[${ctx?.NUM()?.size}] = { "
            } else {
                res += "$variable[] = { "
            }
            ctx?.NUM()?.forEachIndexed { index, node ->
                res += if (index == ctx.NUM().size - 1) {
                    "$node };\n"
                } else {
                    "$node, "
                }
            }
            return res
        }
    }
}