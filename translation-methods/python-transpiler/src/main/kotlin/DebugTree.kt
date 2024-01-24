import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.tree.RuleNode
import org.antlr.v4.runtime.tree.TerminalNode

class DebugTree(private val tree: ParseTree) {
    override fun toString() = treeString(tree, "")

    private fun treeString(node: ParseTree, prefix: String): String {
        return when {
            node is PythonParser.FileStatementContext && node.childCount == 1 -> visitPrimary(node)
            node is TerminalNode -> visitTerminal(node)
            node !is RuleNode -> ""
            else -> {
                val name = PythonParser.ruleNames[node.ruleContext.ruleIndex]
                val builder = StringBuilder(name)
                for (i in 0..< node.childCount) {
                    val atEnd = (i == node.childCount - 1)
                    val symbol = if(atEnd) "└──" else "├──"
                    val child = node.getChild(i)
                    val childSymbol = if(atEnd) " " else "│"
                    val childStr = treeString(child, "$prefix$childSymbol   ")
                    builder.append("\n$prefix$symbol $childStr")
                }
                "$builder"
            }
        }
    }

    private fun visitPrimary(node: PythonParser.FileStatementContext): String {
        val name = PythonParser.ruleNames[node.ruleContext.ruleIndex]
        val childStr = visitTerminal(node.getChild(0) as TerminalNode)
        return "$name ── $childStr"
    }

    private fun visitTerminal(node: TerminalNode): String {
        if(node.symbol.type < 1) return "P'$node'"
        val id = PythonLexer
            .ruleNames[node.symbol.type - 1]
            .let { if("T__" in it) 'P' else it[0] }

        return "$id'$node'"
    }
}
