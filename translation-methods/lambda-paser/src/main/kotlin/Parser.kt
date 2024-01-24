import java.io.File
import java.nio.charset.StandardCharsets

/**
* Provides an easy api for parsing strings,
* just a wrapper around ParserHelper
*/
class Parser {

    /**
     * Parses the input string.
     * @return The root node of the parse tree for the input string
     * and may return null if the string is not a part of the given grammar
     */
    fun parse(string: String): Node? {
        val parserHelper = ParserHelper(string)
        try {
            val correct = parserHelper.parse()
            println("\"$string\" belongs to the grammar! :)")
            return correct
        } catch (e: Exception) {
            println("$string does not belong to the grammar\n $e")
        }
        return null
    }

    private fun isToken(s: String): Boolean {
        return when (s.uppercase()) {
            "CONST", "ID", "(", ")", "+", "-", "/", "*", "LAMBDA", ",", ":" -> true
            else -> false
        }
    }

    /**
     * Takes in a node and generates a .dot file containing the parse tree
     * @param graph the head of the parse tree
     */
    fun createGraph(graph: Node) {
        val fileName = "out/graph.dot"
        val file = File(fileName)
        file.bufferedWriter(StandardCharsets.UTF_8).use { out ->
            out.write("digraph {\n")
            val stack = ArrayDeque<Node>()
            stack.addLast(graph)
            while (!stack.isEmpty()) {
                val cur = stack.removeLast()
                cur.children.forEach {
                    if (isToken(cur.nodeName)) {
                        out.write("\t${cur.hashCode()}[label=\"${cur.nodeName}\", color=red]\n")
                    } else {
                        out.write("\t${cur.hashCode()}[label=\"${cur.nodeName}\"]\n")
                    }
                    if (isToken(it.nodeName)) {
                        out.write("\t${it.hashCode()}[label=\"${it.nodeName}\", color=red]\n")
                    } else {
                        out.write("\t${it.hashCode()}[label=\"${it.nodeName}\"]\n")
                    }
                    out.write("\t\"${cur.hashCode()}\" -> \"${it.hashCode()}\"\n")
                    if (cur.children.size != 0) {
                        stack.addLast(it)
                    }
                }
            }
            out.write("}\n")
        }
    }
}