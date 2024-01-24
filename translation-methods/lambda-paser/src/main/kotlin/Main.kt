fun main() {
    // val inp = "lambda x, y : x + ( lambda a, b : a + b ) (x, y)"
    val inp = readln()
    val parser = Parser()
    parser.parse(inp).also { it?.let { it1 -> parser.createGraph(it1) } }
}
