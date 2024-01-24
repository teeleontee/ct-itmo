
/**
 * src.ParserHelper parses a given string, for this
 * contains a lexer that returns tokens, uses recursive
 * descent that optimizes by using Tail Recursion
 * @param string The input to be parsed
 */
class ParserHelper(private val string: String) {
    private var lexer = Lexer(string)
    private var curToken: Token

    init {
        curToken = lexer.scan()
    }

    private fun match(t: TokenId) {
        if (curToken.tag == t) {
            curToken = lexer.scan()
        }
        else throw Exception("Error Parsing: Expected $t, but got ${curToken.tag}")
    }

    private fun start(): Node {
        match(TokenId.LAMBDA)
        val paramsNode = params()
        match(TokenId.COLON)
        val exprNode = expr()
        return Node("S",
            arrayListOf(
                Node("lambda"),
                Node("P", paramsNode),
                Node(":"),
                exprNode)
        )
    }

    private fun params(): ArrayList<Node> {
        match(TokenId.ID)
        val params2Node = params2()
        return arrayListOf(Node("ID"), params2Node)
    }

    private fun params2(): Node {
        var p2 = Node("P'")
        val rootP2 = p2
        while (true) {
            when (curToken.tag) {
                TokenId.COMMA -> {
                    p2.addChild(Node(","))
                    curToken = lexer.scan()
                    match(TokenId.ID)
                    p2.addChild(Node("ID"))
                }
                else -> {
                    p2.addChild(Node("eps"))
                    break
                }
            }
            p2.addChild(Node("P'"))
            p2 = p2.children[2]
        }
        return rootP2
    }

    private fun expr(): Node {
        val t = term()
        var es = Node("E'")
        val rootEs = es
        while (true) { // inlined expr2
            when (curToken.tag) {
                TokenId.PLUS -> {
                    curToken = lexer.scan()
                    es.addChild(Node("+"))
                    val trm = term()
                    es.addChild(trm)
                }
                TokenId.MINUS -> {
                    curToken = lexer.scan()
                    es.addChild(Node("-"))
                    val trm = term()
                    es.addChild(trm)
                }
                else -> {
                    es.addChild(Node("eps"))
                    break
                }
            }
            es.addChild(Node("E'"))
            es = es.children[2]
        }
        return Node("E", arrayListOf(t, rootEs))
    }

    private fun term(): Node {
        val fr = factor()
        var trm = Node("T'")
        val rootTrm = trm
        while (true) { // inlined term2()
            when (curToken.tag) {
                TokenId.TIMES -> {
                    curToken = lexer.scan()
                    trm.addChild(Node("*"))
                    val fct = factor()
                    trm.addChild(fct)
                }
                TokenId.DIVIDE -> {
                    trm.addChild(Node("/"))
                    curToken = lexer.scan()
                    val fct = factor()
                    trm.addChild(fct)
                }
                else -> {
                    trm.addChild(Node("eps"))
                    break
                }
            }
            trm.addChild(Node("T'"))
            trm = trm.children[2]
        }
        return Node("T", arrayListOf(fr, rootTrm))
    }

    private fun factor(): Node {
        val f = Node("F")
        when (curToken.tag) {
            TokenId.LPAREN -> {
                curToken = lexer.scan()
                val f2 = factor2()
                f.addChild(f2)
            }
            TokenId.ID -> {
                curToken = lexer.scan()
                f.addChild(Node("ID"))
            }
            TokenId.CONST -> {
                curToken = lexer.scan()
                f.addChild(Node("CONST"))
            }
            else ->
                throw Exception("Error in procedure F -> num | id | ( expr ), got ${curToken.tag}")
        }
        return f
    }

    // modification
    private fun factor2(): Node {
        val f2 = Node("F'")
        when (curToken.tag) {
            TokenId.LAMBDA -> {
                val s2 = start()
                match(TokenId.RPAREN)
                match(TokenId.LPAREN)
                val p2 = params()
                match(TokenId.RPAREN)
                f2.addChildren(arrayListOf(Node("("),
                    s2,
                    Node(")"),
                    Node("(")))
                f2.addChildren(p2)
                f2.addChildren(arrayListOf( Node(")")))
            }
            TokenId.LPAREN, TokenId.CONST, TokenId.ID -> {
                val e = expr()
                match(TokenId.RPAREN)
                f2.addChildren(arrayListOf(Node("("), e, Node(")")))
            }
            else ->
                throw Exception("Error in procedure F -> num | id | ( expr ), got ${curToken.tag}")
        }
        return f2
    }

    /**
     * Parses the string given when the class is initialized
     * @return Node if the string belongs to the grammar
     * @throws Exception if the string cannot be parsed
     */
    fun parse(): Node {
        val s = start()
        match(TokenId.END)
        return s
    }
}
