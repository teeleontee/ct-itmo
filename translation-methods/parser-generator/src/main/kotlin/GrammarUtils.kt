class Terminal(val name: String, val isRegex: Boolean = false) {
    override fun toString(): String = name + isRegex.toString()
}

class NonTerminal(val name: String,
                  val returns: String?,
                  val params: List<Parameter>?,
                  val productions: List<Production>) {
    override fun toString(): String = "rule $name ($params) -> $returns : $productions"
}

class Parameter(val name: String, val type: String) {
    override fun toString(): String = "$name : $type"
}

class Production(val production: List<String>, val returns: String?) {
    override fun toString(): String {
        val sb = StringBuilder()
        production.forEach {
            sb.append("$it ")
        }
        if (returns != null) {
            sb.append("returns : $returns")
        }
        return sb.toString()
    }
}
