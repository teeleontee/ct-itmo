class Node(val nodeName: String, var children: ArrayList<Node>) {
    constructor(nodeName: String) : this(nodeName, arrayListOf()){
    }

    fun addChildren(children: List<Node>) {
        this.children.addAll(children)
    }

    fun addChild(child: Node) {
        this.children.add(child)
    }
}
