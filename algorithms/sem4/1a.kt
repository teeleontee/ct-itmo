private fun readInts() = readln().split(' ').map { it.toInt() }

fun dfs(v: Int,
        marked: ArrayList<Boolean>,
        graph: ArrayList<ArrayList<Int>>,
        pairs: HashMap<Int, Int>): Boolean {
    if (marked[v])
        return false
    marked[v] = true
    for (u in graph[v]) {
        if (!pairs.containsKey(u)) {
            pairs[u] = v
            return true
        }
        else if (dfs(pairs[u]!!, marked, graph, pairs)) {
            pairs[u] = v
            return true
        }
    }
    return false
}

fun main() {
    val (n, m) = readInts()
    val g = ArrayList<ArrayList<Int>>()

    for (i in 0 until n) {
        g.addAll(arrayListOf(readInts().filter { it != 0 } as ArrayList<Int>))
    }

    val mark = ArrayList<Boolean>()
    val pairs = HashMap<Int, Int>()

    for (i in 0 until n) {
        mark.add(false)
    }

    for (v in 0 until n) {
        mark.fill(false)
        dfs(v, mark, g, pairs)
    }

    println(pairs.keys.size)
    for ((l, r) in pairs) {
        println("${r+1} $l")
    }
}

