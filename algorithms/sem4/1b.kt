import java.util.*
import kotlin.collections.ArrayList
import kotlin.collections.HashMap

private fun readInts() = readln().split(' ').map { it.toInt() }

fun dfs1(v: Int,
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
        else if (dfs1(pairs[u]!!, marked, graph, pairs)) {
            pairs[u] = v
            return true
        }
    }
    return false
}

fun dfs2(v: Int,
         g: ArrayList<ArrayList<Int>>,
         marked: ArrayList<Boolean>,
         out: HashMap<Int, Int>,
         new: HashMap<Int, Int>,
         lPlus: TreeSet<Int>,
         rPlus: TreeSet<Int>) {

    if (v - 1 < 0 || v - 1 > marked.size || v - 1 > g.size) {
        return
    }

    if (marked[v - 1]) {
        return
    }
    marked[v - 1] = true
    lPlus.add(v)
    for (u in g[v - 1]) {
        if (new[v] != u) {
            rPlus.add(u)
            if (out.containsKey(u))
                dfs2(out[u]!! + 1, g, marked, out, new, lPlus, rPlus)

        }
    }
}

fun inverseGraph(graph: ArrayList<ArrayList<Int>>, m: Int): ArrayList<ArrayList<Int>> {
    val newGraph = ArrayList<ArrayList<Int>>()
    graph.forEach { iter ->
        val hm = HashMap<Int, Boolean>()
        val temp = ArrayList<Int>()
        iter.forEach {
            hm[it] = true
        }
        (1..m).forEach {
            if (!hm.containsKey(it)) {
                temp.add(it)
            }
        }
        newGraph.add(temp)
    }
    return newGraph
}

fun main() {
    val k = readln().toInt()
    for (i in 0 until k) {
        val (n, m) = readInts()
        val tempG = ArrayList<ArrayList<Int>>()
        for (j in 0 until n) {
            tempG.addAll(arrayListOf(readInts().filter { it != 0 } as ArrayList<Int>))
        }
        val g = inverseGraph(tempG, m)
        val mark = ArrayList<Boolean>()
        val pairs = HashMap<Int, Int>()
        for (j in 0 .. n) {
            mark.add(false)
        }
        for (v in 0 until n) {
            mark.fill(false)
            dfs1(v, mark, g, pairs)
        }
        val lPlus = TreeSet<Int>()
        val rPlus = TreeSet<Int>()
        val newPairs = HashMap<Int, Int>()
        for ((l, r) in pairs) {
            newPairs[r + 1] = l
        }
        for (j in 1..n) {
            if (newPairs.containsKey(j)) {
                continue
            }
            lPlus.add(j)
        }
        mark.fill(false)
        val tmp = lPlus.clone()
        for (u in tmp as TreeSet<Int>) {
            dfs2(u , g, mark, pairs, newPairs, lPlus, rPlus)
        }
        val rMinus = (1..m).filter { !rPlus.contains(it) }
        println(lPlus.size + rMinus.size)
        println("${lPlus.size} ${rMinus.size}")
        lPlus.forEach { print("$it ") }
        println()
        rMinus.forEach { print("$it ") }
        println()
    }
}

