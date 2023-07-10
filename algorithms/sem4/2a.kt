import java.util.LinkedList
import kotlin.math.min

private data class Edge(val from: Int, val to: Int, var flow: Int, val capacity: Int)

private const val inf = 100000000

private val g = ArrayList<ArrayList<Int>>()
private val edges = ArrayList<Edge>()
private val distance = ArrayList<Int>()
private val used = ArrayList<Boolean>()

private fun readInts() = readln().split(' ').map { it.toInt() }
private fun readInt() = readln().toInt()

private fun bfs(end: Int): Boolean {
    used.fill(false)
    distance.fill(-1)
    distance[0] = 0
    val q = LinkedList<Int>().also { it.add(0) }
    while (distance[end] == -1 && !q.isEmpty()) {
        val cur = q.poll()
        for (v in g[cur]) {
            val next = edges[v].to
            if (edges[v].flow < edges[v].capacity && distance[next] == -1) {
                distance[next] = distance[cur] + 1
                if (!used[next]) {
                    q.add(next)
                }
            }
        }
    }
    return distance[end] != -1
}

private fun dfs(u: Int, minDelta: Int, end: Int): Int {
    if (u == end || minDelta == 0) {
        return minDelta
    }
    if (used[u]) {
        return minDelta
    }
    used[u] = true
    for (index in g[u]) {
        val cur = edges[index].to
        val diff = edges[index].capacity - edges[index].flow
        if (distance[cur] == distance[u] + 1 && !used[cur]) {
            val delta = dfs(cur, min(diff, minDelta), end)
            if (delta > 0) {
                edges[index].flow += delta
                edges[index xor 1].flow -= delta
                return delta
            }
        }
    }
    return 0
}


fun main() {
    val n = readInt()
    val m = readInt()

    (0..n).forEach { _ -> g.add(arrayListOf()) }

    (0 until m).forEach { _ ->
        val (u, v, c) = readInts()
        g[u - 1].add(edges.size)
        edges.add(Edge(u - 1, v - 1, 0, c))
        g[v - 1].add(edges.size)
        edges.add(Edge(v - 1, u - 1, 0, c))
    }

    (0 until n).forEach { _ ->
        used.add(false)
        distance.add(-1)
    }

    var flow = 0
    while (true) {
        if (!bfs(n - 1)) {
            break
        }
        while (true) {
            used.fill(false)
            val delta = dfs(0, inf, n - 1)
            flow += delta
            if (delta == 0) {
                break
            }
        }
    }
    println(flow)
    for (i in 0 until m * 2 step 2) {
        println(edges[i].flow)
    }
}

