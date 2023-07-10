import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import java.util.Collections

private val c = ArrayList<ArrayList<Int>>()
private val u = ArrayList<Int>()
private val v = ArrayList<Int>()
private val p = ArrayList<Int>()
private val way = ArrayList<Int>()

private const val inf = 100000000

private fun init(n: Int) {
    (0..n + 1).forEach { _ ->
        u.add(0)
        v.add(0)
        p.add(0)
        way.add(0)
    }
}

private fun checkUsed(
    n: Int,
    used: ArrayList<Boolean>,
    delta: Int,
    minv: ArrayList<Int>
) {
    (0..n).forEach {
        if (used[it]) {
            u[p[it]] += delta
            v[it] -= delta
        } else {
            minv[it] -= delta
        }
    }
}

private fun vengerka(n: Int) {
    var index = 1
    (1..n).forEach { _ ->
        p[0] = index++
        var last = 0
        val minv = ArrayList<Int>(Collections.nCopies(n + 1, inf))
        val used = ArrayList<Boolean>(Collections.nCopies(n + 1, false))
        while (true) {
            if (p[last] == 0) {
                break
            }
            used[last] = true
            val i0 = p[last]
            var delta = inf
            var j1 = 0
            (1..n).forEach {
                if (!used[it]) {
                    val current = c[i0][it] - u[i0] - v[it]
                    if (current < minv[it]) {
                        minv[it] = current
                        way[it] = last
                    }
                    if (minv[it] < delta) {
                        delta = minv[it]
                        j1 = it
                    }
                }
            }
            checkUsed(n, used, delta, minv)
            last = j1
        }

        while (last != 0) {
            p[last] = p[way[last]].also { last = way[last] }
        }
    }
}

private fun readFileAsTextUsingInputStream(fileName: String)
        = Files.readAllLines(Paths.get("assignment.in"))



fun main() {
    val m = readFileAsTextUsingInputStream("assignment.in")

    val n = m[0].toInt()
    val empty = ArrayList<Int>(Collections.nCopies(n + 1, 0))
    c.add(empty)

    (1 until m.size).forEach {
        val list = (arrayListOf(0) + m[it].split(' ').map { iter -> iter.toInt() }).toCollection(ArrayList())
        c.add(list)
    }

    init(n)
    vengerka(n)
    val ans = ArrayList<Int>(Collections.nCopies(n + 1, 0))
    (1..n).forEach {
        ans[p[it]] = it
    }

    File("assignment.out").printWriter().use { out ->
        out.println("${-v[0]}")
        (1 until ans.size).forEach {
            out.println("$it ${ans[it]}")
        }
    }
}


