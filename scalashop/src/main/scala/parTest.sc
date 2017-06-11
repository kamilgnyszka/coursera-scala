val width = 101
val numTasks = 4

val ranges = Math.ceil(width.toDouble/numTasks.toDouble).toInt

val r = 0 to width by ranges
val rAdj = if (r.last != width) r.toList:::List(width) else r.toList
val taskRanges = (rAdj) zip ((rAdj).tail)

