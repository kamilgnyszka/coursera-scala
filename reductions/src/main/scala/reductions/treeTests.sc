import reductions.LineOfSight

val t = LineOfSight.upsweep(Array(0,7,10,33,48),0,5,1)

val input = Array(0f,7f,10f,33f,48f,55f,90f)

var o = new Array[Float](input.length)
LineOfSight.parLineOfSight(input,o,2)

o.mkString(" ")