import funsets.FunSets._
import funsets._


FunSets.contains(x => x>89, 100)

exists(x=>x>0 && x< 7, x => x > 10)

//contains(map(x=>x>0 && x< 7, x => x +10),16)

FunSets.toString(intersect(x=>x>0 && x< 7, x => x  < 10 && x> 2))

contains(map(x=>x>0 && x< 7,x=>2*x),0)


