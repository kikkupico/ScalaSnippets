
object ArrayTreeTrial extends App {
  val lnOf2 = scala.math.log(2) // natural log of 2
  def log2(x: Double): Double = scala.math.log(x) / lnOf2

  def nodeDepth(n:Int):Int = Math.floor(log2(n+1)).toInt

  def treeDepth(tree:Vector[Int]):Int = nodeDepth(tree.length-1)

  def subtree(tree:Vector[Int], pos:Int):Vector[Int] = {
    if (tree.isEmpty) Vector()
    else {
      val childrenIndicesOfThis = children(tree, pos)
      val childrenOfThis = children(tree, pos)
      if (childrenOfThis.isEmpty) Vector()
      else childrenOfThis ++ childrenIndicesOfThis.flatMap( x => subtree(tree, x))
    }
  }

  def children(tree:Vector[Int], pos:Int):Vector[Int] = {
    val childrenIndicesOfThis = childrenIndices(tree, pos)
    tree.slice(childrenIndicesOfThis._1, childrenIndicesOfThis._2+1)
  }

  def childrenIndices(tree:Vector[Int], pos:Int):Tuple2[Int, Int] = (pos*2+1, pos*2+2)

  def childrenIndices(tree:Vector[Int], pos:Int, level:Int):Tuple2[Int, Int] = {
    val factor = Math.pow(2.0,level.toDouble).toInt
    (pos*factor+level+1, pos*factor+level+factor)
  }

  val tree = (0 to 18).toVector
  println(0 until 18 map (x => nodeDepth(x)))
  println(childrenIndices(tree, 2, 2))
}
