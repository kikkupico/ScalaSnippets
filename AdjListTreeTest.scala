import scala.collection.mutable.Queue

object AdjListTreeTest extends App {
  type BinaryTree = Map[Int, (Int,Int)]
  type BinaryTreeWithDepth = Map[Int, (Int,Int,Int)]

  def zipWithDepth(tree:BinaryTree, from:Int, acc:BinaryTreeWithDepth, depth:Int):BinaryTreeWithDepth = {
    if(from == -1) acc
    else {
      val newAcc = acc ++ Map(from -> (tree(from)._1, tree(from)._2, depth))
      zipWithDepth(tree, tree(from)._1, zipWithDepth(tree, tree(from)._2, newAcc, depth+1), depth+1)
    }
  }

  def inOrderIndexed(tree:BinaryTreeWithDepth, from:Int):Vector[Int] = {
    if (tree.isEmpty) Vector()
    else if(from == -1) Vector()
    else inOrderIndexed(tree, tree(from)._1) ++ Vector(from) ++ inOrderIndexed(tree, tree(from)._2)
  }

  def swappedNode(node:(Int, Int, Int)):(Int, Int, Int) = (node._2, node._1, node._3)

  def swappedTree(tree:BinaryTreeWithDepth, d:Int):BinaryTreeWithDepth = tree.mapValues( x => if (x._3 == d) swappedNode(x) else x)

  def kSwappedTree(tree:BinaryTreeWithDepth, k:Int):BinaryTreeWithDepth = tree.mapValues( x => if (x._3 % k == 0) swappedNode(x) else x)

  def getTreeFromInput():BinaryTreeWithDepth = {
    var tree = Map(1 -> (-1,-1))
    var q = Queue[Int](1)
    0 until scala.io.StdIn.readInt() foreach( _ => {
      val children = scala.io.StdIn.readLine().split(' ').toList.map(_.toInt)
      children foreach( c=> if (c > -1) q.enqueue(c))
      val currentNode:Int = q.dequeue
      tree =  tree ++ Map(currentNode -> (children(0), children(1)))
    })
    zipWithDepth(tree, 1, Map[Int, (Int,Int,Int)](), 1)
  }

  var tree = getTreeFromInput()

  0 until scala.io.StdIn.readInt() foreach(_ => {
    val k = scala.io.StdIn.readInt()
    tree = kSwappedTree(tree, k)
    println(inOrderIndexed(tree,1).mkString(" "))
  })
}
