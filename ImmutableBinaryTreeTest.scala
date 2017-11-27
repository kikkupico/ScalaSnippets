sealed trait BinaryTree {
  def +(x: Int): BinaryTree
  def ->(d:Char): BinaryTree
  def addAt(route:List[Char], x:Int):BinaryTree
  def getElem:Option[Int]
  def children:Vector[BinaryTree]
  def inOrder():List[Int]
}

object EmptyBinaryTree extends BinaryTree {
  def +(x: Int): BinaryTree = NonEmptyBinaryTree(x, EmptyBinaryTree, EmptyBinaryTree)
  def ->(d:Char):BinaryTree = this
  def addAt(route:List[Char], x:Int):BinaryTree = NonEmptyBinaryTree(x, EmptyBinaryTree, EmptyBinaryTree)
  def inOrder():List[Int] = List()
  def getElem:Option[Int] = None
  def children:Vector[BinaryTree] = Vector()
  override def toString = " . "
}

case class NonEmptyBinaryTree(elem: Int, left: BinaryTree, right: BinaryTree) extends BinaryTree {

  def +(x: Int): BinaryTree =
    (left, right) match {
      case (EmptyBinaryTree, EmptyBinaryTree) => NonEmptyBinaryTree(elem, left + x, right)
      case (NonEmptyBinaryTree(e, l, r), EmptyBinaryTree) => NonEmptyBinaryTree(elem, left, right + x)
      case (NonEmptyBinaryTree(le, ll, lr), NonEmptyBinaryTree(re, rl, rr)) => NonEmptyBinaryTree(elem, left + x, right)
      case _ => NonEmptyBinaryTree(elem, left, right + x)
    }

  def ->(d:Char):BinaryTree = d match {
    case 'L' => left
    case 'R' => right
  }

  def addAt(route:List[Char], x:Int):BinaryTree = route match {
    case Nil => NonEmptyBinaryTree(elem, EmptyBinaryTree, EmptyBinaryTree)
    case d :: Nil => d match {
      case 'L' => NonEmptyBinaryTree(elem, NonEmptyBinaryTree(x, EmptyBinaryTree, EmptyBinaryTree), right)
      case 'R' => NonEmptyBinaryTree(elem, left, NonEmptyBinaryTree(x, EmptyBinaryTree, EmptyBinaryTree))
    }
    case d :: rest => d match {
      case 'L' => NonEmptyBinaryTree(elem, left.addAt(rest, x), right)
      case 'R' => NonEmptyBinaryTree(elem, left, right.addAt(rest, x))
    }
  }

  def inOrder():List[Int] = left.inOrder() ++ List(elem) ++ right.inOrder()

  def getElem:Option[Int] = Some(elem)

  override def toString:String = " { " + left + elem + right + " } "

  def children:Vector[BinaryTree] = Vector(left, right)

}

object TreeWalker {
  def nodesAtDepth(d:Int, tree:BinaryTree):Vector[BinaryTree] =
    if(d == 0) Vector(tree)
    else nodesAtDepth(d-1, tree) flatMap( _.children)

  def routes(n:Int):List[Char] = n match {
    case 0 => List()
    case _ =>
      val parent = Math.floor((n-1)/2.0).toInt
      routes(parent) :+ List('L','R')(n-(parent*2)-1)
  }

  def listToTree(l:List[Int]):BinaryTree = {
    var tree = EmptyBinaryTree + 0
    for ((x, i) <- l.zipWithIndex) {
      tree = tree.addAt(routes(i), x)
      println(tree)
    }
    tree
  }

  /**
    * L R
    * -LL LR
    * -RL RR
    * LRL LRR
    * RRL RRR
    *
    */
}

object ImmutableBinaryTreeTest extends App {
//  val tree = NonEmptyBinaryTree(0, NonEmptyBinaryTree(1, NonEmptyBinaryTree(3, EmptyBinaryTree, EmptyBinaryTree), NonEmptyBinaryTree(4, EmptyBinaryTree, EmptyBinaryTree)), NonEmptyBinaryTree(2, EmptyBinaryTree, EmptyBinaryTree))
//  println(tree)
//  println(TreeWalker.nodesAtDepth(1, tree))

  val L = 'L'
  val R = 'R'

//  val tree2 = EmptyBinaryTree + 1 + 2 + 3 + 4 + 5
//  println(tree2)
//  println(tree2 -> L -> L)
//  println(tree2.addAt(L :: L :: Nil, 10))

  println(TreeWalker.listToTree((0 to 14).toList).inOrder())

  println((1 to 10).foldLeft(EmptyBinaryTree+0)(_ + _).inOrder())
}
