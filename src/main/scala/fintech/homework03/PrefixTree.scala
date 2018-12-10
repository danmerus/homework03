package fintech.homework03


// Реализовать интерфейс PrefixTree
// Интерфейс позволяет складывать объекты произвольного класса V по заданному "пути" Seq[K] в дерево
// и изымать их используя комбинацию методов sub и get

// Например, можно на каждом "уровне" дерева хранить Option[V] и Map[K, PrefixTree[K, V]]

trait PrefixTree[K, +V] {
  def put[U >: V](path: Seq[K], value: U): PrefixTree[K, U]
  def sub(path: Seq[K]): PrefixTree[K, V]
  def get: V
}

class TreeNode[K, V](val element : Option[V] = None,
                     children: Map[K, PrefixTree[K, V]] = Map.empty[K, PrefixTree[K, V]]) extends PrefixTree[K, V] {

  def get: V = element.get

  def put[U >: V](path: Seq[K], value: U): PrefixTree[K, U] = path match {
    case Seq() => new TreeNode[K, U](Option(value), this.children)
    case Seq(head, _*) => children.get(head) match {
      case Some(v) => new TreeNode[K, U](
        this.element, children + (path.head -> children(path.head).put(path.tail, value)))
      case None => new TreeNode[K, U](
        this.element, children + (path.head -> new TreeNode[K, U]().put(path.tail, value)))
    }
  }

  override def toString: String = {
    this.children.mkString
  }

  override def sub(path: Seq[K]): PrefixTree[K, V] = path match {
    case Seq() => this
    case Seq(head, _*) => children.get(head) match {
      case Some(_) => children(path.head).sub(path.tail)
      case None => new TreeNode[K, V]()
    }
  }

}
