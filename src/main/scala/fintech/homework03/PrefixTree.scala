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

class TreeNode[K, V](var element : Option[V] = None,
                     var location : Seq[K]) extends PrefixTree[K, V] {

  var children: Map[K, TreeNode[K, V]] =  Map[K, TreeNode[K, V]]()

  def get: V = element.get

  def put[U >: V](path: Seq[K], value: U): PrefixTree[K, U] = new PutObj(path, value, new TreeNode[K,U](this.element, this.location))

  override def toString: String = {
    this.children.mkString
  }

  override def sub(path: Seq[K]): PrefixTree[K, V] = {
      if (this.location == path) {
        this
      } else {
        def helperSub(node: TreeNode[K, V], keyIndex: Int): TreeNode[K, V] = {
          if (node.children.isDefinedAt(path(keyIndex))) {
            if (keyIndex + 1 == path.length) {
              node.children(path(keyIndex))
            } else {
              helperSub(node.children(path(keyIndex)), keyIndex + 1)
            }
          } else {
            new TreeNode[K, V](None, Seq[K]())
          }
        }
        helperSub(this, 0)
      }
  }
}

class PutObj[K,V](location: Seq[K], element: V = None, tree: TreeNode[K,V]) extends PrefixTree[K,V] {

  var children: Map[K, TreeNode[K, V]] =  Map[K, TreeNode[K, V]]()
  helperPut(tree, 0)
  def helperPut(node: TreeNode[K,V], keyIndex: Int) {
    if (keyIndex == location.length) {
      node.element = Option(element)
    }
    else {
      if (node.children.isDefinedAt(location(keyIndex))) {
        helperPut(node.children(location(keyIndex)),keyIndex + 1)
      } else {
        val result = new TreeNode[K, V](None, location :+ location(keyIndex))
        node.children = node.children + ((location(keyIndex), result))
        helperPut(result, keyIndex + 1)
      }
    }
  }

  override def put[U >: V](path: Seq[K], value: U): PrefixTree[K, U] =
    new PutObj(path, value, new TreeNode[K,U](Option(this.element), this.location))

  override def sub(path: Seq[K]): PrefixTree[K, V] = {
    if (tree.location == path) {
      tree
    } else {
      def helperSub(node: TreeNode[K, V], keyIndex: Int): TreeNode[K, V] = {
        if (node.children.isDefinedAt(path(keyIndex))) {
          if (keyIndex + 1 == path.length) {
            node.children(path(keyIndex))
          } else {
            helperSub(node.children(path(keyIndex)), keyIndex + 1)
          }
        } else {
          new TreeNode[K, V](None, Seq[K]())
        }
      }
      helperSub(tree, 0)
    }
  }

  override def get: V =  tree.element.get

  override def toString: String = {
    tree.children.mkString
  }
}

// object test {
//   def main (args: Array[String] ): Unit = {
//     val tree: PrefixTree[Char, Int] = new TreeNode[Char,Int]( None,"" )
//
//     val with42: PrefixTree[Char, Int] = tree.put("abcd", 42)
//     println(with42)
//     println(with42.sub("ab").sub("cd").get)
//
//     val withDouble: PrefixTree[Char, AnyVal] = with42.put("abcde", 13.0)
//
//     println(with42.sub("abcd").get)
//     println(withDouble.sub("ab").sub("cde").get)
//   }
// }