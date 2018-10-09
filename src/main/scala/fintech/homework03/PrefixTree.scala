package fintech.homework03

import java.util.Optional

import scala.collection.mutable
import scala.collection.immutable
import scala.collection.mutable.Seq


// Реализовать интерфейс PrefixTree
// Интерфейс позволяет складывать объекты произвольного класса V по заданному "пути" Seq[K] в дерево
// и изымать их используя комбинацию методов sub и get

// Например, можно на каждом "уровне" дерева хранить Option[V] и Map[K, PrefixTree[K, V]]

trait PrefixTree[K, V] {
  def put(path: mutable.Seq[K], value: V): PrefixTree[K, V]
  def sub(path: mutable.Seq[K]): PrefixTree[K, V]
  def get: V
}

class TreeNode[K, V](var element : Option[V] = None,
                     var location : mutable.Seq[K]) extends PrefixTree[K, V] {

  var children:immutable.Map[K, TreeNode[K, V]] =  immutable.Map[K, TreeNode[K, V]]()

  def get: V = element.get

  def put(path: mutable.Seq[K], value: V): PrefixTree[K, V] = {

    var nodeCount = 0
    def helperPut(node: TreeNode[K,V], keyIndex: Int) {
      if (keyIndex == path.length) {
        node.element = Option(value)
      }
      else {
        if (node.children.isDefinedAt(path(keyIndex))) {
          helperPut(node.children(path(keyIndex)),keyIndex + 1)
        } else {
          val result = new TreeNode[K, V](None, location :+ path(keyIndex))
          nodeCount += 1
          node.children = node.children + ((path(keyIndex), result))
          helperPut(result, keyIndex + 1)
        }
      }
      }
    helperPut(this, 0)
    this
  }

  override def toString: String = {
    this.children.mkString
  }

  override def sub(path: mutable.Seq[K]): PrefixTree[K, V] = {
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
            new TreeNode[K, V](None, mutable.Seq[K]())
          }
        }
        helperSub(this, 0)
      }
  }
}

 object test {
   def main (args: Array[String] ): Unit = {
     val tree: PrefixTree[Char, Int] = new TreeNode[Char, Int](location = mutable.Seq(' '))
     val first = tree.put(mutable.Seq('a','b','c'), 12)
     //println(first)
     tree.put(mutable.Seq('a','b','c','d'), 13)
     val with42: PrefixTree[Char, Int] = tree.put(mutable.Seq('a','b','c','d','e','f'), 42)
     val out = with42.sub(mutable.Seq('a','b','c','d','e'))
     //println(out.get)
     //println(with42)
   }
 }