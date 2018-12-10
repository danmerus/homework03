package fintech.homework03
import org.scalatest.{FlatSpec, Matchers}

class PrefixTreeSpec extends FlatSpec with Matchers {
  it should "work well with strings" in {
    val tree: PrefixTree[Char, Int] = new TreeNode[Char,Int]()

    val with42: PrefixTree[Char, Int] = tree.put("abcd", 42)
    with42.sub("ab").sub("cd").get should be (42)

    val withDouble: PrefixTree[Char, AnyVal] = with42.put("abcde", 13.0)
    withDouble.sub("ab").sub("cd").get should be (42)
    withDouble.sub("ab").sub("cde").get should be (13.0)
  }
  it should "put in empty string" in {
    val tree: PrefixTree[Char, Int] = new TreeNode[Char, Int]().put("", 1)
    tree.sub("").get should be (1)
  }
  it should "should replace correctly" in {
    var basicTree: PrefixTree[Char, Int] = new TreeNode[Char,Int]().put("abc", 1)
    basicTree = basicTree.put("abc", 2)
    basicTree.sub("abc").get should be (2)
  }
}
