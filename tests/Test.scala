import scala.annotation.tailrec
import scala.collection.generic.{GenericCompanion, GenericTraversableTemplate}
import scala.collection.immutable.List
import scala.collection.{AbstractIterator, GenSeq, Iterator, LinearSeq, LinearSeqLike, Seq, SeqLike, mutable}

object Test {
  def main(args: Array[String]): Unit = {
    class Foo {
      def bar: Int = 42
    }

    val foo = new Foo
    foo.bar
  }
}
