package scala.quoted

import scala.quoted.Types.TaggedType
import scala.reflect.ClassTag
import scala.runtime.quoted.Unpickler.Pickled

sealed abstract class Type[T] {
  type `$splice` = T
}

/** Some basic type tags, currently incomplete */
object Type {
  /** A term quote is desugared by the compiler into a call to this method */
  def apply[T]: Type[T] =
    throw new Error("Internal error: this method call should have been replaced by the compiler")
}

/** All implementations of Type[T].
 *  These should never be used directly.
 */
object Types {
  /** A Type backed by a pickled TASTY tree */
  final class TastyType[T](val tasty: Pickled, val args: Seq[Any]) extends Type[T] {
    override def toString(): String = s"Type(<pickled tasty>)"
  }

  /** An Type backed by a value */
  final class TaggedType[T](implicit val ct: ClassTag[T]) extends Type[T] {
    override def toString: String = s"Type($ct)"
  }

  /** An Type backed by a tree */
  final class TreeType[Tree](val typeTree: Tree) extends quoted.Type[Any] {
    override def toString: String = s"Type(<tasty tree>)"
  }
}
