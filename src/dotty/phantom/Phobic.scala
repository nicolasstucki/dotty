package dotty.phantom

import scala.annotation.Annotation

class Phobic[T](d: Disallowed[T]) extends Annotation


final class Disallowed[T] private ()

object Disallowed {
  def apply[T]: Disallowed[T] = new Disallowed[T]
}
