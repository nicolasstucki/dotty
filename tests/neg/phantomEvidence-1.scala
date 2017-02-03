/* This is a example of how to implement =:= using erasable phantom types.
 *
 * Run this test with
 *   `run tests/neg/phantomEvidence-1.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomParamErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 *
 * See also: ../pos/phantomEvidence-1.scala
 */


/** In this implementation variant of =:= (called =::=) we erase all instantiations and definitions of =::= */
object WithNormalState {
  import WithNormalStatePhantoms._
  trait State
  sealed trait On extends State
  sealed trait Off extends State

  object Instance {
    def newInstance(): Instance[Off] = new Instance[Off]
  }
  class Instance[S <: State] private {
    def getOnInstance(implicit ev: S =::= Off): Instance[On] = new Instance[On]
    def getOffInstance(implicit ev: S =::= On): Instance[Off] = new Instance[Off]
  }

  def run() = {
    val instance = Instance.newInstance()
    instance.getOffInstance // error
    instance.getOnInstance.getOnInstance // error
  }

}

object WithNormalStatePhantoms extends PhantomAny {
  final class =::=[From, To] extends PhantomAny
  object =::= extends PhantomAny {
    implicit def tpEquals[A]: A =::= A = new =::=[A, A]
  }
}
