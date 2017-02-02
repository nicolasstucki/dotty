import dotty.phantom.PhantomAny

/* This is a example of how to implement =:= using erasable phantom types.
 *
 * Run this test with
 *   `run tests/pos/phantomEvidence-1.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomParamErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 *
 * See also: ../neg/phantomEvidence-1.scala
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
    def getOnInstance(implicit ev: S =::= Off): Instance[On] = new Instance[On] // phantom parameter ev is erased
    def getOffInstance(implicit ev: S =::= On): Instance[Off] = new Instance[Off] // phantom parameter ev is erased
  }

  def run() = {
    val instance = Instance.newInstance()
    instance.getOnInstance // infered phantom evidence parameter =::= is erased
    instance.getOnInstance.getOffInstance.getOnInstance.getOffInstance // all infered phantom evidence parameters =::= are erased
  }

}

object WithNormalStatePhantoms extends PhantomAny {
  final class =::=[From, To] extends PhantomAny // This phantom class is erased
  object =::= extends PhantomAny { // This phantom object is erased
    implicit def tpEquals[A]: A =::= A = new =::=[A, A]
  }
}
