import dotty.phantom.PhantomAny

/* This is a example of how to implement =:= using erasable phantom types.
 *
 * Run this test with
 *   `run tests/pos/phantomEvidence.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 *
 * See also: ../neg/phantomEvidence.scala
 */
object phantomEvidence {

  /** In this implementation variant of =:= (called =::=) we erase all instantiations and definitions of =::= */
  object WithNormalState {
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

    final class =::=[From, To] extends PhantomAny // This phantom class is erased
    object =::= extends PhantomAny { // This phantom object is erased
      implicit def tpEquals[A]: A =::= A = new =::=[A, A]
    }
  }

  /** In this variant, the traits for the stats are also erased. */
  object WithPhantomState {
    trait State extends PhantomAny // This phantom trait is erased
    sealed trait On extends State // This phantom trait is erased
    sealed trait Off extends State // This phantom trait is erased

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

    final class =::=[From <: PhantomAny, To <: PhantomAny] extends PhantomAny // This phantom class is erased
    object =::= extends PhantomAny { // This phantom object is erased
      implicit def tpEquals[A <: PhantomAny]: A =::= A = new =::=[A, A]
    }
  }
}
