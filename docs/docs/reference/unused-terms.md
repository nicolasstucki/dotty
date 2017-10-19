---
layout: doc-page
title: "Unused Parameters"
---

Why unused parameters?
----------------------
TODO


What are unused parameter?
--------------------------
Unused parameter are values that are know not to be used. Parameters of methods and functions 
can be declared as unused. Those parameters wont be usable for computations, thought they can 
be used as arguments to other unused parameters.

```scala
def methodWithUnusedEv(unused ev: Ev): Int = 42

val lambdaWithUnusedEv: unused Ev => Int = 
  unused (ev: Ev) => 42
```

Not only parameters can be marked as unused, `val` and `def` can also be marked with `unused`.
The will also only be usable as arguments to `unused` parameters.

```scala
unused val unusedEvidence: Ev = ...
methodWithUnusedEv(unusedEvidence)
```


What happens with unused values at runtime?
-------------------------------------------
As `unused` are guaranteed not to be used in computations, they can and will be erased.

```scala
// becomes def methodWithUnusedEv(): Int at runtime
def methodWithUnusedEv(unused ev: Ev): Int = ...  

def evidence1: Ev = ...
unused def unusedEvidence2: Ev = ... // does not exist at runtime
unused val unusedEvidence3: Ev = ... // does not exist at runtime

// evidence1 is evaluated but the result is not passed to methodWithUnusedEv
methodWithUnusedEv(evidence1)

// unusedEvidence2 is not evaluated and its result is not passed to methodWithUnusedEv
methodWithUnusedEv(unusedEvidence2)
```

State machine example
---------------------
The following examples shows an implementation of a simple state machine which can be in a state `On` or `Off`.
The machine can change state from `Off` to `On` with `turnedOn` only if it is currently `Off`, 
conversely from `On` to `Off` with `turnedOff` only if it is currently `On`. These last constraint are
captured with the `IsOff[S]` and `IsOn[S]` implicit evidence only exist for `IsOff[Off]` and `InOn[On]`. 
For example, not allowing calling `turnedOff` on in an `Off` state as we would require an evidence `IsOn[Off]` 
that will not be found.

As the implicit evidences of `turnedOn` and `turnedOff` are not used in the bodies of those functions 
we can mark them as `unused`. This will remove the evidence parameters at runtime, but we would still 
evaluate the `isOn` and `isOff` implicits that where found as arguments.
As `isOn` and `isOff` are not used except as as `unused` arguments, we can mark them as `unused`, hence 
removing the evaluation of the `isOn` and `isOff` evidences.

```scala
import scala.annotation.implicitNotFound

sealed trait State
final class On extends State
final class Off extends State

@implicitNotFound("State is must be Off")
class IsOff[S <: State]
object IsOff {
  unused implicit def isOff: IsOff[Off] = new IsOff[Off]
}

@implicitNotFound("State is must be On")
class IsOn[S <: State]
object IsOn {
  unused implicit def isOn: IsOn[On] = new IsOn[On]
}

class Machine[S <: State] private {
  def turnedOn(implicit unused ev: IsOff[S]): Machine[On] = new Machine[On]
  def turnedOff(implicit unused ev: IsOn[S]): Machine[Off] = new Machine[Off]
}

object Machine {
  def newMachine(): Machine[Off] = new Machine[Off]
}

object Test {
  def main(args: Array[String]): Unit = {
    val m = Machine.newMachine()
    m.turnedOn
    m.turnedOn.turnedOff

    // m.turnedOff
    //            ^
    //            State is must be On

    // m.turnedOn.turnedOn
    //                    ^
    //                    State is must be Off
  }
}
```
