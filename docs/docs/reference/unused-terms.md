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

Examples
--------
TODO

```scala

```
