package dotty.runtime

abstract class ErasedPhantomAny {
  throw new RuntimeException("Cannot instantiate phantom types.")
}
