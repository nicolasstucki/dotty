object MyPhantom extends Phantom {
  type Foo = this.Any { type Member = Int }

  type Bar <: this.Any
  type Foo2 = Bar { type Member = Int }
}
