
class phantomInstanceOf1 {
  null.asInstanceOf[PhantomAny] // error
  null.asInstanceOf[PhantomNothing] // error
  "".asInstanceOf[PhantomAny] // error
  "".asInstanceOf[PhantomNothing] // error
}
