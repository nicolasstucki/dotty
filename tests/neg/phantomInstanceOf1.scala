import dotty.phantom.PhantomAny
import dotty.phantom.PhantomNothing

class phantomInstanceOf1 {
  null.asInstanceOf[PhantomAny] // error
  null.asInstanceOf[PhantomNothing] // error
  "".asInstanceOf[PhantomAny] // error
  "".asInstanceOf[PhantomNothing] // error
}
