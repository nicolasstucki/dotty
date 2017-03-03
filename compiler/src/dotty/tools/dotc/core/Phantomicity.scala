package dotty.tools.dotc.core

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._

import scala.collection.mutable

/** Suffix encoding M of `PhantomFunctionM` and `ImplicitPhantomFunctionM`
 *
 *  `FunctionN` and `ImplicitFunctionN` can also be encoded as
 *  `PhantomFunctionM` and `ImplicitPhantomFunctionM` where `M` has `N + 1` `O`s
 */
class Phantomicity private (val encodedNames: Array[TermName]) extends AnyVal {
  import Phantomicity._

  /** If has phantom parameters or return type */
  def hasPhantoms: Boolean = encodedNames.exists(_ != scalaLattice)

  /** If all parameters and return type are phantoms */
  def allPhantoms: Boolean = encodedNames.forall(_ != scalaLattice)

  /** If the return type is a phantom */
  def returnsPhantom: Boolean = encodedNames.last != scalaLattice

  /** Arity of this function. Including phantom parameters. */
  def arity: Int = if (isValid) encodedNames.length - 1 else -1

  /** Erased arity of this function. Without phantom parameters. */
  def erasedArity: Int =
    if (isValid) encodedNames.init.count(_ == scalaLattice)
    else -1

  def tParamBounds(i: Int)(implicit ctx: Context): TypeBounds = {
    val lattice = encodedNames(i)
    if (lattice == scalaLattice) {
      TypeBounds.empty
    } else {
      val top = phantomLatticeTop.get(lattice).get
      TypeBounds(defn.bottomOf(top), top)
    }
  }

  /** Is a valid phantomicity */
  def isValid: Boolean = encodedNames ne invalid.encodedNames

  /** Encoded suffix of the funtion name */
  def encodedString: String = "$$" + encodedNames.mkString(separator.toString)

}

object Phantomicity {
  private val separator = '_'
  private val scalaLattice = "scala".toTermName

  private val phantomLatticeTop: mutable.Map[TermName, Type] = mutable.Map.empty
  private def registeredLatticeName(name: TermName, top: Type)(implicit ctx: Context): TermName = {
    if (!phantomLatticeTop.contains(name)) {
      phantomLatticeTop.put(name, top)
    }
    name
  }

  lazy val invalid: Phantomicity = new Phantomicity(Array.empty)

  def apply(types: List[Type])(implicit ctx: Context): Phantomicity = {
    def typeToString(tp: Type) = defn.topOf(tp.phantomTopClass) match {
      case top: TypeRef if !(tp <:< defn.AnyType) =>
        val name = top.prefix.termSymbol.fullName.encode.asTermName
        registeredLatticeName(name, top)
      case _ => registeredLatticeName(scalaLattice, defn.AnyType)
    }
    val encodedStrings = types.iterator.map(typeToString).toArray
    new Phantomicity(encodedStrings)
  }

  def from(str: String): Phantomicity = {
    val encodedStrings = str.substring(2).split(separator).map(_.toTermName)
    if (encodedStrings.forall(n => !phantomLatticeTop.contains(n))) invalid
    else new Phantomicity(encodedStrings)
  }

  def noPhantoms(arity: Int)(implicit ctx: Context): Phantomicity = apply(List.fill(arity + 1)(defn.AnyType))

}
