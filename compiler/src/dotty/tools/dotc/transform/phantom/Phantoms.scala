package dotty.tools.dotc.transform.phantom

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types.{ClassInfo, DeepTypeMap, ImplicitMethodType, JavaMethodType, MethodType, MethodicType, PolyType, RefinedType, Type, TypeRef}

import scala.annotation.tailrec

object Phantoms {

  def erasedPhantomParameters(tp: Type)(implicit ctx: Context): Type = {
    if (returnsPhantom(tp)) tp
    else erasedPhantomParametersImpl(tp)
  }

  @tailrec def returnsPhantom(tp: Type)(implicit ctx: Context): Boolean = tp match {
    case tp: MethodicType => returnsPhantom(tp.resultType)
    case _                => tp.isPhantom
  }

  private def erasedPhantomParametersImpl(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: JavaMethodType => tp
    case tp: MethodType =>
      val erasedResultType = erasedPhantomParametersImpl(tp.resultType)
      val (erasedParamNames, erasedParamTypes) =
        tp.paramNames.zip(tp.paramTypes).filter(tup => !tup._2.isPhantom).unzip
      if (tp.resultType == erasedResultType && tp.paramNames == erasedParamNames && tp.paramTypes == erasedParamTypes) {
        tp
      } else {
        tp match {
          case _: ImplicitMethodType => ImplicitMethodType(erasedParamNames, erasedParamTypes, erasedResultType)
          case _                     => MethodType(erasedParamNames, erasedParamTypes, erasedResultType)
        }
      }

    case tp: PolyType => tp.derivedPolyType(tp.paramNames, tp.paramBounds, erasedPhantomParametersImpl(tp.resultType))
    case _            => tp
  }

}
