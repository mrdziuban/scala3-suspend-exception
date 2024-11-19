package example

import scala.deriving.Mirror
import scala.quoted.*

private class VisitorMacros()(using val q: Quotes) {
  import q.reflect.*

  private def getExpectedMethods[T: Type]: List[String] =
    Expr.summon[Mirror.SumOf[T]] match {
      // If the type is a sum type, recurse to get the method names of its subtypes
      case Some('{ ${ _ }: Mirror.SumOf[T] { type MirroredElemTypes = types } }) =>
        def go[T <: Tuple: Type](acc: List[String]): List[String] =
          Type.of[T] match {
            case '[h *: t] => ("visit" ++ TypeRepr.of[h].typeSymbol.name) :: acc
            case '[EmptyTuple] => acc
          }

        go[types & Tuple](Nil)

      case None => report.errorAndAbort(s"Failed to find Mirror.SumOf[${Type.show[T]}]")
    }: @annotation.nowarn("msg=match may not be exhaustive")

  def mkVisitorTypeImpl[T: Type]: Expr[VisitorType[T]] = {
    def refinedType(acc: TypeRepr => TypeRepr, methods: List[String]): TypeRepr =
      methods match {
        case Nil => TypeLambda(List("a"), _ => List(TypeBounds.empty), l => acc(l.param(0)))
        case h :: t => refinedType(a => Refinement(acc(a), h, a), t)
      }

    refinedType(_ => TypeRepr.of[Selectable], getExpectedMethods[T].toList).asType match {
      case '[type f[X]; f] => '{ new VisitorType[T] { type Out[a] = f[a] } }
    }
  }
}

private def mkVisitorTypeImpl[T: Type](using Quotes): Expr[VisitorType[T]] = new VisitorMacros().mkVisitorTypeImpl[T]

transparent inline def mkVisitorType[T]: VisitorType[T] = ${ mkVisitorTypeImpl[T] }
