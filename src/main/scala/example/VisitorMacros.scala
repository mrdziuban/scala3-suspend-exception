package example

import scala.deriving.Mirror
import scala.quoted.*

private def mkVisitorTypeImpl[T: Type](using q: Quotes): Expr[VisitorType[T]] = {
  import q.reflect.*

  val expectedMethods = Expr.summon[Mirror.SumOf[T]] match {
    case Some('{ ${ _ }: Mirror.SumOf[T] { type MirroredElemTypes = types } }) =>
      List.unfold(TypeRepr.of[types])(_.asType match {
        case '[h *: t] => Some(("visit" ++ TypeRepr.of[h].typeSymbol.name, TypeRepr.of[t]))
        case '[EmptyTuple] => None
      })

    case None => report.errorAndAbort(s"Failed to find Mirror.SumOf[${Type.show[T]}]")
  }: @annotation.nowarn("msg=match may not be exhaustive")

  def refinedType(acc: TypeRepr => TypeRepr, methods: List[String]): TypeRepr =
    methods match {
      case Nil => TypeLambda(List("a"), _ => List(TypeBounds.empty), l => acc(l.param(0)))
      case h :: t => refinedType(a => Refinement(acc(a), h, a), t)
    }

  refinedType(_ => TypeRepr.of[Selectable], expectedMethods).asType match {
    case '[type f[X]; f] => '{ new VisitorType[T] { type Out[a] = f[a] } }
  }
}

transparent inline def mkVisitorType[T]: VisitorType[T] = ${ mkVisitorTypeImpl[T] }
