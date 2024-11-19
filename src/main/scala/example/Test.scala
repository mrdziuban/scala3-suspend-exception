package example

sealed trait Test

object Test {
  case object Foo extends Test

  val visitorType = mkVisitorType[Test]

  trait Visitor[A] extends BaseVisitor[Test, A] {
    type V[a] = visitorType.Out[a]
  }
}
