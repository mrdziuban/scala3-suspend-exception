package example

trait BaseVisitor[T, A] {
  type V[a]
}

trait VisitorType[T] {
  type Out[A]
}
