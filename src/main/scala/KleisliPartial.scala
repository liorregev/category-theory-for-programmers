object KleisliPartial {
  def identity[A](v: A): Option[A] = Option(v)
  def compose[A, B, C](f1: A => Option[B], f2: B => Option[C]): A => Option[C] = {
    (v: A) => f1(v) match {
      case Some(r) => f2(r)
      case _ => None
    }
  }

  def safeRoot(x: Double): Option[Double] = {
    if(x >= 0) {
      Option(Math.sqrt(x))
    } else {
      None
    }
  }

  def safeReciprocal(x: Double): Option[Double] = {
    if(x != 0) {
      Option(1/x)
    } else {
      None
    }
  }

  def main(args: Array[String]): Unit = {
    val safeRootReciprocal = compose(safeReciprocal, safeRoot)
    println(safeRootReciprocal(0.25))
    println(safeRootReciprocal(-0.25))
    println(safeRootReciprocal(0))
  }
}
