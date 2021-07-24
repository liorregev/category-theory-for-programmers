package naturalxform

import cats.Functor


object MaybeToList extends App {
  implicit val F: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
      case None    => None
      case Some(a) => Some(f(a))
    }
  }
  implicit val G: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa match {
      case Nil => Nil
      case head :: tail => f(head) :: map(tail)(f)
    }
  }

  def alpha[A]: Option[A] => List[A] = {
    case None => Nil
    case Some(v) => v :: Nil
  }

  type a = String
  type b = Int

  def f(s: a): b = s.length

  val left: Option[a] => List[b] = G.lift(f) compose alpha[a]
  val right: Option[a] => List[b] = alpha[b] compose F.lift(f)

  // for None
  // left
  assert(left(None) == G.lift(f)(alpha[a](None)))
  assert(G.lift(f)(Nil) == Nil)
  // right
  assert(right(None) == alpha[b](F.lift(f)(None)))
  assert(alpha[b](None) == Nil)

  // for Some
  // left
  assert(left(Some("aa")) == G.lift(f)(alpha[a](Some("aa"))))
  assert(G.lift(f)(List("aa")) == List(2))
  // right
  assert(right(Some("aa")) == alpha[b](F.lift(f)(Some("aa"))))
  assert(alpha[b](Some(2)) == List(2))
}
