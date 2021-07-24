package naturalxform

import cats.Functor
import cats.data.Reader


object ReaderToList extends App {
  implicit def readerFunctor[E]: Functor[Reader[E, *]] = new Functor[Reader[E, *]] {
    override def map[A, B](fa: Reader[E, A])(f: A => B): Reader[E, B] =
      Reader(x => f(fa.run(x)))
  }

  implicit val F: Functor[Reader[Unit, *]] = readerFunctor[Unit]

  implicit val G: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa match {
      case Nil => Nil
      case head :: tail => f(head) :: map(tail)(f)
    }
  }

  def alpha[A]: Reader[Unit, A] => List[A] = r => List(r.run(()))
  def beta[A]: Reader[Unit, A] => List[A] = r => List(r.run(()), r.run(()))

  type a = String
  type b = Int

  def f(s: a): b = s.length

  def naturality[A](nat: Reader[Unit, A] => List[A]): a => Boolean = (x: a) => {
    val left = G.lift(f) compose alpha[a]
    val right = alpha[b] compose F.lift(f)
    val reader = Reader[Unit, a](_ => x)
    left(reader) == right(reader)
  }
  assert(naturality(alpha)("aa"))
  assert(naturality(beta)("aa"))
}
