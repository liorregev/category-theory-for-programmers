object ReaderFunctor extends App {
  case class Reader[In, Out]() {
    def fmap[B](g: Out => B)(f: In => Out): In => B = f andThen g
  }
  type ReaderToInt[In] = Reader[In, Int]
  private val myReader = new ReaderToInt[String]
  private val lifted = myReader.fmap((i: Int) => i > 10) _
  private val stringToBoolean: String => Boolean = lifted(_.length)
  println(stringToBoolean("aaaaaaaaa"))
}
