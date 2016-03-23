import shapeless._

// based on https://github.com/echojc/sdu16/blob/f5e33fe2bf08527c6663b976d85e727a5f0cae34/shapeless-gen/src/main/scala/MapReader.scala

trait MapReader[A] {
  type K
  def read(map: Map[String, Any]): A
}

object MapReader {
  type Aux[A, K0] = MapReader[A] { type K = K0 }

  implicit def mrInt[K0 <: Symbol](implicit wk: Witness.Aux[K0]): MapReader.Aux[Int, K0] =
    new MapReader[Int] {
      type K = K0
      def read(map: Map[String, Any]): Int =
        map(wk.value.name).asInstanceOf[Int]
    }

  implicit def mrString[K0 <: Symbol](implicit wk: Witness.Aux[K0]): MapReader.Aux[String, K0] =
    new MapReader[String] {
      type K = K0
      def read(map: Map[String, Any]): String =
        map(wk.value.name).asInstanceOf[String]
    }

  implicit def mrBoolean[K0 <: Symbol](implicit wk: Witness.Aux[K0]): MapReader.Aux[Boolean, K0] =
    new MapReader[Boolean] {
      type K = K0
      def read(map: Map[String, Any]): Boolean =
        map(wk.value.name).asInstanceOf[Boolean]
    }

  implicit val mrHNil: MapReader.Aux[HNil, HNil] =
    new MapReader[HNil] {
      type K = HNil
      def read(map: Map[String, Any]): HNil = HNil
    }

  implicit def mrHCons[K0, J <: HList, H, T <: HList](implicit
    mrH: MapReader.Aux[H, K0],
    mrT: MapReader.Aux[T, J]
  ): MapReader.Aux[H :: T, K0 :: J] =
    new MapReader[H :: T] {
      type K = K0 :: J
      def read(map: Map[String, Any]): H :: T =
        mrH.read(map) :: mrT.read(map)
    }

  implicit def mrCaseClass[A, K0 <: HList, L <: HList](implicit
    lab: DefaultSymbolicLabelling.Aux[A, K0],
    gen: Generic.Aux[A, L],
    mr: MapReader.Aux[L, K0]
  ): MapReader.Aux[A, K0] =
    new MapReader[A] {
      type K = K0
      def read(map: Map[String, Any]): A =
        gen.from(mr.read(map))
    }
}

object MapReaderExample extends App {

  case class Foo(i: Int, s: String, b: Boolean)

  val mrFoo = implicitly[MapReader[Foo]]

  println(mrFoo.read(Map("i" → 1, "s" → "foo", "b" → true)))
}
