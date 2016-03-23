import shapeless._

// based on https://github.com/echojc/sdu16/blob/f5e33fe2bf08527c6663b976d85e727a5f0cae34/shapeless-gen/src/main/scala/MapReader.scala

trait MapReaderWriter[A] {
  type K
  def read(map: Map[String, Any]): A
  def write(a: A): Map[String, Any]
}

object MapReaderWriter {
  type Aux[A, K0] = MapReaderWriter[A] { type K = K0 }

  implicit def mrInt[K0 <: Symbol](implicit wk: Witness.Aux[K0]): MapReaderWriter.Aux[Int, K0] =
    new MapReaderWriter[Int] {
      type K = K0
      val name: String = wk.value.name
      def read(map: Map[String, Any]): Int = map(name).asInstanceOf[Int]
      def write(value: Int): Map[String, Any] = Map[String, Any](name → value)
    }

  implicit def mrString[K0 <: Symbol](implicit wk: Witness.Aux[K0]): MapReaderWriter.Aux[String, K0] =
    new MapReaderWriter[String] {
      type K = K0
      val name: String = wk.value.name
      def read(map: Map[String, Any]): String = map(wk.value.name).asInstanceOf[String]
      def write(value: String): Map[String, Any] = Map[String, Any](name → value)
    }

  implicit def mrBoolean[K0 <: Symbol](implicit wk: Witness.Aux[K0]): MapReaderWriter.Aux[Boolean, K0] =
    new MapReaderWriter[Boolean] {
      type K = K0
      val name: String = wk.value.name
      def read(map: Map[String, Any]): Boolean = map(wk.value.name).asInstanceOf[Boolean]
      def write(value: Boolean): Map[String, Any] = Map[String, Any](name → value)
    }

  implicit val mrHNil: MapReaderWriter.Aux[HNil, HNil] =
    new MapReaderWriter[HNil] {
      type K = HNil
      def read(map: Map[String, Any]): HNil = HNil
      def write(value: HNil): Map[String, Any] = Map.empty
    }

  implicit def mrHCons[K0, J <: HList, H, T <: HList](implicit
    mrH: MapReaderWriter.Aux[H, K0],
    mrT: MapReaderWriter.Aux[T, J]): MapReaderWriter.Aux[H :: T, K0 :: J] =
    new MapReaderWriter[H :: T] {
      type K = K0 :: J
      def read(map: Map[String, Any]): H :: T = mrH.read(map) :: mrT.read(map)
      def write(hcons: H :: T): Map[String, Any] = {
        mrH.write(hcons.head) ++ mrT.write(hcons.tail)
      }
    }

  implicit def mrCaseClass[A, K0 <: HList, L <: HList](implicit
    lab: DefaultSymbolicLabelling.Aux[A, K0],
    gen: Generic.Aux[A, L],
    mr: MapReaderWriter.Aux[L, K0]): MapReaderWriter.Aux[A, K0] =
    new MapReaderWriter[A] {
      type K = K0
      def read(map: Map[String, Any]): A = gen.from(mr.read(map))
      def write(a: A): Map[String, Any] = mr.write(gen.to(a))
    }
}

object MapReaderWriterExample extends App {
  case class Foo(i: Int, s: String, b: Boolean)

  val mrFoo = implicitly[MapReaderWriter[Foo]]

  val foo = Foo(1, "bar", true)
  val fooMap = Map("i" → 1, "s" → "bar", "b" → true)

  assert(mrFoo.read(fooMap) == foo)
  assert(mrFoo.write(foo) == fooMap)
}
