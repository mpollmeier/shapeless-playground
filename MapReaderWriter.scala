import shapeless._

// based on https://github.com/echojc/sdu16/blob/f5e33fe2bf08527c6663b976d85e727a5f0cae34/shapeless-gen/src/main/scala/MapReader.scala

trait MapReaderWriter[A] {
  type K
  def read(map: Map[String, Any]): A
  def write(a: A): Map[String, Any]
}

trait WithLabel {
  def label(): String
}

object LabelReader {
  def label(a: Any): String = a match {
    case a: WithLabel => a.label
    case other => other.getClass.getSimpleName
  }
}

object MapReaderWriter {
  type Aux[A, K0] = MapReaderWriter[A] { type K = K0 }

  def mrSimple[T, K0 <: Symbol](implicit wk: Witness.Aux[K0]): MapReaderWriter.Aux[T, K0] =
    new MapReaderWriter[T] {
      println(s"instantiating mrSimple for ${wk.value.name}")
      type K = K0
      val name: String = wk.value.name
      def read(map: Map[String, Any]): T = map(wk.value.name).asInstanceOf[T]
      def write(value: T): Map[String, Any] = Map[String, Any](name → value)
    }

  implicit def mrInt[K0 <: Symbol](implicit wk: Witness.Aux[K0]): MapReaderWriter.Aux[Int, K0] = mrSimple[Int, K0]
  implicit def mrString[K0 <: Symbol](implicit wk: Witness.Aux[K0]): MapReaderWriter.Aux[String, K0] = mrSimple[String, K0]
  implicit def mrBoolean[K0 <: Symbol](implicit wk: Witness.Aux[K0]): MapReaderWriter.Aux[Boolean, K0] = mrSimple[Boolean, K0]

  implicit def mrOption[B, K0 <: Symbol](implicit wk: Witness.Aux[K0]): MapReaderWriter.Aux[Option[B], K0] =
    new MapReaderWriter[Option[B]] {
      type K = K0
      val name: String = wk.value.name
      def read(map: Map[String, Any]): Option[B] = map.get(wk.value.name).asInstanceOf[Option[B]]
      def write(value: Option[B]): Map[String, Any] = value match {
        case Some(value) => Map[String, Any](name → value)
        case None => Map.empty
      }
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
      def write(hcons: H :: T): Map[String, Any] = mrH.write(hcons.head) ++ mrT.write(hcons.tail)
    }

  implicit def mrCaseClass[A, K0 <: HList, L <: HList](implicit
    lab: DefaultSymbolicLabelling.Aux[A, K0],
    gen: Generic.Aux[A, L],
    mr: MapReaderWriter.Aux[L, K0]): MapReaderWriter.Aux[A, K0] =
    new MapReaderWriter[A] {
      println("instantiating mrCaseClass")
      type K = K0
      def read(map: Map[String, Any]): A = gen.from(mr.read(map))
      def write(a: A): Map[String, Any] = mr.write(gen.to(a))
    }

  import scala.reflect.runtime.universe
  import scala.reflect.runtime.currentMirror
  implicit def mrValueClass[K0 <: Symbol, VC <: AnyVal: universe.TypeTag : scala.reflect.ClassTag](implicit wk: Witness.Aux[K0]): MapReaderWriter.Aux[VC, K0] =
    new MapReaderWriter[VC] {
      println("instantiating mrValueClass")
      type K = K0
      val name: String = wk.value.name
      def read(map: Map[String, Any]): VC = {
        // Bar(map(wk.value.name).asInstanceOf[Int]).asInstanceOf[VC]
        ???
      }

      def write(value: VC): Map[String, Any] = {
        val tpe = universe.typeOf[VC]
        val wrappedValues = tpe.members.filter(_.asTerm.isVal)
        assert(wrappedValues.size == 1, s"a value class must have exactly one member val, but ${value.getClass} has ${wrappedValues.size}")
        val underlyingField = wrappedValues.head.asTerm
        val underlyingValue = currentMirror.reflect(value).reflectField(underlyingField).get
        Map[String, Any](name → underlyingValue)
      }
    }
}

// TODO: organise imports, rename universe to ru, clean up type classes
// TODO: remove printlns

object MapReaderWriterExample extends App {
  case class Bar(wrappedValue: Int) extends AnyVal
  // implicit def mrBar[K0 <: Symbol](implicit wk: Witness.Aux[K0]): MapReaderWriter.Aux[Bar, K0] =
  //   new MapReaderWriter[Bar] {
  //     println("instantiating mrBar")
  //     type K = K0
  //     val name: String = wk.value.name
  //     def read(map: Map[String, Any]): Bar = Bar(map(wk.value.name).asInstanceOf[Int])
  //     def write(value: Bar): Map[String, Any] = Map[String, Any](name → value.wrappedValue)
  //   }

  case class Foo(i: Int, s: String, b: Boolean, so: Option[String], bar: Bar)
  val mrFoo = implicitly[MapReaderWriter[Foo]]

  // TODO: remove
  // val fooWithSome = Foo(1, "bar", true, Some("soValue"), Bar(42))
  // println(mrFoo.write(fooWithSome))


  val fooWithSome = Foo(1, "bar", true, Some("soValue"), Bar(42))
  val fooWithNone = Foo(1, "bar", true, None, Bar(42))
  Seq(fooWithSome, fooWithNone) foreach { foo =>
    val fooMap = mrFoo.write(foo)
    println(foo + " <==> " + fooMap)
    assert(mrFoo.read(fooMap) == foo)
    assert(fooMap("bar") == 42, s"bar must be `42`, but was `${foo.bar}`")
  }

  // case class CCWithLabel(i: Int) extends WithLabel {
  //   def label = "my custom label"
  // }

  // val ccWithLabel = CCWithLabel(1)
  // println(LabelReader.label(fooWithSome))
  // println(LabelReader.label(ccWithLabel))
  // assert(LabelReader.label(fooWithSome) == "Foo")
  // assert(LabelReader.label(ccWithLabel) == "my custom label")
}

object ReflectionApp extends App {
  import scala.reflect.runtime.universe
  import scala.reflect.runtime.currentMirror

  case class Foo(myValue: Int) extends AnyVal
  val foo = Foo(42)

  val tpe = universe.typeOf[Foo]
  val wrappedValues = tpe.members.filter(_.asTerm.isVal)
  assert(wrappedValues.size == 1, s"a value class must have exactly one member val, but $foo has ${wrappedValues.size}")
  val underlyingField = wrappedValues.head.asTerm

  val underlyingValue = currentMirror.reflect(foo).reflectField(underlyingField).get
  println(underlyingValue)
}
