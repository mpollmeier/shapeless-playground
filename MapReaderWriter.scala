import shapeless._
import scala.reflect.ClassTag
import scala.reflect.runtime.{currentMirror, universe ⇒ ru}
import shapeless.test.illTyped

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
    case a: WithLabel ⇒ a.label
    case other        ⇒ other.getClass.getSimpleName
  }
}

object MapReaderWriter {
  type Aux[A, K0] = MapReaderWriter[A] { type K = K0 }

  def mrSimple[T, K0 <: Symbol](implicit wk: Witness.Aux[K0]): MapReaderWriter.Aux[T, K0] =
    new MapReaderWriter[T] {
      type K = K0
      println("instantiating mrSimple")
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
      println("instantiating mrOption")
      val name: String = wk.value.name
      def read(map: Map[String, Any]): Option[B] = map.get(wk.value.name).asInstanceOf[Option[B]]
      def write(value: Option[B]): Map[String, Any] = value match {
        case Some(value) ⇒ Map[String, Any](name → value)
        case None        ⇒ Map.empty
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

  implicit def mrValueClass[K0 <: Symbol, VC <: AnyVal: ru.TypeTag: ClassTag](implicit wk: Witness.Aux[K0]): MapReaderWriter.Aux[VC, K0] =
    new MapReaderWriter[VC] {
      println("instantiating mrValueClass")
      type K = K0
      val tpe = ru.typeOf[VC]
      val name: String = wk.value.name
      def read(map: Map[String, Any]): VC = {
        val valueClass = tpe.typeSymbol.asClass
        val ctor = tpe.decl(ru.termNames.CONSTRUCTOR).asMethod
        val ctorm = currentMirror.reflectClass(valueClass).reflectConstructor(ctor)
        ctorm(map(wk.value.name)).asInstanceOf[VC]
      }

      def write(value: VC): Map[String, Any] = {
        val wrappedValues = tpe.members.filter(_.asTerm.isVal)
        assert(wrappedValues.size == 1, s"a value class must have exactly one member val, but ${value.getClass} has ${wrappedValues.size}")
        val underlyingField = wrappedValues.head.asTerm
        val underlyingValue = currentMirror.reflect(value).reflectField(underlyingField).get
        Map[String, Any](name → underlyingValue)
      }
    }
}

object MapReaderWriterExample extends App {
  case class MyValueClass(wrappedValue: Int) extends AnyVal
  case class CCWithAll(i: Int, s: String, b: Boolean, so: Option[String], bar: MyValueClass)
  val mrCCWithAll = implicitly[MapReaderWriter[CCWithAll]]

  val ccWithSome = CCWithAll(1, "bar", true, Some("soValue"), MyValueClass(42))
  val ccWithNone = CCWithAll(1, "bar", true, None, MyValueClass(42))
  Seq(ccWithSome, ccWithNone) foreach { cc ⇒
    val ccMap = mrCCWithAll.write(cc)
    println(cc + " <==> " + ccMap)
    assert(mrCCWithAll.read(ccMap) == cc)
    assert(ccMap("bar") == 42, s"bar must be `42`, but was `${cc.bar}`")
  }

  case class CCWithLong(l: Long, l2: Long)
  // TODO: should not compile because there's no implicit MRW for Long...
  implicitly[MapReaderWriter[CCWithLong]]
  // illTyped { """implicitly[MapReaderWriter[CCWithLong]]""" }

  case class CCWithLabel(i: Int) extends WithLabel {
    def label = "my custom label"
  }

  val ccWithLabel = CCWithLabel(1)
  println(LabelReader.label(ccWithSome))
  println(LabelReader.label(ccWithLabel))
  assert(LabelReader.label(ccWithSome) == "CCWithAll")
  assert(LabelReader.label(ccWithLabel) == "my custom label")
}

object ReflectionGetValueApp extends App {
  case class Foo(myValue: Int) extends AnyVal
  val foo = Foo(42)

  val tpe = ru.typeOf[Foo]
  val wrappedValues = tpe.members.filter(_.asTerm.isVal)
  assert(wrappedValues.size == 1, s"a value class must have exactly one member val, but $foo has ${wrappedValues.size}")
  val underlyingField = wrappedValues.head.asTerm

  val underlyingValue = currentMirror.reflect(foo).reflectField(underlyingField).get
  println(underlyingValue)
}

object ReflectionCallConstrApp extends App {
  case class Foo(myValue: Int) extends AnyVal
  val foo = Foo(42)

  val tpe = ru.typeOf[Foo]
  val valueClass = tpe.typeSymbol.asClass
  val ctor = tpe.decl(ru.termNames.CONSTRUCTOR).asMethod
  val ctorm = currentMirror.reflectClass(valueClass).reflectConstructor(ctor)
  val p = ctorm(foo.myValue)
  println(p)
  assert(foo == p)
}
