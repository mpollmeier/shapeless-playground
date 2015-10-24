import shapeless._
import shapeless.poly._
import shapeless.ops.hlist._
import shapeless.ops.tuple.IsComposite
import shapeless.ops.product._

object Main extends App {
  object SizeTest {
    // callWithSized(HNil)
    // callWithSized(5 :: HNil)
    callWithSized(5 :: "five" :: HNil)

    // witness that an hlist has exactly length 2
    // def callWithSized[L <: HList](l: L)(implicit ev: Length.Aux[L, Nat._2]) = 6

    // witness that hlist has at least two elements
    def callWithSized[L <: HList, H0, T0 <: HList](l: L)(
      implicit hasOne: IsHCons.Aux[L, H0, T0],
      hasTwo: IsHCons[T0]
    ) = 6

    // get size of HList
    val size = Nat.toInt(Length[Int :: Int :: HNil].apply) //2
  }
  val sizeTest = SizeTest

  object TupleTest {
    val tuple: (Int, String) = convertToTuple(5 :: "five" :: HNil)
    val tuple2: (Int, String) = convertToTuple2(5 :: "five" :: HNil)

    def convertToTuple[L <: HList, Out](l: L)(
      implicit tupler: Tupler.Aux[L, Out]
    ) : Out = tupler(l)

    def convertToTuple2[L <: HList](l: L)(
      implicit tupler: Tupler[L]
    ) : tupler.Out = tupler(l)
  }
  val tupleTest = TupleTest


  object RightFoldWithMappedTest {
    case class Label[A](name: String)
    case class LabelWithValue[A](label: Label[A], value: A)

    val label1 = Label[Int]("a")
    val labels = label1 :: HNil

    object combineLabelWithValue extends Poly2 {
      implicit def atLabel[A, B <: HList] = at[Label[A], (B, Map[String, Any])] {
        case (label, (acc, values)) ⇒
          (LabelWithValue(label, values(label.name).asInstanceOf[A]) :: acc, values)
      }
    }

    object GetLabelValue extends (LabelWithValue ~> Id) {
      def apply[B](labelWithValue: LabelWithValue[B]) = labelWithValue.value
    }

    val rightFoldWithMapped: Int :: HNil = rightFoldWithMapped(labels)
    println(rightFoldWithMapped)

    def rightFoldWithMapped[L <: HList, P <: Product2[_, _], WithValues <: HList, Values <: HList](labels: L)(
      implicit folder: RightFolder.Aux[L, (HNil, Map[String, Any]), combineLabelWithValue.type, P],
      ic: IsComposite.Aux[P, WithValues, _],
      mapper: Mapper.Aux[GetLabelValue.type, WithValues, Values]
    ): Values = {
      val state = Map("a" -> 5, "b" -> "five")
      val resultTuple = labels.foldRight((HNil: HNil, state))(combineLabelWithValue)
      val withValues: WithValues = ic.head(resultTuple)
      withValues.map(GetLabelValue)
    }
  }
  // val rightFoldWithMapped = RightFoldWithMappedTest

  object DestructureTupleTest {
    case class Label[A](name: String)

    val label1 = Label[Int]("a")
    val labels = label1 :: HNil

    object getValue extends Poly2 {
      implicit def atLabel[A, B <: HList] = at[Label[A], (B, Map[String, Any])] {
        case (label, (acc, values)) ⇒
          (values(label.name).asInstanceOf[A] :: acc, values)
      }
    }

    // compiles fine
    val untupled1: Int :: HNil = rightFoldUntupled1(labels)

    // [error] could not find implicit value for parameter folder:
    // shapeless.ops.hlist.RightFolder.Aux[shapeless.::[Main.DestructureTupleTest.Label[Int],shapeless.HNil],
    //(shapeless.HNil, Map[String,Any]),Main.DestructureTupleTest.getValue.type,P]
    // val untupled2: Int :: HNil = rightFoldUntupled2(labels)

    def rightFoldUntupled1[L <: HList, P <: Product2[_, _], Values <: HList](labels: L)(
      implicit folder: RightFolder.Aux[L, (HNil, Map[String, Any]), getValue.type, P],
      ic: IsComposite.Aux[P, Values, _]
    ): Values = {
      val state = Map("a" -> 5, "b" -> "five")
      val resultTuple = labels.foldRight((HNil: HNil, state))(getValue)
      ic.head(resultTuple)
    }

    def rightFoldUntupled2[L <: HList, Values, P <: Product2[_, Values]](labels: L)(
      implicit folder: RightFolder.Aux[L, (HNil, Map[String, Any]), getValue.type, P]
    ): Values = {
      val state = Map("a" -> 5, "b" -> "five")
      val resultTuple = labels.foldRight((HNil: HNil, state))(getValue)
      resultTuple._2
    }
  }
  val destructureTupleTest = DestructureTupleTest
}
