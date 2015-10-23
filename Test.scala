import shapeless._
import shapeless.poly._
import shapeless.ops.hlist._
import shapeless.ops.tuple.IsComposite
import shapeless.ops.product._

object Main extends App {
  case class Label[A](name: String)

  val label1 = Label[Int]("a")
  val labels = label1 :: HNil

  object GetValue extends Poly2 {
    implicit def atLabel[A, B <: HList] = at[Label[A], (B, Map[String, Any])] {
      case (label, (acc, values)) ⇒
        (values(label.name).asInstanceOf[A] :: acc, values)
    }
  }

  val values: Int :: HNil = getValues(labels)

  def getValues[L <: HList, P, Values](labels: L)(
    implicit folder: RightFolder.Aux[L, (HNil.type, Map[String, Any]), GetValue.type, P],
    ic: IsComposite.Aux[P, Values, _]
  ): Values = {
    val state = Map("a" -> 5, "b" -> "five")
    val resultTuple = labels.foldRight((HNil, state))(GetValue)
    ic.head(resultTuple)
  }


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

  val tuple: (Int, String) = convertToTuple(5 :: "five" :: HNil)
  def convertToTuple[L <: HList, Out](l: L)(
    implicit tupler: Tupler.Aux[L, Out]
  ) : Out = tupler(l)

}
