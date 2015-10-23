import shapeless._
import shapeless.poly._
import shapeless.ops.hlist._
import shapeless.ops.tuple.IsComposite

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
  def callWithSized[L <: HList](l: L)(implicit ev: Sized[L, Nat._2]) =
    6

  // def callWithSized[L <: HList :IsHCons](l: L) = 6

}
