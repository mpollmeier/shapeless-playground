import shapeless._
import shapeless.poly._
import shapeless.ops.hlist._
import shapeless.ops.tuple.IsComposite

object Main extends App {
  case class Label[A](name: String)
  case class LabelWithValue[A](label: Label[A], value: A)

  object combineLabelWithValue extends Poly2 {
    implicit def atLabel[A, B <: HList] = at[Label[A], (B, Map[String, Any])] {
      case (label, (acc, values)) ⇒
        (LabelWithValue(label, values.get(label.name).asInstanceOf[A]) :: acc, values)
    }
  }

  object GetLabelValue extends (LabelWithValue ~> Id) {
    def apply[B](labelWithValue: LabelWithValue[B]) = labelWithValue.value
  }

  val label1 = Label[Int]("a")
  val label2 = Label[String]("b")
  val labels = label1 :: label2 :: HNil

  val labelsWithValues: LabelWithValue[Int] :: LabelWithValue[String] :: HNil = getValues(labels)
  val values = labelsWithValues.map(GetLabelValue)
  // val values: Int :: String :: HNil = getValues(labels)
  println(values)

  def getValues[L <: HList, P, WithValue, Value](labels: L)(
    implicit folder: RightFolder.Aux[L, (HNil.type, Map[String, Any]), combineLabelWithValue.type, P],
    ic: IsComposite.Aux[P, WithValue, _]
    ): WithValue = {
    val state = Map("a" -> 5, "b" -> "five")
    val resultTuple = labels.foldRight((HNil, state))(combineLabelWithValue)
    ic.head(resultTuple)
  }
}
