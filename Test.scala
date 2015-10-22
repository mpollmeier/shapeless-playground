import shapeless._
import shapeless.poly._
import shapeless.ops.hlist._
import shapeless.ops.tuple.IsComposite

object Main extends App {
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

  val labelsWithValues: LabelWithValue[Int] :: HNil = getLabelWithValues(labels)
  // manually mapping it works fine:
  val valuesManual: Int :: HNil = labelsWithValues.map(GetLabelValue)

  // using a second function with Mapper works fine:
  val valuesSecondFn: Int :: HNil = getValues(labelsWithValues)

  // error: could not find implicit value for parameter mapper: shapeless.ops.hlist.Mapper.Aux[Main.GetLabelValue.type,WithValues,Values]
  // val valuesFull: Int :: HNil = getValuesFull(labels)


  def getLabelWithValues[L <: HList, P, WithValues](labels: L)(
    implicit folder: RightFolder.Aux[L, (HNil.type, Map[String, Any]), combineLabelWithValue.type, P],
    ic: IsComposite.Aux[P, WithValues, _]
  ): WithValues = {
    val state = Map("a" -> 5, "b" -> "five")
    val resultTuple = labels.foldRight((HNil, state))(combineLabelWithValue)
    ic.head(resultTuple)
  }

  def getValues[WithValues <: HList, Values <: HList](withValues: WithValues)(
    implicit mapper: Mapper.Aux[GetLabelValue.type, WithValues, Values]
  ): Values = {
    withValues.map(GetLabelValue)
  }

  def getValuesFull[L <: HList, P, WithValues <: HList, Values <: HList](labels: L)(
    implicit folder: RightFolder.Aux[L, (HNil.type, Map[String, Any]), combineLabelWithValue.type, P],
    ic: IsComposite.Aux[P, WithValues, _],
    mapper: Mapper.Aux[GetLabelValue.type, WithValues, Values]
  ): Values = {
    val state = Map("a" -> 5, "b" -> "five")
    val resultTuple = labels.foldRight((HNil, state))(combineLabelWithValue)
    val withValues: WithValues = ic.head(resultTuple)
    withValues.map(GetLabelValue)
  }
}
