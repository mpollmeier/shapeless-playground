import shapeless._
import shapeless.poly._
import shapeless.ops.hlist._
import shapeless.UnaryTCConstraint._

object Main extends App {
  case class Label[A](name: String)
  case class LabelWithValue[A](label: Label[A], value: A)

  var horribleGlobalState: Map[String, Any] = _
  object GetLabelWithValue extends Poly1 {
    implicit def caseLabel[A] = at[Label[A]] { label ⇒
      // TODO: avoid the horrible global state - pass in the Map as a parameter
      LabelWithValue(label, horribleGlobalState.get(label.name).asInstanceOf[A])
    }
  }

  val label1 = Label[Int]("a")
  val label2 = Label[String]("b")
  val labels = label1 :: label2 :: HNil
  val labelsWithValues: LabelWithValue[Int] :: LabelWithValue[String] :: HNil = getValues(labels)
  println(labelsWithValues)

  def getValues[L <: HList : *->*[Label]#λ, M <: HList](labels: L)(
    implicit mapper: Mapper.Aux[GetLabelWithValue.type, L, M]) = {

    // simplified - imagine we get the content of the map from a database
    horribleGlobalState = Map("a" -> 5, "b" -> "five")
    // val a: Int =
    labels map GetLabelWithValue
  }
}
