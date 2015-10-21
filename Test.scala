import shapeless._
import shapeless.poly._
import shapeless.ops.hlist._
import shapeless.UnaryTCConstraint._

object Main extends App {
  case class Label[A](name: String)
  case class LabelWithValue[A](label: Label[A], value: A)

  object combineLabelWithValue extends Poly2 {
    implicit def atLabel[A, B <: HList] = at[Label[A], (Map[String, Any], B)] {
      case (i, (map, res)) ⇒
        (map, LabelWithValue(i, map.get(i.name).asInstanceOf[A]) :: res)
    }
  }

  val label1 = Label[Int]("a")
  val label2 = Label[String]("b")
  val labels = label1 :: label2 :: HNil
  val labelsWithValues: LabelWithValue[Int] :: LabelWithValue[String] :: HNil = getValues(labels)._2
  println(labelsWithValues)

  // def getValues[L <: HList : *->*[Label]#λ, X, Z <: Product2[_,X]](labels: L)(
  def getValues[L <: HList : *->*[Label]#λ, Z](labels: L)(
    implicit folder: RightFolder.Aux[L, (Map[String, Any], HNil.type), combineLabelWithValue.type, Z]) = {

    val state = Map("a" -> 5, "b" -> "five")
    labels.foldRight((state, HNil))(combineLabelWithValue)
  }
}

// object Main extends App {
//   case class Label[A](name: String)
//   case class LabelWithValue[A](label: Label[A], value: A)

//   object combine extends Poly2 {
//     implicit def workS[A <: HList, B] = at[Label[B], (Map[String, Any], A)] {
//       case (i, (map, res)) ⇒
//         (map, LabelWithValue(i, map.get(i.name).asInstanceOf[B]) :: res)
//     }
//   }

//   val state: Map[String, Any] = Map("a" -> 5, "b" -> "five")

//   val label1 = Label[Int]("a")
//   val label2 = Label[String]("b")

//   val labels = label1 :: label2 :: HNil
//   val mapped = labels.foldRight((state, HNil))(combine)._2
//   println(mapped)
// }
