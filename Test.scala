import shapeless._
import shapeless.poly._
import shapeless.ops.hlist._
import shapeless.UnaryTCConstraint._

object Util {
  case class Label[A](name: String, value: A)

  object GetLabelName extends (Label ~> Const[String]#λ) {
    def apply[A](label: Label[A]) = label.name
  }
}

object Main extends App {
  import Util._

  val label = Label("a", 5)
  val label2 = Label("b", "someString")
  // val labelName = GetLabelName(label)
  // val s: String = labelName
  // println(labelName)

  val labels = label :: label2 :: HNil
  println(labelNames(labels))
  // val names = labels map GetLabelName
  // println(names)



  def compiles = {
    val names = "a" :: "b" :: HNil
    bar(names.toList)
  }

  def bar(l: List[String]) = l

  def labelNames[L <: HList : *->*[Label]#λ, M <: HList](labels: L)(
    implicit mapper: Mapper.Aux[GetLabelName.type, L, M],
    trav: ToTraversable.Aux[M, List, String]): List[String] =
    bar(labels.map(GetLabelName).toList)

  // A is an HList whose members are all Label[_]
  def doesNotCompile[A <: HList : *->*[Label]#λ](labels: A)(
    implicit ev1: Mapper[GetLabelName.type, A]) = {
  // implicit ev1: Mapper[String, A]) = {

    val names = labels map GetLabelName
    // names is of type `ev1.Out` - I want it to be an HList of Strings

    // bar(names.toList)
    // error: could not find implicit value for parameter toTraversableAux:
    // shapeless.ops.hlist.ToTraversable.Aux[ev1.Out,List,Lub]
  }
}
