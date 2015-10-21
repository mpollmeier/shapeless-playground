import shapeless._
import shapeless.poly._
import shapeless.ops.hlist._

object Main extends App {
  case class Label[A](name: String)
  case class LabelWithValue[A](label: Label[A], value: A)

  object combineLabelWithValue extends Poly2 {
    implicit def atLabel[A, B <: HList] = at[Label[A], (Map[String, Any], B)] {
      case (label, (values, acc)) ⇒
        // println((label, (values, acc)))
        (values, LabelWithValue(label, values.get(label.name).asInstanceOf[A]) :: acc)
    }
  }

  val label1 = Label[Int]("a")
  // val label2 = Label[String]("b")
  // val labels = label1 :: label2 :: HNil
  val labels = label1 :: HNil
  // val labelsWithValues: LabelWithValue[Int] :: LabelWithValue[String] :: HNil = getValues(labels)._2
  // val labelsWithValues: LabelWithValue[Int] :: LabelWithValue[String] :: HNil = getValues(labels)
  // println(labelsWithValues)
  // println(getValues(labels))
  // println(getValues(labels)._2)

  // type Wanted = LabelWithValue[Int] :: LabelWithValue[String] :: HNil
  // type Wanted = Label[Int] :: Label[String] :: HNil


  // trait Extractor[MappedHList, TupleWithMappedHList <: Product2[_,MappedHList]] extends DepFn0 with Serializable { type Out }
  // trait Extractor[MappedHList, TupleWithMappedHList <: Product2[_,MappedHList]] { type Out }
  // object Extractor {
  //   def apply[MappedHList, TupleWithMappedHList <: Product2[_,MappedHList]](implicit Extractor: Extractor[MappedHList, TupleWithMappedHList]): Aux[MappedHList, TupleWithMappedHList, Extractor.Out] = Extractor

  //   type Aux[MappedHList, TupleWithMappedHList <: Product2[_,MappedHList], Out0] = Extractor[MappedHList, TupleWithMappedHList] { type Out = Out0 }

  //   implicit def create[MappedHList, TupleWithMappedHList <: Product2[_,MappedHList]]: Aux[MappedHList, TupleWithMappedHList, MappedHList] =
  //     new Extractor[MappedHList, TupleWithMappedHList] {
  //       // TODO: calculate the  part - get some implicit? that's what this one is for...
  //       type Out = MappedHList
  //       // def apply(): Out = ???
  //     }
  // }

  // trait Extractor[R, P <: Product2[_, R]] { type Out }
  // object Extractor {
  //   def apply[R, P <: Product2[_, R]](implicit extractor: Extractor[R, P]): Aux[R, P] = extractor

  //   type Aux[R, P <: Product2[_, R]] = Extractor[R,P] { type Out = R }

  //   implicit def create[R, P <: Product2[_, R]]: Aux[R, P] = ???
  // }

  // import Extractor._

  // trait RightFolder2[T, U, P] extends DepFn2[T, U] with Serializable

  // object RightFolder2 {
  //   def apply[T, U, P](implicit folder2: RightFolder2[T, U, P]): Aux[T, U, P, folder2.Out] = folder2

  //   type Aux[T, U, P, Out0] = RightFolder2[T, U, P] { type Out = Out0 }

  //   implicit def folder2[T, L <: HList, U, P]
  //     (implicit gen: Generic.Aux[T, L], folder2: RightFolder2[L, U, P]): Aux[T, U, P, folder2.Out] =
  //     new RightFolder2[T, U, P] {
  //       type Out = folder2.Out
  //       def apply(t: T, u: U): Out = folder2(gen.to(t), u)
  //     }
  // }


  // import shapeless.ops.function._
  // import shapeless.syntax.std._
  // import shapeless.syntax.std.product._
  // import RightFolder2._
  import shapeless.ops.tuple.IsComposite
  import shapeless.ops.tuple.IsComposite.isComposite
  val labelsWithValues = getValues(labels)
  // val labelsWithValues: LabelWithValue[Int] :: HNil = getValues(labels)._2
  // val labelsWithValues: LabelWithValue[Int] :: HNil = getValues(labels)

  // val p: Product2[_, String] = (5, "five")
  // val p = (5, "five")
  // val po: Int = productOps(p)

  def getValues[L <: HList, P: IsComposite](labels: L)(
    // def getValues[L <: HList, P](labels: L)(
    implicit folder: RightFolder.Aux[L, (Map[String, Any], HNil.type), combineLabelWithValue.type, P]) = {
    // implicit folder: RightFolder2.Aux[L, (Map[String, Any], HNil.type), combineLabelWithValue.type, P],
    // ev: ProductOps[P]) = {
    // extractor: Extractor.Aux[TupleWithMappedHList, RightPart]): RightPart = {
    val state = Map("a" -> 5, "b" -> "five")
???
    // val ret = labels.foldRight((state, HNil))(combineLabelWithValue)
    // ret
  }

  // def getValues[L <: HList, MappedHList, RightPart, TupleWithMappedHList <: Product2[_, MappedHList]](labels: L)(
  //   implicit folder: RightFolder.Aux[L, (Map[String, Any], HNil.type), combineLabelWithValue.type, TupleWithMappedHList],
  //   extractor: RightExtractor.Aux[MappedHList, TupleWithMappedHList, RightPart]) = {
  //   val state = Map("a" -> 5, "b" -> "five")
  //   val ret = labels.foldRight((state, HNil))(combineLabelWithValue)
  //   // ret._2
  //   ret
  // }



  // def getValues[L <: HList, Z, Z1](labels: L)(
  //   implicit folder: RightFolder.Aux[L, (Map[String, Any], HNil.type), combineLabelWithValue.type, Z],
  //   extractor: TupleExtractor.Aux[Z, Z1]): Z1 = {
  //   val state = Map("a" -> 5, "b" -> "five")
  //   val ret = labels.foldRight((state, HNil))(combineLabelWithValue)

  //   // import syntax.std.tuple._
  //   // ret.head
  //   // println(ret.getClass)
  //   // println(ret.productArity)
  //   ret
  //   // ret._2
  // }
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
