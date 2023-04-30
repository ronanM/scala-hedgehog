package hedgehog.examples.state

import hedgehog.Range.linear
import hedgehog._
import hedgehog.examples.state.FromVar._
import hedgehog.runner._
import hedgehog.state._

import java.util.UUID.randomUUID
import scala.collection.mutable

trait CRMultiple {

  def create(v: String): CRId

  def read(id: CRId): Option[String]

  def create2(v1: String, v2: String): (CRId, CRId)

  def createList(vs: List[String]): List[CRId]

  def readList(ids: List[CRId]): List[Option[String]]
}

// $ sbt "~exampleJVM/runMain hedgehog.examples.state.CRMultipleTest"
// Original trick by Ollie Charles in comment https://github.com/hedgehogqa/haskell-hedgehog/issues/459#issuecomment-1152383992
object CRMultipleTest extends Properties {

  override def tests: List[Prop] =
    List(
      property("test CRMultiple", testSequential())
      //.withTests(10_000)
    )

  def commands(sut: CRMultiple): List[CommandIO[StateMultiple]] = // `sut`: System Under Test.
    List(
      commandPut(sut),
      commandPut2(sut),
      commandPutMultiple(sut),
      commandGet(sut),
      commandGetMultiple(sut)
    )

  def testSequential(): Property = {
    val datas:       mutable.Map[CRId, String] = mutable.Map.empty
    val sut:         CRMultiple                = CRMultiple.withMutableMapCRId(datas)
    var testCounter: Int                       = 0
    sequential(
      range    = linear(1, 10),
      initial  = StateMultiple(Map()),
      commands = commands(sut),
      cleanup = () => {
        datas.clear()
        testCounter += 1
        println(s"--- Cleanup $testCounter ------------------------------------------------------------------------------------------------")
      }
    )
  }

  case class StateMultiple(map: Map[FromVar[CRId], String])

  case class Create(value:          String)
  case class Read(key:              FromVar[CRId])
  case class Create2(v1:            String, v2: String)
  case class CreateMultiple(values: List[String])
  case class ReadMultiple(keys:     List[FromVar[CRId]])

  def commandPut(sut: CRMultiple): CommandIO[StateMultiple] =
    new Command[StateMultiple, Create, CRId] {

      override def gen(s: StateMultiple): Option[Gen[Create]] =
        Some(Gen.string(Gen.lower, linear(1, 10)).map(Create))

      override def execute(env: Environment, i: Create): Either[String, CRId] =
        Right(sut.create(i.value))

      override def update(s: StateMultiple, i: Create, o: Var[CRId]): StateMultiple =
        s.copy(map = s.map + (Direct(o) -> i.value))

      override def ensure(env: Environment, s0: StateMultiple, s: StateMultiple, i: Create, o: CRId): Result =
        Result.success
    }

  def commandGet(sut: CRMultiple): CommandIO[StateMultiple] =
    new Command[StateMultiple, Read, Option[String]] {

      override def gen(s: StateMultiple): Option[Gen[Read]] =
        s.map.keys.toList match {
          case Nil    => None
          case h :: t => Some(Gen.element(h, t).map(Read))
        }

      override def vars(i: Read): List[Var[_]] =
        List(getVar(i.key))

      override def execute(env: Environment, i: Read): Either[String, Option[String]] =
        Right(sut.read(getValue(i.key, env)))

      override def update(s: StateMultiple, i: Read, o: Var[Option[String]]): StateMultiple =
        s

      override def ensure(env: Environment, s0: StateMultiple, s: StateMultiple, i: Read, o: Option[String]): Result =
        s.map.get(i.key) ==== o
    }

  def commandPut2(sut: CRMultiple): CommandIO[StateMultiple] =
    new Command[StateMultiple, Create2, (CRId, CRId)] {

      override def gen(s: StateMultiple): Option[Gen[Create2]] =
        Some(for {
          v1 <- Gen.string(Gen.lower, linear(1, 10))
          v2 <- Gen.string(Gen.lower, linear(1, 10))
        } yield Create2(v1, v2))

      override def execute(env: Environment, i: Create2): Either[String, (CRId, CRId)] =
        Right(sut.create2(i.v1, i.v2))

      override def update(s: StateMultiple, i: Create2, o: Var[(CRId, CRId)]): StateMultiple =
        s.copy(map = s.map + (Fst(o) -> i.v1) + (Snd(o) -> i.v2))

      override def ensure(env: Environment, before: StateMultiple, after: StateMultiple, i: Create2, o: (CRId, CRId)): Result =
        Result.success
    }

  def commandPutMultiple(sut: CRMultiple): CommandIO[StateMultiple] =
    new Command[StateMultiple, CreateMultiple, List[CRId]] {

      override def gen(s: StateMultiple): Option[Gen[CreateMultiple]] =
        Some(Gen.list(Gen.string(Gen.lower, linear(1, 10)), linear(1, 5)).map(CreateMultiple))

      override def execute(env: Environment, i: CreateMultiple): Either[String, List[CRId]] =
        Right(sut.createList(i.values))

      override def update(s: StateMultiple, i: CreateMultiple, o: Var[List[CRId]]): StateMultiple =
        s.copy(map = s.map ++ i.values.zipWithIndex.map { case (str, i) => AtIndex(i, o) -> str })

      override def ensure(env: Environment, before: StateMultiple, after: StateMultiple, i: CreateMultiple, o: List[CRId]): Result =
        Result.success
    }

  def commandGetMultiple(sut: CRMultiple): CommandIO[StateMultiple] =
    new Command[StateMultiple, ReadMultiple, List[Option[String]]] {

      override def gen(s: StateMultiple): Option[Gen[ReadMultiple]] =
        s.map.keys.toList match {
          case Nil        => None
          case l @ h :: t => Some(Gen.list(Gen.element(h, t), linear(1, l.size)).map(_.distinct).map(ReadMultiple))
        }

      override def vars(i: ReadMultiple): List[Var[_]] =
        i.keys.map(getVar)

      override def execute(env: Environment, i: ReadMultiple): Either[String, List[Option[String]]] =
        Right(sut.readList(i.keys.map(getValue[CRId](_, env))))

      override def update(s: StateMultiple, i: ReadMultiple, o: Var[List[Option[String]]]): StateMultiple =
        s

      override def ensure(env: Environment, s0: StateMultiple, s: StateMultiple, i: ReadMultiple, os: List[Option[String]]): Result =
        i.keys.zip(os).map { case (key, o) => s.map.get(key) ==== o }.fold(Result.success)(_ and _)
    }
}

object CRMultiple {

  def withMutableMapCRId(datas: mutable.Map[CRId, String]): CRMultiple =
    new CRMultiple {

      override def create(v: String): CRId = {
        val id = CRId(randomUUID.toString)
        datas += (id -> v)
        id
      }

      override def read(id: CRId): Option[String] = {
        val buggedCode = false

        if (datas.contains(id)) {
          val v         = datas(id)
          val inBugCase = buggedCode && v.contains("h") // && v.endsWith("d") && v.map(_.toInt).sum % 9 == 3

          if (inBugCase) Some("In bug case") else Some(v)
        } else {
          None
        }
      }

      override def create2(v1: String, v2: String): (CRId, CRId) = (create(v1), create(v2))

      override def createList(vs: List[String]): List[CRId] = vs.map(create)

      override def readList(ids: List[CRId]): List[Option[String]] = ids.map(read)
    }
}

// The trick is to express in pure data ADT (no lambdas, "defunctionalized functions" in Ollie Charles comment)
// the "way to access" to one piece of data from a `Var[_]` that may contains multiples data.
// And store theses "ways" in the State.
trait FromVar[A]

object FromVar {

  final case class Direct[A](v:      Var[A]) extends FromVar[A]
  final case class Fst[A, B](v:      Var[(A, B)]) extends FromVar[A]
  final case class Snd[A, B](v:      Var[(A, B)]) extends FromVar[B]
  final case class AtIndex[A](index: Int, v: Var[List[A]]) extends FromVar[A]

  def getVar(fromVar: FromVar[_]): Var[_] =
    fromVar match {
      case Direct(v)         => v
      case Fst(v)            => v
      case Snd(v)            => v
      case AtIndex(index, v) => v
      case _                 => sys.error(s"getVar($fromVar)")
    }

  def getValue[A](fromVar: FromVar[A], env: Environment): A =
    fromVar match {
      case Direct(v)         => v.get(env)
      case Fst(v)            => v.get(env)._1
      case Snd(v)            => v.get(env)._2
      case AtIndex(index, v) => v.get(env)(index)
      case _                 => sys.error(s"getValue($fromVar)")
    }
}
