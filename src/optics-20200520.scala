package p
object Infra {

  trait Functor[F[_]] {
    def fmap[A, B](f: A => B): F[A] => F[B]
  }

  trait Applicative[F[_]] {
    def pure[A]: A => F[A]
    def ap[A, B](f: F[A => B]): F[A] => F[B]
  }
}

object Optics {
import Infra._

case class Person(name: String, age: Int)

case class VehicleRegistration(num: String, owner: Person)

case class Vehicle(make: String, reg: VehicleRegistration)

// Use-case: the vehicle's owner has just had a birthday

def birthday_naive: Vehicle => Vehicle = {
  case Vehicle(make, reg) =>
    reg match {
      case VehicleRegistration(num, owner) =>
        owner match {
          case Person(name, age) =>
            Vehicle(make, VehicleRegistration(num, Person(name, age+1)))
        }
    }
  }

def modifyVehicleRegistration1: 
  (VehicleRegistration => VehicleRegistration) =>
  Vehicle => Vehicle =
  k => {
    case Vehicle(make, reg) =>
      Vehicle(make, k(reg))
  }

def modifyOwner1:
  (Person => Person)
  => VehicleRegistration
  => VehicleRegistration =
  k => {
    case VehicleRegistration(num, owner) =>
      VehicleRegistration(num, k(owner))
  }

def modifyAge1: 
  (Int => Int)
  => Person
  => Person =
  k => {
    case Person(name, age) =>
      Person(name, k(age))
  }

def modifyVehicleOwnerAge1:
  (Int => Int)
  => Vehicle
  => Vehicle =
  modifyVehicleRegistration1 compose modifyOwner1 compose modifyAge1

def birthday_composed: Vehicle => Vehicle =
  modifyVehicleOwnerAge1(_+1)

// Vehicle has exactly one VehicleRegistration and some other things
// VehicleRegistration has exactly one Person and some other things
// Person has exactly one Int and some other things
//        has exactly one     **and** some other things

sealed trait ParseError
case class UnexpectedEOF() extends ParseError
case class UnexpectedChar(expected: Char, actual: Char) extends ParseError

sealed trait ParseResult[A]
case class SuccessResult[A](a: A) extends ParseResult[A]
case class ErrorResult[A](e: ParseError) extends ParseResult[A]

// Use-case: upper-case a ParseResult's actual character

def modifyActualChar: (Char => Char) => (Char, Char) => (Char, Char) =
  k => {
    case (e, a) => (e, k(a))
  }

def modifyChars: ((Char, Char) => (Char, Char)) => ParseError => ParseError =
  k => {
    case UnexpectedEOF() => UnexpectedEOF()
    case UnexpectedChar(e, a) => {
      val (ee, aa) = k(e, a)
      UnexpectedChar(ee, aa)
    }
  
// a ParseResult has one ParseError or some other things
def modifyParseError[A]:
  (ParseError => ParseError)
  => ParseResult[A]
  => ParseResult[A] =
  k => {
    case SuccessResult(a) => SuccessResult(a)
    case ErrorResult(e) => ErrorResult(k(e))
  }

def modifyParseResultActualChar[A]: (Char => Char) => ParseResult[A] => ParseResult[A] =
  modifyParseError compose modifyChars compose modifyActualChar
}

// A has exactly one B and some other things (lens A B)
// A has exactly one B or some other things  (prism A B)
// A has exactly one B and/or some other things (traversal A B)
// A has ......      B ...... some other things (optic A B)

// These relationships come up _all the time_ in every day programming

def selectFromDatabase(/* query stuff */): Vehicle =
  ???

def insertToDatabase(v: Vehicle) /* : inserted stuff */ =
  insert into v.make, v.fdsg..dfg.age

// We will state the schema update problem:
// * we want to add a field to the Vehicle, VehicleRegistration or Person
// * we want to remove a field from our data types
// We want to do this since our data model has changed

sealed trait ErrorType1 // sum type
// C1, C2 extends ErrorType1
sealed trait ErrorType2 // sum type

def func1(/* args */): Anything Either Int =
  ???

def func2(/* args */): ErrorType2 Either String =
  ???

def func3Again: (ErrorType1 => ErrorType2) => (ErrorType1 Either ErrorType2) Either (Int, String) =
def func3(/* args */): (ErrorType1 Either ErrorType2) Either (Int, String) =
  for {
    i <- func1().left.map(Left(_))
    s <- func2().left.map(Right(_))
  } yield (i, s)

def func4() =
  try {
    func()
    // this will never occur
  } catch (ErrorType2) {

  }

sealed trait ErrorType1Again
// ErrorType1, C3

type Modify1[A, B] = (B => B) => A => A

def set(m: Modify1[A, B]):A => B => A = 

def get(m: Modify1[A, B]): A => B =

case class Identity[A](run: A)
object Identity {
  implicit def FunctorIdentity: Functor[Identity] = ... 
  implicit def ApplicativeIdentity: Applicative[Identity] = ... 
}

def modifyAge2[F[_]: Functor]: (Int => F[Int]) => Person => F[Person] =
  k => {
    case Person(name, age) =>
      implicitly[Functor[F]].fmap(_age => Person(name, _age))(k(age))
  }

type Modify2[F[_], A, B] = (B => F[B]) => A => F[A]
//          ↑ belongs existential

trait Modify3[A, B] {
  def run[F[_]: Functor]: (B => F[B]) => A => F[A]
}

def set[A, B](m: Modify3[A, B]): A => B => A =

def get[A, B](m: Modify3[A, B]): A => B =
  a => {
    m.run[({type l[X]=Const[B, X]})#l](Const(_))(a).run
  }

case class Const[A, B](run: A)

trait Semigroup[A] {
  def op: A => A => A
}

trait Monoid[A] extends Semigroup[A] {
  def id: A
}

object Const {
  implicit def ConstFunctor[X]: Functor[({type l[B]=Const[X, B]})#l] =
    new Functor[({type l[B]=Const[X, B]})#l] {
      def fmap[X, Y](f: X => Y) = c => Const(c.run)
    }

  implicit def ConstApplicative[X: Monoid]: Applicative[({type l[B]=Const[X, B]})#l] =

}

type Modify3[F[_], ~>[_, _], A, B] = (B ~> F[B]) => A ~> F[A]

// Recall our relationships:
// * "A has exactly one B and some other things"
// * "A has exactly one B or some other things"
// * or _the composition_ of these

trait Profunctor[~>[_, _]] {
  def dimap[A, B, C, D](f: A => B, g: C => D): (B ~> C) => (A ~> D)
}

object Profunctor {
  implicit def Function1Profunctor: Profunctor[Function1] = ...
}

trait Choice[~>[_, _]] extends Profunctor[~>] {
  def left[A, B, C](f: A ~> B): (A Either C) ~> (B Either C)
  def right[A, B, C](f: A ~> B): (C Either A) ~> (C Either B)
}

trait Strong[~>[_, _]] extends Profunctor[~>] {
  def first[A, B, C](f: A ~> B): (A Tuple2 C) ~> (B Tuple2 C)
  def second[A, B, C](f: A ~> B): (C Tuple2 A) ~> (C Tuple2 B)
}

def modifyChars2[F[_]: Applicative, ~>[_, _]: Choice]: (((Char, Char)) => F[(Char, Char)]) => ParseError => F[ParseError] =
  k => {
    def toEither: ParseError => (Char, Char) Either Unit =
      ...

    def _fromEither: (Char, Char) Either Unit => ParseError =
      ..

    def fromEither: (F[(Char, Char)] Either Unit) => F[ParseError] = {
      case Left(x) =>
        implicitly[Functor[F]].fmap((ea: (Char, Char)) => UnexpectedChar(ea._1, ea._2)
      case Right(()) =>
        implicitly[Applicative[F]].pure(UnexpectedEOF())
    }

    implicitly[Profunctor[~>]].dimap(toEither, fromEither)(implicitly[Choice[~>]].left(k))
  }
}

trait Optic[F[_], ~>[_, _], A, B] {
  def run: (B ~> F[B]) => A ~> F[A]
}

// A has exactly one B and some other things (lens)
// * F must be any functor
// * ~> must be Function1

// A has exactly one B or some other things (prism)
// * F must be any applicative
// * ~> must be any Choice

// A has exactly one B and/or some other things (traversal)
// * F must be any applicative
// * ~> must be Function1

// A has exactly one B and no other things (isomorphism)
// * F must be functor
// * ~> must be Profunctor

// A is B (and therefore A) (equality)
// optic
