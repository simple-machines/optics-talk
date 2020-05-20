object Infra {

trait Functor[F[_]] {
  def fmap[A, B](f: A => B): F[A] => F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  def pure[A]: A => F[A]
  def ap[A, B](f: F[A => B]): F[A] => F[B]
}

}

// This problem requires tying a lot of disjoint concepts together.
// There will be some context switching.
// Stop at any time for questions if not following.

object Optics {
import Infra._

case class Person(name: String, age: Int)

case class VehicleRegistration(num: String, owner: Person)

case class Vehicle(make: String, reg: VehicleRegistration)

// Use-case: the vehicle's owner has just had a birthday

def birthday_ew: Vehicle => Vehicle = {
  case Vehicle(make, reg) =>
    reg match {
      case VehicleRegistration(num, owner) =>
        owner match {
          case Person(name, age) =>
            Vehicle(make, VehicleRegistration(num, Person(name, age + 1)))
        }
    }
}

def modifyVehicleRegistration1:
  (VehicleRegistration => VehicleRegistration)
  => Vehicle
  => Vehicle =
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

// and now we can compose:
def modifyVehicleOwnerAge1:
  (Int => Int)
  => Vehicle
  => Vehicle =
  modifyVehicleRegistration1 compose modifyOwner1 compose modifyAge1

// this gives us:
def birthday_composed: Vehicle => Vehicle =
  modifyVehicleOwnerAge1(_+1)

// We also have a dual problem
// Not product types, but sum types

sealed trait ParseError
case class UnexpectedEOF() extends ParseError
case class UnexpectedChar(expected: Char, actual: Char) extends ParseError

sealed trait ParseResult[A]
case class SuccessResult[A](a: A) extends ParseResult[A]
case class ErrorResult[A](e: ParseError) extends ParseResult[A]

// Use-case: upper-case a ParseResult's actual character in the error

def uppercase1_ew[A]:
  ParseResult[A]
  => ParseResult[A] = {
    case SuccessResult(a) => SuccessResult(a)
    case ErrorResult(e) =>
      ErrorResult(
        e match {
          case UnexpectedEOF() => UnexpectedEOF()
          case UnexpectedChar(e, a) => UnexpectedChar(e, a.toUpper)
        }
      )
  }

def modifyActualChar:
  (Char => Char)
  => (Char, Char)
  => (Char, Char) =
  k => {
    case (e, a) => (e, k(a))
  }

def modifyChars:
  ((Char, Char) => (Char, Char))
  => ParseError
  => ParseError =
  k => {
    case UnexpectedEOF() => UnexpectedEOF()
    case UnexpectedChar(e, a) => {
      val (ee, aa) = k(e, a)
      UnexpectedChar(ee, aa)
    }
  }
  
def modifyParseError[A]:
  (ParseError => ParseError)
  => ParseResult[A]
  => ParseResult[A] =
  k => {
    case SuccessResult(a) => SuccessResult(a)
    case ErrorResult(e) => ErrorResult(k(e))
  }

// this gives us:
def modifyParseResultActualChar[A]:
  (Char => Char)
  => ParseResult[A]
  => ParseResult[A] =
  modifyParseError compose modifyChars compose modifyActualChar

// and now we can compose:
def uppercase1_composed[A]:
  ParseResult[A]
  => ParseResult[A] =
  modifyParseResultActualChar[A](_.toUpper)

// All of these "modify" relationships are in one of two forms:
// * "A has exactly one B and some other things"
// * "A has exactly one B or some other things"
// * or _the composition_ of these
// For example:
// * Vehicle has exactly one VehicleRegistration _and_ some other things
// * VehicleRegistration has exactly one Person (owner) _and_ some other things
// * Person has exactly one Int (age) _and_ some other things
// Since this relationship is consistent through composition, it is preserved under composition:
// * Vehicle has exactly one Int (age) _and_ some other things

// However, let's look at ParseError as it relates to "actual char in an error"
// * ParseResult has exactly one ParseError _or_ some other things
// * ParseError has exactly one (Char, Char) _or_ some other things
// * (Char, Char) has exactly one Char _and_ some other things
// This relationship is _not_ preserved under composition
// * ParseResult has exactly one (Char, Char) or some other things
// * ParseResult has exactly one Char and/or some other things

// We ultimately have three relationships:
// 1. A has exactly one B and some other things
// 2. A has exactly one B or some other things
// 3. A has exactly one B and/or some other things
// where the third relationship is strictly weaker than the other two

////

// These relationships come up _all the time_ in every day programming
// Here are some example database operations:

def selectFromDatabase(/* query stuff */): Vehicle =
  ???

def insertToDatabase(v: Vehicle) /* : inserted stuff */ =
  ???

// We are omitting the "update" and "delete" operations, since it raises questions
// * should the database be append-only?
// * what are the consequences for our data model if we allow update/delete?

// However, we *will* allow insert and delete on the data types themselves.
// e.g. add or remove a field from Vehicle
// * delete then insert = update

// We are familiar with the "database schema update problem"
// We will state it specifically here as:
// * "we want to add a field to the Vehicle, VehicleRegistration or Person data types"
// * "we want to remove a field from the Vehicle, VehicleRegistration or Person data types"
// We want to do this because our data model has changed
// However, we still have prior records that reflect the current data model

// Here is another problem that comes up in every day programming:

sealed trait ErrorType1
// ErrorType1 (sum type) constructors
sealed trait ErrorType2
// ErrorType2 (sum type) constructors

def func1(/* args */): ErrorType1 Either Int =
  ???
def func2(/* args */): ErrorType2 Either String =
  ???
//                  This is not cool ↓↓
def func3(/* args */): (ErrorType1 Either ErrorType2) Either (Int, String) = 
  for {
    i <- func1().left.map(Left(_)) // promote
    s <- func2().left.map(Right(_)) // promote
  } yield (i, s)

// Either[ErrorType1, X] has exactly one ErrorType1 or some other things
// Either[ErrorType2, X] has exactly one ErrorType2 or some other things
// We want the composition of these as our program builds up
// We also might wish to add constructors to our error types after the fact
// Actually, this particular problem has some research behind it: The Expression Problem (Wadler)

// In particular for our two given every day programming problems:
// * We want to make the data type updates without adjusting our calling code, except when necessary
// * For example, if a function uses only the age field of a Vehicle
//   * if we have only added a new field to Vehicle, that function continues operating
//   * if we have deleted a field from a Vehicle, and that field is not age, that function continues operating
//   * if we have deleted the age field, *and it is necessary to the continued operation of the function*, we select:
//     * we can write a minimal "migration strategy" for the function to continue operating
//     * the function stops operating and is no longer used
// * Similarly, we have a function that uses a constructor argument of ErrorType1
//   * if we have only added a new constructor to ErrorType1, that function continues operating
//   * if we have deleted a constructor from ErrorType1, and that constructor is not used by our function, that function continues operating
//   * if we have deleted a constructor from ErrorType1, *and it is necessary to the continued operation of the function*, we select:
//     * we can write a minimal "migration strategy" for the function to continue operating
//     * the function stops operating and is no longer used

// Overall, we want to maximise the case where the function continues operating
// We can do this by writing our functions in terms of a _minimal abstraction_ that allows the function to achieve its goal _and nothing more_

// Consider, for example, the function:
//   def insertToDatabase(v: Vehicle) /* : inserted stuff */ =
// This function is overly-specified in its type.
// It does *not* insert a Vehicle. Rather, it inserts any data type that has certain (and minimally expressed) relationships to its structure
// * X such that it has exactly one Y and exactly one Z

////

// The canonical name for this abstraction is an Optic.
// Let's discover optics.

// First, let's go back and look at the types of our modify operations from earlier:
// (VehicleRegistration => VehicleRegistration) => Vehicle => Vehicle
// (ParseError => ParseError) => ParseResult[A] => ParseResult[A]
// (Char => Char) => (Char, Char) => (Char, Char)
// (Int => Int) => Person => Person

// These all have a similar structure:
// (B => B) => A => A
type Modify1[A, B] = (B => B) => A => A

// However, this structure is not enough to write other interesting functions.
// For example, using a modify operation, we could write a set function:
// B => A => A
// But we could not write a get function:
// A => B
// We have fallen short of our desire quite quickly.
// We want to write many different and useful functions in terms of our modify operation.

// Recall: Identity
case class Identity[A](run: A)
object Identity {
  // Identity is a Functor and Applicative
  implicit def IdentityApplicative: Applicative[Identity] =
    new Applicative[Identity] {
      def fmap[A, B](f: A => B) = i => Identity(f(i.run))
      def pure[A] = Identity(_)
      def ap[A, B](f: Identity[A => B]) = i => Identity(f.run(i.run))
    }
}

def modifyVehicleRegistration2:
  (VehicleRegistration => Identity[VehicleRegistration])
  => Vehicle
  => Identity[Vehicle] =
  k => {
    case Vehicle(make, reg) =>
      implicitly[Functor[Identity]].fmap(_reg => Vehicle(make, _reg))(k(reg))
  }

def modifyOwner2:
  (Person => Identity[Person])
  => VehicleRegistration
  => Identity[VehicleRegistration] =
  k => {
    case VehicleRegistration(num, owner) =>
      implicitly[Functor[Identity]].fmap(_owner => VehicleRegistration(num, _owner))(k(owner))
  }

def modifyAge2:
  (Int => Identity[Int])
  => Person
  => Identity[Person] =
  k => {
    case Person(name, age) =>
      implicitly[Functor[Identity]].fmap(_age => Person(name, _age))(k(age))
  }

// We can recover the original modify operation by removing Identity

// More importantly, if we examine the type of the modify operation, is overly-specialised.
// For example, we can replace Identity with any Functor:

def modifyVehicleRegistration3[F[_]: Functor]:
  (VehicleRegistration => F[VehicleRegistration])
  => Vehicle
  => F[Vehicle] =
  k => {
    case Vehicle(make, reg) =>
      implicitly[Functor[F]].fmap(_reg => Vehicle(make, _reg))(k(reg))
  }

// Since we know that Functors are closed under composition...

case class Composition[F[_], G[_], A](run: F[G[A]])

object Composition {
  // In other words, if F is a Functor, and G is a Functor,
  // then F ∘ G is a Functor
  implicit def CompositionFunctor[F[_]: Functor, G[_]: Functor]: Functor[({type l[A]=Composition[F, G, A]})#l] =
    new Functor[({type l[A]=Composition[F, G, A]})#l] {
      def fmap[A, B](f: A => B) =
        comp =>
          Composition(implicitly[Functor[F]].fmap(implicitly[Functor[G]].fmap(f))(comp.run))
    }
}

// ... and since our original modify operations were also closed under composition,
// then it follows that the composition of our new modify operations will preserve the relationship

type Modify2[F[_], A, B] = (B => F[B]) => A => F[A]
// This      ↑ belongs in existential position

trait Modify3[A, B] {
  def run[F[_]: Functor]: (B => F[B]) => A => F[A]
}

// Using our generalised modify function, we can write a set function.
//   B => A => B
// We would specialise F to Identity and const away the "previous value" in the function

def set1[A, B](m: Modify3[A, B]): B => A => A =
  b => a => {
    val i = m.run[Identity]
    i(_ => Identity(b))(a).run
  }

// But can we write a get function?
//   A => B

// Recall: Const
case class Const[A, B](run: A)

trait Semigroup[A] {
  def op: A => A => A
}

trait Monoid[A] extends Semigroup[A] {
  def id: A
}

// Const[X, _] is a Functor and Applicative with Monoid[X] constraint
object Const {
  implicit def ConstFunctor[X]: Functor[({type l[B]=Const[X, B]})#l] =
    new Functor[({type l[B]=Const[X, B]})#l] {
      def fmap[A, B](f: A => B) = c => Const(c.run)
    }

  implicit def ConstApplicative[X: Monoid]: Applicative[({type l[B]=Const[X, B]})#l] =
    new Applicative[({type l[B]=Const[X, B]})#l] {
      def fmap[A, B](f: A => B) = c => Const(c.run)
      def pure[A] = _ => Const(implicitly[Monoid[X]].id)
      def ap[A, B](f: Const[X, A => B]) = c => Const(implicitly[Monoid[X]].op(f.run)(c.run))
    }
}

def get1[A, B](m: Modify3[A, B]): A => B = 
  a => {
    val r = m.run[({type l[X]=Const[B, X]})#l]
    r(Const(_))(a).run
  }

// We have a get function!

// Observation: Our modify operation (Modify3) only works for one particular relationship:
// Specifically, one A has exactly one B and some other things
// What about our other relationships?

// Let's generalise our modify operation further.
// First, we will move F to forall position:

trait Modify4[A, B, F[_]] {
  def run: (B => F[B]) => A => F[A]
}

// Next, we generalise the arrows.
// They are more than functions (~>):

trait Modify5[A, B, F[_], ~>[_, _]] {
  def run: (B ~> F[B]) => A ~> F[A]
}

// We have arrived at our definition for Optic:
// Modify5 is Optic

type Optic[A, B, F[_], ~>[_, _]] = Modify5[A, B, F, ~>]

// We can recover our specialised modify operation by constraining F and ~>:
// * F is any value with Functor
// * ~> is Function1
// Scala will require a new data type for this.
//                  ↓ This actually belongs in existential position
trait Modify6[A, B, F[_]] {
  def run(implicit F: Functor[F]): Optic[A, B, F, Function1]
}

// Why this generalisation?

// Recall our relationships:
// * "A has exactly one B and some other things"
// * "A has exactly one B or some other things"
// * or _the composition_ of these

// We have successfully handled the first one.
// Let's look at the others, in terms of Optic.

// First, introducing profunctor:

// A profunctor is any binary functor:
// * contravariant in the first position
// * covariant in the second position
trait Profunctor[~>[_, _]] {
  def dimap[A, B, C, D](f: A => B, g: C => D): (B ~> C) => (A ~> D) 
}

// A canonical example of a profunctor is Function1:

object Profunctor {
  implicit def Function1Profunctor: Profunctor[Function1] =
    new Profunctor[Function1] {
      def dimap[A, B, C, D](f: A => B, g: C => D) =
        h =>
          g compose h compose f
    }
}

// Profunctor has several interesting subclasses.
// We will pay attention to one of those:

trait Choice[~>[_, _]] extends Profunctor[~>] {
  def left[A, B, C](f: A ~> B): (A Either C) ~> (B Either C)
  def right[A, B, C](f: A ~> B): (C Either A) ~> (C Either B)
}

// The left and right functions can be written in terms of each other.
// Function1 is another candidate instance:
object Choice {
  implicit def Function1Choice: Choice[Function1] =
    new Choice[Function1] {
      def dimap[A, B, C, D](f: A => B, g: C => D) =
        h =>
          g compose h compose f
      def left[A, B, C](f: A => B): (A Either C) => (B Either C) = {
          case Left(a) => Left(f(a))
          case Right(b) => Right(b)
        }
      def right[A, B, C](f: A => B): (C Either A) => (C Either B) = {
          case Left(c) => Left(c)
          case Right(a) => Right(f(a))
        }
    }
}

// Recall:
// a ParseError has exactly one (Char, Char) or some other things
// This gives us a modify function
// def modifyChars: ((Char, Char) => (Char, Char)) => ParseError => ParseError
// We wrote it earlier using pattern-matching
// Let's rewrite it differently:
def modifyChars2[F[_]: Applicative]: (((Char, Char)) => F[(Char, Char)]) => ParseError => F[ParseError] = 
  k => {
    // first a convenient pair of functions that focus on the (Char, Char) in a ParseError through Either on the Left:
    def toEither: ParseError => (Char, Char) Either Unit = {
      case UnexpectedEOF() => Right(())
      case UnexpectedChar(e: Char, a: Char) => Left((e, a))
    }

    // We won't actually use this function, but let's look at how it works:
    def _fromEither: ((Char, Char) Either Unit) => ParseError = {
      case Left((e, a)) => UnexpectedChar(e, a)
      case Right(()) => UnexpectedEOF()
    }

    // We will actually want it more generalised.
    // Notice how it runs through any Applicative (F):
    def fromEither: (F[(Char, Char)] Either Unit) => F[ParseError] = {
      case Left(x) => implicitly[Applicative[F]].fmap((ea: (Char, Char)) => UnexpectedChar(ea._1, ea._2): ParseError)(x)
      case Right (()) => implicitly[Applicative[F]].pure(UnexpectedEOF())
    }

    // We have over-specialised to Function1 here.
    // It actually works on any Choice:
    implicitly[Profunctor[Function1]].dimap(toEither, fromEither)(implicitly[Choice[Function1]].left(k))
  }

// Let's rewrite it.
// However notice the changes:
// * the return type takes on any profunctor ~>, not necessarily =>
// * ~> is constrained by choice
def modifyChars3[F[_]: Applicative, ~>[_, _]: Choice]: (((Char, Char)) ~> F[(Char, Char)]) => ParseError ~> F[ParseError] = 
  k => {
    // first a convenient pair of functions that focus on the (Char, Char) in a ParseError through Either on the Left:
    def toEither: ParseError => (Char, Char) Either Unit = {
      case UnexpectedEOF() => Right(())
      case UnexpectedChar(e: Char, a: Char) => Left((e, a))
    }

    // We won't actually use this function, but let's look at how it works:
    def _fromEither: ((Char, Char) Either Unit) => ParseError = {
      case Left((e, a)) => UnexpectedChar(e, a)
      case Right(()) => UnexpectedEOF()
    }

    // We will actually want it more generalised.
    // Notice how it runs through any Applicative (F):
    def fromEither: (F[(Char, Char)] Either Unit) => F[ParseError] = {
      case Left(x) => implicitly[Applicative[F]].fmap((ea: (Char, Char)) => UnexpectedChar(ea._1, ea._2): ParseError)(x)
      case Right (()) => implicitly[Applicative[F]].pure(UnexpectedEOF())
    }

    implicitly[Profunctor[~>]].dimap(toEither, fromEither)(implicitly[Choice[~>]].left(k))
  }

// Recall:
// trait Optic[A, B, F[_], ~>[_, _]] {
//   def run: (B ~> F[B]) => A ~> F[A]
// }

// "A has exactly one B and some other things"
// This relationship gives us any optic, such that:
// * F is any Functor
// * ~> is equal to Function1
// Optic[A, B, F[_]: Functor, Function1]

// "A has exactly one B or some other things"
// This relationship gives us any optic, such that:
// * F is any Applicative
// * ~> is any Choice

// Let's rewrite the modify function to observe this more clearly.
// We will just call modifyChars3 and alter the type to use a type-alias
def modifyChars4[F[_]: Applicative, ~>[_, _]: Choice]: Optic[ParseError, (Char, Char), F, ~>] =
  new Optic[ParseError, (Char, Char), F, ~>] {
    def run =  modifyChars3[F, ~>]
  }

// "A has exactly one B and some other things"
// The canonical name for this relationship is "lens"
// Scala does not do existential types
//               ↓ This actually belongs in existential position
trait Lens[A, B, F[_]] {
  def run(implicit F: Functor[F]): Optic[A, B, F, Function1]
}

// "A has exactly one B or some other things"
// The canonical name for this relationship is "prism"
//                ↓     ↓ These actually belong in existential position
trait Prism[A, B, F[_], ~>[_, _]] {
  def run(implicit F: Applicative[F], C: Choice[~>]): Optic[A, B, F, ~>]
}

// Lens is an optic such that:
// * F is any Functor
// * ~> is equal to Function1
// Prism is an optic, such that:
// * F is any Applicative
// * ~> is any Choice

// In other words:
// The constraint on F for a Prism is stronger than for a Lens
// The constraint on ~> for a Prism is weaker than for a Lens

// What happens when we compose a Lens and a Prism?

// Recall:
// def modifyParseResultActualChar[A]: (Char => Char) => ParseResult[A] => ParseResult[A]
// The relationship down the data type composed both Lens and Prism
// The name for this relationship is "traversal"
//                    ↓ This actually belongs in existential position
trait Traversal[A, B, F[_]] {
  def run(implicit F: Applicative[F]): Optic[A, B, F, Function1]
}

// Now to the schema update problem, the expression problem, etc

// First, we accept on face value that there is an enormous library of functions that can be written in terms of these optics.
// We have written get and set, but we could write thousands more.
// We will assume that this library can account for almost all every day programming tasks on data types.
// This assumption is necessary to complete the solution.
// You are encouraged to check it for yourself :)

// We have the minimal required abstraction to achieve our goal, _and nothing more_

// Recall:
// def insertToDatabase(v: Vehicle) /* : inserted stuff */ =

// For demonstration only, let us assume that only the Vehicle make, and owner's age is required to perform this operation.

// We will use a "classy lens" to approach this.
// However, be aware that even "lens" is over-specified.
// We only use the "get" function.
// We are leaving out "other optics"
//               ↓ This actually belongs in existential position
trait HasMake[A, F[_]] { // type-class
  def make: Lens[A, String, F]
}

object HasMake {
  // We will write the lens for make, specialised to the Vehicle instance
  implicit def VehicleHasMake[F[_]]: HasMake[Vehicle, F] =
    new HasMake[Vehicle, F] {
      def make =
        new Lens[Vehicle, String, F] {
          def run(implicit F: Functor[F]) =
            new Optic[Vehicle, String, F, Function1] {
              def run =
                k => v =>
                  implicitly[Functor[F]].fmap((_make: String) => Vehicle(_make, v.reg))(k(v.make))
            }
        }
    }

  // The lens for a data type onto itself is identity.
  // This is true for any optic.
  implicit def StringHasMake[F[_]]: HasMake[String, F] =
    new HasMake[String, F] {
      def make =
        new Lens[String, String, F] {
          def run(implicit F: Functor[F]) =
            new Optic[String, String, F, Function1] {
              def run =
                identity
            }
        }
    }
}

trait HasAge[A, F[_]] { // type-class
  def age: Lens[A, Int, F]
}

// Instances omitted

// Was:
// def insertToDatabase(v: Vehicle) /* : inserted stuff */ =

// Now:
def insertToDatabase2[A, F[_]](v: A)(implicit _Make: HasMake[A, F], _Age: HasAge[A, F]) /* : inserted stuff */ =
  ???

// This operation would continue to work on a Vehicle if a field were added to the data type.
// A field added to a data type could be written using a new data type:
case class Vehicle_Version2(v: Vehicle, colour: String)

// This data type has instances of both HasMake and HasAge by composition
// It also has an instance of the HasVehicle lens

// We would do the same for a Prism-like relationship:
trait AsChars[A, F[_], ~>[_, _]] {
  def chars: Prism[A, (Char, Char), F, ~>]
}

// For which we'd write an instance for ParseError.

// Implementing instances that traverse multi-levels becomes trivial given in all the instances that go one-level deep.
// The implementation is simply composition.
// For example, to write a Lens[A, C] given, f: Lens[A, B] and g: Lens[B, C]:
// _ compose _

// Downsides of classy lenses:
// 1. Flexibility can limit some use-cases by carrying the functor and profunctor around in the forall position.
//    I have not found a good solution to Scala's lack of existential types.
// 2. Performance has not been measured and is likely to be relatively poor.
//    Trading flexibility and performance has always been a careful and cautious for Scala ime.
}