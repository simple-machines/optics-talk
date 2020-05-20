// This problem requires tying a lot of disjoint concepts together.
// There will be some context switching.
// Stop at any time for questions if not following.

object Optics {

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

def modifyVehicleOwnerAge1:
  (Int => Int)
  => Vehicle
  => Vehicle =
  modifyVehicleRegistration1 compose modifyOwner1 compose modifyAge1

def birthday_composed: Vehicle => Vehicle =
  modifyVehicleOwnerAge1(_+1)

// We also have a dual problem
// Not product types, but sum types

sealed trait ParseError
case class UnexpectedEOF() extends ParseError
case class ExpectedChar(expected: Char, actual: Char) extends ParseError

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
          case ExpectedChar(e, a) => ExpectedChar(e, a.toUpper)
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
    case ExpectedChar(e, a) => {
      val (ee, aa) = k(e, a)
      ExpectedChar(ee, aa)
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

def modifyParseResultActualChar[A]:
  (Char => Char)
  => ParseResult[A]
  => ParseResult[A] =
  modifyParseError compose modifyChars compose modifyActualChar

////

def selectFromDatabase(/* query stuff */): Vehicle =
  ???

def insertToDatabase(v: Vehicle) /* : inserted stuff */ =
  ???

// We are omitting the "update" and "delete" operations, since it raises questions
//   * should the database be append-only?
//   * what are the consequences for our data model if we allow update/delete?

// However, we *will* allow insert and delete on the data types themselves.
// e.g. add or remove a field from Vehicle
// * delete then insert = update

}
