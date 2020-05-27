{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes #-}

import Control.Applicative(Const(..))
import Data.Functor.Identity(Identity(..))

-- The emphasis on the term "lens" is a historical artifact.
-- The original view/update problem focussed on databases, which hold *records*.
-- Records have a specific relationship to their columns.
-- Specifically, a record has a "1 and some other parts" relationship with column values

-- The emphasis on this problem led to implementation in the data-lens and scalaz libraries in around 2008, which resolved it well.
-- However, other relationships arose in production code e.g.
--   a JSON object has a "1 or some other parts" with the boolean constructor
--   a homogeneous list has a "many and/or some other parts" with its elements
-- Experiments were performed in the data-lens library and the results were never satisfactory.

-- A general encoding of a lens took the form of a pair of get/set pair, with fusion on the record object.
data OldLens a b =
  OldLens (a -> (b -> a, b))

-- Suppose:

data Person =
  Person
    String -- name
    Int    -- age
  deriving (Eq, Show)

data VehicleRegistration =
  VehicleRegistration
    String -- reg number
    Person -- owner
  deriving (Eq, Show)

data Vehicle =
  Vehicle
    String -- vehicle make
    VehicleRegistration
  deriving (Eq, Show)

-- A Vehicle has 1 VehicleRegistration and some other parts
-- A VehicleRegistration has 1 Person (owner) and some other parts
-- A Person has 1 Int (age) and some other parts
-- Observe: the relationship is preserved down the structure

-- Use-case: A Vehicle's owner has had a birthday, and the age needs updating.

-- Naive solution:
birthday1 :: Vehicle -> Vehicle
birthday1 v =
  case v of
    Vehicle make reg ->
      case reg of
        VehicleRegistration num own ->
          case own of
            Person name age ->
              Vehicle make (VehicleRegistration num (Person name (age+1)))

-- This might lead to the temptation of just having one sneaky little variable:
-- v.reg.owner.age += 1
-- As we are all aware, this causes the age to become -1 when put in production, and a NullPointerException on a server at a different department altogether.

-- The key insight

modifyVehicleRegistration1 ::
  (VehicleRegistration -> VehicleRegistration)
  -> Vehicle
  -> Vehicle
modifyVehicleRegistration1 modifier veh =
  case veh of
    Vehicle make reg ->
      Vehicle make (modifier reg)

modifyOwner1 ::
  (Person -> Person)
  -> VehicleRegistration
  -> VehicleRegistration
modifyOwner1 modifier reg =
  case reg of
    VehicleRegistration num own ->
      VehicleRegistration num (modifier own)

modifyAge1 ::
  (Int -> Int)
  -> Person
  -> Person
modifyAge1 modifier own =
  case own of
    Person name age ->
      Person name (modifier age)

-- Given these three modifier functions at "one level deep", we can achieve "any level deep" by function composition:

modifyVehicleOwnerAge1 ::
  (Int -> Int)
  -> Vehicle
  -> Vehicle
modifyVehicleOwnerAge1 =
  modifyVehicleRegistration1 . modifyOwner1 . modifyAge1

birthday2 :: Vehicle -> Vehicle
birthday2 = modifyVehicleOwnerAge1 (+1)

-- We have resolved the problem for a specific case:
--   the relationship from the record object to the focus is "has 1 and some other parts"
--   a modify operation and any derivative operations e.g. set

-- However, what about other relationships?
-- e.g. What if we have a [Vehicle] and all of the owners have had a birthday?
--      the relationship from [Vehicle] to Int is no longer "has 1 and some other parts"
-- What about other operations? e.g. get

-- We will, for no apparent reason just yet, insert Identity into our modify functions:
modifyVehicleRegistration2 ::
  (VehicleRegistration -> Identity VehicleRegistration)
  -> Vehicle
  -> Identity Vehicle
modifyVehicleRegistration2 modifier veh =
  case veh of
    Vehicle make reg ->
      fmap (\reg' -> Vehicle make reg') (modifier reg)

modifyOwner2 ::
  (Person -> Identity Person)
  -> VehicleRegistration
  -> Identity VehicleRegistration
modifyOwner2 modifier reg =
  case reg of
    VehicleRegistration num own ->
      fmap (\own' -> VehicleRegistration num own') (modifier own)

modifyAge2 ::
  (Int -> Identity Int)
  -> Person
  -> Identity Person
modifyAge2 modifier own =
  case own of
    Person name age ->
      fmap (\age' -> Person name age') (modifier age)

-- We can always recover the original "raw" modify operation by stripping Identity:
rawModify2 ::
  ((b -> Identity b) -> a -> Identity a)
  -> (b -> b) -> a -> a
rawModify2 f g b =
  runIdentity (f (Identity . g) b)

-- and we still have preservation under composition:

modifyVehicleOwnerAge2 ::
  (Int -> Identity Int)
  -> Vehicle
  -> Identity Vehicle
modifyVehicleOwnerAge2 =
  modifyVehicleRegistration2 . modifyOwner2 . modifyAge2

-- Note: the implementations of these modify operations do nothing specific to Identity
--       they all use fmap
modifyVehicleRegistration3 ::
  Functor f =>
  (VehicleRegistration -> f VehicleRegistration)
  -> Vehicle
  -> f Vehicle
modifyVehicleRegistration3 modifier veh =
  case veh of
    Vehicle make reg ->
      fmap (\reg' -> Vehicle make reg') (modifier reg)

modifyOwner3 ::
  Functor f =>
  (Person -> f Person)
  -> VehicleRegistration
  -> f VehicleRegistration
modifyOwner3 modifier reg =
  case reg of
    VehicleRegistration num own ->
      fmap (\own' -> VehicleRegistration num own') (modifier own)

modifyAge3 ::
  Functor f =>
  (Int -> f Int)
  -> Person
  -> f Person
modifyAge3 modifier own =
  case own of
    Person name age ->
      fmap (\age' -> Person name age') (modifier age)

-- We still have preservation under composition:

modifyVehicleOwnerAge3 ::
  Functor f =>
  (Int -> f Int)
  -> Vehicle
  -> f Vehicle
modifyVehicleOwnerAge3 =
  modifyVehicleRegistration3 . modifyOwner3 . modifyAge3

-- But what does that give us?

-- We are still preserving the relationship of "has 1 and some other parts"
-- However, we can now derive a "get" operation from these modify operations.
-- Just like we recovered the raw modify operation by substituting the Identity functor
-- We will substitute the (Const b) functor

get1 ::
  ((b -> Const b b) -> a -> Const b a)
  -> a -> b
get1 f a =
  getConst (f Const a)

-- We can recover several other useful operations.
-- "Useful" is an under-statement. We can now recover most of the uninteresting industry software that exists in the wild.
-- But what about other relationships?
--   "1 and some other parts" is covered
--   e.g.
--   "1 or some other parts"
--   "many and/or some other parts"
--   "1 and/or no other parts"

-- Our modify operations are of this form, where:
-- p ~ (->)
-- Functor f =>
type Optic p f a b =
  p b (f b) -> p a (f a)

-- The key insight is:
-- What happens when we modify the constraints on (p) and (f)?
-- Does anything interesting come about?

-- Suppose:
data ParseResult a =
  ParseFailure String
  | ParseSuccess a
  deriving (Eq, Ord, Show)

-- a ParseResult has one String or some other parts
-- A raw modify operation looks like this:
modifyParseFailure1 ::
  (String -> String)
  -> ParseResult a
  -> ParseResult a
modifyParseFailure1 modifier r =
  case r of
    ParseSuccess a -> ParseSuccess a
    ParseFailure s -> ParseFailure (modifier s)

-- We can do the same thing by introducing the Identity functor:
modifyParseFailure2 ::
  (String -> Identity String)
  -> ParseResult a
  -> Identity (ParseResult a)
modifyParseFailure2 modifier r =
  case r of
    ParseSuccess a -> pure (ParseSuccess a)
    ParseFailure s -> fmap ParseFailure (modifier s)

-- Note again we did nothing specific to Identity
-- We only used fmap and pure
modifyParseFailure3 ::
  Applicative f =>
  (String -> f String)
  -> ParseResult a
  -> f (ParseResult a)
modifyParseFailure3 modifier r =
  case r of
    ParseSuccess a -> pure (ParseSuccess a)
    ParseFailure s -> fmap ParseFailure (modifier s)

-- However, the interesting operations that come about are quite limited.
-- We can generalise further.

-- Introducing Profunctor:
class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

-- A profunctor is any binary functor, which is contravariant in the first position and covariant in the second
-- For example, (->)
instance Profunctor (->) where
  dimap f g h =
    g . h . f

-- Like Functor, Profunctor has many interesting subclasses. Here is one:
class Profunctor p => Choice p where
  left  :: p a b -> p (Either a c) (Either b c)

-- And again, (->) is an instance:
instance Choice (->) where
  left f =
    either (Left . f) Right

-- We can rewrite the modify operation for ParseResult to ParseError:
modifyParseFailure4 ::
  Applicative f =>
  (String -> f String)
  -> ParseResult a -> f (ParseResult a)
modifyParseFailure4 modifier =
  let toEither_String ::
        ParseResult x
        -> Either String (ParseResult x)
      toEither_String (ParseFailure e) = Left e
      toEither_String (ParseSuccess a) = Right (ParseSuccess a)
      fromEither_String ::
        Applicative f =>
        Either (f String) (ParseResult x)
        -> f (ParseResult x)
      fromEither_String (Left e) = fmap ParseFailure e
      fromEither_String (Right x) = pure x
  in  dimap toEither_String fromEither_String (left modifier)

-- Note that we used nothing specific to (->)
-- only dimap and left
modifyParseFailure5 ::
  (Choice p, Applicative f) =>
  (p String (f String))
  -> p (ParseResult a) (f (ParseResult a))
modifyParseFailure5 modifier =
  let toEither_String ::
        ParseResult x
        -> Either String (ParseResult x)
      toEither_String (ParseFailure e) = Left e
      toEither_String (ParseSuccess a) = Right (ParseSuccess a)
      fromEither_String ::
        Applicative f =>
        Either (f String) (ParseResult x)
        -> f (ParseResult x)
      fromEither_String (Left e) = fmap ParseFailure e
      fromEither_String (Right x) = pure x
  in  dimap toEither_String fromEither_String (left modifier)

-- in other words, this is yet another Optic where:
--   Choice p =>
--   Applicative f =>

-- Summary:
type Lens a b =
  forall f. Functor f => Optic (->) f a b

type Prism a b =
  forall p f. (Applicative f, Choice p) => Optic p f a b

-- Interesting question:
-- what happens when we compose both Lens and Prism?
-- the constraints will become the least upper bound of the two:

type Traversal a b =
  forall f. Applicative f => Optic (->) f a b

-- A Prism is a Traversal, but a specific type of Traversal
-- A Lens is a Traversal, but a specific, but different, type of Traversal

-- We have handled the relationships:
--   1 and some other other parts (Lens)
--   1 or some other other parts (Prism)
--   many and/or some other other parts (Traversal)

-- Last one: 1 and/or no other parts
-- this is even more specific than Lens and Prism
-- therefore, the constraints will become even more general:

type Iso a b =
  forall p f. (Profunctor p, Functor f) => Optic p f a b

-- Summary, when you have:
-- a data type A with 1 B and some other parts, think Lens
-- a data type A with 1 B or some other parts, think Prism
-- a data type A with many B and/or some other parts, think Prism
-- a data type A with 1 B and no other parts, think Iso
-- these relationships exist under a hierarchy that is preserved under composition

-- Now we can easily navigate our data types!

