{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes #-}

import Control.Applicative(Const(..))
import Data.Functor.Identity(Identity(..))

data OldLens a b =
  OldLens (a -> (b -> a, b))

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

-- Use-case: A Vehicle's owner has had a birthday, and the age needs updating.

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

modifyVehicleOwnerAge1 ::
  (Int -> Int)
  -> Vehicle
  -> Vehicle
modifyVehicleOwnerAge1 =
  modifyVehicleRegistration1 . modifyOwner1 . modifyAge1

birthday2 :: Vehicle -> Vehicle
birthday2 = modifyVehicleOwnerAge1 (+1)

-- data Identity a = Identity a

modifyVehicleRegistration2 ::
  Functor f =>
  (VehicleRegistration -> f VehicleRegistration)
  -> Vehicle
  -> f Vehicle
modifyVehicleRegistration2 modifier veh =
  case veh of
    Vehicle make reg ->
      fmap (\reg' -> Vehicle make reg') (modifier reg)

modifyOwner2 ::
  Functor f =>
  (Person -> f Person)
  -> VehicleRegistration
  -> f VehicleRegistration
modifyOwner2 modifier reg =
  case reg of
    VehicleRegistration num own ->
      fmap (\own' -> VehicleRegistration num own') (modifier own)

modifyAge2 ::
  Functor f =>
  (Int -> f Int)
  -> Person
  -> f Person
modifyAge2 modifier own =
  case own of
    Person name age ->
      fmap (\age' -> Person name age') (modifier age)

rawModify2 ::
  ((b -> Identity b) -> a -> Identity a)
  -> (b -> b) -> a -> a
rawModify2 modd f a =
  runIdentity (modd (Identity . f) a)

modifyVehicleOwnerAge2 ::
  -- Functor f =>
  {-
  (Int -> f Int)
  -> Vehicle
  -> f Vehicle
  -}
  Lens Vehicle Int
modifyVehicleOwnerAge2 =
  modifyVehicleRegistration2 . modifyOwner2 . modifyAge2

-- modify :: (b -> b) -> a -> a
-- modifyI :: Functor f => (b -> f b) -> a -> f a
-- set :: b -> a -> a
-- get :: a -> b

-- data Const a b = Const a
get1 :: ((b -> Const b b) -> a -> Const b a) -> a -> b
get1 modd a =
  getConst (modd Const a)

type Optic p f a b =
  p b (f b) -> p a (f a)

type Lens a b =
  forall f. Functor f => Optic (->) f a b

-- where:
-- p ~ (->)
-- Functor f =>
-- called: Lens
data ParseResult a =
  ParseFailure String
  | ParseSuccess a
  deriving (Eq, Ord, Show)

modifyParseFailure2 ::
  Applicative f =>
  (String -> f String)
  -> ParseResult a
  -> f (ParseResult a)
modifyParseFailure2 modifier r =
  case r of
    ParseSuccess a -> pure (ParseSuccess a)
    ParseFailure s -> fmap ParseFailure (modifier s)

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

instance Profunctor (->) where
  dimap f g h =
    g . h . f

class Profunctor p => Choice p where
  left  :: p a b -> p (Either a c) (Either b c)

-- And again, (->) is an instance:
instance Choice (->) where
  left f =
    either (Left . f) Right

modifyParseFailure4 ::
  (Choice p, Applicative f) =>
  p String (f String)
  -> p (ParseResult a) (f (ParseResult a))
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

-- Any Optic p f a b
-- such that:
-- Choice p =>
-- Applicative f =>
type Prism a b =
  forall p f. (Choice p, Applicative f) => Optic p f a b


-- Any Optic p f a b
-- such that:
-- Choice p =>
-- Applicative f =>
-- Call prism


-- Any Optic p f a b
-- such that:
-- p ~ (->)
-- Functor f =>
-- called Lens

-- p ~ (->)
-- Applicative f =>
-- called ?
type Traversal a b =
  forall f. Applicative f => Optic (->) f a b

-- a has many b and/or other parts

-- a has one b and no other parts
type Iso a b =
  forall p f. (Profunctor p, Functor f) => Optic p f a b

-- Summary, when you have:
-- a data type A with 1 B and some other parts, think Lens
-- a data type A with 1 B or some other parts, think Prism
-- a data type A with many B and/or some other parts, think Traversal
-- a data type A with 1 B and no other parts, think Iso
-- these relationships exist under a hierarchy that is preserved under composition
