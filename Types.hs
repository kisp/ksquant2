module Types(
  InfInt
  , WInt
  , Time
  , Divs
  , Err
  , Timesig
  , Tempo
  , WRat
  )
where

type WRat = Rational

type InfInt = Integer

type WInt = Integer

type Divs = [WInt]

type Time = Float

-- | Computation result of type 'a' or error as a String.
type Err a = Either String a

type Timesig = (WInt, WInt)
type Tempo = (WInt, Rational)
