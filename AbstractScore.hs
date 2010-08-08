module AbstractScore
    (Score(..)
    ,Part(..)
    ,Voice(..)
    ,completeToScore
    ,mapVoices)
where
import Lisp
      
data Score a = Score { scoreParts :: [Part a] }
           deriving Show
data Part a = Part { partVoices :: [Voice a] }
           deriving Show
data Voice a = Voice { voiceMeasures :: [a] }
           deriving Show

class AbstractScore c a where
    completeToScore :: (c a) -> Score a
    mapVoices :: (Voice a -> Voice b) -> (c a) -> (c b)

instance AbstractScore Score a where
    completeToScore = id
    mapVoices f x = Score $ map (mapVoices f) (scoreParts x)

instance AbstractScore Part a where
    completeToScore x = Score [x]
    mapVoices f x = Part $ map (mapVoices f) (partVoices x)

instance AbstractScore Voice a where
    completeToScore x = completeToScore (Part [x])
    mapVoices f x = f x

-- Functor
instance Functor Score where
    fmap f x = Score $ map (fmap f) (scoreParts x)

instance Functor Part where
    fmap f x = Part $ map (fmap f) (partVoices x)

instance Functor Voice where
    fmap f x = Voice $ f `fmap` voiceMeasures x

-- Sexp
instance (Sexp a) => Sexp (Score a) where
    toSexp s = LispList $ map toSexp (scoreParts s)
    fromSexp s = error ""

instance (Sexp a) => Sexp (Part a) where
    toSexp s = LispList $ map toSexp (partVoices s)
    fromSexp s = error ""

instance (Sexp a) => Sexp (Voice a) where
    toSexp s = LispList $ map toSexp (voiceMeasures s)
    fromSexp s = error ""

