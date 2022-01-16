module CofreeBot.Utils where

import Control.Applicative
import Control.Monad.Except
import Data.Kind

(|*|) :: Applicative f => f a -> f b -> f (a, b)
(|*|) = liftA2 (,)

infixr |*|

type (/\) = (,)

infixr /\

type (\/) = Either

infixr \/

type a \?/ b = Maybe (Either a b)

pattern (:&) :: a -> b -> (a, b)
pattern a :& b = (a, b)

{-# COMPLETE (:&) #-}

infixr :&

type Transformers
  :: [(Type -> Type) -> Type -> Type]
  -> (Type -> Type) -> Type -> Type
type family Transformers ts m
  where
  Transformers '[] m = m
  Transformers (t ': ts) m = t (Transformers ts m)

same :: Either x x -> x
same = either id id

note :: MonadError e m => e -> Maybe a -> m a
note e = maybe (throwError e) pure

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just