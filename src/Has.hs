
module Has where

import Polysemy
import Polysemy.State

import GHC.Types (Symbol)

-- Marked type.
--
newtype (s :: Symbol) =: a = Tagged { untag :: a }

-- Access to marked element of state.
--
get' :: forall t a m. Member (State (t =: a)) m => Sem m a
get' = gets' @t id

-- Access to marked element of state.
--
gets' :: forall t a m b. Member (State (t =: a)) m => (a -> b) -> Sem m b
gets' f = gets @(t =: a) (f . untag)

modify'' :: forall t a m b. Member (State (t =: a)) m => (a -> a) -> Sem m ()
modify'' f = modify @(t =: a) $ Tagged . f . untag

put' :: forall t a m b. Member (State (t =: a)) m => a -> Sem m ()
put' = modify'' @t . const
