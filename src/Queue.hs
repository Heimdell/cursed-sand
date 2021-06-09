
module Queue where

data T a = Queue
  { input, output :: [a]
  }
  deriving stock (Eq, Ord, Show)

empty :: T a
empty = Queue [] []

fromList :: [a] -> T a
fromList = flip pushAll empty

push :: a -> T a -> T a
push a (Queue input output) = Queue (a : input) output

pushAll :: [a] -> T a -> T a
pushAll = flip (foldr push)

pop :: T a -> Maybe (T a, a)
pop (Queue input output) = case output of
  [] -> case input of
    [] -> Nothing
    _  -> let h : t = reverse input in Just (Queue [] t, h)

  a : rest -> Just (Queue input rest, a)
