
module Queue where

data T a = Queue
  { input, output :: [a]
  , qlen :: Int
  }
  deriving stock (Eq, Ord, Show)

empty :: T a
empty = Queue [] [] 0

fromList :: [a] -> T a
fromList list = pushAll list empty

push :: a -> T a -> T a
push a queue = queue { input = a : input queue, qlen = qlen queue + 1 }

pushAll :: [a] -> T a -> T a
pushAll list queue = foldr push queue list

pop :: T a -> Maybe (T a, a)
pop (Queue input output qlen) = case output of
  [] -> case input of
    [] -> Nothing
    _  -> let h : t = reverse input in Just (Queue [] t (qlen - 1), h)

  a : rest -> Just (Queue input rest (qlen - 1), a)

append :: T a -> T a -> T a
append q1 q2 = pushAll (toList q2) q1

toList :: T a -> [a]
toList (Queue input output _) = output ++ reverse input

len :: T a -> Int
len = qlen
