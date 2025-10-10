data Card = Joker | Heart Int | Club Int | Spade Int | Diamond Int
data Color = MakeColor Int Int Int

red = MakeColor 255 0 0

-- data Maybe a = Nothing | Just a

--          type       Constr  Value     Value   type       The right tree
data BSTree key value = BSTree key value (BSTree key value) (BSTree key value) | Empty

height :: BSTree key value -> Int
height Empty            = 0
height (BSTree _ _ l r) = 1 + max (height l) (height r)

tLookup :: (Ord key) => key -> BSTree key value -> Maybe value
tLookup _ Empty = Nothing
tLookup x (BSTree y v l r)
  | x > y = tLookup x r
  | x < y = tLookup x l
  | otherwise = Just v

tInsert :: (Ord key) => key -> value -> BSTree key value -> BSTree key value
tInsert k v Empty = BSTree k v Empty Empty
tInsert x v (BSTree y v2 l r)
  | x > y = BSTree y v2 l (tInsert x v r) 
  | x < y = BSTree y v2 (tInsert x v l) l
  | otherwise = BSTree x v Empty Empty


