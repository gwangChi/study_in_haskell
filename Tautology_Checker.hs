data Prop = Val Bool
          | Var Char
          | Neg Prop
          | Conj Prop Prop
          | Impl Prop Prop
          | Disj Prop Prop
          | Equi Prop Prop

type Key = [(Char,Bool)]

eval :: Prop -> Key -> Bool
-- Define pattern for the logical evaluation function
eval (Val a) _ = a
eval (Var x) key = head [a | (x',a) <- key, x == x']
eval (Neg a) key = not (eval a key)
eval (Conj a b) key = eval a key && eval b key
eval (Impl a b) key = not (eval a key) || eval b key
eval (Disj a b) key = eval a key || eval b key
eval (Equi a b) key = eval (Conj (Impl a b) (Impl b a)) key

listVar :: Prop -> [Char]
-- Generate a list of variables used to generate the enumeration table of associate pairs
listVar (Val _) = []
listVar (Var x) = [x]
listVar (Neg a) = listVar a
listVar (Conj a b) = listVar a ++ listVar b
listVar (Impl a b) = listVar a ++ listVar b
listVar (Disj a b) = listVar a ++ listVar b
listVar (Equi a b) = listVar a ++ listVar b

filterList :: Eq a => [a] -> [a]
-- Filters the listed variable characters to be singled out
filterList [] = []
filterList (x:xs) = [x] ++ filterList [l | l <- xs, l /= x]

genKey :: [Char] -> [Key]
-- Enumerates all possible combinations of Bool values of the variables in the filtered list
genKey xs = [zip xs key| key <- bools (length xs)]

bools :: Int -> [[Bool]]
-- Enumerates all possible combinations of Bool values
bools 0 = [[]]
bools n = map (False:) (bools (n-1)) ++ map (True:) (bools (n-1))

isTaut :: Prop -> Bool
isTaut prop = filterList [eval prop key| key <- (genKey . filterList . listVar) prop] == [True]
