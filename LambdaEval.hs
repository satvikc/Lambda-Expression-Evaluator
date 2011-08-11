import qualified Data.Set as Set
import qualified Data.Char as Ch 

data Expr = V String
          | Ap Expr Expr
          | L String Expr
        deriving (Show,Eq)

freeVariable :: Expr -> Set.Set String
freeVariable (V a) = Set.singleton a
freeVariable (Ap e1 e2) = Set.union (freeVariable e1) (freeVariable e2)
freeVariable (L a e1) = Set.difference (freeVariable e1) (Set.singleton a)

free :: String->Expr->Bool
free a e = Set.member a $ freeVariable e

etaReduction :: Expr->Expr
etaReduction f@(L a (Ap e (V b))) = if a==b && not (free a e) then e else f
etaReduction x = x

nextChar :: Char->Char
nextChar a | Ch.isLower a = Ch.toUpper a
           | Ch.isUpper a = Ch.toLower a
           | Ch.isDigit a = 'a'
           | otherwise  = 'a'

newString :: [String] -> String -> String
newString [] a = a
newString (x:xs) a | length x < length a  = newString xs a
                   | length x == length a = newString xs $ a ++ "a"
                   | otherwise            = newString xs $ a ++ [nextChar $ (!!(length a)) x]

vReplace :: Expr->String->Expr->Expr
vReplace t a (V b) | a==b = t
vReplace f@(V t) a b = if a==t then b else f
vReplace (Ap e1 e2) a b = Ap (vReplace e1 a b) (vReplace e2 a b)
vReplace f@(L t e) a b | t == a = f
                       | free t b = let t' = newString  (Set.toList $ freeVariable b) ""
                                        e' = vReplace e t (V t') 
                                            in L t' $ vReplace e' a b
                       | otherwise = L t $ vReplace e a b

lambdaEval :: Expr -> Expr
lambdaEval f@(V a) = f
lambdaEval (L a e) = etaReduction $ L a (lambdaEval e)
lambdaEval (Ap (L a e) y) = lambdaEval $ vReplace e a y 
lambdaEval (Ap e1 e2) = case lambdaEval e1 of
                f@(L _ _) -> lambdaEval $ Ap f e2
                f         -> Ap f $ lambdaEval e2
