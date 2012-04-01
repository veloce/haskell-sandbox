import Control.Monad.Error

type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left,right)
    | abs ((left + n) - right) < 4 = Right (left + n, right)
    | otherwise                    = Left $ "Haaaa! Fucking birds! " ++ show (left, right)

landRight :: Birds -> Pole -> Either String Pole
landRight n (left,right)
    | abs (left - (right + n)) < 4 = Right (left, right + n)
    | otherwise                    = Left $ "Haaaa! Fucking birds! " ++ show (left, right)

