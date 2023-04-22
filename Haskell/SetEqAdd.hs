module SetEqAdd ()
where
import SetEq

powerList  :: [a] -> [[a]]
powerList  [] = [[]]
powerList  (x:xs) = (powerList xs) ++ (map (x:) (powerList xs))


powerSet :: Eq a => Set a -> Set (Set a)
powerSet (Set xs) = Set (map (\xs -> (Set xs)) (powerList xs))

showSubSets::Eq a=>Int->Set a->Set (Set a)
showSubSets k (Set x) = list2set (map list2set (powerList k x))

permutation::Int->[a]->[[a]]
permutation r xs = [y| x<-(map permute' (powerList r xs)), y<-x]


