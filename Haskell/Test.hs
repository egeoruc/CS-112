module Test (divides) where 


divides:: Integer -> Integer -> Bool
divides d n = (rem n d == 0)

mult2 :: Integer -> Integer
mult2 x = x*2
 
divides3 :: Integer -> Bool
divides3 x = (rem x 3 == 0)

ldf k n | divides k n = k
        | k == n      = n
        | otherwise   = ldf (k+1) n

ld n = ldf 2 n

prime0 :: Integer -> Bool
prime0 n | n < 1 = error "not a valid number "
         | n == 1 = False
         | otherwise = ld n == n

ldf1 k n | (rem n k == 0)  = k
         | k == n          =n
         | otherwise       =ldf1 (k+1) n

prime1 n | n < 1           = error "not valid value"
         | n == 1          = False
         |otherwise        = ldf1 2 n == n

mnmInt :: [Int] -> Int 
mnmInt [] = error "no element"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

min' :: Int -> Int -> Int
min' x y | x<=y = x
         | otherwise = y

removeFst :: [Int] -> Int -> [Int]
removeFst [] m = []
removeFst (x:xs) m |m == x = xs
                   |otherwise = (x:removeFst xs m) 

srtInt:: [Int]->[Int]
srtInt [] = []
srtInt  x = (mnmInt x) : (srtInt(removeFst x (mnmInt x)))

average:: [Int]->Rational
average [] = error "empty list"
average x = toRational (sum x)/ toRational (length x)

rootof:: Integer -> Float
rootof a = sqrt (fromInteger a)

checkC:: Char -> String -> Bool
checkC c [] = False
checkC c (x:xs) | c == x =True
                |otherwise = checkC c xs

prefix:: String-> String-> Bool
prefix [] ys= True
prefix (x:xs) []= False
prefix (x:xs) (y:ys)= (x==y) && prefix xs ys

factors:: Integer -> [Integer]
factors n | n<1 = error "not valid value"
          | n==1 = []
          | otherwise = p: factors (div n p) where p =ld n

maxL:: [Int]->Int
maxL[]=0
maxL (x:xs)= max x (maxL xs)

maxElement::[[Int]]->Int
maxElement a = maxL (map maxL a)


filter1:: (a -> Bool) -> [a] -> [a]
filter1 p [] = []
filter1 p (x:xs) | p x = x : filter1 p xs 
                 | otherwise = filter1 p xs 

elem0::Eq a=>a->[a]->Bool
elem0 x []= False
elem0 x (y:ys) | x==y = True
               |otherwise = elem0 x ys


nub:: (Eq a)=> [a]->[a]
nub []=[]
nub (x:xs)= x : nub ( remove x xs) where remove y []= []
                                         remove y (z:zs) |y==z = remove y zs
                                                         |otherwise = z: remove y zs

delete:: Eq a=> a->[a]->[a]
delete x []= []
delete x (y:ys)|x == y =ys
               |otherwise = delete x ys

union:: Eq a => [a]->[a]->[a]
union [] ys= ys
union (x:xs) ys= x : union xs (delete x ys)

inter:: Eq a=> [a]-> [a]-> [a]
inter [] s = []
inter (x:xs) s | elem0 x s == True = x: inter xs s      
               | otherwise = inter xs s

addElem :: a-> [[a]]->[[a]]
addElem x y =map (x:)y 

powerList :: [a] -> [[a]]
powerList[]=[[]]
powerList (x:xs)= (powerList xs) ++ (addElem x (powerList xs))



