data MobileDevice = Tablet Int String Colors
                  | Laptop Int String Colors
                  | Smartphone Int String Colors

data Colors = Rosu
            | Verde
            | Albastru
            | Mov
            | Maro
            | Galben
            | Portocaliu
            deriving(Show)

info::MobileDevice->String
info (Tablet x firma culoare) = "Tableta de la " ++ firma ++ " de culoare " ++ show(culoare) ++ " ce are ID-ul " ++ show(x)
info (Laptop x firma culoare) = "Laptop de la " ++ firma ++ " de culoare " ++ show(culoare) ++ " ce are ID-ul " ++ show(x)
info (Smartphone x firma culoare) = "Smartphone de la " ++ firma ++ " de culoare " ++ show(culoare) ++ " ce are ID-ul " ++ show(x)

data Nat =  Zero
          | Succ Nat

adun::Nat->Nat->Nat
adun Zero Zero = Zero
adun Zero x = x
adun x Zero = x
adun (Succ x) y = Succ (adun x y)

mult::Nat->Nat->Nat
mult Zero x = Zero
mult x Zero = Zero
mult (Succ x) y = adun y (mult x y)

data ErrorNat = Error
              | Val Nat
              deriving (Show, Eq)

minus::ErrorNat->ErrorNat->ErrorNat
minus (Val x) (Val Zero) = Val x
minus (Val Zero) (Val x) = Error
minus (Val (Succ x)) (Val (Succ y)) = minus (Val x) (Val y)  

impart::ErrorNat->ErrorNat->Nat->ErrorNat
impart (Val x) (Val Zero) res = Error
impart (Val Zero) (Val x) res = Val res
impart (Val x) (Val y) res = if (minus (Val x) (Val y)) == Error then
                                 Error
                             else impart (minus (Val x) (Val y)) (Val y) (Succ res)

impartire::ErrorNat->ErrorNat->ErrorNat
impartire (Val x) (Val y) = impart (Val x) (Val y) Zero

data List = Vida
          | Elem Int List
          deriving(Show, Eq)

cauta::List->Int->Int->Int
cauta Vida y poz = (-1)
cauta (Elem x list) y poz = if x == y then
                               poz
                            else cauta list y (poz + 1)

caut::List->Int->Int                                
caut list y = cauta list y 0

data Primitive = Err
               | A Int
               | B String
               | C Char
               | D Bool
               deriving(Show)

prel::(Primitive,Primitive,Primitive,Primitive)->Int->Primitive
prel (x,w,y,z) 0 = x
prel (x,w,y,z) 1 = w
prel (x,w,y,z) 2 = y
prel (x,w,y,z) 3 = z
prel (x,w,y,z) a = Err

addThree::Int->(Int->(Int->Int))
addThree x y w = x+y+w

plusOne::Int->Int
plusOne x = x + 1

timesTwo::Int->Int
timesTwo x = x * 2

sumAll::(Int->Int)->Int->Int->Int
sumAll f x y = if x==y then
                  f x
               else f x + sumAll f (x+1) y

comp::a->(a->b)->(b->c)->c    
comp x g f = f (g x)           

dec2nat::[Int]->Int
dec2nat (hd:tl) = foldl (\x y -> 10*x+y) 0 (hd:tl)

insert::a->[a]->Int->Int->[a]
insert x (hd:tl) poz curr = if curr == poz then
                               [x] ++ (hd:tl)
                        else [hd] ++ insert x tl poz (curr + 1)   

insertAt::a->[a]->Int->[a]
insertAt x list poz = insert x list poz 1

intToDigits::Int->[Int]
intToDigits x = if x > 0 then
                   [mod x 10] ++ intToDigits (div x 10)
                else [] 

count::Char->[Char]->Int
count x [] = 0
count x (hd:tl) = if x == hd then
                     1 + count x tl
                  else count x tl                    

dupl::[a]->Int->[a]
dupl [] poz = []
dupl (hd:tl) poz = if (mod poz 2) == 1 then 
                       [hd, hd] ++ dupl tl (poz + 1)
                   else [hd] ++ dupl tl (poz + 1)    

dup1::[a]->[a]
dup1 (hd:tl) = dupl (hd:tl) 0

pulamea :: [a]->[a]->[a]
pulamea [] [] = []
pulamea [] (hd:tl) = (hd:tl)
pulamea (hd:tl) [] = (hd:tl)
pulamea (hd1:tl1) (hd2:tl2) = (hd1:(pulamea tl1 (hd2:tl2)))

cealaltapula::[a]->[a]
cealaltapula [] = []
cealaltapula (hd:tl) = cealaltapula tl ++ [hd]

instance Show Nat where
    show Zero = "o"
    show (Succ x) = "s" ++ show x

instance Ord Nat where
    (<) Zero Zero = False
    (<) (Succ x) Zero = False
    (<) Zero (Succ x) = True
    (<) (Succ x) (Succ y) = (<) x y    
    (<=) Zero _ = True
    (<=) (Succ x) Zero = False
    (<=) (Succ x) (Succ y) = (<=) x y

data Arb = Nod Int Arb Arb
         | Nup

instance Show Arb where
    show Nup = "()"
    show (Nod x a1 a2) = "(" ++ (show x) ++ (show a1) ++ (show a2) ++ ")"  

instance Eq Nat where
    (==) Zero Zero = True
    (==) (Succ x) Zero = False
    (==) Zero (Succ x) = False
    (==) (Succ x) (Succ y) = (==) x y    

class Pretty Nat where
    prettyPrint::a->String
    
instance (Show a) => (Pretty Arb a) where 
    prettyPrint x =    