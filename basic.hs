-- Operators ---------------------------

plus :: (Num n) => n -> n -> n
plus x y = x + y

minus :: Int -> Int -> Int
minus x y = x - y

multiplication :: (Num n) => n -> n -> n
multiplication x y = x * y

division :: (Floating n) => n -> n -> n
division x y = x / y

powerOfTwo :: Double -> Double
powerOfTwo x = x ** 2

restOfDivision :: (Integral n) => n -> n -> n
restOfDivision x y = mod x y
-- (Num n) create a label call n that represents the type of Num.

-----------------------------------------------------------------
-- Conditions --------------------------
actionWithPattern :: (Eq n, Num n) => n -> n -> n
actionWithPattern x 0 = x * 2
actionWithPattern 0 y = y * 3
actionWithPattern x y = x + y

ftEven :: (Integral n) => n -> Bool
ftEven n = if mod n 2 == 0 then True else False

ftOdd :: Int -> Bool
ftOdd n = if mod n 2 /= 0 then True else False

ftMax :: Int -> Int -> Int
ftMax x y = if x > y then x else y

ftMin :: (Ord n, Num n) => n -> n -> n
ftMin x y = if x < y then x else y

-----------------------------------------------------------------
-- Const -------------------------------
x_pi :: (Floating f) => f
x_pi = 3.14159

x_e :: (Floating f) => f
x_e = 2.171828

-----------------------------------------------------------------
-- Simple Recursion --------------------
ftPowerSlower :: Int -> Int -> Int
ftPowerSlower x 0 = 0
ftPowerSlower x 1 = x
ftPowerSlower x y = x * ftPowerSlower x (y - 1)

-- Tail Recursion ----------------------
ftPowerAux :: Int -> Int -> Int -> Int
ftPowerAux x 0 a = 0
ftPowerAux x 1 a = x * a
ftPowerAux x y a = ftPowerAux x (y - 1) (x * a)

ftPower :: Int -> Int -> Int
ftPower x y = ftPowerAux x y 1

ftFibonacciAux :: (Integral n) => n -> n -> n -> n
ftFibonacciAux 0 before current = before
ftFibonacciAux 1 before current = current
ftFibonacciAux 2 before current = current + before
ftFibonacciAux num before current = ftFibonacciAux (num - 1) current (before + current)

ftFibonacci :: (Integral n) => n -> n
ftFibonacci num = ftFibonacciAux num 0 1
-----------------------------------------------------------------
-- Lambda ------------------------------

-- lambda de x.(x+1)
-- (\x -> x + 1)
-- use => (\x -> x + 1) 9

-- lambda de xy.(x + y)
-- (\x y -> x +  y)
-- use (\x y -> x +  y) 8 9

-- lambda de x.(lambda de y.(x + y))
-- (\x -> \y -> x + y)
-- use (\x -> \y -> x + y) 7 7

