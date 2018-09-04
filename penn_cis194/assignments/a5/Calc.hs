{-# LANGUAGE FlexibleInstances #-}
-- Note that FlexibleInstances was needed for this to work, instead of
-- TypeSynonymInstances, which did not work.

module Calc where

import ExprT
import Parser
import Control.Monad (liftM)
import qualified StackVM as VM


-- Exercise 1
eval :: ExprT -> Integer
eval (Lit i)     = i
eval (Add eL eR) = (eval eL) + (eval eR)
eval (Mul eL eR) = (eval eL) * (eval eR)


-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = liftM eval . parseExp Lit Add Mul 


-- Exercise 3
class Expr t where
    lit :: Integer -> t
    add :: t -> t -> t
    mul :: t -> t -> t

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

reify :: ExprT -> ExprT
reify = id


-- Exercise 4
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (> 0)
    add = (||)
    mul = (&&)

instance Expr MinMax where
    lit                       = MinMax
    add (MinMax x) (MinMax y) = MinMax $ max x y
    mul (MinMax x) (MinMax y) = MinMax $ min x y

instance Expr Mod7 where
    lit                   = Mod7 . (`mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"


-- Exercise 5

-- Helper function to pull values out of "StackVal" context and
-- place them into "StackExp" context. This is used for taking the
-- result of one stack computation and placing it into another stack.
convertStackVal :: VM.StackVal -> VM.StackExp
convertStackVal VM.Void     = error "Encountered Void stack."
convertStackVal (VM.IVal i) = VM.PushI i
convertStackVal (VM.BVal b) = VM.PushB b

-- Helper type and function to factor out the boiler plate code for
-- handling results of stackVM.
type StackRes = Either String VM.StackVal

switchStackVal :: StackRes -> StackRes -> VM.StackExp -> VM.Program
switchStackVal (Left s) _ _ = error s
switchStackVal _ (Left s) _ = error s
switchStackVal (Right v1) (Right v2) op = 
    [convertStackVal v1, convertStackVal v2, op]


-- Instance declaration for "Program". Here, a "literal" program is one
-- that just contains a value constructed from an integer. Addition of
-- two programs means to evaluate the programs, put the results into a
-- stack context, and then place the Add instruction in front of them.
-- Multiplication is the same, but with the Mul instruction. 
instance Expr VM.Program where
    lit = (:[]) . VM.PushI
    add p1 p2 = switchStackVal (VM.stackVM p1) (VM.stackVM p2) VM.Add
    mul p1 p2 = switchStackVal (VM.stackVM p1) (VM.stackVM p2) VM.Mul


compile :: String -> Maybe VM.Program
compile = parseExp lit add mul
              
