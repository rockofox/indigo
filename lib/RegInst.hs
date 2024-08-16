{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module RegInst where

import Control.Monad.State
import Data.List (find)
import Debug.Trace
import VM

data RegInst = RegInst Instruction [Int]
    deriving (Eq, Show)

data RegCell = RegCell

data RegInstState = RegInstState
    { virtualStack :: [Int]
    , localMapping :: [(Int, String)]
    }
    deriving (Eq, Show)

unstack :: [Instruction] -> Int -> State RegInstState [RegInst]
unstack (x : xs) n = do
    virtualStack' <- gets virtualStack
    modify $ \s -> s{virtualStack = drop n (virtualStack s)} -- TODO: !!!
    rs <- toRegInst' xs
    return $ RegInst x virtualStack' : rs
unstack [] _ = return []

toRegInst :: [Instruction] -> [RegInst]
toRegInst is = evalState (toRegInst' is) (RegInstState [] [])

createLocalMapping :: String -> State RegInstState ()
createLocalMapping name = do
    s <- get
    let i = length (localMapping s) + length (virtualStack s)
    put $ s{localMapping = (i, name) : localMapping s}

toRegInst' :: [Instruction] -> State RegInstState [RegInst]
toRegInst' [] = return []
toRegInst' (Push x : is) = do
    s <- get
    let vs = virtualStack s
    let i = length vs
    put $ s{virtualStack = i : vs}
    rs <- toRegInst' is
    return $ RegInst (Mov i x) [] : rs
toRegInst' (Pop : is) = do
    s <- get
    let vs = virtualStack s
    let i = head vs
    put $ s{virtualStack = tail vs}
    rs <- toRegInst' is
    return $ RegInst (Mov i (DInt 0)) [] : rs
toRegInst' (Swp : is) = do
    s <- get
    let vs = virtualStack s
    let i1 = head vs
    let i2 = head $ tail vs
    put $ s{virtualStack = i2 : i1 : tail (tail vs)}
    rs <- toRegInst' is
    return $ RegInst (Mov i1 (DInt 0)) [] : RegInst (Mov i2 (DInt 0)) [] : rs
toRegInst' (l@(Label _) : is) = do
    rs <- toRegInst' is
    return $ RegInst l [] : rs
-- Push a value out of a register onto the stack
toRegInst' (PushReg r : is) = do
    modify $ \s -> s{virtualStack = r : virtualStack s}
    toRegInst' is
-- Push a value out of a register onto the stack
toRegInst' (MovReg r : is) = do
    modify $ \s -> s{virtualStack = drop 1 (r : virtualStack s)}
    toRegInst' is
toRegInst' (LStore name : is) = do
    createLocalMapping name
    toRegInst' is
toRegInst' (LLoad name : is) = do
    regForName <- gets (find (\(_, n) -> n == name) . localMapping)
    case regForName of
        Just (i, _) -> do
            modify $ \s -> s{virtualStack = i : virtualStack s}
            toRegInst' is
        Nothing -> error $ "Variable " ++ name ++ " not found"
toRegInst' x@(Call _ : _) = unstack x 0
-- toRegInst' (l@Add : is) = do
--     virtualStack' <- gets virtualStack
--     modify $ \s -> s { virtualStack = drop 2 (virtualStack s) } -- TODO: !!!
--     rs <- toRegInst' is
--     return $ RegInst l virtualStack' : rs
toRegInst' x@(Add : _) = unstack x 2
toRegInst' x@(Builtin Print : _) = unstack x 1
toRegInst' (x : xs) = do
    rs <- toRegInst' xs
    return $ RegInst x [] : rs
