{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module RegInst where

import Control.Monad.State
import VM

data RegInst = RegInst Instruction [Int]
    deriving (Eq, Show)

data RegCell = RegCell

data RegInstState = RegInstState
    { virtualStack :: [Int]
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
toRegInst is = evalState (toRegInst' is) (RegInstState [])

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
    case vs of
        (_ : rest) -> do
            put $ s{virtualStack = rest}
            toRegInst' is
        [] -> error "Pop: empty virtual stack"
toRegInst' (Swp : is) = do
    s <- get
    let vs = virtualStack s
    case vs of
        (i1 : i2 : rest) -> do
            put $ s{virtualStack = i2 : i1 : rest}
            rs <- toRegInst' is
            return $ RegInst (Mov i1 (DInt 0)) [] : RegInst (Mov i2 (DInt 0)) [] : rs
        _ -> error "Swp: expected at least 2 items on virtual stack"
toRegInst' (l@(Label _) : is) = do
    rs <- toRegInst' is
    return $ RegInst l [] : rs
-- toRegInst' (l@Add : is) = do
--     virtualStack' <- gets virtualStack
--     modify $ \s -> s { virtualStack = drop 2 (virtualStack s) } -- TODO: !!!
--     rs <- toRegInst' is
--     return $ RegInst l virtualStack' : rs
toRegInst' x@(Add : _) = unstack x 2
toRegInst' _ = error "Not implemented"
