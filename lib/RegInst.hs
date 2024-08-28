{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module RegInst where

import Control.Monad.State
import Data.List (find)
import VM

data RegInst = RegInst
    { instruction :: Instruction
    , vs :: [Int]
    , resultingRegister :: Int
    }
    deriving (Eq, Show)

data RegCell = RegCell

data RegInstState = RegInstState
    { virtualStack :: [Int]
    , localMapping :: [(Int, String)]
    }
    deriving (Eq, Show)

printRegInstProgram :: [RegInst] -> String
printRegInstProgram = concatMap (\(RegInst i vs rr) -> init (printAssembly' i) <> " " <> show vs <> " " <> show rr <> "\n")

getResultingRegister :: State RegInstState Int
getResultingRegister = gets (length . virtualStack)

pushVS :: State RegInstState ()
pushVS = do
    vsLength <- gets (length . virtualStack)
    modify $ \s -> s{virtualStack = vsLength : virtualStack s}

{-
    virtualStack' <- gets virtualStack
    modify $ \s -> s{virtualStack = drop n (virtualStack s)} -- TODO: !!!
    resultingRegister <- getResultingRegister
    replicateM_ nrs pushVS
    return [RegInst x virtualStack' resultingRegister]
-}
unstack :: [Instruction] -> Int -> Int -> State RegInstState [RegInst]
unstack (x : _) n nrs = do
    virtualStack' <- gets virtualStack
    modify $ \s -> s{virtualStack = drop n (virtualStack s)} -- TODO: !!!
    resultingRegister <- getResultingRegister
    replicateM_ nrs pushVS
    return [RegInst x virtualStack' resultingRegister]
unstack [] _ _ = return []

toRegInst :: [Instruction] -> [RegInst]
toRegInst is = evalState (toRegInst' is) (RegInstState [] [])

findFreeRegisterId :: State RegInstState Int
findFreeRegisterId = do
    i <- gets (length . localMapping)
    return $ 100 + i

createLocalMapping :: String -> State RegInstState Int
createLocalMapping name = do
    s <- get
    -- TODO: not good
    localMapping' <- gets localMapping
    -- if not $ any (\(_, na) -> name == na) localMapping'
    --     then do
    i <- findFreeRegisterId
    put $ s{localMapping = (i, name) : filter (\(_, n) -> n /= name) (localMapping s)}
    return i

-- else do
-- let (i, _) = fromJust $ find (\(_, na) -> na == name) localMapping'
-- return i

toRegInst' :: [Instruction] -> State RegInstState [RegInst]
toRegInst' [] = return []
toRegInst' (Push x : is) = do
    s <- get
    let vs = virtualStack s
    let i = length vs
    put $ s{virtualStack = i : vs}
    rs <- toRegInst' is
    resultingRegister <- getResultingRegister
    return $ RegInst (Mov i (Lit x)) [] resultingRegister : rs
toRegInst' (Pop : is) = do
    s <- get
    let vs = virtualStack s
    let i = head vs
    put $ s{virtualStack = tail vs}
    rs <- toRegInst' is
    resultingRegister <- getResultingRegister
    return $ RegInst (Mov i (Lit $ DInt 0)) [] resultingRegister : rs
toRegInst' (Swp : is) = do
    s <- get
    let vs = virtualStack s
    let i1 = head vs
    let i2 = head $ tail vs
    put $ s{virtualStack = i2 : i1 : tail (tail vs)}
    rs <- toRegInst' is
    resultingRegister <- getResultingRegister -- ???
    return $ RegInst (Mov i1 (Lit $ DInt 0)) [] resultingRegister : RegInst (Mov i2 (Lit $ DInt 0)) [] resultingRegister : rs
toRegInst' (Dup : is) = do
    vsTop <- gets (head . virtualStack)
    vsLen <- gets (length . virtualStack)
    modify $ \s -> s{virtualStack = vsLen : s.virtualStack}
    resultingRegister <- getResultingRegister
    rs <- toRegInst' is
    return $ RegInst (Mov vsLen (Reg vsTop)) [] resultingRegister : rs
-- toRegInst' (l@(Label _) : is) = do
toRegInst' (l@(Label _) : pmt@(PragmaMethodTypes t) : is) = do
    replicateM_ (length t) pushVS
    rs <- toRegInst' is
    vs <- gets (reverse . virtualStack)
    -- vsLength <- gets (length . virtualStack)
    resultingRegister <- getResultingRegister
    return $ RegInst l vs resultingRegister : RegInst pmt vs resultingRegister : rs
toRegInst' (PushReg v : is) = do
    s <- get
    let vs = virtualStack s
    let i = length vs
    put $ s{virtualStack = i : vs}
    rs <- toRegInst' is
    resultingRegister <- getResultingRegister
    return $ RegInst (Mov i (Reg v)) [] resultingRegister : rs
toRegInst' (MovReg r : is) = do
    vsTop <- gets (head . virtualStack)
    modify $ \s -> s{virtualStack = drop 1 (virtualStack s)}
    resultingRegister <- getResultingRegister
    rs <- toRegInst' is
    return $ RegInst (Mov r (Reg vsTop)) [] resultingRegister : rs
toRegInst' (LStore name : is) = do
    -- vsLength <- gets (length . virtualStack)
    i <- createLocalMapping name
    -- let vsTop = vsLength + 1
    vsTop <- gets (head . virtualStack)
    modify $ \s -> s{virtualStack = drop 1 (virtualStack s)}
    resultingRegister <- getResultingRegister
    rs <- toRegInst' is
    return $ RegInst (Mov i (Reg vsTop)) [] resultingRegister : rs
toRegInst' (LLoad name : is) = do
    regForName <- gets (find (\(_, n) -> n == name) . reverse . localMapping)
    case regForName of
        Just (i, _) -> do
            -- traceM $ "Register with name " ++ name ++ " is " ++ show i
            vsLength <- gets (length . virtualStack)
            modify $ \s -> s{virtualStack = vsLength : virtualStack s}
            rs <- toRegInst' is
            return $ RegInst (Mov vsLength (Reg i)) [] (-1) : rs
        Nothing -> error $ "Local " ++ name ++ " not found"
toRegInst' (x@(Call _) : xs) = liftM2 (++) (unstack [x] 0 1) (toRegInst' xs)
toRegInst' (x@(CallLocal _) : xs) = liftM2 (++) (unstack [x] 0 1) (toRegInst' xs)
toRegInst' (x@(CallS{}) : xs) = liftM2 (++) (unstack [x] 1 1) (toRegInst' xs)
toRegInst' (x@(Jmp _) : xs) = liftM2 (++) (unstack [x] 0 1) (toRegInst' xs)
-- toRegInst' (x@(Jnz _) : xs) = liftM2 (++) (unstack [x] 1 1) (toRegInst' xs)
-- toRegInst' (x@(Jz _) : xs) = liftM2 (++) (unstack [x] 1 1) (toRegInst' xs)
toRegInst' (x@(Jt _) : xs) = liftM2 (++) (unstack [x] 1 1) (toRegInst' xs)
toRegInst' (x@(Jf _) : xs) = liftM2 (++) (unstack [x] 1 1) (toRegInst' xs)
toRegInst' x@(Add : xs) = liftM2 (++) (unstack x 2 1) (toRegInst' xs)
toRegInst' x@(Sub : xs) = liftM2 (++) (unstack x 2 1) (toRegInst' xs)
toRegInst' x@(Mul : xs) = liftM2 (++) (unstack x 2 1) (toRegInst' xs)
toRegInst' x@(Div : xs) = liftM2 (++) (unstack x 2 1) (toRegInst' xs)
toRegInst' x@(Mod : xs) = liftM2 (++) (unstack x 2 1) (toRegInst' xs)
toRegInst' x@(Pow : xs) = liftM2 (++) (unstack x 2 1) (toRegInst' xs)
toRegInst' x@(Abs : xs) = liftM2 (++) (unstack x 1 1) (toRegInst' xs)
toRegInst' x@(Gt : xs) = liftM2 (++) (unstack x 2 1) (toRegInst' xs)
toRegInst' x@(Lt : xs) = liftM2 (++) (unstack x 2 1) (toRegInst' xs)
toRegInst' x@(Not : xs) = liftM2 (++) (unstack x 1 1) (toRegInst' xs)
toRegInst' x@(Eq : xs) = liftM2 (++) (unstack x 2 1) (toRegInst' xs)
toRegInst' x@(Neq : xs) = liftM2 (++) (unstack x 2 1) (toRegInst' xs)
toRegInst' x@(And : xs) = liftM2 (++) (unstack x 2 1) (toRegInst' xs)
toRegInst' x@(Or : xs) = liftM2 (++) (unstack x 2 1) (toRegInst' xs)
toRegInst' x@(Length : xs) = liftM2 (++) (unstack x 1 1) (toRegInst' xs)
toRegInst' x@(Index : xs) = liftM2 (++) (unstack x 2 1) (toRegInst' xs)
toRegInst' x@(Slice : xs) = liftM2 (++) (unstack x 3 1) (toRegInst' xs)
toRegInst' x@((Concat n) : xs) = liftM2 (++) (unstack x n 1) (toRegInst' xs)
toRegInst' x@((PackList n) : xs) = liftM2 (++) (unstack x n 1) (toRegInst' xs)
toRegInst' x@(PragmaMethodTypes{} : xs) = liftM2 (++) (unstack x 0 0) (toRegInst' xs)
toRegInst' x@((PushPf _ nargs) : xs) = liftM2 (++) (unstack x nargs 1) (toRegInst' xs)
toRegInst' x@(Exit : xs) = liftM2 (++) (unstack x 0 0) (toRegInst' xs)
toRegInst' (StackLength : is) = do
    vsLength <- gets (length . virtualStack)
    modify $ \s -> s{virtualStack = vsLength : s.virtualStack}
    rs <- toRegInst' is
    resultingRegister <- getResultingRegister
    return $ RegInst (Mov vsLength (Lit (DInt vsLength))) [vsLength] resultingRegister : rs
toRegInst' x@((Builtin Print) : xs) = liftM2 (++) (unstack x 1 0) (toRegInst' xs)
toRegInst' (Ret : is) = do
    resultingRegister <- getResultingRegister
    vs <-
        gets virtualStack >>= \case
            [] -> return []
            (x : _) -> return [x]
    modify $ \s -> s{virtualStack = []}
    fmap ([RegInst Ret vs resultingRegister] ++) (toRegInst' is)
-- toRegInst' (x : xs) = do
--     rs <- toRegInst' xs
--     resultingRegister <- getResultingRegister
--     return $ RegInst x [] resultingRegister : rs
toRegInst' (x : _) = error ("Instruction " ++ show x ++ " not implemented in RegInst module")
