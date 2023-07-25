{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VM (run, runPc, runVM, initVM, toBytecode, fromBytecode, printAssembly, VM (..), Instruction (..), Program, Action (..), Data (..), StackFrame (..)) where

import Control.Exception (SomeException, catch, throw)
import Control.Monad.Error (Error (strMsg), MonadError (catchError, throwError))
import Control.Monad.RWS
import Control.Monad.State.Lazy
import Data.Binary (Binary, decode, encode)
import Data.ByteString.Lazy (LazyByteString)
import Data.Maybe (fromMaybe)
import Data.Text qualified
import Debug.Trace (trace, traceM, traceShow, traceShowM)
import GHC.Generics (Generic)

data Instruction
    = -- | Push a value onto the stack
      Push Data
    | -- | Pop two values off the stack, add them, and push the result
      Add
    | -- | Pop two values off the stack, subtract them, and push the result
      Sub
    | -- | Pop two values off the stack, multiply them, and push the result
      Mul
    | -- | Pop two values off the stack, divide them, and push the result
      Div
    | -- | Pop two values off the stack, divide them, and push the remainder
      Mod
    | -- | Pop two values off the stack, push T if the first is greater than the second, F otherwise
      Gt
    | -- | Pop two values off the stack, push T if the first is less than the second, F otherwise
      Lt
    | -- | Pop a value off the stack, push T if it is falsy, F otherwise
      Not
    | -- | Pop two values off the stack, push T if they are equal, F otherwise
      Eq
    | -- | Perform an IO action on the top value(s) of the stack
      Intr Action
    | -- | Jump to a label
      Jmp String
    | -- | Jump to a label if the top value of the stack is not zero
      Jnz String
    | -- | Jump to a label if the top value of the stack is falsy
      Jz String
    | -- | Jump to a label if the top value of the stack is truthy
      Jt String
    | -- | Jump to a label if the top value of the stack is falsy
      Jf String
    | -- | Call a function
      Call String
    | -- | Call a function from the stack
      CallS
    | -- | Push a partial function onto the stack
      PushPf String Int
    | -- | Pop a value off the stack
      Pop
    | -- | Duplicate the top value of the stack
      Dup
    | -- | Duplicate the n values of the stack
      DupN Int
    | -- | Swap the top two values of the stack
      Swp
    | -- | A label
      Label String
    | -- | Locate a label
      Locate String
    | -- | Pop a value off the stack and store it in a free address in the data memory
      Store
    | -- | Push the value at the given address in the data memory onto the stack
      Load Int
    | -- | Return to the caller
      Ret
    | -- | Pop a value off the stack and store it in a local variable with the given name
      LStore String
    | -- | Push the value of a local variable with the given name onto the stack
      LLoad String
    | -- | Pop n values off the stack, concatenate them, and push the result
      Concat Int
    | -- | Index into a list
      Index
    | -- | Slice a list
      Slice
    | -- | Casts a to the type of b
      Cast
    | -- | Similar to a comment, adds additional information for tools like compilers to use during compilation and other stages
      Meta String
    | -- | Exit the program
      Exit
    deriving (Show, Eq, Generic)

data Action = Print deriving (Show, Eq, Generic)

data Data = DInt Int | DFloat Float | DString String | DBool Bool | DList [Data] | DNone | DChar Char | DFuncRef String [Data] deriving (Generic)

instance Binary Instruction

instance Binary Action

instance Binary Data

instance Show Data where
    show (DInt x) = show x
    show (DFloat x) = show x
    show (DString x) = x
    show (DBool x) = show x
    show (DList x) = show x
    show DNone = "None"
    show (DChar x) = show x
    show (DFuncRef x args) = "<" ++ x ++ "(" ++ show args ++ ")>"

instance Eq Data where
    (DInt x) == (DInt y) = x == y
    (DFloat x) == (DFloat y) = x == y
    (DString x) == (DString y) = x == y
    (DBool x) == (DBool y) = x == y
    (DList x) == (DList y) = x == y
    x == y = error $ "Cannot eq " ++ show x ++ " and " ++ show y

instance Ord Data where
    (DInt x) `compare` (DInt y) = x `compare` y
    (DFloat x) `compare` (DFloat y) = x `compare` y
    (DString x) `compare` (DString y) = x `compare` y
    (DBool x) `compare` (DBool y) = x `compare` y
    (DList x) `compare` (DList y) = x `compare` y
    (DString x) `compare` (DList y) = x `compare` show y
    (DList x) `compare` (DString y) = show x `compare` y
    x `compare` y = error $ "Cannot compare " ++ show x ++ " and " ++ show y

data StackFrame = StackFrame
    { returnAddress :: Int
    , locals :: [(String, Data)]
    }
    deriving (Show)

data VM = VM
    { program :: Program
    , stack :: [Data]
    , pc :: Int
    , running :: Bool
    , labels :: [(String, Int)]
    , memory :: [Data]
    , callStack :: [StackFrame]
    , breakpoints :: [Int]
    }
    deriving (Show)

initVM :: Program -> VM
initVM program = VM{program = program, stack = [], pc = 0, running = True, labels = [], memory = [], callStack = [], breakpoints = []}

type Program = [Instruction]

safeHead :: [a] -> a
safeHead [] = error "Empty list"
safeHead (x : _) = x

tailOrNothing :: [a] -> Maybe [a]
tailOrNothing [] = Nothing
tailOrNothing (_ : xs) = Just xs

headOrEmpty :: [a] -> [a]
headOrEmpty [] = []
headOrEmpty (x : _) = [x]

headOrError :: String -> [a] -> a
headOrError err [] = error err
headOrError _ (x : _) = x

stackPush :: Data -> StateT VM IO ()
stackPush d = do
    vm <- get
    put $ vm{stack = d : stack vm}

stackPop :: StateT VM IO Data
stackPop = do
    vm <- get
    put $ vm{stack = fromMaybe (error "Empty stack") (tailOrNothing $ stack vm)}
    return $ safeHead $ stack vm

stackPopN :: Int -> StateT VM IO [Data]
stackPopN n = do
    vm <- get
    put $ vm{stack = drop n $ stack vm}
    return $ take n $ stack vm

stackPushN :: [Data] -> StateT VM IO ()
stackPushN ds = do
    vm <- get
    put $ vm{stack = ds ++ stack vm}

stackPeek :: StateT VM IO Data
stackPeek = safeHead . stack <$> get

run :: Program -> IO ()
run program = do
    let vm = initVM program
    evalStateT (run' program) vm

runPc :: Program -> Int -> IO ()
runPc program pc = do
    let vm = initVM program
    evalStateT (run' program) vm{pc = pc}

runVM :: VM -> IO ()
runVM vm = evalStateT (run' (program vm)) vm

locateLabels :: Program -> [(String, Int)]
locateLabels program = [(x, n) | (Label x, n) <- zip program [0 ..]]

showDebugInfo :: VM -> String
showDebugInfo vm = show (pc vm) ++ "\t" ++ show (program vm !! pc vm) ++ "\t" ++ show (stack vm) ++ "\t" ++ show (head $ callStack vm)

run' :: Program -> StateT VM IO ()
run' program = do
    vm <- get
    let inst = program !! pc vm
    -- traceM $ show (pc vm) ++ "\t" ++ show inst ++ "\t" ++ show (stack vm) -- ++ "\t" ++ show (callStack vm)
    put $ vm{program = program, labels = locateLabels program}
    catchError (runInstruction inst) $ \e -> do
        traceM $ "### Error: " ++ show e
        traceM $ "### STACK: " ++ show (stack vm)
        traceM $ "### CALLSTACK: " ++ show (callStack vm)
        traceM $ "### PC: " ++ show (pc vm)
    vm <- get
    when (not (null (breakpoints vm)) && pc vm `elem` breakpoints vm || -1 `elem` breakpoints vm) $ traceM $ showDebugInfo vm
    put $ vm{pc = pc vm + 1}
    when (running vm) $ run' program

instance Num Data where
    (+) (DInt x) (DInt y) = DInt $ x + y
    (+) (DFloat x) (DFloat y) = DFloat $ x + y
    (+) (DString x) (DString y) = DString $ x ++ y
    (+) (DList x) (DList y) = DList $ x ++ y
    (+) x y = error $ "Cannot add " ++ show x ++ " and " ++ show y
    (-) (DInt x) (DInt y) = DInt $ x - y
    (-) (DFloat x) (DFloat y) = DFloat $ x - y
    (-) x y = error $ "Cannot subtract " ++ show x ++ " and " ++ show y
    (*) (DInt x) (DInt y) = DInt $ x * y
    (*) (DFloat x) (DFloat y) = DFloat $ x * y
    (*) x y = error $ "Cannot multiply " ++ show x ++ " and " ++ show y

instance Fractional Data where
    (/) (DInt x) (DInt y) = DInt $ x `div` y
    (/) (DFloat x) (DFloat y) = DFloat $ x / y
    (/) x y = error $ "Cannot divide " ++ show x ++ " and " ++ show y

stackLen :: StateT VM IO Int
stackLen = length . stack <$> get

runInstruction :: Instruction -> StateT VM IO ()
-- Basic stack operations
runInstruction (Push d) = stackPush d
runInstruction (PushPf name nArgs) = stackPopN nArgs >>= \args -> stackPush $ DFuncRef name args
runInstruction Pop = void stackPop
-- Arithmetic
runInstruction Add = stackPopN 2 >>= \[y, x] -> stackPush $ x + y
runInstruction Sub = stackPopN 2 >>= \[y, x] -> stackPush $ x - y
runInstruction Mul = stackPopN 2 >>= \[y, x] -> stackPush $ x * y
runInstruction Div = stackPopN 2 >>= \[y, x] -> stackPush $ x / y
-- runInstruction Mod = stackPopN 2 >>= \[y, x] -> stackPush $ x `mod` y
runInstruction Mod = error "Mod not implemented"
-- IO
runInstruction (Intr Print) = stackPop >>= liftIO . putStr . show
runInstruction Exit = modify $ \vm -> vm{running = False}
-- Control flow
runInstruction (Call x) = modify $ \vm -> vm{pc = fromMaybe (error $ "Label not found: " ++ x) $ lookup x $ labels vm, callStack = StackFrame{returnAddress = pc vm, locals = []} : callStack vm}
runInstruction CallS = do
    stackPop >>= \d -> case d of
        DFuncRef x args -> do
            stackPushN args
            runInstruction (Call x)
        _ -> error $ "Cannot call " ++ show d
runInstruction (Jmp x) = modify $ \vm -> vm{pc = fromMaybe (error $ "Label not found: " ++ x) $ lookup x $ labels vm}
runInstruction (Jnz x) = stackPop >>= \d -> when (d /= DInt 0) $ runInstruction (Jmp x)
runInstruction (Jz x) = stackPop >>= \d -> when (d == DInt 0) $ runInstruction (Jmp x)
runInstruction (Jt x) = stackPop >>= \d -> when (d == DBool True) $ runInstruction (Jmp x)
runInstruction (Jf x) = stackPop >>= \d -> when (d == DBool False) $ runInstruction (Jmp x)
runInstruction Ret = do
    -- stackLen >>= \l -> when (l > 1) (error "Stack contains more than one item before return")
    modify $ \vm -> vm{pc = returnAddress $ headOrError "Tried to return, but callstack was empty" (callStack vm), callStack = tail $ callStack vm}
-- Comparison
runInstruction Eq = stackPopN 2 >>= \(x : y : _) -> stackPush $ DBool $ x == y
runInstruction Lt = stackPopN 2 >>= \(x : y : _) -> stackPush $ DBool $ x < y
runInstruction Gt = stackPopN 2 >>= \(x : y : _) -> stackPush $ DBool $ x > y
runInstruction Not = stackPop >>= \d -> stackPush $ DBool $ not $ case d of DBool x -> x; _ -> error "Not a boolean"
-- Label
runInstruction (Label _) = return () -- modify $ \vm -> vm{labels = (x, pc vm) : labels vm}
runInstruction (Locate x) = get >>= \vm -> stackPush $ DInt $ fromMaybe (error $ "Label not found: " ++ x) $ lookup x $ labels vm
-- Stack
runInstruction Swp = stackPopN 2 >>= \(x : y : _) -> stackPushN [y, x]
runInstruction Dup = stackPeek >>= stackPush
runInstruction (DupN n) = stackPeek >>= \d -> stackPushN $ replicate n d
-- Memory
runInstruction (Load x) = get >>= \vm -> stackPush $ memory vm !! x
runInstruction Store = stackPop >>= \d -> get >>= \vm -> put $ vm{memory = d : memory vm, stack = DInt (length (memory vm)) : stack vm}
-- Locals
runInstruction (LStore name) = do
    vm <- get
    let localsc = locals $ safeHead $ callStack vm
    let local = lookup name localsc
    case local of
        Just _ -> do
            d <- stackPop
            modify $ \vm -> vm{callStack = (safeHead $ callStack vm){locals = (name, d) : filter ((/= name) . fst) localsc} : tail (callStack vm)}
        Nothing -> do
            d <- stackPop
            modify $ \vm -> vm{callStack = (safeHead $ callStack vm){locals = (name, d) : localsc} : tail (callStack vm)}
runInstruction (LLoad name) = do
    vm <- get
    let localsc = locals $ safeHead $ callStack vm
    let local = lookup name localsc
    case local of
        Just x -> stackPush x
        Nothing -> error $ "Local not found: " ++ name
-- List
runInstruction (Concat 0) = stackPush $ DList []
runInstruction (Concat n) =
    -- TODO: make this more efficient, it's yucky
    stackPopN n >>= \x -> do
        -- let lists = [x | DList x <- x]
        -- let strings = [x | DString x <- x]
        -- case x of
        -- (DList _ : _) -> stackPush $ DList $ concat (reverse lists)
        -- (DString _ : _) -> stackPush $ DString $ concat (reverse strings)
        -- x -> stackPush $ DList x
        stackPush $ DList $ concatMap (\case DList x -> x; DString x -> (map DChar x); x -> [x]) x
        stackPeek >>= \case DList x -> when (all (\case DChar _ -> True; _ -> False) x) $ stackPop >>= \d -> stackPush $ DString $ map (\(DChar x) -> x) x; _ -> return ()
-- stackPush $ DList x
-- when (any (\case DString _ -> True; _ -> False) x) $ stackPop >>= \d -> stackPush $ DString $ show d
runInstruction Index = stackPopN 2 >>= \(DInt i : DList l : _) -> stackPush $ l !! i
-- runInstruction Slice = stackPopN 3 >>= \(DInt i : DInt j : DList l : _) -> stackPush $ DList $ take (j - i) $ drop i l
runInstruction Slice = do
    start <- stackPop
    end <- stackPop
    DList list <- stackPop
    stackPush $ case (end, start) of
        (DInt start, DInt end) -> DList $ slice start (Just end) list
        (DInt start, DNone) -> DList $ slice start Nothing list
        (DNone, DInt end) -> DList $ slice 0 (Just end) list
        (DNone, DNone) -> DList $ slice 0 Nothing list
        _ -> error "Invalid slice"
  where
    slice :: Int -> Maybe Int -> [a] -> [a]
    slice start maybeEnd xs
        | start' >= end' = []
        | otherwise = take (end' - start') $ drop start' xs
      where
        len = length xs
        start' = if start >= 0 then min start len else max 0 (len + start)
        end' = case maybeEnd of
            Just end -> if end > 0 then min end len else max 0 (len + end)
            Nothing -> len
-- Type
runInstruction Cast = do
    to <- stackPop
    stackPop >>= \case
        (DInt x) -> stackPush $ case to of
            DInt _ -> DInt x
            DFloat _ -> DFloat $ fromIntegral x
            DString _ -> DString $ show x
            DBool _ -> DBool $ x /= 0
            DList _ -> DList [DInt x]
            DNone -> DNone
            DChar _ -> DChar $ toEnum x
        (DFloat x) -> stackPush $ case to of
            DInt _ -> DInt $ round x
            DFloat _ -> DFloat x
            DString _ -> DString $ show x
            DBool _ -> DBool $ x /= 0
            DList _ -> DList [DFloat x]
            DNone -> DNone
            DChar _ -> DChar $ toEnum $ round x
        (DString x) -> stackPush $ case to of
            DInt _ -> DInt $ read x
            DFloat _ -> DFloat $ read x
            DString _ -> DString x
            DBool _ -> DBool $ x /= ""
            DList _ -> DList [DString x]
            DNone -> DNone
            DChar _ -> DChar $ head x
        x -> error $ "Cannot cast " ++ show x ++ " to " ++ show to
runInstruction (Meta _) = return ()

-- then
--     if start < 0
--         then stackPush $ DList $ take (abs start) $ reverse list
--         else stackPush $ DList $ drop start list
-- else stackPush $ DList $ take (end - start) $ drop start list

-----
-- runInstruction x = error $ show x ++ ": not implemented"

test :: Program
test =
    [ Push $ DInt 100
    , Label "loop"
    , Push $ DInt 1
    , Sub
    , DupN 2
    , Intr Print
    , Jnz "loop"
    , Exit
    ]

printAssembly :: Program -> Bool -> String
printAssembly program showLineNumbers = do
    if not showLineNumbers
        then concatMap printAssembly' program
        else concatMap (\(n, i) -> show n ++ "    " ++ printAssembly' i) $ zip [0 :: Integer ..] program
  where
    printAssembly' :: Instruction -> String
    printAssembly' (Label name) = name ++ ":\n"
    printAssembly' a = "\t" ++ Data.Text.unpack a'' ++ "\n"
      where
        a' = Data.Text.pack $ show a
        a'' = Data.Text.replace "\n" "\\n" a'

toBytecode :: Program -> LazyByteString
toBytecode = encode

fromBytecode :: LazyByteString -> [Instruction]
fromBytecode = decode