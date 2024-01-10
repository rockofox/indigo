{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module VM (run, runPc, runVM, initVM, toBytecode, fromBytecode, printAssembly, runVMVM, VM (..), Instruction (..), Program, Action (..), Data (..), StackFrame (..), IOBuffer (..), IOMode (..)) where

import Control.Monad
import Control.Monad.RWS hiding (local)
import Control.Monad.State.Lazy
import Data.Binary (Binary, decode, encode)
import Data.ByteString.Lazy (LazyByteString)
import Data.Function ((&))
import Data.List.Split (chunksOf)
import Data.Map qualified
import Data.Map qualified as Data
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified
import Data.Vector qualified as V
import Debug.Trace
import GHC.Generics (Generic)
import System.Random (randomIO)
#ifdef FFI
import Ffi
import Foreign.LibFFI.Base (newStorableStructArgRet, newStructCType, sizeAndAlignmentOfCType)
import Foreign.LibFFI.FFITypes
import Foreign.LibFFI
#endif

import Control.Monad.Reader
import Data.Binary qualified (get, put)
import Data.Char (chr, ord)
import Data.IORef (IORef)
import Data.Text.Internal.Unsafe.Char
import Foreign.C (CDouble, newCString)
import Foreign.C.String (castCharToCChar)
import Foreign.C.Types (CChar, CFloat, CInt, CSChar, CUChar)
import Foreign.LibFFI.Internal (CType)
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.IO (unsafePerformIO)
import GHC.IORef
import GHC.Int qualified as Ghc.Int

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
    | -- | Pop two values off the stack, calculate the power, and push the result
      Pow
    | -- | Absolute value
      Abs
    | -- | Pop two values off the stack, push T if the first is greater than the second, F otherwise
      Gt
    | -- | Pop two values off the stack, push T if the first is less than the second, F otherwise
      Lt
    | -- | Pop a value off the stack, push T if it is falsy, F otherwise
      Not
    | -- | Pop two values off the stack, push T if they are equal, F otherwise
      Eq
    | -- | Pop two values off the stack, push T if they are not equal, F otherwise
      Neq
    | -- | Pop two values off the stack and push T if they are both truthy, F otherwise
      And
    | -- | Pop two values off the stack and push T if either is truthy, F otherwise
      Or
    | -- | Perform a builtin action
      Builtin Action -- TODO: Seperate IO?
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
    | -- | Call a function and keep locals
      CallLocal String
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
    | -- | Pop a value off the stack and store it in the data memory
      Store Int
    | -- | Push a value from the data memory onto the stack
      Load Int
    | -- | Return to the caller
      Ret
    | -- | Pop a value off the stack and store it in a local variable with the given name
      LStore String
    | -- | Push the value of a local variable with the given name onto the stack
      LLoad String
    | -- | Pop n values off the stack, concatenate them, and push the result
      Concat Int
    | -- | Construct a list from the top n values of the stack
      PackList Int
    | -- | Index into a list
      Index
    | -- | Slice a list
      Slice
    | -- | Length of a list
      Length
    | -- | Get length of the stack
      StackLength
    | -- | Casts a to the type of b
      Cast
    | -- | Get the type of a
      TypeOf
    | -- | Panic with a message
      Panic
    | -- | Repeat the last instruction (n - 1) times
      Repeat Int
    | -- | Comment, does nothing
      Comment String
    | -- | Similar to a comment, adds additional information for tools like compilers to use during compilation and other stages
      Meta String
    | -- | Access a field
      Access String
    | -- | Pack map
      PackMap Int
    | -- | Compares two types and pushes T if they are compatible, F otherwise
      TypeEq
    | -- | mov
      Mov Int Data
    | -- | Call FFI
      CallFFI String String Int
    | -- | Exit the program
      Exit
    deriving (Show, Eq, Generic)

data Action = Print | GetLine | GetChar | Random deriving (Show, Eq, Generic)

data Data
    = DInt Int
    | DFloat Float
    | DDouble Double
    | DString String
    | DBool Bool
    | DList [Data]
    | DNone
    | DChar Char
    | DFuncRef String [Data] [(String, Data)]
    | DMap (Data.Map String Data)
    | DTypeQuery String
    | DCPtr WordPtr
    deriving (Generic, Eq)

instance Binary Instruction

instance Binary Action

instance Binary Data

instance Binary WordPtr where
    put = Data.Binary.put . toInteger
    get = fromInteger <$> Data.Binary.get

instance Show Data where
    show (DInt x) = show x
    show (DFloat x) = show x
    show (DString x) = x
    show (DBool x) = show x
    show (DList x) = show x
    show DNone = "None"
    show (DChar x) = show x
    show (DFuncRef x args _) = "<" ++ x ++ "(" ++ show args ++ ")>"
    show (DMap x) = show x
    show (DTypeQuery x) = "TypeQuery " ++ x
    show (DCPtr x) = "CPtr " ++ show x
    show (DDouble x) = show x

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
    , ioMode :: IOMode
    , ioBuffer :: IOBuffer
    }
    deriving (Show)

data IOMode = HostDirect | VMBuffer deriving (Show, Eq)

data IOBuffer = IOBuffer {input :: String, output :: String} deriving (Show, Eq)

initVM :: Program -> VM
initVM program = VM{program = program, stack = [], pc = 0, running = True, labels = [], memory = [], callStack = [], breakpoints = [], ioMode = HostDirect, ioBuffer = IOBuffer{input = "", output = ""}}

type Program = V.Vector Instruction

safeHead :: [a] -> a
safeHead [] = error "Empty list"
safeHead (x : _) = x

tailOrNothing :: [a] -> Maybe [a]
tailOrNothing [] = Nothing
tailOrNothing (_ : xs) = Just xs

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

stackLen :: StateT VM IO Int
stackLen = length . stack <$> get

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

-- FIXME: This is a mess
runVMVM :: VM -> IO VM
runVMVM vm = execStateT (run' (program vm)) vm

locateLabels :: Program -> [(String, Int)]
locateLabels program = [(x, n) | (Label x, n) <- zip (V.toList program) [0 ..]] -- TODO: Might be inefficient

showDebugInfo :: StateT VM IO String
-- showDebugInfo = get >>= \vm -> return $ show (pc vm) ++ "\t" ++ show (program vm V.! pc vm) ++ "\t" ++ show (stack vm) ++ "\t" ++ show (safeHead $ callStack vm) -- Showing the stack breaks stuff for some reason??
showDebugInfo = do
    vm <- get
    -- Show the current instruction, two values from the stack, and the result
    let inst = program vm V.! pc vm
    -- let stack' = stack vm
    -- let stack'' = if length stack' > 1 then take 2 stack' else stack'
    -- return $ show (pc vm) ++ "\t" ++ show inst ++ "\t" ++ show stack'' ++ "\t" ++ show (safeHead $ callStack vm)
    let layers = length (callStack vm) - 1
    return $ replicate layers ' ' ++ show (pc vm) ++ " " ++ show inst -- ++ " (" ++ show (stack vm) ++ ")"

run' :: Program -> StateT VM IO ()
run' program = do
    pc <- gets pc
    breakpoints <- gets breakpoints
    let inst = program V.! pc
    modify $ \vm -> vm{program = program, labels = locateLabels program}
    when (not (null breakpoints) && pc `elem` breakpoints || -1 `elem` breakpoints) $ showDebugInfo >>= traceM
    runInstruction inst
    modify $ \vm -> vm{pc = vm.pc + 1}
    gets running >>= flip when (run' program)

instance Real Data where
    toRational (DInt x) = toRational x
    toRational (DFloat x) = toRational x
    toRational (DDouble x) = toRational x
    toRational x = error $ "Cannot convert " ++ show x ++ " to Rational"

instance Enum Data where
    toEnum = DInt
    fromEnum (DInt x) = x
    fromEnum (DFloat x) = round x
    fromEnum (DDouble x) = round x
    fromEnum x = error $ "Cannot convert " ++ show x ++ " to Int"

instance Integral Data where
    quot (DInt x) (DInt y) = DInt $ quot x y
    quot _ _ = undefined
    rem (DInt x) (DInt y) = DInt $ rem x y
    rem _ _ = undefined
    quotRem (DInt x) (DInt y) = (DInt $ quot x y, DInt $ rem x y)
    quotRem _ _ = undefined
    toInteger (DInt x) = toInteger x
    toInteger (DFloat x) = round x
    toInteger (DDouble x) = round x
    toInteger x = error $ "Cannot convert " ++ show x ++ " to Integer"

instance Num Data where
    (+) (DInt x) (DInt y) = DInt $ x + y
    (+) (DFloat x) (DFloat y) = DFloat $ x + y
    (+) (DDouble x) (DDouble y) = DDouble $ x + y
    (+) (DString x) (DString y) = DString $ x ++ y
    (+) (DChar x) (DChar y) = DChar $ Data.Char.chr (Data.Char.ord x + Data.Char.ord y)
    (+) x y = error $ "Cannot add " ++ show x ++ " and " ++ show y
    (-) (DInt x) (DInt y) = DInt $ x - y
    (-) (DFloat x) (DFloat y) = DFloat $ x - y
    (-) (DDouble x) (DDouble y) = DDouble $ x - y
    (-) (DList x) (DList y) = DList $ filter (`notElem` y) x
    (-) (DChar x) (DChar y) = DChar $ Data.Char.chr (Data.Char.ord x - Data.Char.ord y)
    (-) x y = error $ "Cannot subtract " ++ show x ++ " and " ++ show y
    (*) (DInt x) (DInt y) = DInt $ x * y
    (*) (DFloat x) (DFloat y) = DFloat $ x * y
    (*) (DList x) (DFloat y) = DList $ map (* DFloat y) x -- TODO
    (*) (DDouble x) (DDouble y) = DDouble $ x * y -- TODO: make generic
    (*) (DChar x) (DChar y) = DChar $ Data.Char.chr (Data.Char.ord x * Data.Char.ord y)
    (*) x y = error $ "Cannot multiply " ++ show x ++ " and " ++ show y
    fromInteger = DInt . fromInteger
    abs (DInt x) = DInt $ abs x
    abs (DFloat x) = DFloat $ abs x
    abs x = error $ "Cannot take absolute value of " ++ show x
    signum (DInt x) = DInt $ signum x
    signum (DFloat x) = DFloat $ signum x
    signum x = error $ "Cannot take signum of " ++ show x

instance Fractional Data where
    (/) (DInt x) (DInt y) = DInt $ x `div` y
    (/) (DFloat x) (DFloat y) = DFloat $ x / y
    (/) (DDouble x) (DDouble y) = DDouble $ x / y
    (/) (DChar x) (DChar y) = DChar $ Data.Char.chr (Data.Char.ord x `div` Data.Char.ord y)
    (/) x y = error $ "Cannot divide " ++ show x ++ " and " ++ show y
    fromRational = DFloat . fromRational

instance Floating Data where
    (**) (DInt x) (DInt y) = DInt $ x ^ y
    (**) (DFloat x) (DFloat y) = DFloat $ x ** y
    (**) (DDouble x) (DDouble y) = DDouble $ x ** y
    (**) x y = error $ "Cannot raise " ++ show x ++ " to the power of " ++ show y
    pi = DFloat pi
    exp = error "exp not implemented"
    log = error "log not implemented"
    sin = error "sin not implemented"
    cos = error "cos not implemented"
    asin = error "asin not implemented"
    acos = error "acos not implemented"
    atan = error "atan not implemented"
    sinh = error "sinh not implemented"
    cosh = error "cosh not implemented"
    asinh = error "asinh not implemented"
    acosh = error "acosh not implemented"
    atanh = error "atanh not implemented"

findLabelFuzzy :: String -> StateT VM IO (String, Int)
findLabelFuzzy x = do
    labels <- gets labels
    case lookup x labels of
        Just (n :: Int) -> return (x, n)
        Nothing ->
            -- Look for fuzzy matches (without #)
            case filter (\(y, _) -> x == takeWhile (/= '#') y) labels of
                [] -> error $ "Label not found: " ++ x
                [(n, l)] -> return (n, l)
                xs -> error $ "Multiple labels found: " ++ show xs

class IntoData a where
    intoData :: a -> Data

instance IntoData Bool where
    intoData = DBool

instance IntoData Char where
    intoData = DChar

instance IntoData Data where
    intoData = id

instance IntoData Int where
    intoData = DInt

instance IntoData Float where
    intoData = DFloat

instance IntoData [Data] where
    intoData = DList

instance IntoData CFloat where
    intoData = DFloat . realToFrac

instance IntoData CDouble where
    intoData = DDouble . realToFrac

instance IntoData CChar where
    intoData = DChar . toEnum . fromEnum

instance IntoData String where
    intoData = DString

instance IntoData Ghc.Int.Int32 where
    intoData = DInt . fromIntegral

instance IntoData Double where
    intoData = DDouble

instance IntoData (Ptr ()) where
    intoData = DCPtr . ptrToWordPtr

#ifdef FFI
clearMap :: Data.Map String Data -> Data.Map String Data
clearMap = Data.Map.delete "__name" . Data.Map.delete "__traits"


globalStructType :: IORef [Ptr CType]
{-# NOINLINE globalStructType #-}
globalStructType = unsafePerformIO $ newIORef []

-- Only used for structs. I really don't like this
instance Storable Data where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    types <- readIORef globalStructType
    sizesAndAlignments <- mapM sizeAndAlignmentOfCType types
    let typesAndOffsets = zip types (scanl (\(o,_) (s,a) -> (o+s,a)) (0,0) sizesAndAlignments)
    b <- mapM mapData typesAndOffsets
    return $ DList b
    where
      mapData :: (Ptr CType, (Int, Int)) -> IO Data
      mapData tsa = do
        let (t,(o,_)) = tsa
        case () of _
                    | t == ffi_type_float -> do
                      return $ DFloat (realToFrac (unsafePerformIO $ peekByteOff ptr o :: CFloat))
                    | t == ffi_type_sint32 -> do
                      return $ DInt (fromIntegral (unsafePerformIO $ peekByteOff ptr o :: CInt))
                    | t == ffi_type_double -> do
                      return $ DDouble (realToFrac (unsafePerformIO $ peekByteOff ptr o :: CDouble))
                    | t == ffi_type_pointer -> do
                      return $ DCPtr (unsafePerformIO $ peekByteOff ptr o :: WordPtr)
                    | t == ffi_type_uchar -> do
                      return $ DChar (toEnum (fromEnum (unsafePerformIO $ peekByteOff ptr o :: CUChar)))
                    | t == ffi_type_void -> do
                      return DNone
                    | otherwise -> error $ "mapData: Invalid type " ++ show t


  poke ptr (DInt x) = poke (castPtr ptr) x
  poke ptr (DFloat x) = poke (castPtr ptr) x
  poke ptr (DString x) = poke (castPtr ptr) (unsafePerformIO $ newCString x)
  poke ptr (DChar x) = poke (castPtr ptr) x
  poke ptr (DMap x) = do
    let values = reverse $ Data.Map.elems (clearMap x)
    let sizes = map sizeOfC values
    mapM_ (\(i, v) -> pokeByteOff ptr (sum $ take i sizes) v) (zip [0..] values)
  poke _ x = error $ "unsupported poke " ++ show x

sizeOfC :: Data -> Int
sizeOfC (DChar _) = 1
sizeOfC (DDouble _) = 8
sizeOfC _ = 4

dataCType (DInt _) = ffi_type_sint32
dataCType (DFloat _) = ffi_type_float
dataCType (DString _) = ffi_type_pointer
dataCType (DBool _) = ffi_type_sint32
dataCType (DChar _) = ffi_type_uchar
dataCType (DList _) = ffi_type_pointer
dataCType (DDouble _) = ffi_type_double
dataCType (DMap _) = ffi_type_pointer
dataCType (DCPtr _) = ffi_type_pointer
dataCType DNone = ffi_type_void
dataCType _ = error "dataCType: Invalid type"

type ArgP = (Arg, Maybe (IO ()))

arg :: Data -> IO ArgP
arg (DInt x) = return (argInt32 (fromIntegral x), Nothing)
arg (DFloat x) =  return (argCFloat (realToFrac x), Nothing)
arg (DString x) = return (argString x, Nothing)
arg (DBool x) =  return (argCInt (if x then 1 else 0), Nothing)
arg (DChar x) =  return (argCChar (fromIntegral $ Data.Char.ord x), Nothing)
arg (DList x@(DInt _ : _)) = do
  withArray (map (\(DInt i) -> fromIntegral i :: CInt) x) $ \ptr -> do
    return (argPtr ptr, Nothing)
arg (DList x@(DChar _ : _)) = do -- TODO: make generic
  withArray (map (\(DChar c) -> castCharToCChar c) x) $ \ptr -> do
    return (argPtr ptr, Nothing)
arg (DDouble x) = return (argCDouble $ realToFrac x, Nothing)
arg d@(DMap x) = do
    if isJust $ Data.Map.lookup "__name" x then do
      let types = map dataCType (Data.Map.elems (clearMap x))
      (argT, _, freeTType) <- newStorableStructArgRet types
      return (argT d, Just freeTType)
    else
      error "no"
arg x = error $ "Cannot convert " ++ show x ++ " to FFI arg"

class FfiRet a b where
  ret :: b -> RetType a

instance FfiRet Ghc.Int.Int32 Data where
  ret (DInt _) = retInt32
  ret _ = error "Invalid return type"

instance FfiRet CFloat Data where
  ret (DFloat _) = retCFloat
  ret _ = error "Invalid return type"

instance FfiRet CDouble Data where
  ret (DDouble _) = retCDouble
  ret _ = error "Invalid return type"

instance FfiRet String Data where
  ret (DString _) = retString
  ret _ = error "Invalid return type"

instance FfiRet CChar Data where
  ret (DChar _) = retCChar
  ret _ = error "Invalid return type"

instance FfiRet (Ptr ()) Data where 
  ret (DCPtr _) = retPtr retVoid
  ret _ = error "Invalid return type"

instance FfiRet () Data where
  ret DNone = retVoid
  ret _ = error "Invalid return type"

stackPushA :: IntoData a => a -> StateT VM IO ()
stackPushA = stackPush . intoData

runInstruction :: Instruction -> StateT VM IO ()
runInstruction (CallFFI name from numArgs) = do
    dl <- if from /= "__default" then liftIO $ dynLibOpen (from ++ "." ++ dllext) else liftIO $ dynLibOpen crtPath
    fun <- liftIO $ dynLibSym dl name
    args <- stackPopN numArgs
    retT <- stackPop
    ffiArgs <- liftIO $ mapM arg args
    let ffiArgs' = map fst ffiArgs
    let frees = map snd ffiArgs 
    case retT of
      DInt{} -> do
        let retType = ret retT :: RetType Ghc.Int.Int32
        result <- liftIO $ callFFI fun retType ffiArgs'
        stackPushA result
      DFloat{} -> do
        let retType = ret retT :: RetType CFloat
        result <- liftIO $ callFFI fun retType ffiArgs'
        stackPushA result
      DString{} -> do
        let retType = ret retT :: RetType String
        result <- liftIO $ callFFI fun retType ffiArgs'
        stackPushA result
      DChar{} -> do
        let retType = ret retT :: RetType CChar
        result <- liftIO $ callFFI fun retType ffiArgs'
        stackPushA result
      DCPtr{} -> do
        let retType = ret retT :: RetType (Ptr ())
        result <- liftIO $ callFFI fun retType ffiArgs'
        stackPushA result
      DDouble{} -> do
        let retType = ret retT :: RetType CDouble
        result <- liftIO $ callFFI fun retType ffiArgs'
        stackPushA result
      DNone -> do
        let retType = ret retT :: RetType ()
        _ <- liftIO $ callFFI fun retType ffiArgs'
        return ()
      DMap x -> do
        let keys = reverse (Data.Map.keys (clearMap x))
        let types = map dataCType (Data.Map.elems (clearMap x))
        liftIO $ writeIORef globalStructType types 
        (_,retType,_) <- liftIO (newStorableStructArgRet types :: IO (Data -> Arg, RetType Data, IO ()))
        (DList values) <- liftIO $ callFFI fun retType ffiArgs'
        let result = DMap $ Data.Map.fromList $ zip keys values
        stackPushA result
      _ -> error $ "Invalid return type: " ++ show retT
    liftIO $ mapM_ (\case Just x -> x; Nothing -> return ()) frees
#else
runInstruction CallFFI{} = error "Tried to call FFI function, but FFI is not enabled in this build/not supported on this platform"
#endif
-- Basic stack operations
runInstruction (Push d) = stackPush d
-- runInstruction (PushPf name nArgs) = stackPopN nArgs >>= \args -> stackPush $ DFuncRef name args
runInstruction (PushPf name nArgs) = do
    (label, _) <- findLabelFuzzy name
    locals <- gets (locals . safeHead . callStack)
    stackPopN nArgs >>= \args -> stackPush $ DFuncRef label args locals
runInstruction Pop = void stackPop
runInstruction StackLength = stackLen >>= stackPush . DInt . fromIntegral
-- Arithmetic
runInstruction Add = stackPopN 2 >>= \[y, x] -> stackPush $ x + y
runInstruction Sub = stackPopN 2 >>= \[y, x] -> stackPush $ x - y
runInstruction Mul = stackPopN 2 >>= \[y, x] -> stackPush $ x * y
runInstruction Div = stackPopN 2 >>= \[y, x] -> stackPush $ x / y
runInstruction Pow = stackPopN 2 >>= \[y, x] -> stackPush $ x ** y
runInstruction Abs =
    stackPop >>= \x -> stackPush $ case x of
        DInt num -> DInt $ abs num
        DFloat num -> DFloat $ abs num
        _ -> error $ "Cannot take absolute value of " ++ show x
runInstruction Mod = stackPopN 2 >>= \[y, x] -> stackPush $ x `mod` y
-- IO
runInstruction (Builtin Print) = do
    vm <- get
    case ioMode vm of
        HostDirect -> stackPop >>= liftIO . putStr . show
        VMBuffer -> stackPop >>= \x -> put $ vm{ioBuffer = (ioBuffer vm){output = output (ioBuffer vm) ++ show x}}
runInstruction (Builtin GetLine) = do
    vm <- get
    case ioMode vm of
        HostDirect -> liftIO getLine >>= stackPush . DString
        VMBuffer -> do
            let input = (ioBuffer vm).input
            let (line, rest) = break (== '\n') input
            put $ vm{ioBuffer = (ioBuffer vm){input = rest}}
            stackPush $ DString line
runInstruction (Builtin GetChar) = do
    vm <- get
    case ioMode vm of
        HostDirect -> liftIO getChar >>= stackPush . DChar
        VMBuffer -> do
            let input = (ioBuffer vm).input
            let (char, rest) = (head input, tail input)
            put $ vm{ioBuffer = (ioBuffer vm){input = rest}}
            stackPush $ DChar char
runInstruction (Builtin Random) = do
    num <- liftIO (randomIO :: IO Float)
    stackPush $ DFloat num
runInstruction Exit = modify $ \vm -> vm{running = False}
-- Control flow
runInstruction (Call x) = do
    nextInstruction <- gets pc >>= \n -> gets program >>= \p -> return $ p V.! (n + 1)
    case nextInstruction of
        Ret -> tailCall
        _ -> modify $ \vm -> vm{pc = fromMaybe (error $ "Label not found: " ++ x) $ lookup x $ labels vm, callStack = StackFrame{returnAddress = pc vm, locals = []} : callStack vm}
  where
    tailCall = do
        -- runInstruction $ Push $ DString "tailcall"
        -- runInstruction $ Builtin Print
        runInstruction $ Jmp x
runInstruction (CallLocal x) = do
    nextInstruction <- gets pc >>= \n -> gets program >>= \p -> return $ p V.! (n + 1)
    case nextInstruction of
        Ret -> tailCall
        _ -> modify $ \vm -> vm{pc = fromMaybe (error $ "Label not found: " ++ x) $ lookup x $ labels vm, callStack = StackFrame{returnAddress = pc vm, locals = (head (callStack vm)).locals} : callStack vm}
  where
    tailCall = do
        traceM "Tail call"
        runInstruction $ Jmp x
runInstruction CallS =
    stackPop >>= \d -> case d of
        DFuncRef x args locals -> do
            stackPushN args
            runInstruction $ Call x
            forM_ locals $ \(name, value) -> runInstruction (Push value) >> runInstruction (LStore name)
        _ -> error $ "Cannot call " ++ show d
runInstruction (Jmp x) = do
    (_, n) <- findLabelFuzzy x
    modify $ \vm -> vm{pc = n}
runInstruction (Jnz x) = stackPop >>= \d -> when (d /= DInt 0) $ runInstruction (Jmp x)
runInstruction (Jz x) = stackPop >>= \d -> when (d == DInt 0) $ runInstruction (Jmp x)
runInstruction (Jt x) = stackPop >>= \d -> when (d == DBool True) $ runInstruction (Jmp x)
runInstruction (Jf x) = stackPop >>= \d -> when (d == DBool False) $ runInstruction (Jmp x)
runInstruction Ret = do
    -- stackLen >>= \l -> when (l > 1) (error $ "Stack contains more than one item before return. Items: " ++ l)
    -- get >>= \vm -> traceM $ show (stack vm)
    modify $ \vm -> vm{pc = returnAddress $ headOrError "Tried to return, but callstack was empty" (callStack vm), callStack = tail $ callStack vm}
-- Comparison
runInstruction Eq = stackPopN 2 >>= \(y : x : _) -> stackPush $ DBool $ x == y
runInstruction Neq = stackPopN 2 >>= \(y : x : _) -> stackPush $ DBool $ x /= y
runInstruction Lt = stackPopN 2 >>= \(y : x : _) -> stackPush $ DBool $ x < y
runInstruction Gt = stackPopN 2 >>= \(y : x : _) -> stackPush $ DBool $ x > y
runInstruction Not = stackPop >>= \d -> stackPush $ DBool $ not $ case d of DBool x -> x; _ -> error "Not a boolean"
runInstruction And = stackPopN 2 >>= \(y : x : _) -> stackPush $ DBool $ case (x, y) of (DBool a, DBool b) -> a && b; _ -> error "Not a boolean"
runInstruction Or = stackPopN 2 >>= \(y : x : _) -> stackPush $ DBool $ case (x, y) of (DBool a, DBool b) -> a || b; _ -> error "Not a boolean"
-- Label
runInstruction (Label _) = return () -- modify $ \vm -> vm{labels = (x, pc vm) : labels vm}
runInstruction (Locate x) = get >>= \vm -> stackPush $ DInt $ fromMaybe (error $ "Label not found: " ++ x) $ lookup x $ labels vm
-- Stack
runInstruction Swp = stackPopN 2 >>= \(x : y : _) -> stackPushN [y, x]
runInstruction Dup = stackLen >>= \sl -> when (sl > 0) $ stackPeek >>= stackPush
runInstruction (DupN n) = stackPeek >>= \d -> stackPushN $ replicate n d
-- Memory
runInstruction (Load x) = get >>= \vm -> stackPush $ memory vm !! x
runInstruction (Store x) = stackPop >>= \d -> modify $ \vm -> vm{memory = take x (memory vm) ++ [d] ++ drop (x + 1) (memory vm)}
-- Locals
runInstruction (LStore name) = do
    localsc <- fmap (locals . safeHead) (gets callStack)
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
        stackPush $ DList $ concatMap (\case DList elements -> elements; DString string -> (map DChar string); el -> [el]) x
        stackPeek >>= \case DList elements -> when (all (\case DChar _ -> True; _ -> False) elements) (stackPop >> stackPush (DString $ map (\(DChar char) -> char) elements)); _ -> return ()
runInstruction Index = stackPopN 2 >>= \(DInt i : DList l : _) -> stackPush $ l !! i
runInstruction Slice = do
    start <- stackPop
    end <- stackPop
    DList list <- stackPop
    stackPush $ case (end, start) of
        (DInt start', DInt end') -> DList $ slice start' (Just end') list
        (DInt start', DNone) -> DList $ slice start' Nothing list
        (DNone, DInt end') -> DList $ slice 0 (Just end') list
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
runInstruction Length =
    stackLen >>= \case
        0 -> stackPush $ DInt (-1)
        _ -> stackPop >>= \case DList l -> stackPush $ DInt $ length l; DString s -> stackPush $ DInt $ length s; _ -> error "Invalid type for length"
-- Type
runInstruction Cast = do
    -- stackPopN 2 >>= \(x : y : _) -> stackPushN [y, x]
    to <- stackPop
    stackPop >>= \case
        (DInt x) -> stackPush $ case to of
            DInt _ -> DInt x
            DFloat _ -> DFloat $ fromIntegral x
            DDouble _ -> DDouble $ fromIntegral x
            DString _ -> DString $ show x
            DBool _ -> DBool $ x /= 0
            DList _ -> DList [DInt x]
            DNone -> DNone
            DChar _ -> DChar $ toEnum x
            DCPtr _ -> DCPtr $ fromIntegral x
            type' -> error $ "Cast to Int for type not implemented: " ++ show type'
        (DFloat x) -> stackPush $ case to of
            DInt _ -> DInt $ round x
            DFloat _ -> DFloat x
            DDouble _ -> DDouble $ realToFrac x
            DString _ -> DString $ show x
            DBool _ -> DBool $ x /= 0
            DList _ -> DList [DFloat x]
            DNone -> DNone
            DChar _ -> DChar $ toEnum $ round x
            type' -> error $ "Cast to Float for type not implemented: " ++ show type'
        (DDouble x) -> stackPush $ case to of
            DInt _ -> DInt $ round x
            DFloat _ -> DFloat $ realToFrac x
            DDouble _ -> DDouble x
            DString _ -> DString $ show x
            DBool _ -> DBool $ x /= 0
            DList _ -> DList [DDouble x]
            DNone -> DNone
            DChar _ -> DChar $ toEnum $ round x
            type' -> error $ "Cast to Double for type not implemented: " ++ show type'
        (DString x) -> stackPush $ case to of
            DInt _ -> DInt $ read x
            DFloat _ -> DFloat $ read x
            DDouble _ -> DDouble $ read x
            DString _ -> DString x
            DBool _ -> DBool $ x /= ""
            DList _ -> DList [DString x]
            DNone -> DNone
            DChar _ -> DChar $ head x
            type' -> error $ "Cast to String for type not implemented: " ++ show type'
        x -> error $ "Cannot cast " ++ show x ++ " to " ++ show to
runInstruction (Meta _) = return ()
runInstruction (Comment _) = return ()
runInstruction (Access x) = stackPop >>= \case DMap m -> stackPush $ fromMaybe DNone $ Data.Map.lookup x m; _ -> error "Invalid type for access"
runInstruction (PackMap n) = do
    elems <- stackPopN n
    stackPush $ DMap $ Data.Map.fromList $ map (\[DString x, y] -> (x, y)) $ chunksOf 2 elems
runInstruction TypeOf =
    stackPop >>= \case
        DInt _ -> stackPush $ DString "Int"
        DFloat _ -> stackPush $ DString "Float"
        DString _ -> stackPush $ DString "String"
        DBool _ -> stackPush $ DString "Bool"
        DList _ -> stackPush $ DString "List"
        DNone -> stackPush $ DString "None"
        DChar _ -> stackPush $ DString "Char"
        DFuncRef{} -> stackPush $ DString "FuncRef"
        DMap m -> do
            case Data.Map.lookup "__name" m of
                Just (DString name) -> stackPush $ DString name
                _ -> stackPush $ DString "Map"
        DTypeQuery s -> stackPush $ DString $ "TypeQuery{" ++ s ++ "}"
        DCPtr _ -> stackPush $ DString "CPtr"
        DDouble _ -> stackPush $ DString "Double"
runInstruction (Repeat n) = do
    vm <- get
    let inst = program vm V.! (pc vm - 1)
    replicateM_ (n - 1) $ runInstruction inst
runInstruction TypeEq =
    stackPopN 2 >>= \(y : x : _) -> stackPush $ DBool $ case (x, y) of
        (DInt _, DInt _) -> True
        (DFloat _, DFloat _) -> True
        (DString _, DString _) -> True
        (DBool _, DBool _) -> True
        (DList _, DList _) -> True
        (DNone, DNone) -> True
        (DChar _, DChar _) -> True
        (DFuncRef{}, DFuncRef{}) -> True
        (DMap a, DMap b) -> do
            not (isJust (Data.Map.lookup "__name" a) && isJust (Data.Map.lookup "__name" b)) || (Data.Map.lookup "__name" a == Data.Map.lookup "__name" b)
        (DMap m, DTypeQuery tq) -> do
            let name = Data.Map.lookup "__name" m
            let traits = Data.Map.lookup "__traits" m
            case (name, traits) of
                (Just (DString n), Just (DList ts)) -> n == tq || DString tq `elem` ts
                _ -> False
        (DInt _, DTypeQuery s) -> s == "Int"
        (DFloat _, DTypeQuery s) -> s == "Float"
        (DString _, DTypeQuery s) -> s == "String"
        (DBool _, DTypeQuery s) -> s == "Bool"
        (DList _, DTypeQuery s) -> s == "List"
        (DNone, DTypeQuery s) -> s == "None"
        (DChar _, DTypeQuery s) -> s == "Char"
        (DFuncRef{}, DTypeQuery s) -> s == "FuncRef"
        _ -> False
runInstruction (PackList n) = stackPopN n >>= stackPush . DList
runInstruction (Mov n dat) = do
    -- Move data into register %n
    vm <- get
    put $ vm{memory = take n (memory vm) ++ [dat] ++ drop (n + 1) (memory vm)}
runInstruction Panic = stackPop >>= \x -> error $ "panic: " ++ show x

-- runInstruction x = error $ show x ++ ": not implemented"

printAssembly :: Program -> Bool -> String
printAssembly program showLineNumbers =
    if not showLineNumbers
        then concatMap printAssembly' program
        else concatMap (\(n, i) -> "\ESC[1;30m" ++ show n ++ "\ESC[0m       " ++ printAssembly' i) $ zip [0 :: Integer ..] (V.toList program)
  where
    printAssembly' :: Instruction -> String
    printAssembly' (Label name) = "\ESC[0;32m" <> name <> "\ESC[0m " ++ ":\n"
    printAssembly' (Comment text) = "\t;\ESC[0;33m " <> text <> " \ESC[0m " ++ "\n"
    printAssembly' (PushPf x y) = asmLine ("push_pf " ++ x ++ " " ++ show y)
    printAssembly' (Push x) = asmLine $ "push " ++ show x
    printAssembly' (Call name) = asmLine ("call " <> name)
    printAssembly' (Jmp name) = asmLine ("jmp " <> name)
    printAssembly' (Jz name) = asmLine ("jz " <> name)
    printAssembly' (Jnz name) = asmLine ("jnz " <> name)
    printAssembly' (Jt name) = asmLine ("jt " <> name)
    printAssembly' (Jf name) = asmLine ("jf " <> name)
    printAssembly' (LLoad name) = asmLine ("lload " <> name)
    printAssembly' a = asmLine $ show a

    asmLine :: String -> String
    asmLine a = "\t" ++ Data.Text.unpack a'''' ++ "\n"
      where
        a' = Data.Text.pack a
        a'' = Data.Text.replace "\n" "\\n" a'
        -- lowercase the first word
        a''' =
            Data.Text.split (== ' ') a'' & \case
                [] -> ""
                (x : xs) -> Data.Text.toLower x <> " " <> Data.Text.unwords xs
        a'''' =
            Data.Text.split (== ' ') a''' & \case
                [] -> ""
                (x : xs) -> "\ESC[0;34m" <> x <> " \ESC[0m " <> Data.Text.unwords xs

toBytecode :: Program -> LazyByteString
toBytecode = encode . V.toList

fromBytecode :: LazyByteString -> [Instruction]
fromBytecode = decode
