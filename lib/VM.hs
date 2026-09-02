{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module VM (run, runPc, runVM, initVM, toBytecode, fromBytecode, printAssembly, runVMVM, VM (..), Instruction (..), Program, Action (..), Data (..), StackFrame (..), IOBuffer (..), IOMode (..)) where

import Control.Exception (IOException, try)
import Control.Monad
import Control.Monad.State.Strict
import Data.Binary (Binary, decode, encode)
import Data.ByteString.Lazy (LazyByteString)
import Data.Function ((&))
import Data.List.Split (chunksOf)
import Data.Map qualified
import Data.Map qualified as Data
import Data.Maybe (fromMaybe)
import Data.Text qualified
import Data.Vector qualified as V
import GHC.Generics (Generic)
import System.Random (randomIO)
import Text.Read (readMaybe)
#ifdef FFI
import Ffi
import Foreign.LibFFI.Base (newStorableStructArgRet, sizeAndAlignmentOfCType)
import Foreign.LibFFI.FFITypes
import Foreign.LibFFI
import Foreign.LibFFI.Internal (CType)
#endif

import Data.Binary qualified (get, put)
import Data.ByteString.Char8 (pack, unpack)
import Data.Char (chr, ord)
import Data.List qualified
import Data.Map ((!?))
import Data.Map.Strict qualified as Map
import Foreign.C (CDouble, newCString)
import Foreign.C.String (castCharToCChar)
import Foreign.C.Types (CChar, CFloat, CInt, CUChar)
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.IO (unsafePerformIO)
import GHC.IORef
import GHC.Int qualified as Ghc.Int
#ifdef POSIX_IO
import Network.Socket (AddrInfo (..), AddrInfoFlag (..), Family (..), ProtocolNumber, Socket, SocketType (..), addrAddress, defaultHints, getAddrInfo)
import Network.Socket qualified as NS
import Network.Socket.ByteString qualified as NSB
import System.IO qualified as SIO
#endif
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import Util (showSanitized)

data Instruction
    = Push Data
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Pow
    | Abs
    | Gt
    | Lt
    | Not
    | Eq
    | Neq
    | And
    | Or
    | Builtin Action
    | Jmp String
    | Jt String
    | Jf String
    | Call String
    | CallLocal String
    | CallS
    | PushPf String Int
    | Pop
    | Dup
    | Swp
    | Label String
    | Store Int
    | Load Int
    | Ret
    | LStore String
    | LLoad String
    | StoreSideStack
    | LoadSideStack
    | ClearSideStack
    | Concat Int
    | ListAdd Int
    | PackList Int
    | UnpackList
    | Index
    | Slice
    | Length
    | Cast
    | TypeOf
    | Comment String
    | Meta String
    | Access String
    | AAccess
    | Keys
    | Update
    | PackMap Int
    | TypeEq
    | Mov Int Data
    | CallFFI String String Int
    | Exit
    deriving (Show, Eq, Generic)

#ifdef POSIX_IO
data Action = Print | GetLine | GetChar | Random | OpenFile | ReadFile | WriteFile | CloseFile | Socket | Bind | Listen | Accept | Connect | Send | Recv | CloseSocket deriving (Show, Eq, Generic)
#else
data Action = Print | GetLine | GetChar | Random deriving (Show, Eq, Generic)
#endif

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
    show (DMap x) =
        case x !? "__name" of
            Just (DString name) ->
                name ++ "{" ++ Data.List.intercalate ", " (map (\(k, v) -> k ++ ": " ++ show v) $ Data.Map.toList $ Data.Map.delete "__name" x) ++ "}"
            _ -> show x
    show (DTypeQuery x) = "TypeQuery " ++ x
    show (DCPtr x) = "CPtr " ++ show x
    show (DDouble x) = show x

instance Ord Data where
    compare (DInt x) (DInt y) = x `compare` y
    compare (DFloat x) (DFloat y) = x `compare` y
    compare (DDouble x) (DDouble y) = x `compare` y
    compare (DString x) (DString y) = x `compare` y
    compare (DBool x) (DBool y) = x `compare` y
    compare (DChar x) (DChar y) = x `compare` y
    compare (DList x) (DList y) = x `compare` y
    compare DNone DNone = EQ
    compare (DTypeQuery x) (DTypeQuery y) = x `compare` y
    compare (DCPtr x) (DCPtr y) = x `compare` y
    compare (DFuncRef n1 a1 _) (DFuncRef n2 a2 _) = (n1, a1) `compare` (n2, a2)
    compare (DMap m1) (DMap m2) = Data.Map.toList m1 `compare` Data.Map.toList m2
    compare (DInt x) (DFloat y) = fromIntegral x `compare` y
    compare (DFloat x) (DInt y) = x `compare` fromIntegral y
    compare (DInt x) (DDouble y) = fromIntegral x `compare` y
    compare (DDouble x) (DInt y) = x `compare` fromIntegral y
    compare (DFloat x) (DDouble y) = realToFrac x `compare` y
    compare (DDouble x) (DFloat y) = x `compare` realToFrac y
    compare (DString x) (DList y) = x `compare` show y
    compare (DList x) (DString y) = show x `compare` y
    compare x y = dataTag x `compare` dataTag y
      where
        dataTag :: Data -> Int
        dataTag DNone = 0
        dataTag (DBool _) = 1
        dataTag (DInt _) = 2
        dataTag (DFloat _) = 3
        dataTag (DDouble _) = 4
        dataTag (DChar _) = 5
        dataTag (DString _) = 6
        dataTag (DList _) = 7
        dataTag (DMap _) = 8
        dataTag (DFuncRef{}) = 9
        dataTag (DTypeQuery _) = 10
        dataTag (DCPtr _) = 11

data StackFrame = StackFrame
    { returnAddress :: !Int
    , locals :: ![(String, Data)]
    }
    deriving (Show)

#ifdef POSIX_IO
data VM = VM
    { program :: !Program
    , stack :: ![Data]
    , pc :: !Int
    , running :: !Bool
    , labels :: ![(String, Int)]
    , memory :: ![Data]
    , callStack :: ![StackFrame]
    , breakpoints :: ![Int]
    , ioMode :: !IOMode
    , ioBuffer :: !IOBuffer
    , sideStack :: ![Data]
    , shouldExit :: !Bool
    , vmError :: !(Maybe String)
    , sockets :: !(Map.Map Int Socket)
    , nextSocketId :: !Int
    , fileHandles :: !(Map.Map Int SIO.Handle)
    , nextHandleId :: !Int
    }
    deriving (Show)
#else
data VM = VM
    { program :: !Program
    , stack :: ![Data]
    , pc :: !Int
    , running :: !Bool
    , labels :: ![(String, Int)]
    , memory :: ![Data]
    , callStack :: ![StackFrame]
    , breakpoints :: ![Int]
    , ioMode :: !IOMode
    , ioBuffer :: !IOBuffer
    , sideStack :: ![Data]
    , shouldExit :: !Bool
    , vmError :: !(Maybe String)
    }
    deriving (Show)
#endif

data IOMode = HostDirect | VMBuffer deriving (Show, Eq)

data IOBuffer = IOBuffer {input :: String, output :: String} deriving (Show, Eq)

initVM :: Program -> VM
#ifdef POSIX_IO
initVM prog = VM{program = prog, stack = [], pc = 0, running = True, labels = locateLabels prog, memory = [], callStack = [], breakpoints = [], ioMode = HostDirect, ioBuffer = IOBuffer{input = "", output = ""}, sideStack = [], shouldExit = True, vmError = Nothing, sockets = Map.empty, nextSocketId = 1, fileHandles = Map.empty, nextHandleId = 1}
#else
initVM prog = VM{program = prog, stack = [], pc = 0, running = True, labels = locateLabels prog, memory = [], callStack = [], breakpoints = [], ioMode = HostDirect, ioBuffer = IOBuffer{input = "", output = ""}, sideStack = [], shouldExit = True, vmError = Nothing}
#endif

type Program = V.Vector Instruction

safeChr :: Int -> Char
safeChr n
    | n < 0 = '\0'
    | n > 0x10FFFF = '\x10FFFF'
    | n >= 0xD800 && n <= 0xDFFF = '\xFFFD'
    | otherwise = chr n

isTruthy :: Data -> Bool
isTruthy (DBool b) = b
isTruthy (DInt i) = i /= 0
isTruthy (DFloat f) = f /= 0
isTruthy (DDouble d) = d /= 0
isTruthy (DString s) = not (null s)
isTruthy (DList l) = not (null l)
isTruthy DNone = False
isTruthy _ = True

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

setVMError :: String -> StateT VM IO ()
setVMError err = modify $ \vm -> vm{running = False, vmError = Just err}

stackPush :: Data -> StateT VM IO ()
stackPush d = modify $ \vm -> vm{stack = d : stack vm}

stackPushN :: [Data] -> StateT VM IO ()
stackPushN ds = modify $ \vm -> vm{stack = ds ++ stack vm}

stackPop :: StateT VM IO Data
stackPop = do
    vm <- get
    case stack vm of
        [] -> do
            setVMError "Stack underflow: pop from empty stack"
            return DNone
        (x : xs) -> do
            put $ vm{stack = xs}
            return x

stackPopN :: Int -> StateT VM IO [Data]
stackPopN n
    | n <= 0 = return []
    | otherwise = do
        vm <- get
        let st = stack vm
        if length st < n
            then do
                setVMError $ "Stack underflow: expected " ++ show n ++ " values, got " ++ show (length st)
                put $ vm{stack = []}
                return st
            else do
                put $ vm{stack = drop n st}
                return (take n st)

stackPeek :: StateT VM IO Data
stackPeek = do
    st <- gets stack
    case st of
        (x : _) -> return x
        [] -> return DNone

stackLen :: StateT VM IO Int
stackLen = length . stack <$> get

storeMemory :: Int -> Data -> [Data] -> [Data]
storeMemory idx val mem
    | idx < 0 = mem
    | idx < length mem = take idx mem ++ [val] ++ drop (idx + 1) mem
    | otherwise = mem ++ replicate (idx - length mem) DNone ++ [val]

loadMemory :: Int -> [Data] -> Data
loadMemory idx mem
    | idx < 0 || idx >= length mem = DNone
    | otherwise = mem !! idx

run :: Program -> IO ()
run prog = do
    let vm = initVM prog
    evalStateT (run' prog) vm

runPc :: Program -> Int -> IO ()
runPc prog startPc = do
    let vm = initVM prog
    evalStateT (run' prog) vm{pc = startPc}

runVM :: VM -> IO ()
runVM vm = do
    finalVM <- execStateT (run' (program vm)) vm
    case vmError finalVM of
        Just err -> putStrLn ("Runtime error: " ++ err)
        Nothing -> return ()

runVMVM :: VM -> IO VM
runVMVM vm = execStateT (run' (program vm)) vm

locateLabels :: Program -> [(String, Int)]
locateLabels prog = [(x, n) | (Label x, n) <- zip (V.toList prog) [0 ..]]

showDebugInfo :: StateT VM IO String
showDebugInfo = do
    vm <- get
    let currentPc = pc vm
    let inst = program vm V.!? currentPc
    let instStr = maybe "<out of bounds>" show inst
    let layers = max 0 (length (callStack vm) - 1)
    return $ replicate layers ' ' ++ show currentPc ++ " " ++ instStr ++ " <- " ++ showSanitized (stack vm)

run' :: Program -> StateT VM IO ()
run' prog = do
    vm <- get
    when (running vm) $ do
        let currentPc = pc vm
        if currentPc < 0 || currentPc >= V.length prog
            then modify $ \v -> v{running = False}
            else do
                let inst = prog V.! currentPc
                case breakpoints vm of
                    [] -> return ()
                    bps | currentPc `elem` bps || (-1) `elem` bps -> do
                        info <- showDebugInfo
                        liftIO $ putStrLn info
                    _ -> return ()
                runInstruction inst
                modify $ \v -> v{pc = pc v + 1}
                gets running >>= flip when (run' prog)

instance Real Data where
    toRational (DInt x) = toRational x
    toRational (DFloat x) = toRational x
    toRational (DDouble x) = toRational x
    toRational (DBool x) = if x then 1 else 0
    toRational (DChar x) = toRational (ord x)
    toRational _ = 0

instance Enum Data where
    toEnum = DInt
    fromEnum (DInt x) = x
    fromEnum (DFloat x) = round x
    fromEnum (DDouble x) = round x
    fromEnum (DBool x) = if x then 1 else 0
    fromEnum (DChar x) = ord x
    fromEnum _ = 0

instance Integral Data where
    quot (DInt x) (DInt y) = if y == 0 then DInt 0 else DInt $ quot x y
    quot (DFloat x) (DFloat y) = if y == 0 then DFloat 0 else DFloat $ fromInteger $ quot (round x) (round y)
    quot (DDouble x) (DDouble y) = if y == 0 then DDouble 0 else DDouble $ fromInteger $ quot (round x) (round y)
    quot a b = quot (DInt $ fromEnum a) (DInt $ fromEnum b)

    rem (DInt x) (DInt y) = if y == 0 then DInt 0 else DInt $ rem x y
    rem (DFloat x) (DFloat y) = if y == 0 then DFloat 0 else DFloat $ fromInteger $ rem (round x) (round y)
    rem (DDouble x) (DDouble y) = if y == 0 then DDouble 0 else DDouble $ fromInteger $ rem (round x) (round y)
    rem a b = rem (DInt $ fromEnum a) (DInt $ fromEnum b)

    quotRem (DInt x) (DInt y) = if y == 0 then (DInt 0, DInt 0) else (DInt $ quot x y, DInt $ rem x y)
    quotRem a b = (quot a b, rem a b)

    toInteger (DInt x) = toInteger x
    toInteger (DFloat x) = round x
    toInteger (DDouble x) = round x
    toInteger (DBool x) = if x then 1 else 0
    toInteger (DChar x) = toInteger (ord x)
    toInteger _ = 0

instance Num Data where
    (+) (DInt x) (DInt y) = DInt $ x + y
    (+) (DFloat x) (DFloat y) = DFloat $ x + y
    (+) (DDouble x) (DDouble y) = DDouble $ x + y
    (+) (DInt x) (DFloat y) = DFloat $ fromIntegral x + y
    (+) (DFloat x) (DInt y) = DFloat $ x + fromIntegral y
    (+) (DInt x) (DDouble y) = DDouble $ fromIntegral x + y
    (+) (DDouble x) (DInt y) = DDouble $ x + fromIntegral y
    (+) (DFloat x) (DDouble y) = DDouble $ realToFrac x + y
    (+) (DDouble x) (DFloat y) = DDouble $ x + realToFrac y
    (+) (DString x) (DString y) = DString $ x ++ y
    (+) (DChar x) (DChar y) = DChar $ safeChr (ord x + ord y)
    (+) (DList x) (DList y) = DList $ x ++ y
    (+) x _ = x

    (-) (DInt x) (DInt y) = DInt $ x - y
    (-) (DFloat x) (DFloat y) = DFloat $ x - y
    (-) (DDouble x) (DDouble y) = DDouble $ x - y
    (-) (DInt x) (DFloat y) = DFloat $ fromIntegral x - y
    (-) (DFloat x) (DInt y) = DFloat $ x - fromIntegral y
    (-) (DInt x) (DDouble y) = DDouble $ fromIntegral x - y
    (-) (DDouble x) (DInt y) = DDouble $ x - fromIntegral y
    (-) (DFloat x) (DDouble y) = DDouble $ realToFrac x - y
    (-) (DDouble x) (DFloat y) = DDouble $ x - realToFrac y
    (-) (DList x) (DList y) = DList $ filter (`notElem` y) x
    (-) (DChar x) (DChar y) = DChar $ safeChr (ord x - ord y)
    (-) x _ = x

    (*) (DInt x) (DInt y) = DInt $ x * y
    (*) (DFloat x) (DFloat y) = DFloat $ x * y
    (*) (DDouble x) (DDouble y) = DDouble $ x * y
    (*) (DInt x) (DFloat y) = DFloat $ fromIntegral x * y
    (*) (DFloat x) (DInt y) = DFloat $ x * fromIntegral y
    (*) (DInt x) (DDouble y) = DDouble $ fromIntegral x * y
    (*) (DDouble x) (DInt y) = DDouble $ x * fromIntegral y
    (*) (DFloat x) (DDouble y) = DDouble $ realToFrac x * y
    (*) (DDouble x) (DFloat y) = DDouble $ x * realToFrac y
    (*) (DList x) (DFloat y) = DList $ map (* DFloat y) x
    (*) (DChar x) (DChar y) = DChar $ safeChr (ord x * ord y)
    (*) x _ = x

    fromInteger = DInt . fromInteger

    abs (DInt x) = DInt $ abs x
    abs (DFloat x) = DFloat $ abs x
    abs (DDouble x) = DDouble $ abs x
    abs x = x

    signum (DInt x) = DInt $ signum x
    signum (DFloat x) = DFloat $ signum x
    signum (DDouble x) = DDouble $ signum x
    signum _ = DInt 0

instance Fractional Data where
    (/) (DInt _) (DInt 0) = DInt 0
    (/) (DInt x) (DInt y) = DInt $ x `div` y
    (/) (DFloat x) (DFloat y) = DFloat $ if y == 0 then 0 else x / y
    (/) (DDouble x) (DDouble y) = DDouble $ if y == 0 then 0 else x / y
    (/) (DInt x) (DFloat y) = DFloat $ if y == 0 then 0 else fromIntegral x / y
    (/) (DFloat x) (DInt y) = DFloat $ if y == 0 then 0 else x / fromIntegral y
    (/) (DInt x) (DDouble y) = DDouble $ if y == 0 then 0 else fromIntegral x / y
    (/) (DDouble x) (DInt y) = DDouble $ if y == 0 then 0 else x / fromIntegral y
    (/) (DFloat x) (DDouble y) = DDouble $ if y == 0 then 0 else realToFrac x / y
    (/) (DDouble x) (DFloat y) = DDouble $ if y == 0 then 0 else x / realToFrac y
    (/) (DChar _) (DChar y) | ord y == 0 = DChar '\0'
    (/) (DChar x) (DChar y) = DChar $ safeChr (ord x `div` ord y)
    (/) x _ = x

    fromRational = DFloat . fromRational

instance Floating Data where
    (**) (DInt x) (DInt y)
        | y < 0 = if x == 0 then DFloat 0 else DFloat $ fromIntegral x ** fromIntegral y
        | otherwise = DInt $ x ^ y
    (**) (DFloat x) (DFloat y) = DFloat $ x ** y
    (**) (DDouble x) (DDouble y) = DDouble $ x ** y
    (**) (DInt x) (DFloat y) = DFloat $ fromIntegral x ** y
    (**) (DFloat x) (DInt y) = DFloat $ x ** fromIntegral y
    (**) (DInt x) (DDouble y) = DDouble $ fromIntegral x ** y
    (**) (DDouble x) (DInt y) = DDouble $ x ** fromIntegral y
    (**) (DFloat x) (DDouble y) = DDouble $ realToFrac x ** y
    (**) (DDouble x) (DFloat y) = DDouble $ x ** realToFrac y
    (**) x _ = x

    pi = DFloat pi
    exp (DFloat x) = DFloat (exp x)
    exp (DDouble x) = DDouble (exp x)
    exp (DInt x) = DFloat (exp (fromIntegral x))
    exp x = x

    log (DFloat x) = DFloat (log x)
    log (DDouble x) = DDouble (log x)
    log (DInt x) = DFloat (log (fromIntegral x))
    log x = x

    sin (DFloat x) = DFloat (sin x)
    sin (DDouble x) = DDouble (sin x)
    sin (DInt x) = DFloat (sin (fromIntegral x))
    sin x = x

    cos (DFloat x) = DFloat (cos x)
    cos (DDouble x) = DDouble (cos x)
    cos (DInt x) = DFloat (cos (fromIntegral x))
    cos x = x

    asin (DFloat x) = DFloat (asin x)
    asin (DDouble x) = DDouble (asin x)
    asin (DInt x) = DFloat (asin (fromIntegral x))
    asin x = x

    acos (DFloat x) = DFloat (acos x)
    acos (DDouble x) = DDouble (acos x)
    acos (DInt x) = DFloat (acos (fromIntegral x))
    acos x = x

    atan (DFloat x) = DFloat (atan x)
    atan (DDouble x) = DDouble (atan x)
    atan (DInt x) = DFloat (atan (fromIntegral x))
    atan x = x

    sinh (DFloat x) = DFloat (sinh x)
    sinh (DDouble x) = DDouble (sinh x)
    sinh (DInt x) = DFloat (sinh (fromIntegral x))
    sinh x = x

    cosh (DFloat x) = DFloat (cosh x)
    cosh (DDouble x) = DDouble (cosh x)
    cosh (DInt x) = DFloat (cosh (fromIntegral x))
    cosh x = x

    asinh (DFloat x) = DFloat (asinh x)
    asinh (DDouble x) = DDouble (asinh x)
    asinh (DInt x) = DFloat (asinh (fromIntegral x))
    asinh x = x

    acosh (DFloat x) = DFloat (acosh x)
    acosh (DDouble x) = DDouble (acosh x)
    acosh (DInt x) = DFloat (acosh (fromIntegral x))
    acosh x = x

    atanh (DFloat x) = DFloat (atanh x)
    atanh (DDouble x) = DDouble (atanh x)
    atanh (DInt x) = DFloat (atanh (fromIntegral x))
    atanh x = x

findLabelFuzzy :: String -> StateT VM IO (Maybe (String, Int))
findLabelFuzzy x = do
    lbls <- gets labels
    case lookup x lbls of
        Just n -> return (Just (x, n))
        Nothing ->
            case filter (\(y, _) -> x == takeWhile (/= '#') y) lbls of
                [] -> return Nothing
                ((n, l) : _) -> return (Just (n, l))

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

instance Storable Data where
    sizeOf _ = 4
    alignment _ = 4
    peek ptr = do
        types <- readIORef globalStructType
        sizesAndAlignments <- mapM sizeAndAlignmentOfCType types
        let typesAndOffsets = zip types (scanl (\(o, _) (s, a) -> (o + s, a)) (0, 0) sizesAndAlignments)
        b <- mapM mapData typesAndOffsets
        return $ DList b
      where
        mapData :: (Ptr CType, (Int, Int)) -> IO Data
        mapData (t, (o, _))
            | t == ffi_type_float = DFloat . realToFrac <$> (peekByteOff ptr o :: IO CFloat)
            | t == ffi_type_sint32 = DInt . fromIntegral <$> (peekByteOff ptr o :: IO CInt)
            | t == ffi_type_double = DDouble . realToFrac <$> (peekByteOff ptr o :: IO CDouble)
            | t == ffi_type_pointer = DCPtr <$> (peekByteOff ptr o :: IO WordPtr)
            | t == ffi_type_uchar = DChar . toEnum . fromEnum <$> (peekByteOff ptr o :: IO CUChar)
            | t == ffi_type_void = return DNone
            | otherwise = return DNone

    poke ptr (DInt x) = poke (castPtr ptr) x
    poke ptr (DFloat x) = poke (castPtr ptr) x
    poke ptr (DString x) = newCString x >>= poke (castPtr ptr)
    poke ptr (DChar x) = poke (castPtr ptr) x
    poke ptr (DMap x) = do
        let values = reverse $ Data.Map.elems (clearMap x)
        let sizes = map sizeOfC values
        mapM_ (\(i, v) -> pokeByteOff ptr (sum $ take i sizes) v) (zip [0 ..] values)
    poke _ _ = return ()

sizeOfC :: Data -> Int
sizeOfC (DChar _) = 1
sizeOfC (DDouble _) = 8
sizeOfC _ = 4

dataCType :: Data -> Ptr CType
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
dataCType _ = ffi_type_pointer

type ArgP = (Arg, Maybe (IO ()))

arg :: Data -> IO ArgP
arg (DInt x) = return (argInt32 (fromIntegral x), Nothing)
arg (DFloat x) = return (argCFloat (realToFrac x), Nothing)
arg (DString x) = return (argString x, Nothing)
arg (DBool x) = return (argCInt (if x then 1 else 0), Nothing)
arg (DChar x) = return (argCChar (fromIntegral $ Data.Char.ord x), Nothing)
arg (DList x@(DInt _ : _)) = do
    withArray [fromIntegral i :: CInt | DInt i <- x] $ \ptr ->
        return (argPtr ptr, Nothing)
arg (DList x@(DChar _ : _)) = do
    withArray [castCharToCChar c | DChar c <- x] $ \ptr ->
        return (argPtr ptr, Nothing)
arg (DDouble x) = return (argCDouble $ realToFrac x, Nothing)
arg d@(DMap x) = do
    let types = map dataCType (Data.Map.elems (clearMap x))
    (argT, _, freeTType) <- newStorableStructArgRet types
    return (argT d, Just freeTType)
arg _ = return (argInt32 0, Nothing)

class FfiRet a b where
    ret :: b -> RetType a

instance FfiRet Ghc.Int.Int32 Data where
    ret _ = retInt32

instance FfiRet CFloat Data where
    ret _ = retCFloat

instance FfiRet CDouble Data where
    ret _ = retCDouble

instance FfiRet String Data where
    ret _ = retString

instance FfiRet CChar Data where
    ret _ = retCChar

instance FfiRet (Ptr ()) Data where
    ret _ = retPtr retVoid

instance FfiRet () Data where
    ret _ = retVoid

stackPushA :: IntoData a => a -> StateT VM IO ()
stackPushA = stackPush . intoData
#endif

#ifdef POSIX_IO
safeFamily :: Int -> Family
safeFamily 1 = AF_UNIX
safeFamily 2 = AF_INET
safeFamily 10 = AF_INET6
safeFamily _ = AF_INET

safeSocketType :: Int -> SocketType
safeSocketType 1 = Stream
safeSocketType 2 = Datagram
safeSocketType 3 = Raw
safeSocketType _ = Stream
#endif

runInstruction :: Instruction -> StateT VM IO ()
#ifdef FFI
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
            (_, retType, _) <- liftIO (newStorableStructArgRet types :: IO (Data -> Arg, RetType Data, IO ()))
            (DList values) <- liftIO $ callFFI fun retType ffiArgs'
            let result = DMap $ Data.Map.fromList $ zip keys values
            stackPushA result
        _ -> setVMError $ "Invalid return type: " ++ show retT
    liftIO $ mapM_ (\case Just x -> x; Nothing -> return ()) frees
#else
runInstruction CallFFI{} = setVMError "FFI is not enabled in this build/not supported on this platform"
#endif

runInstruction (Push d) = stackPush d
runInstruction (PushPf name nArgs) = do
    lblRes <- findLabelFuzzy name
    let labelName = maybe name fst lblRes
    cStack <- gets callStack
    let currentLocals = case cStack of
            (frame : _) -> locals frame
            [] -> []
    args <- stackPopN nArgs
    stackPush $ DFuncRef labelName args currentLocals
runInstruction Pop = void stackPop
runInstruction Add =
    stackPopN 2 >>= \case
        [y, x] -> stackPush $ x + y
        _ -> setVMError "Add: expected 2 values on stack"
runInstruction Sub =
    stackPopN 2 >>= \case
        [y, x] -> stackPush $ x - y
        _ -> setVMError "Sub: expected 2 values on stack"
runInstruction Mul =
    stackPopN 2 >>= \case
        [y, x] -> stackPush $ x * y
        _ -> setVMError "Mul: expected 2 values on stack"
runInstruction Div =
    stackPopN 2 >>= \case
        [y, x] -> case (x, y) of
            (DInt _, DInt 0) -> setVMError "Division by zero"
            (DInt a, DInt b) -> stackPush $ DInt (a `div` b)
            (DChar _, DChar b) | ord b == 0 -> setVMError "Division by zero"
            (DChar a, DChar b) -> stackPush $ DChar $ safeChr (ord a `div` ord b)
            _ -> stackPush $ x / y
        _ -> setVMError "Div: expected 2 values on stack"
runInstruction Pow =
    stackPopN 2 >>= \case
        [y, x] -> stackPush $ x ** y
        _ -> setVMError "Pow: expected 2 values on stack"
runInstruction Abs = stackPop >>= stackPush . abs
runInstruction Mod =
    stackPopN 2 >>= \case
        [y, x] -> case (x, y) of
            (DInt a, DInt b) -> stackPush $ DInt $ if b == 0 then 0 else a `mod` b
            (DInt a, DFloat b) -> stackPush $ DFloat $ if b == 0 then 0 else fromIntegral a - b * fromIntegral (floor (fromIntegral a / b) :: Int)
            (DFloat a, DFloat b) -> stackPush $ DFloat $ if b == 0 then 0 else a - b * fromIntegral (floor (a / b) :: Int)
            _ -> stackPush $ DInt 0
        _ -> setVMError "Mod: expected 2 values on stack"
runInstruction (Builtin Print) = do
    vm <- get
    case ioMode vm of
        HostDirect -> stackPop >>= liftIO . putStr . show
        VMBuffer -> stackPop >>= \x -> modify $ \v -> v{ioBuffer = (ioBuffer vm){output = output (ioBuffer vm) ++ show x}}
runInstruction (Builtin GetLine) = do
    vm <- get
    case ioMode vm of
        HostDirect -> liftIO getLine >>= stackPush . DString
        VMBuffer -> do
            let inp = (ioBuffer vm).input
            let (line, rest) = break (== '\n') inp
            let nextInput = case rest of ('\n' : r) -> r; _ -> rest
            modify $ \v -> v{ioBuffer = (ioBuffer vm){input = nextInput}}
            stackPush $ DString line
runInstruction (Builtin GetChar) = do
    vm <- get
    case ioMode vm of
        HostDirect -> liftIO getChar >>= stackPush . DChar
        VMBuffer -> do
            let inp = (ioBuffer vm).input
            case inp of
                (char : rest) -> do
                    modify $ \v -> v{ioBuffer = (ioBuffer vm){input = rest}}
                    stackPush $ DChar char
                [] -> stackPush $ DChar '\0'
runInstruction (Builtin Random) = do
    num <- liftIO (randomIO :: IO Float)
    stackPush $ DFloat num

#ifdef POSIX_IO
runInstruction (Builtin OpenFile) = do
    popped <- stackPopN 2
    case popped of
        [mode, path] -> case (path, mode) of
            (DString p, DString m) -> do
                let iomode = case m of
                        "r" -> SIO.ReadMode
                        "w" -> SIO.WriteMode
                        "a" -> SIO.AppendMode
                        "r+" -> SIO.ReadWriteMode
                        "w+" -> SIO.ReadWriteMode
                        "a+" -> SIO.ReadWriteMode
                        _ -> SIO.ReadMode
                result <- liftIO $ tryIO $ SIO.openFile p iomode
                case result of
                    Right handle -> do
                        vm <- get
                        let handleId = nextHandleId vm
                        modify $ \v -> v{fileHandles = Map.insert handleId handle (fileHandles v), nextHandleId = handleId + 1}
                        stackPush $ DInt handleId
                    Left _ -> stackPush $ DInt (-1)
            _ -> stackPush $ DInt (-1)
        _ -> setVMError "OpenFile: expected String path and String mode"
runInstruction (Builtin ReadFile) = do
    popped <- stackPopN 2
    case popped of
        [size, fd] -> case (fd, size) of
            (DInt f, DInt s) -> do
                vm <- get
                case Map.lookup f (fileHandles vm) of
                    Just handle -> do
                        result <- liftIO $ tryIO $ do
                            let readChars :: Int -> [Char] -> IO String
                                readChars n acc
                                    | n <= 0 = return $ reverse acc
                                    | otherwise = do
                                        eof <- SIO.hIsEOF handle
                                        if eof
                                            then return $ reverse acc
                                            else do
                                                char <- SIO.hGetChar handle
                                                readChars (n - 1) (char : acc)
                            readChars (fromIntegral s) []
                        case result of
                            Right contents -> stackPush $ DString contents
                            Left _ -> stackPush $ DString ""
                    Nothing -> stackPush $ DString ""
            _ -> stackPush $ DString ""
        _ -> setVMError "ReadFile: expected Int fd and Int size"
runInstruction (Builtin WriteFile) = do
    popped <- stackPopN 2
    case popped of
        [content, fd] -> case (fd, content) of
            (DInt f, DString c) -> do
                vm <- get
                case Map.lookup f (fileHandles vm) of
                    Just handle -> do
                        result <- liftIO $ tryIO $ SIO.hPutStr handle c
                        case result of
                            Right _ -> stackPush $ DInt $ length c
                            Left _ -> stackPush $ DInt (-1)
                    Nothing -> stackPush $ DInt (-1)
            _ -> stackPush $ DInt (-1)
        _ -> setVMError "WriteFile: expected Int fd and String content"
runInstruction (Builtin CloseFile) = do
    fd <- stackPop
    case fd of
        DInt f -> do
            vm <- get
            case Map.lookup f (fileHandles vm) of
                Just handle -> do
                    _ <- liftIO $ tryIO $ SIO.hClose handle
                    modify $ \v -> v{fileHandles = Map.delete f (fileHandles v)}
                Nothing -> return ()
        _ -> return ()
runInstruction (Builtin Socket) = do
    popped <- stackPopN 3
    case popped of
        [protocol, socktype, domain] -> case (domain, socktype, protocol) of
            (DInt d, DInt t, DInt p) -> do
                result <- liftIO $ tryIO $ NS.socket (safeFamily d) (safeSocketType t) (fromIntegral p :: ProtocolNumber)
                case result of
                    Right sock -> do
                        vm <- get
                        let sockId = nextSocketId vm
                        modify $ \v -> v{sockets = Map.insert sockId sock (sockets v), nextSocketId = sockId + 1}
                        stackPush $ DInt sockId
                    Left _ -> stackPush $ DInt (-1)
            _ -> stackPush $ DInt (-1)
        _ -> setVMError "Socket: expected Int domain, Int type, Int protocol"
runInstruction (Builtin Bind) = do
    popped <- stackPopN 3
    case popped of
        [port, host, fd] -> case (fd, host, port) of
            (DInt f, DString h, DInt p) -> do
                vm <- get
                case Map.lookup f (sockets vm) of
                    Just sock -> do
                        let host' = if h == "" || h == "0.0.0.0" then Nothing else Just h
                        let hints = defaultHints{addrFlags = [AI_PASSIVE], addrFamily = AF_INET}
                        addrsResult <- liftIO $ tryIO $ getAddrInfo (Just hints) host' (Just $ show p)
                        case addrsResult of
                            Right (addr : _) -> do
                                result <- liftIO $ tryIO $ NS.bind sock (addrAddress addr)
                                case result of
                                    Right _ -> return ()
                                    Left _ -> setVMError "Bind failed"
                            _ -> setVMError "No address found for bind"
                    Nothing -> setVMError "Socket not found"
            _ -> setVMError "Bind: expected Int fd, String host, Int port"
        _ -> setVMError "Bind: expected 3 values on stack"
runInstruction (Builtin Listen) = do
    popped <- stackPopN 2
    case popped of
        [backlog, fd] -> case (fd, backlog) of
            (DInt f, DInt b) -> do
                vm <- get
                case Map.lookup f (sockets vm) of
                    Just sock -> do
                        result <- liftIO $ tryIO $ NS.listen sock (fromIntegral b)
                        case result of
                            Right _ -> return ()
                            Left _ -> setVMError "Listen failed"
                    Nothing -> setVMError "Socket not found"
            _ -> setVMError "Listen: expected Int fd and Int backlog"
        _ -> setVMError "Listen: expected 2 values on stack"
runInstruction (Builtin Accept) = do
    fd <- stackPop
    case fd of
        DInt f -> do
            vm <- get
            case Map.lookup f (sockets vm) of
                Just sock -> do
                    result <- liftIO $ tryIO $ NS.accept sock
                    case result of
                        Right (clientSock, _) -> do
                            let sockId = nextSocketId vm
                            modify $ \v -> v{sockets = Map.insert sockId clientSock (sockets v), nextSocketId = sockId + 1}
                            stackPush $ DInt sockId
                        Left _ -> stackPush $ DInt (-1)
                Nothing -> stackPush $ DInt (-1)
        _ -> stackPush $ DInt (-1)
runInstruction (Builtin Connect) = do
    popped <- stackPopN 3
    case popped of
        [port, host, fd] -> case (fd, host, port) of
            (DInt f, DString h, DInt p) -> do
                vm <- get
                case Map.lookup f (sockets vm) of
                    Just sock -> do
                        addrsResult <- liftIO $ tryIO $ getAddrInfo Nothing (Just h) (Just $ show p)
                        case addrsResult of
                            Right (addr : _) -> do
                                result <- liftIO $ tryIO $ NS.connect sock (addrAddress addr)
                                case result of
                                    Right _ -> return ()
                                    Left _ -> setVMError "Connect failed"
                            _ -> setVMError "No address found for connect"
                    Nothing -> setVMError "Socket not found"
            _ -> setVMError "Connect: expected Int fd, String host, Int port"
        _ -> setVMError "Connect: expected 3 values on stack"
runInstruction (Builtin Send) = do
    popped <- stackPopN 3
    case popped of
        [flags, data', fd] -> case (fd, data', flags) of
            (DInt f, DString d, DInt _) -> do
                vm <- get
                case Map.lookup f (sockets vm) of
                    Just sock -> do
                        result <- liftIO $ tryIO $ NSB.send sock (pack d)
                        case result of
                            Right bytes -> stackPush $ DInt $ fromIntegral bytes
                            Left _ -> stackPush $ DInt (-1)
                    Nothing -> stackPush $ DInt (-1)
            _ -> stackPush $ DInt (-1)
        _ -> setVMError "Send: expected Int fd, String data, Int flags"
runInstruction (Builtin Recv) = do
    popped <- stackPopN 3
    case popped of
        [flags, size, fd] -> case (fd, size, flags) of
            (DInt f, DInt s, DInt _) -> do
                vm <- get
                case Map.lookup f (sockets vm) of
                    Just sock -> do
                        result <- liftIO $ tryIO $ NSB.recv sock (fromIntegral s)
                        case result of
                            Right bytes -> stackPush $ DString $ unpack bytes
                            Left _ -> stackPush $ DString ""
                    Nothing -> stackPush $ DString ""
            _ -> stackPush $ DString ""
        _ -> setVMError "Recv: expected Int fd, Int size, Int flags"
runInstruction (Builtin CloseSocket) = do
    fd <- stackPop
    case fd of
        DInt f -> do
            vm <- get
            case Map.lookup f (sockets vm) of
                Just sock -> do
                    _ <- liftIO $ tryIO $ NS.close sock
                    modify $ \v -> v{sockets = Map.delete f (sockets v)}
                Nothing -> return ()
        _ -> return ()
#endif

runInstruction Exit = do
    modify $ \vm -> vm{running = False}
    actuallyExit <- gets shouldExit
    when actuallyExit $ do
        st <- gets stack
        case st of
            (DInt 0 : _) -> liftIO exitSuccess
            (DInt code : _) -> liftIO $ exitWith (ExitFailure code)
            _ -> liftIO exitSuccess
    unless actuallyExit $ void stackPop
runInstruction (Call x) = do
    p <- gets program
    currPc <- gets pc
    let nextInstruction = p V.!? (currPc + 1)
    case nextInstruction of
        Just Ret -> runInstruction (Jmp x)
        _ -> do
            lblRes <- findLabelFuzzy x
            case lblRes of
                Just (_, target) ->
                    modify $ \vm' -> vm'{pc = target, callStack = StackFrame{returnAddress = currPc, locals = []} : callStack vm'}
                Nothing -> setVMError $ "Label not found: " ++ x
runInstruction (CallLocal x) = do
    p <- gets program
    currPc <- gets pc
    let nextInstruction = p V.!? (currPc + 1)
    case nextInstruction of
        Just Ret -> runInstruction (Jmp x)
        _ -> do
            vm <- get
            case callStack vm of
                (frame : _) -> do
                    lblRes <- findLabelFuzzy x
                    case lblRes of
                        Just (_, target) ->
                            modify $ \v -> v{pc = target, callStack = StackFrame{returnAddress = currPc, locals = frame.locals} : callStack v}
                        Nothing -> setVMError $ "Label not found: " ++ x
                [] -> do
                    lblRes <- findLabelFuzzy x
                    case lblRes of
                        Just (_, target) ->
                            modify $ \v -> v{pc = target, callStack = StackFrame{returnAddress = currPc, locals = []} : callStack v}
                        Nothing -> setVMError $ "Label not found: " ++ x
runInstruction CallS =
    stackPop >>= \case
        DFuncRef x args fnLocals -> do
            stackPushN args
            runInstruction $ Call x
            forM_ fnLocals $ \(name, value) -> do
                stackPush value
                runInstruction (LStore name)
        d -> setVMError $ "Cannot call " ++ show d
runInstruction (Jmp x) = do
    lblRes <- findLabelFuzzy x
    case lblRes of
        Just (_, target) -> modify $ \vm -> vm{pc = target}
        Nothing -> setVMError $ "Label not found: " ++ x
runInstruction (Jt x) = do
    d <- stackPop
    when (isTruthy d) $ runInstruction (Jmp x)
runInstruction (Jf x) = do
    d <- stackPop
    unless (isTruthy d) $ runInstruction (Jmp x)
runInstruction Ret = do
    vm <- get
    case callStack vm of
        (frame : rest) -> modify $ \v -> v{pc = returnAddress frame, callStack = rest}
        [] -> modify $ \v -> v{running = False}
runInstruction Eq =
    stackPopN 2 >>= \case
        [y, x] -> do
            let result = case (x, y) of
                    (DString s, DList l) -> case extractStringFromList l of
                        Just lstr -> s == lstr
                        Nothing -> False
                    (DList l, DString s) -> case extractStringFromList l of
                        Just lstr -> lstr == s
                        Nothing -> False
                    _ -> x == y
            stackPush $ DBool result
        _ -> setVMError "Eq: expected 2 values on stack"
  where
    extractStringFromList l =
        let chars = [c | DChar c <- l]
         in if length chars == length l then Just chars else Nothing
runInstruction Neq =
    stackPopN 2 >>= \case
        [y, x] -> stackPush $ DBool $ x /= y
        _ -> setVMError "Neq: expected 2 values on stack"
runInstruction Lt =
    stackPopN 2 >>= \case
        [y, x] -> stackPush $ DBool $ x < y
        _ -> setVMError "Lt: expected 2 values on stack"
runInstruction Gt =
    stackPopN 2 >>= \case
        [y, x] -> stackPush $ DBool $ x > y
        _ -> setVMError "Gt: expected 2 values on stack"
runInstruction Not = do
    d <- stackPop
    stackPush $ DBool $ not (isTruthy d)
runInstruction And =
    stackPopN 2 >>= \case
        [y, x] -> stackPush $ DBool $ isTruthy x && isTruthy y
        _ -> setVMError "And: expected 2 values on stack"
runInstruction Or =
    stackPopN 2 >>= \case
        [y, x] -> stackPush $ DBool $ isTruthy x || isTruthy y
        _ -> setVMError "Or: expected 2 values on stack"
runInstruction (Label _) = return ()
runInstruction Swp =
    stackPopN 2 >>= \case
        [x, y] -> stackPushN [y, x]
        _ -> setVMError "Swp: expected 2 values on stack"
runInstruction Dup = do
    sl <- stackLen
    when (sl > 0) $ stackPeek >>= stackPush
runInstruction (Load x) = do
    mem <- gets memory
    stackPush $ loadMemory x mem
runInstruction (Store x) = do
    d <- stackPop
    modify $ \vm -> vm{memory = storeMemory x d (memory vm)}
runInstruction (LStore name) = do
    d <- stackPop
    modify $ \vm -> case callStack vm of
        (frame : rest) ->
            let updatedLocals = (name, d) : filter ((/= name) . fst) (locals frame)
             in vm{callStack = frame{locals = updatedLocals} : rest}
        [] ->
            vm{callStack = [StackFrame{returnAddress = pc vm, locals = [(name, d)]}]}
runInstruction (LLoad name) = do
    vm <- get
    case callStack vm of
        (frame : _) ->
            case lookup name (locals frame) of
                Just x -> stackPush x
                Nothing -> stackPush DNone
        [] -> stackPush DNone
runInstruction (Concat 0) = stackPush $ DList []
runInstruction (Concat n) = do
    elems <- stackPopN n
    let combined = concatMap (\case DList xs -> xs; DString s -> map DChar s; x -> [x]) elems
    stackPush $ DList combined
runInstruction Index =
    stackPopN 2 >>= \case
        [DInt idx, DList list] ->
            if idx < 0 || idx >= length list
                then setVMError $ "Index out of bounds: " ++ show idx ++ " (length " ++ show (length list) ++ ")"
                else stackPush (list !! idx)
        [DInt idx, DString str] ->
            if idx < 0 || idx >= length str
                then setVMError $ "Index out of bounds: " ++ show idx ++ " (length " ++ show (length str) ++ ")"
                else stackPush (DChar (str !! idx))
        _ -> setVMError "Invalid types for index"
runInstruction Slice = do
    end <- stackPop
    start <- stackPop
    target <- stackPop
    case target of
        DString s -> do
            let res = sliceHelper start end s
            stackPush $ DString res
        DList l -> do
            let res = sliceHelper start end l
            stackPush $ DList res
        _ -> stackPush DNone
  where
    sliceHelper sVal eVal xs =
        let len = length xs
            s = case sVal of
                DInt start' -> if start' >= 0 then min start' len else max 0 (len + start')
                _ -> 0
            e = case eVal of
                DInt end' -> if end' >= 0 then min end' len else max 0 (len + end')
                _ -> len
         in if s >= e then [] else take (e - s) (drop s xs)
runInstruction Length = do
    st <- gets stack
    if null st
        then stackPush $ DInt (-1)
        else do
            d <- stackPop
            stackPush $ DInt $ case d of
                DList l -> length l
                DString s -> length s
                DMap m -> Map.size m
                _ -> 0
runInstruction Cast = do
    to <- stackPop
    val <- stackPop
    stackPush $ case (val, to) of
        (DInt x, DInt _) -> DInt x
        (DInt x, DFloat _) -> DFloat (fromIntegral x)
        (DInt x, DDouble _) -> DDouble (fromIntegral x)
        (DInt x, DString _) -> DString (show x)
        (DInt x, DBool _) -> DBool (x /= 0)
        (DInt x, DList _) -> DList [DInt x]
        (DInt x, DChar _) -> DChar (safeChr x)
        (DInt x, DCPtr _) -> DCPtr (fromIntegral x)
        (DFloat x, DInt _) -> DInt (round x)
        (DFloat x, DFloat _) -> DFloat x
        (DFloat x, DDouble _) -> DDouble (realToFrac x)
        (DFloat x, DString _) -> DString (show x)
        (DFloat x, DBool _) -> DBool (x /= 0)
        (DFloat x, DList _) -> DList [DFloat x]
        (DFloat x, DChar _) -> DChar (safeChr (round x))
        (DDouble x, DInt _) -> DInt (round x)
        (DDouble x, DFloat _) -> DFloat (realToFrac x)
        (DDouble x, DDouble _) -> DDouble x
        (DDouble x, DString _) -> DString (show x)
        (DDouble x, DBool _) -> DBool (x /= 0)
        (DDouble x, DList _) -> DList [DDouble x]
        (DDouble x, DChar _) -> DChar (safeChr (round x))
        (DString x, DInt _) -> DInt (fromMaybe 0 (readMaybe x))
        (DString x, DFloat _) -> DFloat (fromMaybe 0 (readMaybe x))
        (DString x, DDouble _) -> DDouble (fromMaybe 0 (readMaybe x))
        (DString x, DString _) -> DString x
        (DString x, DBool _) -> DBool (not (null x))
        (DString x, DList _) -> DList (map DChar x)
        (DString x, DChar _) -> case x of (c : _) -> DChar c; [] -> DChar '\0'
        (DChar x, DInt _) -> DInt (ord x)
        (DChar x, DFloat _) -> DFloat (fromIntegral (ord x))
        (DChar x, DDouble _) -> DDouble (fromIntegral (ord x))
        (DChar x, DString _) -> DString [x]
        (DChar x, DBool _) -> DBool (ord x /= 0)
        (DChar x, DChar _) -> DChar x
        (DBool x, DInt _) -> DInt (if x then 1 else 0)
        (DBool x, DFloat _) -> DFloat (if x then 1 else 0)
        (DBool x, DDouble _) -> DDouble (if x then 1 else 0)
        (DBool x, DString _) -> DString (show x)
        (DBool x, DBool _) -> DBool x
        (DList [], DList _) -> DList []
        (DList x, DList _) -> DList x
        (DList x, DString _) ->
            let chars = [c | DChar c <- x]
             in if length chars == length x then DString chars else DString (show x)
        (x, _) | x == to || to == DNone -> x
        (x, _) -> x
runInstruction (Meta _) = return ()
runInstruction (Comment _) = return ()
runInstruction (Access x) =
    stackPop >>= \case
        DMap m -> stackPush $ fromMaybe DNone (Map.lookup x m)
        _ -> stackPush DNone
runInstruction AAccess =
    stackPopN 2 >>= \case
        [DString x, DMap m] -> stackPush $ fromMaybe DNone (Map.lookup x m)
        _ -> stackPush DNone
runInstruction Keys =
    stackPop >>= \case
        DMap m -> stackPush $ DList $ map DString (Map.keys m)
        _ -> stackPush $ DList []
runInstruction Update =
    stackPopN 3 >>= \case
        [value, DString key, DMap m] -> stackPush $ DMap $ Map.insert key value m
        [value, DString key, _] -> stackPush $ DMap $ Map.singleton key value
        _ -> return ()
runInstruction (PackMap n) = do
    elems <- stackPopN n
    let pairs = concatMap (\case [DString k, v] -> [(k, v)]; [k, v] -> [(show k, v)]; _ -> []) $ chunksOf 2 elems
    stackPush $ DMap $ Map.fromList pairs
runInstruction TypeOf =
    stackPop >>= \case
        DInt _ -> stackPush $ DString "Int"
        DFloat _ -> stackPush $ DString "Float"
        DDouble _ -> stackPush $ DString "Double"
        DString _ -> stackPush $ DString "String"
        DBool _ -> stackPush $ DString "Bool"
        DList _ -> stackPush $ DString "List"
        DNone -> stackPush $ DString "None"
        DChar _ -> stackPush $ DString "Char"
        DFuncRef{} -> stackPush $ DString "FuncRef"
        DMap m -> case Map.lookup "__name" m of
            Just (DString name) -> stackPush $ DString name
            _ -> stackPush $ DString "Map"
        DTypeQuery s -> stackPush $ DString $ "TypeQuery{" ++ s ++ "}"
        DCPtr _ -> stackPush $ DString "CPtr"
runInstruction TypeEq = do
    popped <- stackPopN 2
    case popped of
        [y, x] -> stackPush $ DBool $ case (x, y) of
            (DInt _, DInt _) -> True
            (DFloat _, DFloat _) -> True
            (DDouble _, DDouble _) -> True
            (DString _, DString _) -> True
            (DBool _, DBool _) -> True
            (DList _, DList _) -> True
            (DNone, DNone) -> True
            (DChar _, DChar _) -> True
            (DFuncRef{}, DFuncRef{}) -> True
            (DMap a, DMap b) ->
                let aName = Map.lookup "__name" a
                    bName = Map.lookup "__name" b
                 in case (aName, bName) of
                        (Just an, Just bn) -> an == bn
                        _ -> True
            (DMap m, DTypeQuery tq) ->
                let name = Map.lookup "__name" m
                    traits = Map.lookup "__traits" m
                 in case (name, traits) of
                        (Just (DString n), Just (DList ts)) -> n == tq || DString tq `elem` ts
                        (Just (DString n), _) -> n == tq
                        _ -> False
            (DInt _, DTypeQuery s) -> s == "Int"
            (DFloat _, DTypeQuery s) -> s == "Float"
            (DDouble _, DTypeQuery s) -> s == "Double"
            (DString _, DTypeQuery s) -> s == "String"
            (DBool _, DTypeQuery s) -> s == "Bool"
            (DList _, DTypeQuery s) -> s == "List"
            (DNone, DTypeQuery s) -> s == "None"
            (DChar _, DTypeQuery s) -> s == "Char"
            (DFuncRef{}, DTypeQuery s) -> s == "FuncRef"
            _ -> False
        _ -> stackPush $ DBool False
runInstruction (PackList n) = stackPopN n >>= stackPush . DList . reverse
runInstruction UnpackList =
    stackPop >>= \case
        DList l -> stackPushN (reverse l)
        DString s -> stackPushN (map DChar (reverse s))
        x -> stackPush x
runInstruction (Mov n dat) =
    modify $ \vm -> vm{memory = storeMemory n dat (memory vm)}
runInstruction StoreSideStack = modify $ \vm -> vm{sideStack = stack vm}
runInstruction LoadSideStack = modify $ \vm -> vm{stack = if null $ sideStack vm then stack vm else sideStack vm}
runInstruction ClearSideStack = modify $ \vm -> vm{sideStack = []}
runInstruction (ListAdd n) = do
    elems <- stackPopN n
    let combined = concatMap (\case DList xs -> xs; DString s -> map DChar s; el -> [el]) elems
    let isAllChar = not (null combined) && all (\case DChar _ -> True; _ -> False) combined
    if isAllChar
        then stackPush $ DString [c | DChar c <- combined]
        else stackPush $ DList combined

printAssembly :: Program -> Bool -> String
printAssembly prog showLineNumbers =
    if not showLineNumbers
        then concatMap printAssembly' prog
        else concatMap (\(n, i) -> "\ESC[1;30m" ++ show n ++ "\ESC[0m       " ++ printAssembly' i) $ zip [0 :: Integer ..] (V.toList prog)
  where
    printAssembly' :: Instruction -> String
    printAssembly' (Label name) = "\ESC[0;32m" <> name <> "\ESC[0m " ++ ":\n"
    printAssembly' (Comment text) = "\t;\ESC[0;33m " <> text <> " \ESC[0m " ++ "\n"
    printAssembly' (PushPf x y) = asmLine ("push_pf " ++ x ++ " " ++ show y)
    printAssembly' (Push x) = asmLine $ "push " ++ show x
    printAssembly' (Call name) = asmLine ("call " <> name)
    printAssembly' (Jmp name) = asmLine ("jmp " <> name)
    printAssembly' (Jt name) = asmLine ("jt " <> name)
    printAssembly' (Jf name) = asmLine ("jf " <> name)
    printAssembly' (LLoad name) = asmLine ("lload " <> name)
    printAssembly' a = asmLine $ show a

    asmLine :: String -> String
    asmLine a = "\t" ++ Data.Text.unpack a'''' ++ "\n"
      where
        a' = Data.Text.pack a
        a'' = Data.Text.replace "\n" "\\n" a'
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
