{-# LANGUAGE CPP #-}

module Ffi where

#ifdef FFI
import Foreign.Ptr (FunPtr)

#if defined(mingw32_HOST_OS)
import Control.Exception (bracket)
import Foreign.Ptr (castPtrToFunPtr)
import System.Win32.DLL
import System.Win32.Types (HMODULE)
#else
import System.Posix.DynamicLinker
#endif

-- | The dynamic library containing the C runtime.
crtPath :: FilePath
#if defined(mingw32_HOST_OS)
crtPath = "msvcrt.dll"
#elif defined(darwin_HOST_OS)
crtPath = "libc.dylib"
#else
-- On other OSes, it suffices to use the name of the current
-- executable, as the dyanmic loader can chase dependencies until it
-- finds the C runtime.
crtPath = ""
#endif

-- | The file extension used for dynamic libraries.
dllext :: String
#if defined(mingw32_HOST_OS)
dllext = "dll"
#elif defined(darwin_HOST_OS)
dllext = "dylib"
#else
dllext = "so"
#endif

-- | The Haskell representation of a loaded dynamic library.
#if defined(mingw32_HOST_OS)
type DynLib = HMODULE
#else
type DynLib = DL
#endif

-- | Return the address of a function symbol contained in a dynamic library.
dynLibSym :: DynLib -> String -> IO (FunPtr a)
#if defined(mingw32_HOST_OS)
dynLibSym source symbol = do
  addr <- getProcAddress source symbol
  return $ castPtrToFunPtr addr
#else
dynLibSym = dlsym
#endif

-- | Load a dynamic library, perform some action on it, and then free it.
withDynLib :: FilePath -> (DynLib -> IO a) -> IO a
#if defined(mingw32_HOST_OS)
withDynLib file f = bracket (loadLibrary file) freeLibrary f
#else
withDynLib file = withDL file [RTLD_NOW]
#endif

-- | Open a dynamic library.
dynLibOpen :: FilePath -> IO DynLib
#if defined(mingw32_HOST_OS)
dynLibOpen = loadLibrary
#else
dynLibOpen x = dlopen x [RTLD_NOW]
#endif
#endif
