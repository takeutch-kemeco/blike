module Blike where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

foreign import ccall safe "bl_putc"
  bl_putc :: Char -> IO ()

foreign import ccall safe "bl_puts"
  __bl_puts :: CString -> IO ()

bl_puts :: String -> IO ()
bl_puts s = do
  p <- newCAString s
  __bl_puts p
  free p

foreign import ccall safe "bl_puts1"
  __bl_puts1 :: CString -> IO ()

bl_puts1 :: String -> IO ()
bl_puts1 s = do
  p <- newCAString s
  __bl_puts1 p
  free p

-- foreign import ccall safe "bl_printf"

-- foreign import ccall safe "bl_scanf"

-- foreign import ccall safe "bl_malloc"

foreign import ccall safe "bl_rand"
  bl_rand :: IO Int

foreign import ccall safe "bl_srand"
  bl_srand :: Int -> IO ()

foreign import ccall safe "bl_gets"
  __bl_gets :: CString -> IO ()

bl_gets :: IO String
bl_gets = alloca $ \p -> do
  __bl_gets p
  s <- peekCAString p
  return s

foreign import ccall safe "bl_openWin"
  bl_openWin :: Int -> Int -> IO ()

foreign import ccall safe "bl_setCol"
  bl_setCol :: Int -> IO ()

foreign import ccall safe "bl_setBCol"
  bl_setBCol :: Int -> IO ()

foreign import ccall safe "bl_rgb"
  bl_rgb :: Int -> Int -> Int -> IO Int

foreign import ccall safe "bl_iCol"
  bl_iCol :: Int -> IO Int

foreign import ccall safe "bl_flshWin"
  bl_flshWin :: Int -> Int -> Int -> Int -> IO Int

-- foreign import ccall safe "bl_getGrpB"

foreign import ccall safe "bl_setPix"
  bl_setPix :: Int -> Int -> Int -> IO ()

foreign import ccall safe "bl_fillRect"
  bl_fillRect :: Int -> Int -> Int -> Int -> IO ()

foreign import ccall safe "bl_drawRect"
  bl_drawRect :: Int -> Int -> Int -> Int -> IO ()

foreign import ccall safe "bl_drawLine"
  bl_drawLine :: Int -> Int -> Int -> Int -> IO ()

foreign import ccall safe "bl_rnd"
  bl_rnd :: Int -> IO Int

foreign import ccall safe "bl_wait"
  bl_wait :: Int -> IO ()

foreign import ccall safe "bl_color"
  bl_color :: Int -> Int -> IO ()

foreign import ccall safe "bl_locate"
  bl_locate :: Int -> Int -> IO ()

foreign import ccall safe "bl_getPix"
  bl_getPix :: Int -> Int -> IO Int

foreign import ccall safe "bl_waitNF"
  bl_waitNF :: Int -> IO ()

foreign import ccall safe "bl_inkey"
  bl_inkey :: Int -> IO Int

foreign import ccall safe "bl_inkey1"
  bl_inkey1 :: IO Int

foreign import ccall safe "bl_cls"
  bl_cls :: IO ()

foreign import ccall safe "bl_inptInt"
  __bl_inptInt :: CString -> IO Int

bl_inptInt :: String -> IO Int
bl_inptInt s = do
  p <- newCAString s
  n <- __bl_inptInt p
  free p
  return n

foreign import ccall safe "bl_inptFlot"
  __bl_inptFlot :: CString -> IO Double

bl_inptFlot :: String -> IO Double
bl_inptFlot s = do
  p <- newCAString s
  n <- __bl_inptFlot p
  free p
  return n

foreign import ccall safe "bl_setMode"
  bl_setMode :: Int -> IO ()

foreign import ccall safe "bl_fillOval"
  bl_fillOval :: Int -> Int -> Int -> Int -> IO ()

foreign import ccall safe "bl_drawStr"
  __bl_drawStr :: Int -> Int -> Int -> Int -> CString -> IO ()

bl_drawStr :: Int -> Int -> Int -> Int -> String -> IO ()
bl_drawStr x0 y0 rx ry s = do
  p <- newCAString s
  __bl_drawStr x0 y0 rx ry p
  free p

foreign import ccall safe "bl_openVWin"
  bl_openVWin :: Int -> Int -> Int -> IO ()

foreign import ccall safe "bl_slctWin"
  bl_slctWin :: Int -> IO ()

foreign import ccall safe "bl_copyRct0"
  bl_copyRct0 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()

foreign import ccall safe "bl_copyRct1"
  bl_copyRct1 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()

foreign import ccall safe "bl_drawPtrn_d"
  __bl_drawPtrn_d :: Int -> Int -> Int -> Int -> CString -> CString -> IO ()

bl_drawPtrn_d :: Int -> Int -> Int -> Int -> String -> String -> IO ()
bl_drawPtrn_d sx sy x0 y0 c p = do
  c' <- newCAString c
  p' <- newCAString p
  __bl_drawPtrn_d sx sy x0 y0 c' p'
  free c'
  free p'

foreign import ccall safe "bl_drawPtrn_r"
  __bl_drawPtrn_r :: Int -> Int -> Int -> Int -> CString -> CString -> IO ()

bl_drawPtrn_r :: Int -> Int -> Int -> Int -> String -> String -> IO ()
bl_drawPtrn_r sx sy x0 y0 c p = do
  c' <- newCAString c
  p' <- newCAString p
  __bl_drawPtrn_r sx sy x0 y0 c' p'
  free c'
  free p'

_KEY_ENTER = 0x0a :: Int
_KEY_ESC = 0x1b :: Int
_KEY_BACKSPACE = 8 :: Int
_KEY_TAB = 9 :: Int
_KEY_LEFT = 331 :: Int
_KEY_RIGHT = 333 :: Int
_KEY_UP = 328 :: Int
_KEY_DOWN = 336 :: Int
_KEY_INS = 338 :: Int
_KEY_DEL = 339 :: Int

_BL_PSET = 0x00000004 :: Int
_BL_PAND = 0x00000005 :: Int
_BL_POR = 0x00000006 :: Int
_BL_PXOR = 0x00000007 :: Int
_BL_FULLHEIGHT = 0x00000010 :: Int
_BL_HALFHEIGHT = 0x00000018 :: Int
_BL_DBGFLSH = 0x00000040 :: Int
_BL_RLSFLSH = 0x00000060 :: Int
_BL_DEBUG = _BL_DBGFLSH
_BL_RELEASE = _BL_RLSFLSH

_BL_WAITKEYF = 0x00000001 :: Int
_BL_WAITKEYNF = 0x00000002 :: Int
_BL_WAITKEY = 0x00000003 :: Int
_BL_GETKEY = 0x00000004 :: Int
_BL_CLEARREP = 0x00000008 :: Int
_BL_DELFFF = 0x00000010 :: Int

_BL_KEYMODE = 0x00000000 :: Int
