{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import BGFX
import Data.Bits
import Foreign.Ptr
import qualified SDL
import Unsafe.Coerce

foreign import ccall unsafe
  "bgfx_sdl_set_window" bgfxSdlSetWindow :: Ptr a -> IO ()

main :: IO ()
main = do
  let width = 1280
  let height = 720
  let debug = BGFX_DEBUG_TEXT
  let reset = BGFX_RESET_VSYNC
  SDL.initializeAll
  w <- SDL.createWindow "bgfx with SDL2" SDL.defaultWindow
  bgfxSdlSetWindow (unsafeCoerce w :: Ptr ())
  bgfxRenderFrame
  bgfxInit BGFX_RENDERER_TYPE_COUNT BGFX_PCI_ID_NONE 0 nullPtr nullPtr
  putStrLn "A"
  bgfxReset width height reset
  bgfxSetDebug debug
  bgfxSetViewClear 0 (BGFX_CLEAR_COLOR .|. BGFX_CLEAR_DEPTH) 0x303030ff 1.0 0
  let loop = do
        --done <- _processEvents width height debug reset
        done <- return False
        if done then return () else do { tick ; loop }
      tick = do
        bgfxSetViewRect 0 0 0 (fromIntegral width) (fromIntegral height)
        bgfxTouch 0
        -- bgfxDbgTextClear
        -- 	bgfx::dbgTextImage(bx::uint16_max(width/2/8, 20)-20
        -- 					 , bx::uint16_max(height/2/16, 6)-6
        -- 					 , 40
        -- 					 , 12
        -- 					 , s_logo
        -- 					 , 160
        -- 					 );
        -- 	bgfx::dbgTextPrintf(0, 1, 0x4f, "bgfx/examples/00-helloworld");
        -- 	bgfx::dbgTextPrintf(0, 2, 0x6f, "Description: Initialization and debug text.");
        bgfxFrame
  loop
  bgfxShutdown
