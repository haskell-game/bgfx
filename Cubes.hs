{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}

module Main where

import Control.Lens ((&), (.~))
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import GHC.Stack
import BGFX
import Data.Bits
import Foreign.Ptr
import Foreign
import Data.Word
import qualified SDL
import Unsafe.Coerce
import Linear
import Data.Distributive
import System.Clock

foreign import ccall unsafe
  "bgfx_sdl_set_window" bgfxSdlSetWindow :: Ptr a -> IO ()

main :: IO ()
main =
  do let width = 1280
     let height = 720
     let debug = BGFX_DEBUG_TEXT
     let reset = BGFX_RESET_VSYNC
     SDL.initializeAll
     w <- SDL.createWindow "bgfx with SDL2" SDL.defaultWindow
     bgfxSdlSetWindow (unsafeCoerce w :: Ptr ())
     bgfxRenderFrame
     bgfxInit BGFX_RENDERER_TYPE_COUNT BGFX_PCI_ID_NONE 0 nullPtr nullPtr
     bgfxReset width height reset
     bgfxSetDebug debug
     bgfxSetViewClear 0
                      (BGFX_CLEAR_COLOR .|. BGFX_CLEAR_DEPTH)
                      808464639
                      1.0
                      0
     posColorVertexDecl <- declarePosColorVertex
     vbh <-
       do ref <-
            withArrayLen
              cubeVertices
              (\len ptr ->
                 bgfxCopy ptr (fromIntegral (len * sizeOf (head cubeVertices))))
          bgfxCreateVertexBuffer ref posColorVertexDecl BGFX_BUFFER_NONE
     ibh <-
       do ptr <- newArray cubeIndices
          ref <-
            bgfxMakeRef
              ptr
              (fromIntegral (length cubeIndices * sizeOf (head cubeIndices)))
          bgfxCreateIndexBuffer ref BGFX_BUFFER_NONE
     program <- loadProgram "vs_cubes.bin" "fs_cubes.bin"
     timeOffset <- getTime Monotonic
     let loop =
           do _ <- SDL.pollEvents
              done <- return False
              if done
                 then return ()
                 else do tick
                         loop
         tick =
           do now <- getTime Monotonic
              let time =
                    fromIntegral (timeSpecAsNanoSecs (diffTimeSpec now timeOffset)) *
                    1.0e-9
              with (distribute
                      (lookAt (V3 0 0 (-35))
                              0
                              (V3 0 1 0) :: M44 Float))
                   (\viewPtr ->
                      with (distribute
                              (perspective
                                 1.047
                                 (fromIntegral width / fromIntegral height :: Float)
                                 0.1
                                 100))
                           (\projPtr -> bgfxSetViewTransform 0 viewPtr projPtr))
              bgfxSetViewRect 0
                              0
                              0
                              (fromIntegral width)
                              (fromIntegral height)
              bgfxTouch 0
              for_ [0 .. 11] $
                \yy ->
                  do for_ [0 .. 11] $
                       \xx ->
                         do with (distribute
                                    (((m33_to_m44 . fromQuaternion)
                                        (axisAngle (V3 1 0 0)
                                                   (time +
                                                    fromIntegral xx * 0.21) *
                                         axisAngle (V3 0 1 0)
                                                   (time +
                                                    fromIntegral yy * 0.37)) :: M44 Float) &
                                     translation .~
                                     V3 (-15 + fromIntegral xx * 3)
                                        (-15 + fromIntegral yy * 3)
                                        0))
                                 (flip bgfxSetTransform 1)
                            bgfxSetVertexBuffer vbh 0 maxBound
                            bgfxSetIndexBuffer ibh 0 maxBound
                            bgfxSetState BGFX_STATE_DEFAULT 0
                            bgfxSubmit 0 program 0
              bgfxFrame
     loop
     bgfxShutdown

loadProgram :: FilePath -> FilePath -> IO BgfxProgramHandle
loadProgram vsName fsName = do
  vs <- loadShader vsName
  fs <- loadShader fsName
  bgfxCreateProgram vs fs True
  where loadShader path = do
          bytes <- BS.readFile path
          mem <- BS.useAsCStringLen (BS.snoc bytes 0) $ \(ptr, len) ->
            bgfxCopy ptr (fromIntegral len)
          bgfxCreateShader mem

declarePosColorVertex :: IO (Ptr BgfxVertexDecl)
declarePosColorVertex = do
  vertexDecl <- malloc
  bgfxVertexDeclBegin vertexDecl BGFX_RENDERER_TYPE_NULL
  bgfxVertexDeclAdd vertexDecl BGFX_ATTRIB_POSITION 3 BGFX_ATTRIB_TYPE_FLOAT False False
  bgfxVertexDeclAdd vertexDecl BGFX_ATTRIB_COLOR0 4 BGFX_ATTRIB_TYPE_UINT8 True False
  bgfxVertexDeclEnd vertexDecl
  return vertexDecl

data PosColorVertex = PosColorVertex (V3 Float) Word32

instance Storable PosColorVertex where
  sizeOf ~(PosColorVertex a b) = sizeOf a + sizeOf b
  peek ptr = do
    PosColorVertex <$> peek (castPtr ptr) <*> peek (castPtr (ptr `plusPtr` fromIntegral (sizeOf (undefined :: V3 Float))))
  poke ptr (PosColorVertex a b)  = do
    poke (castPtr ptr) a
    poke (castPtr (ptr `plusPtr` fromIntegral (sizeOf (undefined :: V3 Float)))) b
  alignment _ = 0

cubeVertices :: [PosColorVertex]
cubeVertices =
  [PosColorVertex (V3 (-1.0) (1.0) (1.0)) 4278190080
  ,PosColorVertex (V3 (1.0) (1.0) (1.0)) 4278190335
  ,PosColorVertex (V3 (-1.0) (-1.0) (1.0)) 4278255360
  ,PosColorVertex (V3 (1.0) (-1.0) (1.0)) 4278255615
  ,PosColorVertex (V3 (-1.0) (1.0) (-1.0)) 4294901760
  ,PosColorVertex (V3 (1.0) (1.0) (-1.0)) 4294902015
  ,PosColorVertex (V3 (-1.0) (-1.0) (-1.0)) 4294967040
  ,PosColorVertex (V3 (1.0) (-1.0) (-1.0)) 4294967295]

cubeIndices :: [Word16]
cubeIndices =
  [0,1,2,1,3,2,4,6,5,5,6,7,0,2,4,4,2,6,1,5,3,5,7,3,0,4,1,4,5,1,2,3,6,6,3,7]
