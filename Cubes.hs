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

foreign import ccall unsafe
  "bgfx_sdl_set_window" bgfxSdlSetWindow :: Ptr a -> IO ()

main :: IO ()
main =
  do let width = 1280
     let height = 720
     let debug = BGFX_DEBUG_TEXT
     let reset = BGFX_RESET_VSYNC
     SDL.initializeAll
     w <-
       SDL.createWindow "bgfx with SDL2" SDL.defaultWindow
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
                 bgfxMakeRef (castPtr ptr :: Ptr ())
                             (fromIntegral len))
          bgfxCreateVertexBuffer ref posColorVertexDecl BGFX_BUFFER_NONE
     ibh <-
       do ref <-
            withArrayLen
              cubeIndices
              (\len ptr ->
                 bgfxMakeRef (castPtr ptr :: Ptr ())
                             (fromIntegral len))
          bgfxCreateIndexBuffer ref BGFX_BUFFER_NONE
     program <-
       loadProgram "vs_cubes.bin" "fs_cubes.bin"
     let loop =
           do done <- return False
              if done
                 then return ()
                 else do tick
                         loop
         tick =
           do with (distribute
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
                           (\projPtr ->
                              bgfxSetViewTransform 0 viewPtr projPtr))
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
                                    ((identity :: M44 Float) & translation .~
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


	-- 	// Create program from shaders.
	-- 	m_program = loadProgram("vs_cubes", "fs_cubes");

	-- 	m_timeOffset = bx::getHPCounter();
	-- }

-- 	bool update() BX_OVERRIDE
-- 	{
-- 		if (!entry::processEvents(m_width, m_height, m_debug, m_reset) )
-- 		{
-- 			int64_t now = bx::getHPCounter();
-- 			static int64_t last = now;
-- 			const int64_t frameTime = now - last;
-- 			last = now;
-- 			const double freq = double(bx::getHPFrequency() );
-- 			const double toMs = 1000.0/freq;

-- 			float time = (float)( (now-m_timeOffset)/double(bx::getHPFrequency() ) );

-- 			// Use debug font to print information about this example.
-- 			bgfx::dbgTextClear();
-- 			bgfx::dbgTextPrintf(0, 1, 0x4f, "bgfx/examples/01-cube");
-- 			bgfx::dbgTextPrintf(0, 2, 0x6f, "Description: Rendering simple static mesh.");
-- 			bgfx::dbgTextPrintf(0, 3, 0x0f, "Frame: % 7.3f[ms]", double(frameTime)*toMs);

-- 			float at[3]  = { 0.0f, 0.0f,   0.0f };
-- 			float eye[3] = { 0.0f, 0.0f, -35.0f };

-- 			// Set view and projection matrix for view 0.
-- 			const bgfx::HMD* hmd = bgfx::getHMD();
-- 			if (NULL != hmd && 0 != (hmd->flags & BGFX_HMD_RENDERING) )
-- 			{
-- 				float view[16];
-- 				bx::mtxQuatTranslationHMD(view, hmd->eye[0].rotation, eye);

-- 				float proj[16];
-- 				bx::mtxProj(proj, hmd->eye[0].fov, 0.1f, 100.0f);

-- 				bgfx::setViewTransform(0, view, proj);

-- 				// Set view 0 default viewport.
-- 				//
-- 				// Use HMD's width/height since HMD's internal frame buffer size
-- 				// might be much larger than window size.
-- 				bgfx::setViewRect(0, 0, 0, hmd->width, hmd->height);
-- 			}
-- 			else
-- 			{
-- 				float view[16];
-- 				bx::mtxLookAt(view, eye, at);

-- 				float proj[16];
-- 				bx::mtxProj(proj, 60.0f, float(m_width)/float(m_height), 0.1f, 100.0f);
-- 				bgfx::setViewTransform(0, view, proj);

-- 				// Set view 0 default viewport.
-- 				bgfx::setViewRect(0, 0, 0, m_width, m_height);
-- 			}

-- 			// This dummy draw call is here to make sure that view 0 is cleared
-- 			// if no other draw calls are submitted to view 0.
-- 			bgfx::touch(0);

-- 			// Submit 11x11 cubes.
-- 			for (uint32_t yy = 0; yy < 11; ++yy)
-- 			{
-- 				for (uint32_t xx = 0; xx < 11; ++xx)
-- 				{
-- 					float mtx[16];
-- 					bx::mtxRotateXY(mtx, time + xx*0.21f, time + yy*0.37f);
-- 					mtx[12] = -15.0f + float(xx)*3.0f;
-- 					mtx[13] = -15.0f + float(yy)*3.0f;
-- 					mtx[14] = 0.0f;

-- 					// Set model matrix for rendering.
-- 					bgfx::setTransform(mtx);

-- 					// Set vertex and index buffer.
-- 					bgfx::setVertexBuffer(m_vbh);
-- 					bgfx::setIndexBuffer(m_ibh);

-- 					// Set render states.
-- 					bgfx::setState(BGFX_STATE_DEFAULT);

-- 					// Submit primitive for rendering to view 0.
-- 					bgfx::submit(0, m_program);
-- 				}
-- 			}

-- 			// Advance to next frame. Rendering thread will be kicked to
-- 			// process submitted rendering primitives.
-- 			bgfx::frame();

-- 			return true;
-- 		}

-- 		return false;
-- 	}

-- 	uint32_t m_width;
-- 	uint32_t m_height;
-- 	uint32_t m_debug;
-- 	uint32_t m_reset;
-- 	bgfx::VertexBufferHandle m_vbh;
-- 	bgfx::IndexBufferHandle m_ibh;
-- 	bgfx::ProgramHandle m_program;
-- 	int64_t m_timeOffset;
-- };

-- ENTRY_IMPLEMENT_MAIN(Cubes);
