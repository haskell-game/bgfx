{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BGFX where

import Data.Word
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr

#include "bgfx/c99/bgfx.h"

foreign import ccall unsafe
  "bgfx_init" bgfxInit :: BgfxRendererType -> Word16 -> Word16 -> Ptr BgfxCallback -> Ptr BgfxAllocator -> IO Bool

pattern BGFX_PCI_ID_NONE = (#const BGFX_PCI_ID_NONE)
pattern BGFX_PCI_ID_AMD = (#const BGFX_PCI_ID_AMD)
pattern BGFX_PCI_ID_INTEL = (#const BGFX_PCI_ID_INTEL)
pattern BGFX_PCI_ID_NVIDIA = (#const BGFX_PCI_ID_NVIDIA)

data BgfxCallback

foreign import ccall unsafe
  "bgfx_shutdown" bgfxShutdown :: IO ()

foreign import ccall unsafe
  "bgfx_reset" bgfxReset :: Word32 -> Word32 -> Word32 -> IO ()

pattern BGFX_RESET_NONE = (#const BGFX_RESET_NONE)
pattern BGFX_RESET_FULLSCREEN = (#const BGFX_RESET_FULLSCREEN) 
pattern BGFX_RESET_FULLSCREEN_SHIFT = (#const BGFX_RESET_FULLSCREEN_SHIFT)
pattern BGFX_RESET_FULLSCREEN_MASK = (#const BGFX_RESET_FULLSCREEN_MASK)
pattern BGFX_RESET_MSAA_X2 = (#const BGFX_RESET_MSAA_X2)
pattern BGFX_RESET_MSAA_X4 = (#const BGFX_RESET_MSAA_X4)
pattern BGFX_RESET_MSAA_X8 = (#const BGFX_RESET_MSAA_X8)
pattern BGFX_RESET_MSAA_X16 = (#const BGFX_RESET_MSAA_X16)
pattern BGFX_RESET_MSAA_SHIFT = (#const BGFX_RESET_MSAA_SHIFT)
pattern BGFX_RESET_MSAA_MASK = (#const BGFX_RESET_MSAA_MASK)
pattern BGFX_RESET_VSYNC = (#const BGFX_RESET_VSYNC)
pattern BGFX_RESET_MAXANISOTROPY = (#const BGFX_RESET_MAXANISOTROPY)
pattern BGFX_RESET_CAPTURE = (#const BGFX_RESET_CAPTURE)
pattern BGFX_RESET_HMD = (#const BGFX_RESET_HMD)
pattern BGFX_RESET_HMD_DEBUG = (#const BGFX_RESET_HMD_DEBUG)
pattern BGFX_RESET_HMD_RECENTER = (#const BGFX_RESET_HMD_RECENTER)
pattern BGFX_RESET_FLUSH_AFTER_RENDER = (#const BGFX_RESET_FLUSH_AFTER_RENDER)
pattern BGFX_RESET_FLIP_AFTER_RENDER = (#const BGFX_RESET_FLIP_AFTER_RENDER)
pattern BGFX_RESET_SRGB_BACKBUFFER = (#const BGFX_RESET_SRGB_BACKBUFFER)
pattern BGFX_RESET_HIDPI = (#const BGFX_RESET_HIDPI)
pattern BGFX_RESET_DEPTH_CLAMP = (#const BGFX_RESET_DEPTH_CLAMP)

foreign import ccall unsafe
  "bgfx_frame" bgfxFrame :: IO Word32

foreign import ccall unsafe
  "bgfx_set_debug" bgfxSetDebug :: Word32 -> IO ()

pattern BGFX_DEBUG_NONE = (#const BGFX_DEBUG_NONE)
pattern BGFX_DEBUG_WIREFRAME = (#const BGFX_DEBUG_WIREFRAME)
pattern BGFX_DEBUG_IFH = (#const BGFX_DEBUG_IFH)
pattern BGFX_DEBUG_STATS = (#const BGFX_DEBUG_STATS)
pattern BGFX_DEBUG_TEXT = (#const BGFX_DEBUG_TEXT)

foreign import ccall unsafe
  "bgfx_dbg_text_clear" bgfxDbgTextClear :: Word8 -> Bool -> IO ()

foreign import ccall unsafe
  "bgfx_get_renderer_type" bgfxGetRendererType :: IO BgfxRendererType

type BgfxRendererType = (#type bgfx_renderer_type_t)

pattern BGFX_RENDERER_TYPE_NULL = (#const BGFX_RENDERER_TYPE_NULL)
pattern BGFX_RENDERER_TYPE_DIRECT3D9 = (#const BGFX_RENDERER_TYPE_DIRECT3D9)
pattern BGFX_RENDERER_TYPE_DIRECT3D11 = (#const BGFX_RENDERER_TYPE_DIRECT3D11)
pattern BGFX_RENDERER_TYPE_DIRECT3D12 = (#const BGFX_RENDERER_TYPE_DIRECT3D12)
pattern BGFX_RENDERER_TYPE_METAL = (#const BGFX_RENDERER_TYPE_METAL)
pattern BGFX_RENDERER_TYPE_OPENGLES = (#const BGFX_RENDERER_TYPE_OPENGLES)
pattern BGFX_RENDERER_TYPE_OPENGL = (#const BGFX_RENDERER_TYPE_OPENGL)
pattern BGFX_RENDERER_TYPE_VULKAN = (#const BGFX_RENDERER_TYPE_VULKAN)
pattern BGFX_RENDERER_TYPE_COUNT = (#const BGFX_RENDERER_TYPE_COUNT)

foreign import ccall unsafe
  "bgfx_get_caps" bgfxGetCaps :: IO (Ptr BgfxCaps)

data BgfxCaps

foreign import ccall unsafe
  "bgfx_get_stats" bgfxGetStats :: IO (Ptr BgfxStats)

data BgfxStats

foreign import ccall unsafe
  "bgfx_get_hmd" bgfxGetHMD :: IO (Ptr BgfxHMD)

data BgfxHMD

foreign import ccall unsafe
  "bgfx_discard" bgfxDiscard :: IO ()

foreign import ccall unsafe
  "bgfx_touch" bgfxTouch :: Word8 -> IO Word32

foreign import ccall unsafe
  "bgfx_set_palette_color" bgxSetPaletteColor :: Word8 -> Ptr CFloat -> IO ()

foreign import ccall unsafe
  "bgfx_save_screenshot" bgfxSaveScreenshot :: CString -> IO ()

foreign import ccall unsafe
  "bgfx_set_view_name" bgfxSetViewName :: Word8 -> CString -> IO ()

foreign import ccall unsafe
  "bgfx_set_view_rect" bgfxSetViewRect :: Word8 -> Word16 -> Word16 -> Word16 -> Word16 -> IO ()

type BgfxBackbufferRatio = (#type bgfx_backbuffer_ratio_t)

foreign import ccall unsafe
  "bgfx_set_view_rect_auto" bgfxSetViewRectAuto :: Word8 -> Word16 -> Word16 -> BgfxBackbufferRatio -> IO ()

pattern BGFX_BACKBUFFER_RATIO_EQUAL = (#const BGFX_BACKBUFFER_RATIO_EQUAL)
pattern BGFX_BACKBUFFER_RATIO_HALF = (#const BGFX_BACKBUFFER_RATIO_HALF)
pattern BGFX_BACKBUFFER_RATIO_QUARTER = (#const BGFX_BACKBUFFER_RATIO_QUARTER)
pattern BGFX_BACKBUFFER_RATIO_EIGHTH = (#const BGFX_BACKBUFFER_RATIO_EIGHTH)
pattern BGFX_BACKBUFFER_RATIO_SIXTEENTH = (#const BGFX_BACKBUFFER_RATIO_SIXTEENTH)
pattern BGFX_BACKBUFFER_RATIO_DOUBLE = (#const BGFX_BACKBUFFER_RATIO_DOUBLE)

foreign import ccall unsafe
  "bgfx_set_view_scissor" bgfxSetViewScissor :: Word8 -> Word16 -> Word16 -> Word16 -> Word16 -> IO ()

foreign import ccall unsafe
  "bgfx_set_view_clear" bgfxSetViewClear :: Word8 -> Word16 -> Word32 -> CFloat -> Word8 -> IO ()

foreign import ccall unsafe
  "bgfx_set_view_clear_mrt" bgfxSetViewClearMrt :: Word8 -> Word16 -> CFloat -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> IO ()

foreign import ccall unsafe
  "bgfx_set_view_seq" bgfxSetViewSeq :: Word8 -> Bool -> IO ()

foreign import ccall unsafe
  "bgfx_set_view_transform" bgfxSetViewTransform :: Word8 -> Ptr () -> Ptr () -> IO ()

foreign import ccall unsafe
  "bgfx_set_view_transform_stereo" bgfxSetViewTransformStereo :: Word8 -> Ptr () -> Ptr () -> Word8 -> Ptr () -> IO ()

foreign import ccall unsafe
  "bgfx_set_view_remap" bgfxSetVieRemap :: Word8 -> Word8 -> Ptr () -> IO ()

type BgfxFrameBufferHandle = Word16

foreign import ccall unsafe
  "bgfx_set_view_frame_buffer" bgfxSetViewframeBuffer :: Word8 -> BgfxFrameBufferHandle -> IO ()

data BgfxAllocator

foreign import ccall unsafe
  "bgfx_set_marker" bgfxSetMarker :: CString -> IO ()

foreign import ccall unsafe
  "bgfx_set_state" bgfxSetState :: Word64 -> Word32 -> IO ()

pattern BGFX_STATE_RGB_WRITE = (#const BGFX_STATE_RGB_WRITE)
pattern BGFX_STATE_ALPHA_WRITE = (#const BGFX_STATE_ALPHA_WRITE)
pattern BGFX_STATE_DEPTH_WRITE = (#const BGFX_STATE_DEPTH_WRITE)

pattern BGFX_STATE_DEPTH_TEST_LESS = (#const BGFX_STATE_DEPTH_TEST_LESS)
pattern BGFX_STATE_DEPTH_TEST_LEQUAL = (#const BGFX_STATE_DEPTH_TEST_LEQUAL)
pattern BGFX_STATE_DEPTH_TEST_EQUAL = (#const BGFX_STATE_DEPTH_TEST_EQUAL)
pattern BGFX_STATE_DEPTH_TEST_GEQUAL = (#const BGFX_STATE_DEPTH_TEST_GEQUAL)
pattern BGFX_STATE_DEPTH_TEST_GREATER = (#const BGFX_STATE_DEPTH_TEST_GREATER)
pattern BGFX_STATE_DEPTH_TEST_NOTEQUAL = (#const BGFX_STATE_DEPTH_TEST_NOTEQUAL)
pattern BGFX_STATE_DEPTH_TEST_NEVER = (#const BGFX_STATE_DEPTH_TEST_NEVER)
pattern BGFX_STATE_DEPTH_TEST_ALWAYS = (#const BGFX_STATE_DEPTH_TEST_ALWAYS)
pattern BGFX_STATE_DEPTH_TEST_SHIFT = (#const BGFX_STATE_DEPTH_TEST_SHIFT)
pattern BGFX_STATE_DEPTH_TEST_MASK = (#const BGFX_STATE_DEPTH_TEST_MASK)

pattern BGFX_STATE_BLEND_ZERO = (#const BGFX_STATE_BLEND_ZERO)
pattern BGFX_STATE_BLEND_ONE = (#const BGFX_STATE_BLEND_ONE)
pattern BGFX_STATE_BLEND_SRC_COLOR = (#const BGFX_STATE_BLEND_SRC_COLOR)
pattern BGFX_STATE_BLEND_INV_SRC_COLOR = (#const BGFX_STATE_BLEND_INV_SRC_COLOR)
pattern BGFX_STATE_BLEND_SRC_ALPHA = (#const BGFX_STATE_BLEND_SRC_ALPHA)
pattern BGFX_STATE_BLEND_INV_SRC_ALPHA = (#const BGFX_STATE_BLEND_INV_SRC_ALPHA)
pattern BGFX_STATE_BLEND_DST_ALPHA = (#const BGFX_STATE_BLEND_DST_ALPHA)
pattern BGFX_STATE_BLEND_INV_DST_ALPHA = (#const BGFX_STATE_BLEND_INV_DST_ALPHA)
pattern BGFX_STATE_BLEND_DST_COLOR = (#const BGFX_STATE_BLEND_DST_COLOR)
pattern BGFX_STATE_BLEND_INV_DST_COLOR = (#const BGFX_STATE_BLEND_INV_DST_COLOR)
pattern BGFX_STATE_BLEND_SRC_ALPHA_SAT = (#const BGFX_STATE_BLEND_SRC_ALPHA_SAT)
pattern BGFX_STATE_BLEND_FACTOR = (#const BGFX_STATE_BLEND_FACTOR)
pattern BGFX_STATE_BLEND_INV_FACTOR = (#const BGFX_STATE_BLEND_INV_FACTOR)
pattern BGFX_STATE_BLEND_SHIFT = (#const BGFX_STATE_BLEND_SHIFT)
pattern BGFX_STATE_BLEND_MASK = (#const BGFX_STATE_BLEND_MASK)

pattern BGFX_STATE_BLEND_EQUATION_ADD = (#const BGFX_STATE_BLEND_EQUATION_ADD)
pattern BGFX_STATE_BLEND_EQUATION_SUB = (#const BGFX_STATE_BLEND_EQUATION_SUB)
pattern BGFX_STATE_BLEND_EQUATION_REVSUB = (#const BGFX_STATE_BLEND_EQUATION_REVSUB)
pattern BGFX_STATE_BLEND_EQUATION_MIN = (#const BGFX_STATE_BLEND_EQUATION_MIN)
pattern BGFX_STATE_BLEND_EQUATION_MAX = (#const BGFX_STATE_BLEND_EQUATION_MAX)
pattern BGFX_STATE_BLEND_EQUATION_SHIFT = (#const BGFX_STATE_BLEND_EQUATION_SHIFT)
pattern BGFX_STATE_BLEND_EQUATION_MASK = (#const BGFX_STATE_BLEND_EQUATION_MASK)

pattern BGFX_STATE_BLEND_INDEPENDENT = (#const BGFX_STATE_BLEND_INDEPENDENT)

pattern BGFX_STATE_CULL_CW = (#const BGFX_STATE_CULL_CW)
pattern BGFX_STATE_CULL_CCW = (#const BGFX_STATE_CULL_CCW)
pattern BGFX_STATE_CULL_SHIFT = (#const BGFX_STATE_CULL_SHIFT)
pattern BGFX_STATE_CULL_MASK = (#const BGFX_STATE_CULL_MASK)

pattern BGFX_STATE_ALPHA_REF_SHIFT = (#const BGFX_STATE_ALPHA_REF_SHIFT)
pattern BGFX_STATE_ALPHA_REF_MASK = (#const BGFX_STATE_ALPHA_REF_MASK)

pattern BGFX_STATE_PT_TRISTRIP = (#const BGFX_STATE_PT_TRISTRIP)
pattern BGFX_STATE_PT_LINES = (#const BGFX_STATE_PT_LINES)
pattern BGFX_STATE_PT_LINESTRIP = (#const BGFX_STATE_PT_LINESTRIP)
pattern BGFX_STATE_PT_POINTS = (#const BGFX_STATE_PT_POINTS)
pattern BGFX_STATE_PT_SHIFT = (#const BGFX_STATE_PT_SHIFT)
pattern BGFX_STATE_PT_MASK = (#const BGFX_STATE_PT_MASK)

pattern BGFX_STATE_POINT_SIZE_SHIFT = (#const BGFX_STATE_POINT_SIZE_SHIFT)
pattern BGFX_STATE_POINT_SIZE_MASK = (#const BGFX_STATE_POINT_SIZE_MASK)

pattern BGFX_STATE_MSAA = (#const BGFX_STATE_MSAA)

foreign import ccall unsafe
  "bgfx_set_stencil" bgfxSetStencil :: Word32 -> Word32 -> IO ()

pattern BGFX_STENCIL_NONE = (#const BGFX_STENCIL_NONE)
pattern BGFX_STENCIL_MASK = (#const BGFX_STENCIL_MASK)
pattern BGFX_STENCIL_DEFAULT = (#const BGFX_STENCIL_DEFAULT)

pattern BGFX_STENCIL_TEST_LESS = (#const BGFX_STENCIL_TEST_LESS)
pattern BGFX_STENCIL_TEST_LEQUAL = (#const BGFX_STENCIL_TEST_LEQUAL)
pattern BGFX_STENCIL_TEST_EQUAL = (#const BGFX_STENCIL_TEST_EQUAL)
pattern BGFX_STENCIL_TEST_GEQUAL = (#const BGFX_STENCIL_TEST_GEQUAL)
pattern BGFX_STENCIL_TEST_GREATER = (#const BGFX_STENCIL_TEST_GREATER)
pattern BGFX_STENCIL_TEST_NOTEQUAL = (#const BGFX_STENCIL_TEST_NOTEQUAL)
pattern BGFX_STENCIL_TEST_NEVER = (#const BGFX_STENCIL_TEST_NEVER)
pattern BGFX_STENCIL_TEST_ALWAYS = (#const BGFX_STENCIL_TEST_ALWAYS)
pattern BGFX_STENCIL_TEST_SHIFT = (#const BGFX_STENCIL_TEST_SHIFT)
pattern BGFX_STENCIL_TEST_MASK = (#const BGFX_STENCIL_TEST_MASK)

foreign import ccall unsafe
  "bgfx_set_scissor" bgfxSetScissor :: Word16 -> Word16 -> Word16 -> Word16 -> IO Word16

foreign import ccall unsafe
  "bgfx_set_scissor_cached" bgfxSetScissorCached :: Word16 -> IO ()
