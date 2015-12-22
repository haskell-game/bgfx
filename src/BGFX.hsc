{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BGFX where

import Data.Int
import Data.Word
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

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
  "bgfx_render_frame" bgfxRenderFrame :: IO ()

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
  "bgfx_set_view_transform" bgfxSetViewTransform :: Word8 -> Ptr a -> Ptr b -> IO ()

foreign import ccall unsafe
  "bgfx_set_view_transform_stereo" bgfxSetViewTransformStereo :: Word8 -> Ptr a -> Ptr b -> Word8 -> Ptr () -> IO ()

foreign import ccall unsafe
  "bgfx_set_view_remap" bgfxSetVieRemap :: Word8 -> Word8 -> Ptr a -> IO ()

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

pattern BGFX_STATE_DEFAULT = (#const BGFX_STATE_DEFAULT)

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

foreign import ccall unsafe
  "bgfx_set_transform" bgfxSetTransform :: Ptr a -> Word16 -> IO Word32

type BgfxOcclusionQueryHandle = Word16

foreign import ccall unsafe
  "bgfx_set_condition" bgfxSetCondition :: BgfxOcclusionQueryHandle -> Bool -> IO ()

type BgfxIndexBufferHandle = Word16

foreign import ccall unsafe
  "bgfx_set_index_buffer" bgfxSetIndexBuffer :: BgfxIndexBufferHandle -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe
  "bgfx_set_dynamic_index_buffer" bgfxSetDynamicIndexBuffer :: BgfxIndexBufferHandle -> Word32 -> Word32 -> IO ()

data BgfxIndexBuffer

foreign import ccall unsafe
  "bgfx_set_transient_index_buffer" bgfxSetTransientIndexBuffer :: Ptr BgfxIndexBuffer -> Word32 -> Word32 -> IO ()

type BgfxVertexBufferHandle = Word16

type BgfxDynamicVertexBufferHandle = Word16

foreign import ccall unsafe
  "bgfx_set_vertex_buffer" bgfxSetVertexBuffer :: BgfxVertexBufferHandle -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe
  "bgfx_set_dynamic_vertex_buffer" bgfxSetDynamicVertexBuffer :: BgfxVertexBufferHandle -> Word32 -> IO ()

data BgfxVertexBuffer

foreign import ccall unsafe
  "bgfx_set_transient_vertex_buffer" bgfxSetTransientVertexBuffer :: Ptr BgfxVertexBuffer -> Word32 -> Word32 -> IO ()

type BgfxInstanceDataBufferHandle = Word16

data BgfxInstanceDataBuffer

foreign import ccall unsafe
  "bgfx_set_instance_data_buffer" bgfxSetInstanceDataBuffer :: Ptr BgfxInstanceDataBuffer -> Word32 -> IO ()

foreign import ccall unsafe
  "bgfx_set_instance_data_buffer_from_vertex_buffer" bgfxSetInstanceDataBufferFromVertexBuffer :: BgfxVertexBufferHandle -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe
  "bgfx_set_instance_data_buffer_from_dynamic_vertex_buffer" bgfxSetInstanceDataBufferFromDynamicVertexBuffer :: BgfxDynamicVertexBufferHandle -> Word32 -> Word32 -> IO ()

type BgfxUniformHandle = Word16

type BgfxTextureHandle = Word16

foreign import ccall unsafe
  "bgfx_set_texture" bgfxSetTexture :: Word8 -> BgfxUniformHandle -> BgfxTextureHandle -> Word32 -> IO ()

foreign import ccall unsafe
  "bgfx_set_texture_from_frame_buffer" bgfxSetTextureFromFrameBuffer :: Word8 -> BgfxUniformHandle -> BgfxFrameBufferHandle -> Word8 -> Word32 -> IO ()

type BgfxProgramHandle = Word16

foreign import ccall unsafe
  "bgfx_submit" bgfxSubmit :: Word8 -> BgfxProgramHandle -> Int32 -> IO Word32

foreign import ccall unsafe
  "bgfx_submit_occlusion_query" bgfxSubmitOcclusionQuery :: Word8 -> BgfxProgramHandle -> BgfxOcclusionQueryHandle -> Int32 -> IO Word32

type BgfxIndirectBufferHandle = Word16

foreign import ccall unsafe
  "bgfx_submit_indirect" bgfxSubmitIndirect :: Word8 -> BgfxProgramHandle -> BgfxIndirectBufferHandle -> Word16 -> Word16 -> Int32 -> IO Word32

pattern BGFX_ACCESS_READ = (#const BGFX_ACCESS_READ)
pattern BGFX_ACCESS_WRITE = (#const BGFX_ACCESS_WRITE)
pattern BGFX_ACCESS_READWRITE = (#const BGFX_ACCESS_READWRITE)
pattern BGFX_ACCESS_COUNT = (#const BGFX_ACCESS_COUNT)

type BgfxAccess = (#type bgfx_access_t)

foreign import ccall unsafe
  "bgfx_set_compute_index_buffer" bgfxSetComputeIndexBuffer :: Word8 -> BgfxIndexBufferHandle -> BgfxAccess -> IO ()

foreign import ccall unsafe
  "bgfx_set_compute_vertex_buffer" bgfxSetComputeVertexBuffer :: Word8 -> BgfxVertexBufferHandle -> BgfxAccess -> IO ()

type BgfxDynamicIndexBufferHandle = Word16

foreign import ccall unsafe
  "bgfx_set_compute_dynamic_index_buffer" bgxSetComputeDynamicIndexBuffer :: Word8 -> BgfxDynamicIndexBufferHandle -> BgfxAccess -> IO ()

foreign import ccall unsafe
  "bgfx_set_compute_dynamic_vertex_buffer" bgfxSetComputeDynamicVertexBuffer :: Word8 -> BgfxDynamicVertexBufferHandle -> BgfxAccess -> IO ()

foreign import ccall unsafe
  "bgfx_set_compute_indirect_buffer" bgfxSetComputeIndirectBuffer :: Word8 -> BgfxIndirectBufferHandle -> BgfxAccess -> IO ()

type BgfxTextureFormat = (#type bgfx_texture_format_t)

pattern BGFX_TEXTURE_FORMAT_BC1 = (#const BGFX_TEXTURE_FORMAT_BC1)
pattern BGFX_TEXTURE_FORMAT_BC2 = (#const BGFX_TEXTURE_FORMAT_BC2)
pattern BGFX_TEXTURE_FORMAT_BC3 = (#const BGFX_TEXTURE_FORMAT_BC3)
pattern BGFX_TEXTURE_FORMAT_BC4 = (#const BGFX_TEXTURE_FORMAT_BC4)
pattern BGFX_TEXTURE_FORMAT_BC5 = (#const BGFX_TEXTURE_FORMAT_BC5)
pattern BGFX_TEXTURE_FORMAT_BC6H = (#const BGFX_TEXTURE_FORMAT_BC6H)
pattern BGFX_TEXTURE_FORMAT_BC7 = (#const BGFX_TEXTURE_FORMAT_BC7)
pattern BGFX_TEXTURE_FORMAT_ETC1 = (#const BGFX_TEXTURE_FORMAT_ETC1)
pattern BGFX_TEXTURE_FORMAT_ETC2 = (#const BGFX_TEXTURE_FORMAT_ETC2)
pattern BGFX_TEXTURE_FORMAT_ETC2A = (#const BGFX_TEXTURE_FORMAT_ETC2A)
pattern BGFX_TEXTURE_FORMAT_ETC2A1 = (#const BGFX_TEXTURE_FORMAT_ETC2A1)
pattern BGFX_TEXTURE_FORMAT_PTC12 = (#const BGFX_TEXTURE_FORMAT_PTC12)
pattern BGFX_TEXTURE_FORMAT_PTC14 = (#const BGFX_TEXTURE_FORMAT_PTC14)
pattern BGFX_TEXTURE_FORMAT_PTC12A = (#const BGFX_TEXTURE_FORMAT_PTC12A)
pattern BGFX_TEXTURE_FORMAT_PTC14A = (#const BGFX_TEXTURE_FORMAT_PTC14A)
pattern BGFX_TEXTURE_FORMAT_PTC22 = (#const BGFX_TEXTURE_FORMAT_PTC22)
pattern BGFX_TEXTURE_FORMAT_PTC24 = (#const BGFX_TEXTURE_FORMAT_PTC24)
pattern BGFX_TEXTURE_FORMAT_UNKNOWN = (#const BGFX_TEXTURE_FORMAT_UNKNOWN)
pattern BGFX_TEXTURE_FORMAT_R1 = (#const BGFX_TEXTURE_FORMAT_R1)
pattern BGFX_TEXTURE_FORMAT_A8 = (#const BGFX_TEXTURE_FORMAT_A8)
pattern BGFX_TEXTURE_FORMAT_R8 = (#const BGFX_TEXTURE_FORMAT_R8)
pattern BGFX_TEXTURE_FORMAT_R8I = (#const BGFX_TEXTURE_FORMAT_R8I)
pattern BGFX_TEXTURE_FORMAT_R8U = (#const BGFX_TEXTURE_FORMAT_R8U)
pattern BGFX_TEXTURE_FORMAT_R8S = (#const BGFX_TEXTURE_FORMAT_R8S)
pattern BGFX_TEXTURE_FORMAT_R16 = (#const BGFX_TEXTURE_FORMAT_R16)
pattern BGFX_TEXTURE_FORMAT_R16I = (#const BGFX_TEXTURE_FORMAT_R16I)
pattern BGFX_TEXTURE_FORMAT_R16U = (#const BGFX_TEXTURE_FORMAT_R16U)
pattern BGFX_TEXTURE_FORMAT_R16F = (#const BGFX_TEXTURE_FORMAT_R16F)
pattern BGFX_TEXTURE_FORMAT_R16S = (#const BGFX_TEXTURE_FORMAT_R16S)
pattern BGFX_TEXTURE_FORMAT_R32I = (#const BGFX_TEXTURE_FORMAT_R32I)
pattern BGFX_TEXTURE_FORMAT_R32U = (#const BGFX_TEXTURE_FORMAT_R32U)
pattern BGFX_TEXTURE_FORMAT_R32F = (#const BGFX_TEXTURE_FORMAT_R32F)
pattern BGFX_TEXTURE_FORMAT_RG8 = (#const BGFX_TEXTURE_FORMAT_RG8)
pattern BGFX_TEXTURE_FORMAT_RG8I = (#const BGFX_TEXTURE_FORMAT_RG8I)
pattern BGFX_TEXTURE_FORMAT_RG8U = (#const BGFX_TEXTURE_FORMAT_RG8U)
pattern BGFX_TEXTURE_FORMAT_RG8S = (#const BGFX_TEXTURE_FORMAT_RG8S)
pattern BGFX_TEXTURE_FORMAT_RG16 = (#const BGFX_TEXTURE_FORMAT_RG16)
pattern BGFX_TEXTURE_FORMAT_RG16I = (#const BGFX_TEXTURE_FORMAT_RG16I)
pattern BGFX_TEXTURE_FORMAT_RG16U = (#const BGFX_TEXTURE_FORMAT_RG16U)
pattern BGFX_TEXTURE_FORMAT_RG16F = (#const BGFX_TEXTURE_FORMAT_RG16F)
pattern BGFX_TEXTURE_FORMAT_RG16S = (#const BGFX_TEXTURE_FORMAT_RG16S)
pattern BGFX_TEXTURE_FORMAT_RG32I = (#const BGFX_TEXTURE_FORMAT_RG32I)
pattern BGFX_TEXTURE_FORMAT_RG32U = (#const BGFX_TEXTURE_FORMAT_RG32U)
pattern BGFX_TEXTURE_FORMAT_RG32F = (#const BGFX_TEXTURE_FORMAT_RG32F)
pattern BGFX_TEXTURE_FORMAT_RGB9E5F = (#const BGFX_TEXTURE_FORMAT_RGB9E5F)
pattern BGFX_TEXTURE_FORMAT_BGRA8 = (#const BGFX_TEXTURE_FORMAT_BGRA8)
pattern BGFX_TEXTURE_FORMAT_RGBA8 = (#const BGFX_TEXTURE_FORMAT_RGBA8)
pattern BGFX_TEXTURE_FORMAT_RGBA8I = (#const BGFX_TEXTURE_FORMAT_RGBA8I)
pattern BGFX_TEXTURE_FORMAT_RGBA8U = (#const BGFX_TEXTURE_FORMAT_RGBA8U)
pattern BGFX_TEXTURE_FORMAT_RGBA8S = (#const BGFX_TEXTURE_FORMAT_RGBA8S)
pattern BGFX_TEXTURE_FORMAT_RGBA16 = (#const BGFX_TEXTURE_FORMAT_RGBA16)
pattern BGFX_TEXTURE_FORMAT_RGBA16I = (#const BGFX_TEXTURE_FORMAT_RGBA16I)
pattern BGFX_TEXTURE_FORMAT_RGBA16U = (#const BGFX_TEXTURE_FORMAT_RGBA16U)
pattern BGFX_TEXTURE_FORMAT_RGBA16F = (#const BGFX_TEXTURE_FORMAT_RGBA16F)
pattern BGFX_TEXTURE_FORMAT_RGBA16S = (#const BGFX_TEXTURE_FORMAT_RGBA16S)
pattern BGFX_TEXTURE_FORMAT_RGBA32I = (#const BGFX_TEXTURE_FORMAT_RGBA32I)
pattern BGFX_TEXTURE_FORMAT_RGBA32U = (#const BGFX_TEXTURE_FORMAT_RGBA32U)
pattern BGFX_TEXTURE_FORMAT_RGBA32F = (#const BGFX_TEXTURE_FORMAT_RGBA32F)
pattern BGFX_TEXTURE_FORMAT_R5G6B5 = (#const BGFX_TEXTURE_FORMAT_R5G6B5)
pattern BGFX_TEXTURE_FORMAT_RGBA4 = (#const BGFX_TEXTURE_FORMAT_RGBA4)
pattern BGFX_TEXTURE_FORMAT_RGB5A1 = (#const BGFX_TEXTURE_FORMAT_RGB5A1)
pattern BGFX_TEXTURE_FORMAT_RGB10A2 = (#const BGFX_TEXTURE_FORMAT_RGB10A2)
pattern BGFX_TEXTURE_FORMAT_R11G11B10F = (#const BGFX_TEXTURE_FORMAT_R11G11B10F)
pattern BGFX_TEXTURE_FORMAT_UNKNOWN_DEPTH = (#const BGFX_TEXTURE_FORMAT_UNKNOWN_DEPTH)
pattern BGFX_TEXTURE_FORMAT_D16 = (#const BGFX_TEXTURE_FORMAT_D16)
pattern BGFX_TEXTURE_FORMAT_D24 = (#const BGFX_TEXTURE_FORMAT_D24)
pattern BGFX_TEXTURE_FORMAT_D24S8 = (#const BGFX_TEXTURE_FORMAT_D24S8)
pattern BGFX_TEXTURE_FORMAT_D32 = (#const BGFX_TEXTURE_FORMAT_D32)
pattern BGFX_TEXTURE_FORMAT_D16F = (#const BGFX_TEXTURE_FORMAT_D16F)
pattern BGFX_TEXTURE_FORMAT_D24F = (#const BGFX_TEXTURE_FORMAT_D24F)
pattern BGFX_TEXTURE_FORMAT_D32F = (#const BGFX_TEXTURE_FORMAT_D32F)
pattern BGFX_TEXTURE_FORMAT_D0S8 = (#const BGFX_TEXTURE_FORMAT_D0S8)
pattern BGFX_TEXTURE_FORMAT_COUN = (#const BGFX_TEXTURE_FORMAT_COUNT)

foreign import ccall unsafe
  "bgfx_set_image" bgfxSetImage :: Word8 -> BgfxUniformHandle -> BgfxTextureHandle -> Word8 -> BgfxAccess -> BgfxTextureFormat -> IO ()

foreign import ccall unsafe
  "bgfx_set_image_from_frame_buffer" bgfxSetImageFromFrameBuffer :: Word8 -> BgfxUniformHandle -> BgfxFrameBufferHandle -> Word8 -> BgfxAccess -> BgfxTextureFormat -> IO ()

foreign import ccall unsafe
  "bgfx_dispatch" bgfxDispatch :: Word8 -> BgfxProgramHandle -> Word16 -> Word16 -> Word16 -> Word8 -> IO Word32

foreign import ccall unsafe
  "bgfx_dispatch_indirect" bgfxDispatchIndirect :: Word8 -> BgfxProgramHandle -> BgfxIndirectBufferHandle -> Word16 -> Word16 -> Word16 -> Word8 -> IO Word32

foreign import ccall unsafe
  "bgfx_blit" bgfxBlit :: Word8 -> BgfxTextureHandle -> Word8 -> Word16 -> Word16 -> Word16 -> BgfxTextureHandle -> Word8 -> Word16 -> Word16 -> Word16 -> Word16-> Word16 -> Word16 -> IO ()

foreign import ccall unsafe
  "bgfx_blit_frame_buffer" bgfxBlitFrameBuffer :: Word8 -> BgfxTextureHandle -> Word8 -> Word16 -> Word16 -> Word16 -> BgfxFrameBufferHandle -> Word8 -> Word8 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> IO ()

data BgfxMemory

foreign import ccall unsafe
  "bgfx_alloc" bgfxAlloc :: Word32 -> IO (Ptr BgfxMemory)

foreign import ccall unsafe
  "bgfx_copy" bgfxCopy :: Ptr a -> Word32 -> IO (Ptr BgfxMemory)

foreign import ccall unsafe
  "bgfx_make_ref" bgfxMakeRef :: Ptr a -> Word32 -> IO (Ptr BgfxMemory)

type BgfxAttrib = (#type bgfx_attrib_t)

type BgfxAttribType = (#type bgfx_attrib_type_t)

pattern BGFX_ATTRIB_POSITION = (#const BGFX_ATTRIB_POSITION)
pattern BGFX_ATTRIB_NORMAL = (#const BGFX_ATTRIB_NORMAL)
pattern BGFX_ATTRIB_TANGENT = (#const BGFX_ATTRIB_TANGENT)
pattern BGFX_ATTRIB_BITANGENT = (#const BGFX_ATTRIB_BITANGENT)
pattern BGFX_ATTRIB_COLOR0 = (#const BGFX_ATTRIB_COLOR0)
pattern BGFX_ATTRIB_COLOR1 = (#const BGFX_ATTRIB_COLOR1)
pattern BGFX_ATTRIB_INDICES = (#const BGFX_ATTRIB_INDICES)
pattern BGFX_ATTRIB_WEIGHT = (#const BGFX_ATTRIB_WEIGHT)
pattern BGFX_ATTRIB_TEXCOORD0 = (#const BGFX_ATTRIB_TEXCOORD0)
pattern BGFX_ATTRIB_TEXCOORD1 = (#const BGFX_ATTRIB_TEXCOORD1)
pattern BGFX_ATTRIB_TEXCOORD2 = (#const BGFX_ATTRIB_TEXCOORD2)
pattern BGFX_ATTRIB_TEXCOORD3 = (#const BGFX_ATTRIB_TEXCOORD3)
pattern BGFX_ATTRIB_TEXCOORD4 = (#const BGFX_ATTRIB_TEXCOORD4)
pattern BGFX_ATTRIB_TEXCOORD5 = (#const BGFX_ATTRIB_TEXCOORD5)
pattern BGFX_ATTRIB_TEXCOORD6 = (#const BGFX_ATTRIB_TEXCOORD6)
pattern BGFX_ATTRIB_TEXCOORD7 = (#const BGFX_ATTRIB_TEXCOORD7)
pattern BGFX_ATTRIB_COUNT = (#const BGFX_ATTRIB_COUNT)
pattern BGFX_ATTRIB_TYPE_UINT8 = (#const BGFX_ATTRIB_TYPE_UINT8)
pattern BGFX_ATTRIB_TYPE_UINT10 = (#const BGFX_ATTRIB_TYPE_UINT10)
pattern BGFX_ATTRIB_TYPE_INT16 = (#const BGFX_ATTRIB_TYPE_INT16)
pattern BGFX_ATTRIB_TYPE_HALF = (#const BGFX_ATTRIB_TYPE_HALF)
pattern BGFX_ATTRIB_TYPE_FLOAT = (#const BGFX_ATTRIB_TYPE_FLOAT)
pattern BGFX_ATTRIB_TYPE_COUNT = (#const BGFX_ATTRIB_TYPE_COUNT)

data BgfxVertexDecl = BgfxVertexDecl Word32 Word16 (Ptr Word16) (Ptr Word16)

instance Storable BgfxVertexDecl where
  sizeOf ~(BgfxVertexDecl a b c d) = sizeOf a + sizeOf b + sizeOf c + sizeOf d
  peek ptr =
    do BgfxVertexDecl <$> peek (castPtr ptr) <*>
         peek (castPtr (ptr `plusPtr`
                        fromIntegral (sizeOf (undefined :: Word32)))) <*>
         peek (castPtr (ptr `plusPtr`
                        fromIntegral
                          (sizeOf (undefined :: Word32) +
                           sizeOf (undefined :: Word16)))) <*>
         peek (castPtr (ptr `plusPtr`
                        fromIntegral
                          (sizeOf (undefined :: Word32) +
                           sizeOf (undefined :: Word16) +
                           sizeOf (undefined :: Ptr Word16))))
  poke ptr (BgfxVertexDecl a b c d) =
    do poke (castPtr ptr) a
       poke (castPtr (ptr `plusPtr` fromIntegral (sizeOf (undefined :: Word32)))) b
       poke (castPtr (ptr `plusPtr`
                      fromIntegral
                        (sizeOf (undefined :: Word32) +
                         sizeOf (undefined :: Word16))))
            c
       poke (castPtr (ptr `plusPtr`
                      fromIntegral
                        (sizeOf (undefined :: Word32) +
                         sizeOf (undefined :: Word16) +
                         sizeOf (undefined :: Ptr Word16))))
            d
  alignment _ = 0

foreign import ccall unsafe
  "bgfx_create_vertex_buffer" bgfxCreateVertexBuffer :: Ptr BgfxMemory -> Ptr BgfxVertexDecl -> Word16 -> IO BgfxVertexBufferHandle

foreign import ccall unsafe
  "bgfx_vertex_decl_begin" bgfxVertexDeclBegin :: Ptr BgfxVertexDecl -> BgfxRendererType -> IO ()

foreign import ccall unsafe
  "bgfx_vertex_decl_add" bgfxVertexDeclAdd :: Ptr BgfxVertexDecl -> BgfxAttrib -> Word8 -> BgfxAttribType -> Bool -> Bool -> IO ()

foreign import ccall unsafe
  "bgfx_vertex_decl_end" bgfxVertexDeclEnd :: Ptr BgfxVertexDecl -> IO ()

foreign import ccall unsafe
  "bgfx_create_index_buffer" bgfxCreateIndexBuffer :: Ptr BgfxMemory -> Word16 -> IO BgfxIndexBufferHandle

pattern BGFX_BUFFER_NONE = (#const BGFX_BUFFER_NONE)
pattern BGFX_BUFFER_COMPUTE_FORMAT_8x1 = (#const BGFX_BUFFER_COMPUTE_FORMAT_8x1)
pattern BGFX_BUFFER_COMPUTE_FORMAT_8x2 = (#const BGFX_BUFFER_COMPUTE_FORMAT_8x2)
pattern BGFX_BUFFER_COMPUTE_FORMAT_8x4 = (#const BGFX_BUFFER_COMPUTE_FORMAT_8x4)
pattern BGFX_BUFFER_COMPUTE_FORMAT_16x1 = (#const BGFX_BUFFER_COMPUTE_FORMAT_16x1)
pattern BGFX_BUFFER_COMPUTE_FORMAT_16x2 = (#const BGFX_BUFFER_COMPUTE_FORMAT_16x2)
pattern BGFX_BUFFER_COMPUTE_FORMAT_16x4 = (#const BGFX_BUFFER_COMPUTE_FORMAT_16x4)
pattern BGFX_BUFFER_COMPUTE_FORMAT_32x1 = (#const BGFX_BUFFER_COMPUTE_FORMAT_32x1)
pattern BGFX_BUFFER_COMPUTE_FORMAT_32x2 = (#const BGFX_BUFFER_COMPUTE_FORMAT_32x2)
pattern BGFX_BUFFER_COMPUTE_FORMAT_32x4 = (#const BGFX_BUFFER_COMPUTE_FORMAT_32x4)
pattern BGFX_BUFFER_COMPUTE_FORMAT_SHIFT = (#const BGFX_BUFFER_COMPUTE_FORMAT_SHIFT)
pattern BGFX_BUFFER_COMPUTE_FORMAT_MASK = (#const BGFX_BUFFER_COMPUTE_FORMAT_MASK)
pattern BGFX_BUFFER_COMPUTE_TYPE_UINT = (#const BGFX_BUFFER_COMPUTE_TYPE_UINT)
pattern BGFX_BUFFER_COMPUTE_TYPE_INT = (#const BGFX_BUFFER_COMPUTE_TYPE_INT)
pattern BGFX_BUFFER_COMPUTE_TYPE_FLOAT = (#const BGFX_BUFFER_COMPUTE_TYPE_FLOAT)
pattern BGFX_BUFFER_COMPUTE_TYPE_SHIFT = (#const BGFX_BUFFER_COMPUTE_TYPE_SHIFT)
pattern BGFX_BUFFER_COMPUTE_TYPE_MASK = (#const BGFX_BUFFER_COMPUTE_TYPE_MASK)
pattern BGFX_BUFFER_COMPUTE_READ = (#const BGFX_BUFFER_COMPUTE_READ)
pattern BGFX_BUFFER_COMPUTE_WRITE = (#const BGFX_BUFFER_COMPUTE_WRITE)
pattern BGFX_BUFFER_DRAW_INDIRECT = (#const BGFX_BUFFER_DRAW_INDIRECT)
pattern BGFX_BUFFER_ALLOW_RESIZE = (#const BGFX_BUFFER_ALLOW_RESIZE)
pattern BGFX_BUFFER_INDEX32 = (#const BGFX_BUFFER_INDEX32)

-- foreign import ccall unsafe
--   "bgfx_make_ref"BGFX_C_API const bgfx_memory_t* bgfx_make_ref(const void* _data, uint32_t _size);

-- foreign import ccall unsafe
-- BGFX_C_API const bgfx_memory_t* bgfx_make_ref_release(const void* _data, uint32_t _size, bgfx_release_fn_t _releaseFn, void* _userData);

type BgfxShaderHandle = Word16

foreign import ccall unsafe
  "bgfx_create_shader" bgfxCreateShader :: Ptr BgfxMemory -> IO BgfxShaderHandle

foreign import ccall unsafe
  "bgfx_get_shader_uniforms" bgfxGetShaderUniforms :: BgfxShaderHandle -> Ptr BgfxUniformHandle -> Word16 -> IO Word16

foreign import ccall unsafe
  "bgfx_destroy_shader" bgfxDestroyShader :: BgfxShaderHandle -> IO ()

foreign import ccall unsafe
  "bgfx_create_program" bgfxCreateProgram :: BgfxShaderHandle -> BgfxShaderHandle -> Bool -> IO BgfxProgramHandle

foreign import ccall unsafe
  "bgfx_create_compute_program" bgfxCreateComputeProgram :: BgfxShaderHandle -> Bool -> IO BgfxProgramHandle

foreign import ccall unsafe
  "bgfx_destroy_program" bgfxDestroyProgram :: BgfxProgramHandle -> IO ()

type BgfxUniformType = (#type bgfx_uniform_type_t)

pattern BGFX_UNIFORM_TYPE_INT1 = (#const BGFX_UNIFORM_TYPE_INT1)
pattern BGFX_UNIFORM_TYPE_END  = (#const BGFX_UNIFORM_TYPE_END)
pattern BGFX_UNIFORM_TYPE_VEC4 = (#const BGFX_UNIFORM_TYPE_VEC4)
pattern BGFX_UNIFORM_TYPE_MAT3 = (#const BGFX_UNIFORM_TYPE_MAT3)
pattern BGFX_UNIFORM_TYPE_MAT4  = (#const BGFX_UNIFORM_TYPE_MAT4)
pattern BGFX_UNIFORM_TYPE_COUNT = (#const BGFX_UNIFORM_TYPE_COUNT)

foreign import ccall unsafe
  "bgfx_create_uniform" bgfxCreateUniform :: CString -> BgfxUniformType -> Word16 -> IO BgfxUniformHandle

foreign import ccall unsafe
  "bgfx_destroy_uniform" bgfxDestroyUniform :: BgfxUniformHandle -> IO ()

pattern BGFX_CLEAR_NONE = (#const BGFX_CLEAR_NONE)
pattern BGFX_CLEAR_COLOR = (#const BGFX_CLEAR_COLOR)
pattern BGFX_CLEAR_DEPTH = (#const BGFX_CLEAR_DEPTH)
pattern BGFX_CLEAR_STENCIL = (#const BGFX_CLEAR_STENCIL)
pattern BGFX_CLEAR_DISCARD_COLOR_0 = (#const BGFX_CLEAR_DISCARD_COLOR_0)
pattern BGFX_CLEAR_DISCARD_COLOR_1 = (#const BGFX_CLEAR_DISCARD_COLOR_1)
pattern BGFX_CLEAR_DISCARD_COLOR_2 = (#const BGFX_CLEAR_DISCARD_COLOR_2)
pattern BGFX_CLEAR_DISCARD_COLOR_3 = (#const BGFX_CLEAR_DISCARD_COLOR_3)
pattern BGFX_CLEAR_DISCARD_COLOR_4 = (#const BGFX_CLEAR_DISCARD_COLOR_4)
pattern BGFX_CLEAR_DISCARD_COLOR_5 = (#const BGFX_CLEAR_DISCARD_COLOR_5)
pattern BGFX_CLEAR_DISCARD_COLOR_6 = (#const BGFX_CLEAR_DISCARD_COLOR_6)
pattern BGFX_CLEAR_DISCARD_COLOR_7 = (#const BGFX_CLEAR_DISCARD_COLOR_7)
pattern BGFX_CLEAR_DISCARD_DEPTH = (#const BGFX_CLEAR_DISCARD_DEPTH)
pattern BGFX_CLEAR_DISCARD_STENCIL = (#const BGFX_CLEAR_DISCARD_STENCIL)
