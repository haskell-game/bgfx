{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BGFX where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int
import Data.Word
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

#include "bgfx/c99/bgfx.h"

pattern BGFX_PCI_ID_NONE = (#const BGFX_PCI_ID_NONE)
pattern BGFX_PCI_ID_AMD = (#const BGFX_PCI_ID_AMD)
pattern BGFX_PCI_ID_INTEL = (#const BGFX_PCI_ID_INTEL)
pattern BGFX_PCI_ID_NVIDIA = (#const BGFX_PCI_ID_NVIDIA)

data BgfxCallback

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

pattern BGFX_DEBUG_NONE = (#const BGFX_DEBUG_NONE)
pattern BGFX_DEBUG_WIREFRAME = (#const BGFX_DEBUG_WIREFRAME)
pattern BGFX_DEBUG_IFH = (#const BGFX_DEBUG_IFH)
pattern BGFX_DEBUG_STATS = (#const BGFX_DEBUG_STATS)
pattern BGFX_DEBUG_TEXT = (#const BGFX_DEBUG_TEXT)

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

data BgfxCaps

data BgfxStats

data BgfxHMD

type BgfxBackbufferRatio = (#type bgfx_backbuffer_ratio_t)

pattern BGFX_BACKBUFFER_RATIO_EQUAL = (#const BGFX_BACKBUFFER_RATIO_EQUAL)
pattern BGFX_BACKBUFFER_RATIO_HALF = (#const BGFX_BACKBUFFER_RATIO_HALF)
pattern BGFX_BACKBUFFER_RATIO_QUARTER = (#const BGFX_BACKBUFFER_RATIO_QUARTER)
pattern BGFX_BACKBUFFER_RATIO_EIGHTH = (#const BGFX_BACKBUFFER_RATIO_EIGHTH)
pattern BGFX_BACKBUFFER_RATIO_SIXTEENTH = (#const BGFX_BACKBUFFER_RATIO_SIXTEENTH)
pattern BGFX_BACKBUFFER_RATIO_DOUBLE = (#const BGFX_BACKBUFFER_RATIO_DOUBLE)

type BgfxFrameBufferHandle = Word16

data BgfxAllocator

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

type BgfxOcclusionQueryHandle = Word16

type BgfxIndexBufferHandle = Word16

data BgfxIndexBuffer

type BgfxVertexBufferHandle = Word16

type BgfxDynamicVertexBufferHandle = Word16

data BgfxVertexBuffer

type BgfxInstanceDataBufferHandle = Word16

data BgfxInstanceDataBuffer

type BgfxUniformHandle = Word16

type BgfxTextureHandle = Word16

type BgfxProgramHandle = Word16

type BgfxIndirectBufferHandle = Word16

pattern BGFX_ACCESS_READ = (#const BGFX_ACCESS_READ)
pattern BGFX_ACCESS_WRITE = (#const BGFX_ACCESS_WRITE)
pattern BGFX_ACCESS_READWRITE = (#const BGFX_ACCESS_READWRITE)
pattern BGFX_ACCESS_COUNT = (#const BGFX_ACCESS_COUNT)

type BgfxAccess = (#type bgfx_access_t)

type BgfxDynamicIndexBufferHandle = Word16

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

data BgfxMemory

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
-- BGFX_C_API const bgfx_memory_t* bgfx_make_ref_release(const void* _data, uint32_t _size, bgfx_release_fn_t _releaseFn, void* _userData);

type BgfxShaderHandle = Word16

type BgfxUniformType = (#type bgfx_uniform_type_t)

pattern BGFX_UNIFORM_TYPE_INT1 = (#const BGFX_UNIFORM_TYPE_INT1)
pattern BGFX_UNIFORM_TYPE_END  = (#const BGFX_UNIFORM_TYPE_END)
pattern BGFX_UNIFORM_TYPE_VEC4 = (#const BGFX_UNIFORM_TYPE_VEC4)
pattern BGFX_UNIFORM_TYPE_MAT3 = (#const BGFX_UNIFORM_TYPE_MAT3)
pattern BGFX_UNIFORM_TYPE_MAT4  = (#const BGFX_UNIFORM_TYPE_MAT4)
pattern BGFX_UNIFORM_TYPE_COUNT = (#const BGFX_UNIFORM_TYPE_COUNT)

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

data BgfxTextureInfo

data BgfxTransientVertexBuffer

data BgfxTransientIndexBuffer

pattern BGFX_OCCLUSION_QUERY_RESULT_INVISIBLE = (#const BGFX_OCCLUSION_QUERY_RESULT_INVISIBLE)
pattern BGFX_OCCLUSION_QUERY_RESULT_VISIBLE = (#const BGFX_OCCLUSION_QUERY_RESULT_VISIBLE)
pattern BGFX_OCCLUSION_QUERY_RESULT_NORESULT = (#const BGFX_OCCLUSION_QUERY_RESULT_NORESULT)
pattern BGFX_OCCLUSION_QUERY_RESULT_COUNT = (#const BGFX_OCCLUSION_QUERY_RESULT_COUNT )

type BgfxOcclusionQueryResult = (#type bgfx_occlusion_query_result_t)

foreign import ccall unsafe "bgfx_init" bgfxInitFFI :: BgfxRendererType -> Word16 -> Word16 -> Ptr BgfxCallback -> Ptr BgfxAllocator -> IO Bool
foreign import ccall unsafe "bgfx_shutdown" bgfxShutdownFFI :: IO ()
foreign import ccall unsafe "bgfx_reset" bgfxResetFFI :: Word32 -> Word32 -> Word32 -> IO () 
foreign import ccall unsafe "bgfx_frame" bgfxFrameFFI :: IO Word32 
foreign import ccall unsafe "bgfx_set_debug" bgfxSetDebugFFI :: Word32 -> IO () 
foreign import ccall unsafe "bgfx_dbg_text_clear" bgfxDbgTextClearFFI :: Word8 -> Bool -> IO () 
foreign import ccall unsafe "bgfx_get_renderer_type" bgfxGetRendererTypeFFI :: IO BgfxRendererType 
foreign import ccall unsafe "bgfx_get_caps" bgfxGetCapsFFI :: IO (Ptr BgfxCaps)
foreign import ccall unsafe "bgfx_get_stats" bgfxGetStatsFFI :: IO (Ptr BgfxStats) 
foreign import ccall unsafe "bgfx_get_hmd" bgfxGetHMDFFI :: IO (Ptr BgfxHMD)
foreign import ccall unsafe "bgfx_render_frame" bgfxRenderFrameFFI :: IO () 
foreign import ccall unsafe "bgfx_discard" bgfxDiscardFFI :: IO () 
foreign import ccall unsafe "bgfx_touch" bgfxTouchFFI :: Word8 -> IO Word32 
foreign import ccall unsafe "bgfx_set_palette_color" bgxSetPaletteColorFFI :: Word8 -> Ptr CFloat -> IO () 
foreign import ccall unsafe "bgfx_save_screen_shot" bgfxSaveScreenshotFFI :: CString -> IO () 
foreign import ccall unsafe "bgfx_set_view_name" bgfxSetViewNameFFI :: Word8 -> CString -> IO () 
foreign import ccall unsafe "bgfx_set_view_rect" bgfxSetViewRectFFI :: Word8 -> Word16 -> Word16 -> Word16 -> Word16 -> IO () 
foreign import ccall unsafe "bgfx_set_view_rect_auto" bgfxSetViewRectAutoFFI :: Word8 -> Word16 -> Word16 -> BgfxBackbufferRatio -> IO ()
foreign import ccall unsafe "bgfx_set_view_scissor" bgfxSetViewScissorFFI :: Word8 -> Word16 -> Word16 -> Word16 -> Word16 -> IO () 
foreign import ccall unsafe "bgfx_set_view_clear" bgfxSetViewClearFFI :: Word8 -> Word16 -> Word32 -> CFloat -> Word8 -> IO ()
foreign import ccall unsafe "bgfx_set_view_clear_mrt" bgfxSetViewClearMrtFFI :: Word8 -> Word16 -> CFloat -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> IO ()
foreign import ccall unsafe "bgfx_set_view_seq" bgfxSetViewSeqFFI :: Word8 -> Bool -> IO ()
foreign import ccall unsafe "bgfx_set_view_transform" bgfxSetViewTransformFFI :: Word8 -> Ptr a -> Ptr b -> IO ()
foreign import ccall unsafe "bgfx_set_view_transform_stereo" bgfxSetViewTransformStereoFFI :: Word8 -> Ptr a -> Ptr b -> Word8 -> Ptr () -> IO ()
foreign import ccall unsafe "bgfx_set_view_remap" bgfxSetVieRemapFFI :: Word8 -> Word8 -> Ptr a -> IO ()
foreign import ccall unsafe "bgfx_set_view_frame_buffer" bgfxSetViewframeBufferFFI :: Word8 -> BgfxFrameBufferHandle -> IO ()
foreign import ccall unsafe "bgfx_set_marker" bgfxSetMarkerFFI :: CString -> IO () 
foreign import ccall unsafe "bgfx_set_state" bgfxSetStateFFI :: Word64 -> Word32 -> IO () 
foreign import ccall unsafe "bgfx_set_stencil" bgfxSetStencilFFI :: Word32 -> Word32 -> IO ()
foreign import ccall unsafe "bgfx_set_scissor" bgfxSetScissorFFI :: Word16 -> Word16 -> Word16 -> Word16 -> IO Word16 
foreign import ccall unsafe "bgfx_set_scissor_cached" bgfxSetScissorCachedFFI :: Word16 -> IO () 
foreign import ccall unsafe "bgfx_set_transform" bgfxSetTransformFFI :: Ptr a -> Word16 -> IO Word32
foreign import ccall unsafe "bgfx_set_condition" bgfxSetConditionFFI :: BgfxOcclusionQueryHandle -> Bool -> IO () 
foreign import ccall unsafe "bgfx_set_index_buffer" bgfxSetIndexBufferFFI :: BgfxIndexBufferHandle -> Word32 -> Word32 -> IO () 
foreign import ccall unsafe "bgfx_set_dynamic_index_buffer" bgfxSetDynamicIndexBufferFFI :: BgfxIndexBufferHandle -> Word32 -> Word32 -> IO () 
foreign import ccall unsafe "bgfx_set_transient_index_buffer" bgfxSetTransientIndexBufferFFI :: Ptr BgfxIndexBuffer -> Word32 -> Word32 -> IO ()
foreign import ccall unsafe "bgfx_set_vertex_buffer" bgfxSetVertexBufferFFI :: BgfxVertexBufferHandle -> Word32 -> Word32 -> IO () 
foreign import ccall unsafe "bgfx_set_dynamic_vertex_buffer" bgfxSetDynamicVertexBufferFFI :: BgfxVertexBufferHandle -> Word32 -> IO ()
foreign import ccall unsafe "bgfx_set_transient_vertex_buffer" bgfxSetTransientVertexBufferFFI :: Ptr BgfxVertexBuffer -> Word32 -> Word32 -> IO ()
foreign import ccall unsafe "bgfx_set_instance_data_buffer" bgfxSetInstanceDataBufferFFI :: Ptr BgfxInstanceDataBuffer -> Word32 -> IO () 
foreign import ccall unsafe "bgfx_set_instance_data_from_vertex_buffer" bgfxSetInstanceDataBufferFromVertexBufferFFI :: BgfxVertexBufferHandle -> Word32 -> Word32 -> IO () 
foreign import ccall unsafe "bgfx_set_instance_data_from_dynamic_vertex_buffer" bgfxSetInstanceDataBufferFromDynamicVertexBufferFFI :: BgfxDynamicVertexBufferHandle -> Word32 -> Word32 -> IO () 
foreign import ccall unsafe "bgfx_set_texture" bgfxSetTextureFFI :: Word8 -> BgfxUniformHandle -> BgfxTextureHandle -> Word32 -> IO () 
foreign import ccall unsafe "bgfx_set_texture_from_frame_buffer" bgfxSetTextureFromFrameBufferFFI :: Word8 -> BgfxUniformHandle -> BgfxFrameBufferHandle -> Word8 -> Word32 -> IO () 
foreign import ccall unsafe "bgfx_submit" bgfxSubmitFFI :: Word8 -> BgfxProgramHandle -> Int32 -> IO Word32 
foreign import ccall unsafe "bgfx_submit_occlusion_query" bgfxSubmitOcclusionQueryFFI :: Word8 -> BgfxProgramHandle -> BgfxOcclusionQueryHandle -> Int32 -> IO Word32
foreign import ccall unsafe "bgfx_submit_indirect" bgfxSubmitIndirectFFI :: Word8 -> BgfxProgramHandle -> BgfxIndirectBufferHandle -> Word16 -> Word16 -> Int32 -> IO Word32
foreign import ccall unsafe "bgfx_set_compute_index_buffer" bgfxSetComputeIndexBufferFFI :: Word8 -> BgfxIndexBufferHandle -> BgfxAccess -> IO () 
foreign import ccall unsafe "bgfx_set_compute_vertex_buffer" bgfxSetComputeVertexBufferFFI :: Word8 -> BgfxVertexBufferHandle -> BgfxAccess -> IO () 
foreign import ccall unsafe "bgfx_set_compute_dynamic_index_buffer" bgxSetComputeDynamicIndexBufferFFI :: Word8 -> BgfxDynamicIndexBufferHandle -> BgfxAccess -> IO () 
foreign import ccall unsafe "bgfx_set_compute_dynamic_vertex_buffer" bgfxSetComputeDynamicVertexBufferFFI :: Word8 -> BgfxDynamicVertexBufferHandle -> BgfxAccess -> IO () 
foreign import ccall unsafe "bgfx_set_compute_indirect_buffer" bgfxSetComputeIndirectBufferFFI :: Word8 -> BgfxIndirectBufferHandle -> BgfxAccess -> IO () 
foreign import ccall unsafe "bgfx_set_image" bgfxSetImageFFI :: Word8 -> BgfxUniformHandle -> BgfxTextureHandle -> Word8 -> BgfxAccess -> BgfxTextureFormat -> IO () 
foreign import ccall unsafe "bgfx_set_image_from_frame_buffer" bgfxSetImageFromFrameBufferFFI :: Word8 -> BgfxUniformHandle -> BgfxFrameBufferHandle -> Word8 -> BgfxAccess -> BgfxTextureFormat -> IO () 
foreign import ccall unsafe "bgfx_dispatch" bgfxDispatchFFI :: Word8 -> BgfxProgramHandle -> Word16 -> Word16 -> Word16 -> Word8 -> IO Word32 
foreign import ccall unsafe "bgfx_dispatch_indirect" bgfxDispatchIndirectFFI :: Word8 -> BgfxProgramHandle -> BgfxIndirectBufferHandle -> Word16 -> Word16 -> Word16 -> Word8 -> IO Word32 
foreign import ccall unsafe "bgfx_blit" bgfxBlitFFI :: Word8 -> BgfxTextureHandle -> Word8 -> Word16 -> Word16 -> Word16 -> BgfxTextureHandle -> Word8 -> Word16 -> Word16 -> Word16 -> Word16-> Word16 -> Word16 -> IO () 
foreign import ccall unsafe "bgfx_blit_frame_buffer" bgfxBlitFrameBufferFFI :: Word8 -> BgfxTextureHandle -> Word8 -> Word16 -> Word16 -> Word16 -> BgfxFrameBufferHandle -> Word8 -> Word8 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> IO () 
foreign import ccall unsafe "bgfx_alloc" bgfxAllocFFI :: Word32 -> IO (Ptr BgfxMemory) 
foreign import ccall unsafe "bgfx_copy" bgfxCopyFFI :: Ptr a -> Word32 -> IO (Ptr BgfxMemory) 
foreign import ccall unsafe "bgfx_make_ref" bgfxMakeRefFFI :: Ptr a -> Word32 -> IO (Ptr BgfxMemory)
foreign import ccall unsafe "bgfx_create_vertex_buffer" bgfxCreateVertexBufferFFI :: Ptr BgfxMemory -> Ptr BgfxVertexDecl -> Word16 -> IO BgfxVertexBufferHandle 
foreign import ccall unsafe "bgfx_vertex_decl_begin" bgfxVertexDeclBeginFFI :: Ptr BgfxVertexDecl -> BgfxRendererType -> IO () 
foreign import ccall unsafe "bgfx_vertex_decl_add" bgfxVertexDeclAddFFI :: Ptr BgfxVertexDecl -> BgfxAttrib -> Word8 -> BgfxAttribType -> Bool -> Bool -> IO () 
foreign import ccall unsafe "bgfx_vertex_decl_end" bgfxVertexDeclEndFFI :: Ptr BgfxVertexDecl -> IO () 
foreign import ccall unsafe "bgfx_create_index_buffer" bgfxCreateIndexBufferFFI :: Ptr BgfxMemory -> Word16 -> IO BgfxIndexBufferHandle 
foreign import ccall unsafe "bgfx_create_shader" bgfxCreateShaderFFI :: Ptr BgfxMemory -> IO BgfxShaderHandle 
foreign import ccall unsafe "bgfx_get_shader_uniforms" bgfxGetShaderUniformsFFI :: BgfxShaderHandle -> Ptr BgfxUniformHandle -> Word16 -> IO Word16 
foreign import ccall unsafe "bgfx_destroy_shader" bgfxDestroyShaderFFI :: BgfxShaderHandle -> IO () 
foreign import ccall unsafe "bgfx_create_program" bgfxCreateProgramFFI :: BgfxShaderHandle -> BgfxShaderHandle -> Bool -> IO BgfxProgramHandle 
foreign import ccall unsafe "bgfx_create_compute_program" bgfxCreateComputeProgramFFI :: BgfxShaderHandle -> Bool -> IO BgfxProgramHandle 
foreign import ccall unsafe "bgfx_destroy_program" bgfxDestroyProgramFFI :: BgfxProgramHandle -> IO () 
foreign import ccall unsafe "bgfx_create_uniform" bgfxCreateUniformFFI :: CString -> BgfxUniformType -> Word16 -> IO BgfxUniformHandle 
foreign import ccall unsafe "bgfx_destroy_uniform" bgfxDestroyUniformFFI :: BgfxUniformHandle -> IO () 
foreign import ccall unsafe "bgfx_calc_texture_size" bgfxCalcTextureSizeFFI :: Ptr BgfxTextureInfo -> Word16 -> Word16 -> Word16 -> Bool -> Word8 -> BgfxTextureFormat -> IO ()
foreign import ccall unsafe "bgfx_create_texture" bgfxCreateTextureFFI :: Ptr BgfxMemory -> Word32 -> Word8 -> Ptr BgfxTextureInfo -> IO BgfxTextureHandle 
foreign import ccall unsafe "bgfx_create_texture_2d" bgfxCreateTexture2DFFI :: Word16 -> Word16 -> Word8 -> BgfxTextureFormat -> Word32 -> Ptr BgfxMemory -> IO BgfxTextureHandle 
foreign import ccall unsafe "bgfx_create_texture_2d_scaled" bgfxCreateTexture2DScaledFFI :: BgfxBackbufferRatio -> Word8 -> BgfxTextureFormat -> Word32 -> IO BgfxTextureHandle 
foreign import ccall unsafe "bgfx_create_texture_3d" bgfxCreateTexture3DFFI :: Word16 -> Word16 -> Word16 -> Word8 -> BgfxTextureFormat -> Word32 -> Ptr BgfxMemory -> IO BgfxTextureHandle 
foreign import ccall unsafe "bgfx_create_texture_cube" bgfxCreateTextureCubeFFI :: Word16 -> Word8 -> BgfxTextureFormat -> Word32 -> Ptr BgfxMemory -> IO BgfxTextureHandle 
foreign import ccall unsafe "bgfx_update_texture_2d" bgfxUpdateTexture2DFFI :: BgfxTextureHandle -> Word8 -> Word16 -> Word16 -> Word16 -> Word16 -> Ptr BgfxMemory -> Word16 -> IO () 
foreign import ccall unsafe "bgfx_update_texture_3d" bgfxUpdateTexture3DFFI :: BgfxTextureHandle -> Word8 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Ptr BgfxMemory -> IO () 
foreign import ccall unsafe "bgfx_update_texture_cube" bgfxUpdateTextureCubeFFI :: BgfxTextureHandle -> Word8 -> Word8 -> Word16 -> Word16 -> Word16 -> Word16 -> Ptr BgfxMemory -> Word16 -> IO () 
foreign import ccall unsafe "bgfx_read_texture" bgfxReadTextureFFI :: BgfxTextureHandle -> Ptr a -> IO () 
foreign import ccall unsafe "bgfx_read_frame_buffer" bgfxReadFrameBufferFFI :: BgfxFrameBufferHandle -> Word8 -> Ptr a -> IO () 
foreign import ccall unsafe "bgfx_destroy_texture" bgfxDestroyTextureFFI :: BgfxTextureHandle -> IO () 
foreign import ccall unsafe "bgfx_create_frame_buffer" bgfxCreateFrameBufferFFI :: Word16 -> Word16 -> BgfxTextureFormat -> Word32 -> IO BgfxFrameBufferHandle 
foreign import ccall unsafe "bgfx_create_frame_buffer_scaled" bgfxCreateFrameBufferScaledFFI :: BgfxBackbufferRatio -> BgfxTextureFormat -> Word32 -> IO BgfxFrameBufferHandle 
foreign import ccall unsafe "bgfx_create_frame_buffer_from_handles" bgfxCreateFrameBufferFromHandlesFFI :: Word8 -> Ptr BgfxTextureHandle -> Bool -> IO BgfxFrameBufferHandle 
foreign import ccall unsafe "bgfx_create_frame_buffer_from_nwh" bgfxCreateFrameBufferFromNWHFFI :: Ptr a -> Word16 -> Word16 -> BgfxTextureFormat -> IO BgfxFrameBufferHandle 
foreign import ccall unsafe "bgfx_destroy_frame_buffer" bgfxDestroyFrameBufferFFI :: BgfxFrameBufferHandle -> IO () 
foreign import ccall unsafe "bgfx_check_avail_transient_index_buffer" bgfxCheckAvailTransientIndexBufferFFI :: Word32 -> IO Bool 
foreign import ccall unsafe "bgfx_check_avail_transient_vertex_buffer" bgfxCheckAvailTransientVertexBufferFFI :: Word32 -> Ptr BgfxVertexDecl -> IO Bool 
foreign import ccall unsafe "bgfx_check_avail_instance_data_buffer" bgfxCheckAvailInstanceDataBufferFFI :: Word32 -> Word16 -> IO Bool 
foreign import ccall unsafe "bgfx_check_avail_transient_buffers" bgfxCheckAvailTransientBuffersFFI :: Word32 -> Ptr BgfxVertexDecl -> Word32 -> IO Bool 
foreign import ccall unsafe "bgfx_alloc_transient_index_buffer" bgfxAllocTransientIndexBufferFFI :: Ptr BgfxTransientIndexBuffer -> Word32 -> IO () 
foreign import ccall unsafe "bgfx_alloc_transient_vertex_buffer" bgfxAllocTransientVertexBufferFFI :: Ptr BgfxTransientVertexBuffer -> Word32 -> Ptr BgfxVertexDecl -> IO () 
foreign import ccall unsafe "bgfx_alloc_transient_buffers" bgfxAllocTransientBuffersFFI :: Ptr BgfxTransientVertexBuffer -> Ptr BgfxVertexDecl -> Word32 -> Ptr BgfxTransientIndexBuffer -> Word32 -> IO () 
foreign import ccall unsafe "bgfx_alloc_instance_data_buffer" bgfxAllocInstanceDataBufferFFI :: Word32 -> Word16 -> IO (Ptr BgfxInstanceDataBuffer) 
foreign import ccall unsafe "bgfx_create_indirect_buffer" bgfxCreateIndirectBufferFFI :: Word32 -> IO BgfxIndirectBufferHandle 
foreign import ccall unsafe "bgfx_destroy_indirect_buffer" bgfxDestroyIndirectBufferFFI :: BgfxIndirectBufferHandle -> IO () 
foreign import ccall unsafe "bgfx_create_occlusion_query" bgfxCreateOcclusionQueryFFI :: IO BgfxOcclusionQueryHandle 
foreign import ccall unsafe "bgfx_get_result" bgfxGetResultFFI :: BgfxOcclusionQueryHandle -> IO BgfxOcclusionQueryResult 
foreign import ccall unsafe "bgfx_destroy_occlusion_query" bgfxDestroyOcclusionQueryFFI :: BgfxOcclusionQueryHandle -> IO () 

bgfxInit :: MonadIO m
         => BgfxRendererType
         -> Word16
         -> Word16
         -> Ptr BgfxCallback
         -> Ptr BgfxAllocator
         -> m Bool
bgfxInit a b c d e = liftIO (bgfxInitFFI a b c d e)

{-# INLINE bgfxInit #-}

bgfxShutdown :: MonadIO m
             => m ()
bgfxShutdown = liftIO bgfxShutdownFFI

{-# INLINE bgfxShutdown #-}

bgfxReset :: MonadIO m
          => Word32 -> Word32 -> Word32 -> m ()
bgfxReset a b c = liftIO (bgfxResetFFI a b c)

{-# INLINE bgfxReset #-}

bgfxFrame :: MonadIO m
          => m Word32
bgfxFrame = liftIO bgfxFrameFFI

{-# INLINE bgfxFrame #-}

bgfxSetDebug :: MonadIO m
             => Word32 -> m ()
bgfxSetDebug a = liftIO (bgfxSetDebugFFI a)

{-# INLINE bgfxSetDebug #-}

bgfxDbgTextClear :: MonadIO m
                 => Word8 -> Bool -> m ()
bgfxDbgTextClear a b = liftIO (bgfxDbgTextClearFFI a b)

{-# INLINE bgfxDbgTextClear #-}

bgfxGetRendererType :: MonadIO m
                    => m BgfxRendererType
bgfxGetRendererType = liftIO (bgfxGetRendererTypeFFI)

{-# INLINE bgfxGetRendererType #-}

bgfxGetCaps :: MonadIO m
            => m (Ptr BgfxCaps)
bgfxGetCaps = liftIO (bgfxGetCapsFFI)

{-# INLINE bgfxGetCaps #-}

bgfxGetStats :: MonadIO m
             => m (Ptr BgfxStats)
bgfxGetStats = liftIO (bgfxGetStatsFFI)

{-# INLINE bgfxGetStats #-}

bgfxGetHMD :: MonadIO m
           => m (Ptr BgfxHMD)
bgfxGetHMD = liftIO (bgfxGetHMDFFI)

{-# INLINE bgfxGetHMD #-}

bgfxRenderFrame :: MonadIO m
                => m ()
bgfxRenderFrame = liftIO (bgfxRenderFrameFFI)

{-# INLINE bgfxRenderFrame #-}

bgfxDiscard :: MonadIO m
            => m ()
bgfxDiscard = liftIO (bgfxDiscardFFI)

{-# INLINE bgfxDiscard #-}

bgfxTouch :: MonadIO m
          => Word8 -> m Word32
bgfxTouch a = liftIO (bgfxTouchFFI a)

{-# INLINE bgfxTouch #-}

bgxSetPaletteColor :: MonadIO m
                   => Word8 -> Ptr CFloat -> m ()
bgxSetPaletteColor a b = liftIO (bgxSetPaletteColorFFI a b)

{-# INLINE bgxSetPaletteColor #-}

bgfxSaveScreenshot :: MonadIO m
                   => CString -> m ()
bgfxSaveScreenshot a = liftIO (bgfxSaveScreenshotFFI a)

{-# INLINE bgfxSaveScreenshot #-}

bgfxSetViewName :: MonadIO m
                => Word8 -> CString -> m ()
bgfxSetViewName a b = liftIO (bgfxSetViewNameFFI a b)

{-# INLINE bgfxSetViewName #-}

bgfxSetViewRect
  :: MonadIO m
  => Word8 -> Word16 -> Word16 -> Word16 -> Word16 -> m ()
bgfxSetViewRect a b c d e = liftIO (bgfxSetViewRectFFI a b c d e)

{-# INLINE bgfxSetViewRect #-}

bgfxSetViewRectAuto
  :: MonadIO m
  => Word8 -> Word16 -> Word16 -> BgfxBackbufferRatio -> m ()
bgfxSetViewRectAuto a b c d = liftIO (bgfxSetViewRectAutoFFI a b c d)

{-# INLINE bgfxSetViewRectAuto #-}

bgfxSetViewScissor
  :: MonadIO m
  => Word8 -> Word16 -> Word16 -> Word16 -> Word16 -> m ()
bgfxSetViewScissor a b c d e = liftIO (bgfxSetViewScissorFFI a b c d e)

{-# INLINE bgfxSetViewScissor #-}

bgfxSetViewClear
  :: MonadIO m
  => Word8 -> Word16 -> Word32 -> CFloat -> Word8 -> m ()
bgfxSetViewClear a b c d e = liftIO (bgfxSetViewClearFFI a b c d e)

{-# INLINE bgfxSetViewClear #-}

bgfxSetViewClearMrt :: MonadIO m
                    => Word8
                    -> Word16
                    -> CFloat
                    -> Word8
                    -> Word8
                    -> Word8
                    -> Word8
                    -> Word8
                    -> Word8
                    -> Word8
                    -> Word8
                    -> Word8
                    -> m ()
bgfxSetViewClearMrt a b c d e f g h i j k m =
  liftIO (bgfxSetViewClearMrtFFI a b c d e f g h i j k m)

{-# INLINE bgfxSetViewClearMrt #-}

bgfxSetViewSeq :: MonadIO m
               => Word8 -> Bool -> m ()
bgfxSetViewSeq a b = liftIO (bgfxSetViewSeqFFI a b)

{-# INLINE bgfxSetViewSeq #-}

bgfxSetViewTransform
  :: MonadIO m
  => Word8 -> Ptr a -> Ptr b -> m ()
bgfxSetViewTransform a b c = liftIO (bgfxSetViewTransformFFI a b c)

{-# INLINE bgfxSetViewTransform #-}

bgfxSetViewTransformStereo :: MonadIO m
                           => Word8
                           -> Ptr a
                           -> Ptr b
                           -> Word8
                           -> Ptr ()
                           -> m ()
bgfxSetViewTransformStereo a b c d e =
  liftIO (bgfxSetViewTransformStereoFFI a b c d e)

{-# INLINE bgfxSetViewTransformStereo #-}

bgfxSetVieRemap
  :: MonadIO m
  => Word8 -> Word8 -> Ptr a -> m ()
bgfxSetVieRemap a b c = liftIO (bgfxSetVieRemapFFI a b c)

{-# INLINE bgfxSetVieRemap #-}

bgfxSetViewframeBuffer
  :: MonadIO m
  => Word8 -> BgfxFrameBufferHandle -> m ()
bgfxSetViewframeBuffer a b = liftIO (bgfxSetViewframeBufferFFI a b)

{-# INLINE bgfxSetViewframeBuffer #-}

bgfxSetMarker :: MonadIO m
              => CString -> m ()
bgfxSetMarker a = liftIO (bgfxSetMarkerFFI a)

{-# INLINE bgfxSetMarker #-}

bgfxSetState :: MonadIO m
             => Word64 -> Word32 -> m ()
bgfxSetState a b = liftIO (bgfxSetStateFFI a b)

{-# INLINE bgfxSetState #-}

bgfxSetStencil :: MonadIO m
               => Word32 -> Word32 -> m ()
bgfxSetStencil a b = liftIO (bgfxSetStencilFFI a b)

{-# INLINE bgfxSetStencil #-}

bgfxSetScissor
  :: MonadIO m
  => Word16 -> Word16 -> Word16 -> Word16 -> m Word16
bgfxSetScissor a b c d = liftIO (bgfxSetScissorFFI a b c d)

{-# INLINE bgfxSetScissor #-}

bgfxSetScissorCached :: MonadIO m
                     => Word16 -> m ()
bgfxSetScissorCached a = liftIO (bgfxSetScissorCachedFFI a)

{-# INLINE bgfxSetScissorCached #-}

bgfxSetTransform :: MonadIO m
                 => Ptr a -> Word16 -> m Word32
bgfxSetTransform a b = liftIO (bgfxSetTransformFFI a b)

{-# INLINE bgfxSetTransform #-}

bgfxSetCondition
  :: MonadIO m
  => BgfxOcclusionQueryHandle -> Bool -> m ()
bgfxSetCondition a b = liftIO (bgfxSetConditionFFI a b)

{-# INLINE bgfxSetCondition #-}

bgfxSetIndexBuffer
  :: MonadIO m
  => BgfxIndexBufferHandle -> Word32 -> Word32 -> m ()
bgfxSetIndexBuffer a b c = liftIO (bgfxSetIndexBufferFFI a b c)

{-# INLINE bgfxSetIndexBuffer #-}

bgfxSetDynamicIndexBuffer
  :: MonadIO m
  => BgfxIndexBufferHandle -> Word32 -> Word32 -> m ()
bgfxSetDynamicIndexBuffer a b c = liftIO (bgfxSetDynamicIndexBufferFFI a b c)

{-# INLINE bgfxSetDynamicIndexBuffer #-}

bgfxSetTransientIndexBuffer
  :: MonadIO m
  => Ptr BgfxIndexBuffer -> Word32 -> Word32 -> m ()
bgfxSetTransientIndexBuffer a b c =
  liftIO (bgfxSetTransientIndexBufferFFI a b c)

{-# INLINE bgfxSetTransientIndexBuffer #-}

bgfxSetVertexBuffer
  :: MonadIO m
  => BgfxVertexBufferHandle -> Word32 -> Word32 -> m ()
bgfxSetVertexBuffer a b c = liftIO (bgfxSetVertexBufferFFI a b c)

{-# INLINE bgfxSetVertexBuffer #-}

bgfxSetDynamicVertexBuffer
  :: MonadIO m
  => BgfxVertexBufferHandle -> Word32 -> m ()
bgfxSetDynamicVertexBuffer a b = liftIO (bgfxSetDynamicVertexBufferFFI a b)

{-# INLINE bgfxSetDynamicVertexBuffer #-}

bgfxSetTransientVertexBuffer :: MonadIO m
                             => Ptr BgfxVertexBuffer
                             -> Word32
                             -> Word32
                             -> m ()
bgfxSetTransientVertexBuffer a b c =
  liftIO (bgfxSetTransientVertexBufferFFI a b c)

{-# INLINE bgfxSetTransientVertexBuffer #-}

bgfxSetInstanceDataBuffer
  :: MonadIO m
  => Ptr BgfxInstanceDataBuffer -> Word32 -> m ()
bgfxSetInstanceDataBuffer a b = liftIO (bgfxSetInstanceDataBufferFFI a b)

{-# INLINE bgfxSetInstanceDataBuffer #-}

bgfxSetInstanceDataBufferFromVertexBuffer :: MonadIO m
                                          => BgfxVertexBufferHandle
                                          -> Word32
                                          -> Word32
                                          -> m ()
bgfxSetInstanceDataBufferFromVertexBuffer a b c =
  liftIO (bgfxSetInstanceDataBufferFromVertexBufferFFI a b c)

{-# INLINE bgfxSetInstanceDataBufferFromVertexBuffer #-}

bgfxSetInstanceDataBufferFromDynamicVertexBuffer
  :: MonadIO m
  => BgfxDynamicVertexBufferHandle -> Word32 -> Word32 -> m ()
bgfxSetInstanceDataBufferFromDynamicVertexBuffer a b c =
  liftIO (bgfxSetInstanceDataBufferFromDynamicVertexBufferFFI a b c)

{-# INLINE bgfxSetInstanceDataBufferFromDynamicVertexBuffer #-}

bgfxSetTexture :: MonadIO m
               => Word8
               -> BgfxUniformHandle
               -> BgfxTextureHandle
               -> Word32
               -> m ()
bgfxSetTexture a b c d = liftIO (bgfxSetTextureFFI a b c d)

{-# INLINE bgfxSetTexture #-}

bgfxSetTextureFromFrameBuffer :: MonadIO m
                              => Word8
                              -> BgfxUniformHandle
                              -> BgfxFrameBufferHandle
                              -> Word8
                              -> Word32
                              -> m ()
bgfxSetTextureFromFrameBuffer a b c d e =
  liftIO (bgfxSetTextureFromFrameBufferFFI a b c d e)

{-# INLINE bgfxSetTextureFromFrameBuffer #-}

bgfxSubmit
  :: MonadIO m
  => Word8 -> BgfxProgramHandle -> Int32 -> m Word32
bgfxSubmit a b c = liftIO (bgfxSubmitFFI a b c)

{-# INLINE bgfxSubmit #-}

bgfxSubmitOcclusionQuery :: MonadIO m
                         => Word8
                         -> BgfxProgramHandle
                         -> BgfxOcclusionQueryHandle
                         -> Int32
                         -> m Word32
bgfxSubmitOcclusionQuery a b c d = liftIO (bgfxSubmitOcclusionQueryFFI a b c d)

{-# INLINE bgfxSubmitOcclusionQuery #-}

bgfxSubmitIndirect :: MonadIO m
                   => Word8
                   -> BgfxProgramHandle
                   -> BgfxIndirectBufferHandle
                   -> Word16
                   -> Word16
                   -> Int32
                   -> m Word32
bgfxSubmitIndirect a b c d e f = liftIO (bgfxSubmitIndirectFFI a b c d e f)

{-# INLINE bgfxSubmitIndirect #-}

bgfxSetComputeIndexBuffer :: MonadIO m
                          => Word8
                          -> BgfxIndexBufferHandle
                          -> BgfxAccess
                          -> m ()
bgfxSetComputeIndexBuffer a b c = liftIO (bgfxSetComputeIndexBufferFFI a b c)

{-# INLINE bgfxSetComputeIndexBuffer #-}

bgfxSetComputeVertexBuffer :: MonadIO m
                           => Word8
                           -> BgfxVertexBufferHandle
                           -> BgfxAccess
                           -> m ()
bgfxSetComputeVertexBuffer a b c = liftIO (bgfxSetComputeVertexBufferFFI a b c)

{-# INLINE bgfxSetComputeVertexBuffer #-}

bgxSetComputeDynamicIndexBuffer :: MonadIO m
                                => Word8
                                -> BgfxDynamicIndexBufferHandle
                                -> BgfxAccess
                                -> m ()
bgxSetComputeDynamicIndexBuffer a b c =
  liftIO (bgxSetComputeDynamicIndexBufferFFI a b c)

{-# INLINE bgxSetComputeDynamicIndexBuffer #-}

bgfxSetComputeDynamicVertexBuffer :: MonadIO m
                                  => Word8
                                  -> BgfxDynamicVertexBufferHandle
                                  -> BgfxAccess
                                  -> m ()
bgfxSetComputeDynamicVertexBuffer a b c =
  liftIO (bgfxSetComputeDynamicVertexBufferFFI a b c)

{-# INLINE bgfxSetComputeDynamicVertexBuffer #-}

bgfxSetComputeIndirectBuffer :: MonadIO m
                             => Word8
                             -> BgfxIndirectBufferHandle
                             -> BgfxAccess
                             -> m ()
bgfxSetComputeIndirectBuffer a b c =
  liftIO (bgfxSetComputeIndirectBufferFFI a b c)

{-# INLINE bgfxSetComputeIndirectBuffer #-}

bgfxSetImage :: MonadIO m
             => Word8
             -> BgfxUniformHandle
             -> BgfxTextureHandle
             -> Word8
             -> BgfxAccess
             -> BgfxTextureFormat
             -> m ()
bgfxSetImage a b c d e f = liftIO (bgfxSetImageFFI a b c d e f)

{-# INLINE bgfxSetImage #-}

bgfxSetImageFromFrameBuffer :: MonadIO m
                            => Word8
                            -> BgfxUniformHandle
                            -> BgfxFrameBufferHandle
                            -> Word8
                            -> BgfxAccess
                            -> BgfxTextureFormat
                            -> m ()
bgfxSetImageFromFrameBuffer a b c d e f =
  liftIO (bgfxSetImageFromFrameBufferFFI a b c d e f)

{-# INLINE bgfxSetImageFromFrameBuffer #-}

bgfxDispatch :: MonadIO m
             => Word8
             -> BgfxProgramHandle
             -> Word16
             -> Word16
             -> Word16
             -> Word8
             -> m Word32
bgfxDispatch a b c d e f = liftIO (bgfxDispatchFFI a b c d e f)

{-# INLINE bgfxDispatch #-}

bgfxDispatchIndirect :: MonadIO m
                     => Word8
                     -> BgfxProgramHandle
                     -> BgfxIndirectBufferHandle
                     -> Word16
                     -> Word16
                     -> Word16
                     -> Word8
                     -> m Word32
bgfxDispatchIndirect a b c d e f g =
  liftIO (bgfxDispatchIndirectFFI a b c d e f g)

{-# INLINE bgfxDispatchIndirect #-}

bgfxBlit :: MonadIO m
         => Word8
         -> BgfxTextureHandle
         -> Word8
         -> Word16
         -> Word16
         -> Word16
         -> BgfxTextureHandle
         -> Word8
         -> Word16
         -> Word16
         -> Word16
         -> Word16
         -> Word16
         -> Word16
         -> m ()
bgfxBlit a b c d e f g h i j k l m n =
  liftIO (bgfxBlitFFI a b c d e f g h i j k l m n)

{-# INLINE bgfxBlit #-}

bgfxBlitFrameBuffer :: MonadIO m
                    => Word8
                    -> BgfxTextureHandle
                    -> Word8
                    -> Word16
                    -> Word16
                    -> Word16
                    -> BgfxFrameBufferHandle
                    -> Word8
                    -> Word8
                    -> Word16
                    -> Word16
                    -> Word16
                    -> Word16
                    -> Word16
                    -> Word16
                    -> m ()
bgfxBlitFrameBuffer a b c d e f g h i j k l m n o =
  liftIO (bgfxBlitFrameBufferFFI a b c d e f g h i j k l m n o)

{-# INLINE bgfxBlitFrameBuffer #-}

bgfxAlloc :: MonadIO m
          => Word32 -> m (Ptr BgfxMemory)
bgfxAlloc a = liftIO (bgfxAllocFFI a)

{-# INLINE bgfxAlloc #-}

bgfxCopy :: MonadIO m
         => Ptr a -> Word32 -> m (Ptr BgfxMemory)
bgfxCopy a b = liftIO (bgfxCopyFFI a b)

{-# INLINE bgfxCopy #-}

bgfxMakeRef
  :: MonadIO m
  => Ptr a -> Word32 -> m (Ptr BgfxMemory)
bgfxMakeRef a b = liftIO (bgfxMakeRefFFI a b)

{-# INLINE bgfxMakeRef #-}

bgfxCreateVertexBuffer
  :: MonadIO m
  => Ptr BgfxMemory -> Ptr BgfxVertexDecl -> Word16 -> m BgfxVertexBufferHandle
bgfxCreateVertexBuffer a b c = liftIO (bgfxCreateVertexBufferFFI a b c)

{-# INLINE bgfxCreateVertexBuffer #-}

bgfxVertexDeclBegin
  :: MonadIO m
  => Ptr BgfxVertexDecl -> BgfxRendererType -> m ()
bgfxVertexDeclBegin a b = liftIO (bgfxVertexDeclBeginFFI a b)

{-# INLINE bgfxVertexDeclBegin #-}

bgfxVertexDeclAdd :: MonadIO m
                  => Ptr BgfxVertexDecl
                  -> BgfxAttrib
                  -> Word8
                  -> BgfxAttribType
                  -> Bool
                  -> Bool
                  -> m ()
bgfxVertexDeclAdd a b c d e f = liftIO (bgfxVertexDeclAddFFI a b c d e f)

{-# INLINE bgfxVertexDeclAdd #-}

bgfxVertexDeclEnd :: MonadIO m
                  => Ptr BgfxVertexDecl -> m ()
bgfxVertexDeclEnd a = liftIO (bgfxVertexDeclEndFFI a)

{-# INLINE bgfxVertexDeclEnd #-}

bgfxCreateIndexBuffer
  :: MonadIO m
  => Ptr BgfxMemory -> Word16 -> m BgfxIndexBufferHandle
bgfxCreateIndexBuffer a b = liftIO (bgfxCreateIndexBufferFFI a b)

{-# INLINE bgfxCreateIndexBuffer #-}

bgfxCreateShader
  :: MonadIO m
  => Ptr BgfxMemory -> m BgfxShaderHandle
bgfxCreateShader a = liftIO (bgfxCreateShaderFFI a)

{-# INLINE bgfxCreateShader #-}

bgfxGetShaderUniforms :: MonadIO m
                      => BgfxShaderHandle
                      -> Ptr BgfxUniformHandle
                      -> Word16
                      -> m Word16
bgfxGetShaderUniforms a b c = liftIO (bgfxGetShaderUniformsFFI a b c)

{-# INLINE bgfxGetShaderUniforms #-}

bgfxDestroyShader :: MonadIO m
                  => BgfxShaderHandle -> m ()
bgfxDestroyShader a = liftIO (bgfxDestroyShaderFFI a)

{-# INLINE bgfxDestroyShader #-}

bgfxCreateProgram :: MonadIO m
                  => BgfxShaderHandle
                  -> BgfxShaderHandle
                  -> Bool
                  -> m BgfxProgramHandle
bgfxCreateProgram a b c = liftIO (bgfxCreateProgramFFI a b c)

{-# INLINE bgfxCreateProgram #-}

bgfxCreateComputeProgram
  :: MonadIO m
  => BgfxShaderHandle -> Bool -> m BgfxProgramHandle
bgfxCreateComputeProgram a b = liftIO (bgfxCreateComputeProgramFFI a b)

{-# INLINE bgfxCreateComputeProgram #-}

bgfxDestroyProgram :: MonadIO m
                   => BgfxProgramHandle -> m ()
bgfxDestroyProgram a = liftIO (bgfxDestroyProgramFFI a)

{-# INLINE bgfxDestroyProgram #-}

bgfxCreateUniform :: MonadIO m
                  => CString
                  -> BgfxUniformType
                  -> Word16
                  -> m BgfxUniformHandle
bgfxCreateUniform a b c = liftIO (bgfxCreateUniformFFI a b c)

{-# INLINE bgfxCreateUniform #-}

bgfxDestroyUniform :: MonadIO m
                   => BgfxUniformHandle -> m ()
bgfxDestroyUniform a = liftIO (bgfxDestroyUniformFFI a)

{-# INLINE bgfxDestroyUniform #-}

bgfxCalcTextureSize :: MonadIO m
                    => Ptr BgfxTextureInfo
                    -> Word16
                    -> Word16
                    -> Word16
                    -> Bool
                    -> Word8
                    -> BgfxTextureFormat
                    -> m ()
bgfxCalcTextureSize a b c d e f g =
  liftIO (bgfxCalcTextureSizeFFI a b c d e f g)

{-# INLINE bgfxCalcTextureSize #-}

bgfxCreateTexture :: MonadIO m
                  => Ptr BgfxMemory
                  -> Word32
                  -> Word8
                  -> Ptr BgfxTextureInfo
                  -> m BgfxTextureHandle
bgfxCreateTexture a b c d = liftIO (bgfxCreateTextureFFI a b c d)

{-# INLINE bgfxCreateTexture #-}

bgfxCreateTexture2D :: MonadIO m
                    => Word16
                    -> Word16
                    -> Word8
                    -> BgfxTextureFormat
                    -> Word32
                    -> Ptr BgfxMemory
                    -> m BgfxTextureHandle
bgfxCreateTexture2D a b c d e f = liftIO (bgfxCreateTexture2DFFI a b c d e f)

{-# INLINE bgfxCreateTexture2D #-}

bgfxCreateTexture2DScaled :: MonadIO m
                          => BgfxBackbufferRatio
                          -> Word8
                          -> BgfxTextureFormat
                          -> Word32
                          -> m BgfxTextureHandle
bgfxCreateTexture2DScaled a b c d =
  liftIO (bgfxCreateTexture2DScaledFFI a b c d)

{-# INLINE bgfxCreateTexture2DScaled #-}

bgfxCreateTexture3D :: MonadIO m
                    => Word16
                    -> Word16
                    -> Word16
                    -> Word8
                    -> BgfxTextureFormat
                    -> Word32
                    -> Ptr BgfxMemory
                    -> m BgfxTextureHandle
bgfxCreateTexture3D a b c d e f g =
  liftIO (bgfxCreateTexture3DFFI a b c d e f g)

{-# INLINE bgfxCreateTexture3D #-}

bgfxCreateTextureCube :: MonadIO m
                      => Word16
                      -> Word8
                      -> BgfxTextureFormat
                      -> Word32
                      -> Ptr BgfxMemory
                      -> m BgfxTextureHandle
bgfxCreateTextureCube a b c d e = liftIO (bgfxCreateTextureCubeFFI a b c d e)

{-# INLINE bgfxCreateTextureCube #-}

bgfxUpdateTexture2D :: MonadIO m
                    => BgfxTextureHandle
                    -> Word8
                    -> Word16
                    -> Word16
                    -> Word16
                    -> Word16
                    -> Ptr BgfxMemory
                    -> Word16
                    -> m ()
bgfxUpdateTexture2D a b c d e f g h =
  liftIO (bgfxUpdateTexture2DFFI a b c d e f g h)

{-# INLINE bgfxUpdateTexture2D #-}

bgfxUpdateTexture3D :: MonadIO m
                    => BgfxTextureHandle
                    -> Word8
                    -> Word16
                    -> Word16
                    -> Word16
                    -> Word16
                    -> Word16
                    -> Word16
                    -> Ptr BgfxMemory
                    -> m ()
bgfxUpdateTexture3D a b c d e f g h i =
  liftIO (bgfxUpdateTexture3DFFI a b c d e f g h i)

{-# INLINE bgfxUpdateTexture3D #-}

bgfxUpdateTextureCube :: MonadIO m
                      => BgfxTextureHandle
                      -> Word8
                      -> Word8
                      -> Word16
                      -> Word16
                      -> Word16
                      -> Word16
                      -> Ptr BgfxMemory
                      -> Word16
                      -> m ()
bgfxUpdateTextureCube a b c d e f g h i =
  liftIO (bgfxUpdateTextureCubeFFI a b c d e f g h i)

{-# INLINE bgfxUpdateTextureCube #-}

bgfxReadTexture
  :: MonadIO m
  => BgfxTextureHandle -> Ptr a -> m ()
bgfxReadTexture a b = liftIO (bgfxReadTextureFFI a b)

{-# INLINE bgfxReadTexture #-}

bgfxReadFrameBuffer
  :: MonadIO m
  => BgfxFrameBufferHandle -> Word8 -> Ptr a -> m ()
bgfxReadFrameBuffer a b c = liftIO (bgfxReadFrameBufferFFI a b c)

{-# INLINE bgfxReadFrameBuffer #-}

bgfxDestroyTexture :: MonadIO m
                   => BgfxTextureHandle -> m ()
bgfxDestroyTexture a = liftIO (bgfxDestroyTextureFFI a)

{-# INLINE bgfxDestroyTexture #-}

bgfxCreateFrameBuffer :: MonadIO m
                      => Word16
                      -> Word16
                      -> BgfxTextureFormat
                      -> Word32
                      -> m BgfxFrameBufferHandle
bgfxCreateFrameBuffer a b c d = liftIO (bgfxCreateFrameBufferFFI a b c d)

{-# INLINE bgfxCreateFrameBuffer #-}

bgfxCreateFrameBufferScaled
  :: MonadIO m
  => BgfxBackbufferRatio
  -> BgfxTextureFormat
  -> Word32
  -> m BgfxFrameBufferHandle
bgfxCreateFrameBufferScaled a b c =
  liftIO (bgfxCreateFrameBufferScaledFFI a b c)

{-# INLINE bgfxCreateFrameBufferScaled #-}

bgfxCreateFrameBufferFromHandles
  :: MonadIO m
  => Word8 -> Ptr BgfxTextureHandle -> Bool -> m BgfxFrameBufferHandle
bgfxCreateFrameBufferFromHandles a b c =
  liftIO (bgfxCreateFrameBufferFromHandlesFFI a b c)

{-# INLINE bgfxCreateFrameBufferFromHandles #-}

bgfxCreateFrameBufferFromNWH
  :: MonadIO m
  => Ptr a -> Word16 -> Word16 -> BgfxTextureFormat -> m BgfxFrameBufferHandle
bgfxCreateFrameBufferFromNWH a b c d =
  liftIO (bgfxCreateFrameBufferFromNWHFFI a b c d)

{-# INLINE bgfxCreateFrameBufferFromNWH #-}

bgfxDestroyFrameBuffer
  :: MonadIO m
  => BgfxFrameBufferHandle -> m ()
bgfxDestroyFrameBuffer a = liftIO (bgfxDestroyFrameBufferFFI a)

{-# INLINE bgfxDestroyFrameBuffer #-}

bgfxCheckAvailTransientIndexBuffer
  :: MonadIO m
  => Word32 -> m Bool
bgfxCheckAvailTransientIndexBuffer a =
  liftIO (bgfxCheckAvailTransientIndexBufferFFI a)

{-# INLINE bgfxCheckAvailTransientIndexBuffer #-}

bgfxCheckAvailTransientVertexBuffer
  :: MonadIO m
  => Word32 -> Ptr BgfxVertexDecl -> m Bool
bgfxCheckAvailTransientVertexBuffer a b =
  liftIO (bgfxCheckAvailTransientVertexBufferFFI a b)

{-# INLINE bgfxCheckAvailTransientVertexBuffer #-}

bgfxCheckAvailInstanceDataBuffer
  :: MonadIO m
  => Word32 -> Word16 -> m Bool
bgfxCheckAvailInstanceDataBuffer a b =
  liftIO (bgfxCheckAvailInstanceDataBufferFFI a b)

{-# INLINE bgfxCheckAvailInstanceDataBuffer #-}

bgfxCheckAvailTransientBuffers :: MonadIO m
                               => Word32
                               -> Ptr BgfxVertexDecl
                               -> Word32
                               -> m Bool
bgfxCheckAvailTransientBuffers a b c =
  liftIO (bgfxCheckAvailTransientBuffersFFI a b c)

{-# INLINE bgfxCheckAvailTransientBuffers #-}

bgfxAllocTransientIndexBuffer
  :: MonadIO m
  => Ptr BgfxTransientIndexBuffer -> Word32 -> m ()
bgfxAllocTransientIndexBuffer a b =
  liftIO (bgfxAllocTransientIndexBufferFFI a b)

{-# INLINE bgfxAllocTransientIndexBuffer #-}

bgfxAllocTransientVertexBuffer :: MonadIO m
                               => Ptr BgfxTransientVertexBuffer
                               -> Word32
                               -> Ptr BgfxVertexDecl
                               -> m ()
bgfxAllocTransientVertexBuffer a b c =
  liftIO (bgfxAllocTransientVertexBufferFFI a b c)

{-# INLINE bgfxAllocTransientVertexBuffer #-}

bgfxAllocTransientBuffers :: MonadIO m
                          => Ptr BgfxTransientVertexBuffer
                          -> Ptr BgfxVertexDecl
                          -> Word32
                          -> Ptr BgfxTransientIndexBuffer
                          -> Word32
                          -> m ()
bgfxAllocTransientBuffers a b c d e =
  liftIO (bgfxAllocTransientBuffersFFI a b c d e)

{-# INLINE bgfxAllocTransientBuffers #-}

bgfxAllocInstanceDataBuffer
  :: MonadIO m
  => Word32 -> Word16 -> m (Ptr BgfxInstanceDataBuffer)
bgfxAllocInstanceDataBuffer a b = liftIO (bgfxAllocInstanceDataBufferFFI a b)

{-# INLINE bgfxAllocInstanceDataBuffer #-}

bgfxCreateIndirectBuffer
  :: MonadIO m
  => Word32 -> m BgfxIndirectBufferHandle
bgfxCreateIndirectBuffer a = liftIO (bgfxCreateIndirectBufferFFI a)

{-# INLINE bgfxCreateIndirectBuffer #-}

bgfxDestroyIndirectBuffer
  :: MonadIO m
  => BgfxIndirectBufferHandle -> m ()
bgfxDestroyIndirectBuffer a = liftIO (bgfxDestroyIndirectBufferFFI a)

{-# INLINE bgfxDestroyIndirectBuffer #-}

bgfxCreateOcclusionQuery
  :: MonadIO m
  => m BgfxOcclusionQueryHandle
bgfxCreateOcclusionQuery = liftIO (bgfxCreateOcclusionQueryFFI)

{-# INLINE bgfxCreateOcclusionQuery #-}

bgfxGetResult
  :: MonadIO m
  => BgfxOcclusionQueryHandle -> m BgfxOcclusionQueryResult
bgfxGetResult a = liftIO (bgfxGetResultFFI a)

{-# INLINE bgfxGetResult #-}

bgfxDestroyOcclusionQuery
  :: MonadIO m
  => BgfxOcclusionQueryHandle -> m ()
bgfxDestroyOcclusionQuery a = liftIO (bgfxDestroyOcclusionQueryFFI a)

{-# INLINE bgfxDestroyOcclusionQuery #-}
