#include "SDL.h"
#include "SDL2/SDL_syswm.h"

#include "bgfx/c99/bgfx.h"

void bgfx_sdl_set_window(SDL_Window* _window)
{
  SDL_SysWMinfo wmi;
  SDL_VERSION(&wmi.version);
  if (!SDL_GetWindowWMInfo(_window, &wmi) )
		{
			return;
		}

  bgfx_platform_data_t pd;
  pd.ndt          = wmi.info.x11.display;
  pd.nwh          = (void*)(uintptr_t)wmi.info.x11.window;
  pd.context      = NULL;
  pd.backBuffer   = NULL;
  pd.backBufferDS = NULL;
  bgfx_set_platform_data(&pd);

  return;
}
