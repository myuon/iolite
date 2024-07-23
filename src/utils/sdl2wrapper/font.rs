use std::{
    ffi::{c_int, CString},
    path::Path,
};

use sdl2::{get_error, surface::Surface, ttf::FontError};
use sdl2_sys::ttf;

pub fn ttf_open_font<P: AsRef<Path>>(path: P, ptsize: u16) -> Result<Font, String> {
    unsafe {
        let cstring = CString::new(path.as_ref().to_str().unwrap()).unwrap();
        let raw = ttf::TTF_OpenFont(cstring.as_ptr(), ptsize as c_int);
        if raw.is_null() {
            Err(get_error())
        } else {
            Ok(Font { raw })
        }
    }
}

pub struct Font {
    raw: *mut ttf::TTF_Font,
}

impl Drop for Font {
    fn drop(&mut self) {
        unsafe {
            if ttf::TTF_WasInit() == 1 {
                ttf::TTF_CloseFont(self.raw);
            }
        }
    }
}

impl Font {
    pub fn render_solid<T>(&self, text: &str, color: T) -> sdl2::ttf::FontResult<Surface<'static>>
    where
        T: Into<sdl2::pixels::Color>,
    {
        let color = color.into().into();
        let source = CString::new(text).unwrap();
        let raw = unsafe { ttf::TTF_RenderUTF8_Solid(self.raw, source.as_ptr(), color) };

        convert_to_surface(raw)
    }

    pub fn render_blended<T>(&self, text: &str, color: T) -> sdl2::ttf::FontResult<Surface<'static>>
    where
        T: Into<sdl2::pixels::Color>,
    {
        let color = color.into().into();
        let source = CString::new(text).unwrap();
        let raw = unsafe { ttf::TTF_RenderUTF8_Blended(self.raw, source.as_ptr(), color) };

        convert_to_surface(raw)
    }
}

fn convert_to_surface<'a>(raw: *mut sdl2_sys::SDL_Surface) -> sdl2::ttf::FontResult<Surface<'a>> {
    if (raw as *mut ()).is_null() {
        Err(FontError::SdlError(get_error()))
    } else {
        Ok(unsafe { Surface::from_ll(raw) })
    }
}
