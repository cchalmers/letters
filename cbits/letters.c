#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include <ft2build.h>
#include FT_FREETYPE_H

#include <harfbuzz/hb.h>
#include <harfbuzz/hb-ft.h>

// Copy the size of points and contours for the current glyph.
void get_glyph_outline_sizes(
  FT_Face ft_face,
  int*    outline_n_points,
  int*    outline_n_contours
  )
{
  FT_Outline ft_outline = ft_face->glyph->outline;
  *outline_n_points     = ft_outline.n_points;
  *outline_n_contours   = ft_outline.n_contours;
}

// Copy the current glyph outline to arrays of the correct size.
void copy_glyph_outline (
  FT_Face    ft_face,
  FT_Vector* outline_points,
  char*      outline_tags,
  short*     outline_contours
  )
{
  int i;
  FT_Outline ft_outline = ft_face->glyph->outline;

  for (i=0; i < ft_outline.n_points; i++) {
    outline_points[i] = ft_outline.points[i];
    outline_tags[i]   = ft_outline.tags[i];
  }

  for (i=0; i < ft_outline.n_contours; i++) {
    outline_contours[i] = ft_outline.contours[i];
  }
}

// Bitmaps

FT_Error bitmap_glyph(
  FT_Face       ft_face,
  FT_UInt       glyph_index,
  unsigned int* left,
  unsigned int* top,
  unsigned int* width,
  unsigned int* height,
  int*          pitch,
  char*         pixel_data
  )
{
  FT_Error     ft_error;

  ft_error = FT_Load_Glyph(ft_face, glyph_index, FT_LOAD_RENDER);
  FT_GlyphSlot ft_glyph = ft_face->glyph;

  if (ft_glyph->format != FT_GLYPH_FORMAT_BITMAP) {
    printf("OMG! DIDN'T RENDER A GLYPH!!\n");
    return -1;
  }

  *left = ft_glyph->bitmap_left;
  *top  = ft_glyph->bitmap_top;

  FT_Bitmap ft_bitmap = ft_glyph->bitmap;
  *width  = ft_bitmap.width;
  *height = ft_bitmap.rows;
  *pitch  = ft_bitmap.pitch;
  for (int i = 0; i < ft_bitmap.width * ft_bitmap.rows; i++) {
    pixel_data[i] = ft_bitmap.buffer[i];
  }

  return ft_error;
}

//----------------------------------------------------------------------
// Harfbuzz
//----------------------------------------------------------------------

// Get the horizontal advances of a string using harfbuzz.
void string_advances(
  FT_Face     ft_face,
  const char* string,
  int*        length,
  int*        char_width,
  int*        codepoints,
  int*        advances
  )
{
  hb_font_t*   hb_font   = hb_ft_font_create (ft_face, NULL);
  hb_buffer_t* hb_buffer = hb_buffer_create();

  hb_buffer_add_utf8(hb_buffer, string, -1, 0, -1);
  hb_buffer_guess_segment_properties(hb_buffer);

  hb_shape(hb_font, hb_buffer, NULL, 0);

  int len = hb_buffer_get_length(hb_buffer);
  *length = len;

  hb_glyph_info_t*     info = hb_buffer_get_glyph_infos(hb_buffer, NULL);
  hb_glyph_position_t* pos  = hb_buffer_get_glyph_positions(hb_buffer, NULL);

  int i;
  int width = 0;

  for (i = 0; i < len; i++) {
    codepoints[i] = info[i].codepoint;
    advances[i]   = pos[i].x_advance;
    width        += pos[i].x_advance;
  }

  *char_width = width;

  hb_buffer_destroy(hb_buffer);
  hb_font_destroy(hb_font);
}

