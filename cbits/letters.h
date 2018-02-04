#ifndef _LETTERS_HELPER_
#define _LETTERS_HELPER_

#include <ft2build.h>
#include FT_FREETYPE_H

// FreeType

// Reexports

FT_Error FT_Init_FreeType(FT_Library *alibrary);
FT_Error FT_Done_FreeType(FT_Library library);

FT_Error FT_New_Face(
  FT_Library   library,
  const char*  filepathname,
  FT_Long      face_index,
  FT_Face*     aface
  );
FT_Error FT_Done_Face(FT_Face face);

FT_Error FT_Set_Char_Size(
  FT_Face     face,
  FT_F26Dot6  char_width,
  FT_F26Dot6  char_height,
  FT_UInt     horz_resolution,
  FT_UInt     vert_resolution
  );

FT_UInt FT_Get_Char_Index(
  FT_Face   face,
  FT_ULong  charcode
  );

// helpers

// Retreive the number of points and contours for current glyph (set
// with FT_Load_Glyph)
void get_glyph_outline_sizes(
  FT_Face ft_face,
  int*    outline_n_points,
  int*    outline_n_contours
  );

// Fill the vector and contour pointers with the current glyph outline.
// The pointers should have the correct length (retrievable by
// get_glyph_outline_sizes
void copy_glyph_outline (
  FT_Face    ft_face,
  FT_Vector* outline_points,
  char*      outline_tags,
  short*     outline_contours
  );

// Render a 8-bit glyph.
FT_Error bitmap_glyph(
  FT_Face       ft_face,
  FT_UInt       glyph_index,
  unsigned int* left,
  unsigned int* top,
  unsigned int* width,
  unsigned int* height,
  int*          pitch,
  char*         pixel_data
  );

// Harfbuff

// Given some utf8 encoded string, return the shaped output.
void string_advances(
  FT_Face       ft_face,
  const char*   string,
  unsigned int* length,
  unsigned int* char_width,
  int*          codepoints,
  int*          advances
  );

#endif
