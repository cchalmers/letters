{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}

module Letters.Internal
  (
    Library
  , newLibrary
  , doneLibrary

  , FontFace
  , newFontFace
  , doneFontFace

  , CharIndex (..)
  , getCharIndex

  -- * Bitmaps
  , Bitmap (..)
  , GlyphBitmap (..)
  , bitmap

  -- * Glyph outlines
  , Outline (..)
  , getGlyphOutline

  -- * Shaping
  , Advances (..)
  , advancesCodepoints
  , advances
  , foldAdvances

  , Vec (..)
  , Segment (..)

  -- * Maths
  , hasMath
  , italicsCorrection

  -- * Debugging
  , printOutline
  , showHB

  -- Sizes
  , F26_6 (..)
  , F16_16 (..)
  , HB_font (..)
  , HB_face (..)
  , HB_math_constant (..)
  , HB_pos (..)

  , hb_ft_face_create
  , hb_ft_font_create

  , getMathConstant
  , pattern HB_OT_MATH_CONSTANT_SCRIPT_PERCENT_SCALE_DOWN
  , pattern HB_OT_MATH_CONSTANT_SCRIPT_SCRIPT_PERCENT_SCALE_DOWN
  , pattern HB_OT_MATH_CONSTANT_DELIMITED_SUB_FORMULA_MIN_HEIGHT
  , pattern HB_OT_MATH_CONSTANT_DISPLAY_OPERATOR_MIN_HEIGHT
  , pattern HB_OT_MATH_CONSTANT_MATH_LEADING
  , pattern HB_OT_MATH_CONSTANT_AXIS_HEIGHT
  , pattern HB_OT_MATH_CONSTANT_ACCENT_BASE_HEIGHT
  , pattern HB_OT_MATH_CONSTANT_FLATTENED_ACCENT_BASE_HEIGHT
  , pattern HB_OT_MATH_CONSTANT_SUBSCRIPT_SHIFT_DOWN
  , pattern HB_OT_MATH_CONSTANT_SUBSCRIPT_TOP_MAX
  , pattern HB_OT_MATH_CONSTANT_SUBSCRIPT_BASELINE_DROP_MIN
  , pattern HB_OT_MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP
  , pattern HB_OT_MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP_CRAMPED
  , pattern HB_OT_MATH_CONSTANT_SUPERSCRIPT_BOTTOM_MIN
  , pattern HB_OT_MATH_CONSTANT_SUPERSCRIPT_BASELINE_DROP_MAX
  , pattern HB_OT_MATH_CONSTANT_SUB_SUPERSCRIPT_GAP_MIN
  , pattern HB_OT_MATH_CONSTANT_SUPERSCRIPT_BOTTOM_MAX_WITH_SUBSCRIPT
  , pattern HB_OT_MATH_CONSTANT_SPACE_AFTER_SCRIPT
  , pattern HB_OT_MATH_CONSTANT_UPPER_LIMIT_GAP_MIN
  , pattern HB_OT_MATH_CONSTANT_UPPER_LIMIT_BASELINE_RISE_MIN
  , pattern HB_OT_MATH_CONSTANT_LOWER_LIMIT_GAP_MIN
  , pattern HB_OT_MATH_CONSTANT_LOWER_LIMIT_BASELINE_DROP_MIN
  , pattern HB_OT_MATH_CONSTANT_STACK_TOP_SHIFT_UP
  , pattern HB_OT_MATH_CONSTANT_STACK_TOP_DISPLAY_STYLE_SHIFT_UP
  , pattern HB_OT_MATH_CONSTANT_STACK_BOTTOM_SHIFT_DOWN
  , pattern HB_OT_MATH_CONSTANT_STACK_BOTTOM_DISPLAY_STYLE_SHIFT_DOWN
  , pattern HB_OT_MATH_CONSTANT_STACK_GAP_MIN
  , pattern HB_OT_MATH_CONSTANT_STACK_DISPLAY_STYLE_GAP_MIN
  , pattern HB_OT_MATH_CONSTANT_STRETCH_STACK_TOP_SHIFT_UP
  , pattern HB_OT_MATH_CONSTANT_STRETCH_STACK_BOTTOM_SHIFT_DOWN
  , pattern HB_OT_MATH_CONSTANT_STRETCH_STACK_GAP_ABOVE_MIN
  , pattern HB_OT_MATH_CONSTANT_STRETCH_STACK_GAP_BELOW_MIN
  , pattern HB_OT_MATH_CONSTANT_FRACTION_NUMERATOR_SHIFT_UP
  , pattern HB_OT_MATH_CONSTANT_FRACTION_NUMERATOR_DISPLAY_STYLE_SHIFT_UP
  , pattern HB_OT_MATH_CONSTANT_FRACTION_DENOMINATOR_SHIFT_DOWN
  , pattern HB_OT_MATH_CONSTANT_FRACTION_DENOMINATOR_DISPLAY_STYLE_SHIFT_DOWN
  , pattern HB_OT_MATH_CONSTANT_FRACTION_NUMERATOR_GAP_MIN
  , pattern HB_OT_MATH_CONSTANT_FRACTION_NUM_DISPLAY_STYLE_GAP_MIN
  , pattern HB_OT_MATH_CONSTANT_FRACTION_RULE_THICKNESS
  , pattern HB_OT_MATH_CONSTANT_FRACTION_DENOMINATOR_GAP_MIN
  , pattern HB_OT_MATH_CONSTANT_FRACTION_DENOM_DISPLAY_STYLE_GAP_MIN
  , pattern HB_OT_MATH_CONSTANT_SKEWED_FRACTION_HORIZONTAL_GAP
  , pattern HB_OT_MATH_CONSTANT_SKEWED_FRACTION_VERTICAL_GAP
  , pattern HB_OT_MATH_CONSTANT_OVERBAR_VERTICAL_GAP
  , pattern HB_OT_MATH_CONSTANT_OVERBAR_RULE_THICKNESS
  , pattern HB_OT_MATH_CONSTANT_OVERBAR_EXTRA_ASCENDER
  , pattern HB_OT_MATH_CONSTANT_UNDERBAR_VERTICAL_GAP
  , pattern HB_OT_MATH_CONSTANT_UNDERBAR_RULE_THICKNESS
  , pattern HB_OT_MATH_CONSTANT_UNDERBAR_EXTRA_DESCENDER
  , pattern HB_OT_MATH_CONSTANT_RADICAL_VERTICAL_GAP
  , pattern HB_OT_MATH_CONSTANT_RADICAL_DISPLAY_STYLE_VERTICAL_GAP
  , pattern HB_OT_MATH_CONSTANT_RADICAL_RULE_THICKNESS
  , pattern HB_OT_MATH_CONSTANT_RADICAL_EXTRA_ASCENDER
  , pattern HB_OT_MATH_CONSTANT_RADICAL_KERN_BEFORE_DEGREE
  , pattern HB_OT_MATH_CONSTANT_RADICAL_KERN_AFTER_DEGREE
  , pattern HB_OT_MATH_CONSTANT_RADICAL_DEGREE_BOTTOM_RAISE_PERCENT



  , glyph_kerning
  , accentAttachment

  , HB_math_kern (..)

  , pattern HB_OT_MATH_KERN_TOP_RIGHT
  , pattern HB_OT_MATH_KERN_TOP_LEFT
  , pattern HB_OT_MATH_KERN_BOTTOM_RIGHT
  , pattern HB_OT_MATH_KERN_BOTTOM_LEFT

  , hb_glyph_variants
  , HB_glyph_variant (..)

  , HB_direction (..)
  , pattern HB_DIRECTION_INVALID
  , pattern HB_DIRECTION_LTR
  , pattern HB_DIRECTION_TTB
  , pattern HB_DIRECTION_RTL
  , pattern HB_DIRECTION_BTT

  , glyphVariants

  , minConnectorOverlap
  , hb_glyph_assembly
  , glyphAssembly
  , HB_glyph_part (..)

  ) where

import Foreign
import Foreign.C
import Data.Semigroup
import Data.Char
import Data.Bool

import GHC.Exts
import Data.Primitive.ByteArray
import Data.Hashable
import Control.Monad (when)

import Data.Foldable

newtype FT_Error = FT_Error Int32
  deriving (Num, Eq, Show)

instance Semigroup FT_Error where
  FT_Error a <> FT_Error b = FT_Error (a .|. b)

instance Monoid FT_Error where
  mempty  = FT_Error 0
  mappend = (<>)

-- | Handle an FT_Error.
ft_error :: FT_Error -> IO ()
ft_error 0   = return ()
ft_error err = error (show err)

-- | Flags for loading a freetype glyph.
newtype LoadFlags = LoadFlags Int32

instance Semigroup LoadFlags where
  LoadFlags a <> LoadFlags b = LoadFlags (a .|. b)

instance Monoid LoadFlags where
  mempty  = LoadFlags 0
  mappend = (<>)

-- | An index representing a character for a particular 'Font'.
newtype CharIndex = CharIndex CUInt
  deriving (Show, Eq, Ord, Storable)

instance Hashable CharIndex where
  hashWithSalt s (CharIndex (CUInt x)) = hashWithSalt s x

-- | A signed 26.6 fixed-point type used for vectorial pixel
--   coordinates. (26 bits for integral portion, 6 bits for fractional
--   portion). Each unit is 1/64th of a pixel.
newtype F26_6 = F26_6 Int32
  deriving (Num, Enum, Real, Eq, Ord, Integral, Show)

-- | A signed 16.16 fixed-point type used for vectorial pixel
--   coordinates. (16 bits for integral portion, 16 bits for fractional
--   portion). Each unit is 1/64th of a pixel.
newtype F16_16 = F16_16 Int32
  deriving (Num, Enum, Real, Eq, Ord, Integral, Show)

-- | The DPI used to render the font glyphs.
newtype DPI = DPI CUInt
  deriving (Num, Enum, Real, Eq, Ord, Integral, Show)

-- Foreign imports -----------------------------------------------------

-- getGlyph :: Char -> IO ()
-- getGlyph c = do
--   alloca $ \nptr ->
--     allocaBytes 4096 $ \vptr -> do
--       let fontpath = "/Users/christopher/Documents/truetype/DejaVuSerif.ttf"
--       withCString fontpath $ \nm -> copy_glyph_outline (ord c) nm nptr vptr
--       n <- fromIntegral <$> peek nptr
--       putStrLn $ show n ++ "points"
--       for_ [0..n] $ \i ->
--         print =<< peekElemOff vptr i

-- glyphOutline :: Char -> IO Outline
-- glyphOutline c = do
--   let fontpath = "/Users/christopher/Documents/truetype/DejaVuSerif.ttf"
--   r <- withCString fontpath $ glyph_outline (ord c)
--   peekOutline ptr

------------------------------------------------------------------------
-- Freetype library
------------------------------------------------------------------------

-- | An open connection to a freetype library.
newtype Library = Library (Ptr ())
  deriving Storable

foreign import ccall unsafe "FT_Init_FreeType"
  ft_init_freetype :: Ptr Library -> IO FT_Error

foreign import ccall unsafe "FT_Done_FreeType"
  ft_done_freetype :: Library -> IO FT_Error

-- | Initialise a new freetype library. This is used to load
--   'FontFace's. When the library and all 'FontFace's are finished
--   with, call 'doneLibrary'.
newLibrary :: IO Library
newLibrary = alloca $ \libPtr -> do
  ft_init_freetype libPtr >>= ft_error
  peek libPtr

-- | Run finalisers for a 'Library'. The 'Library' can no longer be
--   used after this.
doneLibrary :: Library -> IO ()
doneLibrary lib = ft_done_freetype lib >>= ft_error

------------------------------------------------------------------------
-- Font faces
------------------------------------------------------------------------

-- | An open connecting to a freetype font face.
newtype FontFace = FontFace (Ptr ())
  deriving Storable

foreign import ccall unsafe "FT_New_Face"
  ft_new_face :: Library -> CString -> CLong -> Ptr FontFace -> IO FT_Error

foreign import ccall unsafe "FT_Done_Face"
  ft_done_face :: FontFace -> IO FT_Error

-- | Load a glyph into the current font face.
foreign import ccall unsafe "FT_Load_Glyph"
  load_glyph :: FontFace -> CharIndex -> LoadFlags -> IO FT_Error

foreign import ccall unsafe "FT_Set_Char_Size"
  ft_set_char_size
    :: FontFace
    -> F26_6
    -> F26_6
    -> DPI
    -> DPI
    -> IO FT_Error

foreign import ccall unsafe "FT_Get_Char_Index"
  ft_get_char_index :: FontFace -> CULong -> IO CharIndex

-- | Initialise a new fontface for the freetype library where the font
--   face is in filepath initially with some size and filepath.
--
--   Freetype allows changing the font size multiple time throught the
--   lifttime of a font face but for simplicity we only allow it during
--   initiation for now.
newFontFace :: Library -> FilePath -> F26_6 -> DPI -> IO FontFace
newFontFace lib path sz dpi = alloca $ \ffPtr -> do
  withCString path (\str -> ft_new_face lib str 0 ffPtr) >>= ft_error
  ff <- peek ffPtr
  ft_set_char_size ff sz sz dpi dpi >>= ft_error
  return ff

-- | Run finalisers for a 'FontFace'. The 'FontFace' can no longer be
--   used after this.
doneFontFace :: FontFace -> IO ()
doneFontFace face = ft_done_face face >>= ft_error

-- | Get a 'CharIndex' for a character in the current font face.
getCharIndex :: FontFace -> Char -> IO CharIndex
getCharIndex ff c = ft_get_char_index ff (fromIntegral $ ord c)
{-# INLINE getCharIndex #-}

-- data FaceSize = FaceSize
--   { facePpem     :: Int -- The number of pixels per 'EM' unit
--   , faceScale    :: F16_16
--   , faceDesender :: F26_6
--   , faceAccender :: F26_6
--   , faceHeight   :: F26_6
--   , maxAdvance   :: F26_6
--   } deriving Show

-- data Face = Face
--   { faceName           :: String
--   , faceStyle          :: String
--   , faceBB             :: CharBB -- ^ maximum char bounding box
--   , faceHeight         :: EM   -- ^ baseline height
--   , maxAdvanceHeight   :: EM
--   , maxAdcanceWidth    :: EM
--   , underlinePos       :: EM
--   , underlineThickness :: EM
--   } deriving Show

------------------------------------------------------------------------
-- Glyphs
------------------------------------------------------------------------

-- Glyph outline -------------------------------------------------------

-- | An 8-bit bitmap with a width and height.
data Bitmap = Bitmap !Int !Int !Int !ByteArray

-- | A single glyph which has been bitmap rendered.
data GlyphBitmap = GlyphBitmap !Int !Int !Bitmap

foreign import ccall unsafe "bitmap_glyph"
  bitmap_glyph
    :: FontFace
    -> CharIndex
    -> Ptr CInt -- ^ left ptr
    -> Ptr CInt -- ^ top ptr
    -> Ptr CInt -- ^ width ptr
    -> Ptr CInt -- ^ height ptr
    -> Ptr CInt -- ^ pitch ptr
    -> MutableByteArray# RealWorld -- ^ img data
    -> IO FT_Error

bitmap :: FontFace -> CharIndex -> IO GlyphBitmap
bitmap ff cix =
  alloca $ \lPtr ->
  alloca $ \tPtr ->
  alloca $ \wPtr ->
  alloca $ \hPtr ->
  alloca $ \pPtr -> do
    pMBA@(MutableByteArray pMBA#) <- newByteArray (256*256)
    -- charIndex <- ft_get_char_index ff (fromIntegral $ ord c)
    err_code <- bitmap_glyph ff cix lPtr tPtr wPtr hPtr pPtr pMBA#
    when (err_code /= 0) $ print err_code
    t <- peek tPtr :: IO CInt
    l <- peek lPtr :: IO CInt
    w <- peek wPtr :: IO CInt
    h <- peek hPtr :: IO CInt
    p <- peek pPtr :: IO CInt
    pBA <- unsafeFreezeByteArray pMBA
    pure $ GlyphBitmap (fromIntegral l) (fromIntegral t)
          (Bitmap (fromIntegral w) (fromIntegral h) (fromIntegral p) pBA)

-- Glyph metrics -------------------------------------------------------

-- data GlyphMetrics = GlyphMetric
--   { glyphWidth        :: !F26_6
--   , glyphHeight       :: !F26_6

--   , glyphHoriBearingX :: !F26_6
--   , glyphHoriBearingY :: !F26_6
--   , glyphHoriAdvance  :: !F26_6

--   , glyphVertBearingX :: !F26_6
--   , glyphVertBearingY :: !F26_6
--   , glyphVertAdvance  :: !F26_6
--   }

-- Glyph outline -------------------------------------------------------

-- | Representation of a glyph in path format.
data Outline = Outline
  { _numPoints   :: !Int
  , _numContours :: !Int
  , _points      :: !ByteArray
  , _tags        :: !ByteArray
  , _contours    :: !ByteArray
  }

-- | Get the size of the outline for the current glyph
foreign import ccall unsafe "get_glyph_outline_sizes"
  get_glyph_outline_sizes
    :: FontFace
    -> Ptr CInt -- n_points
    -> Ptr CInt -- n_contours
    -> IO ()

-- | Fill the bytearrays with the respective outline infomation for the
-- current glyph.
--
--   By using ByteArray# we ensure ghc will not move the bytearray while
--   copying the outline.
foreign import ccall unsafe "copy_glyph_outline"
  copy_glyph_outline
    :: FontFace
    -> MutableByteArray# RealWorld -- outline_points
    -> MutableByteArray# RealWorld -- outline_tags
    -> MutableByteArray# RealWorld -- outline_contours
    -> IO ()

printOutline :: Outline -> IO ()
printOutline (Outline nPts nCs pts tgs cs) = do
  putStrLn "points:"
  for_ [0..nPts-1] $ \i -> do
    let t = indexByteArray tgs i
        x = indexByteArray pts (2*i)
        y = indexByteArray pts (2*i+1)
    putStrLn $ show (t::Word8) ++ "\t" ++ show (x::Int64) ++ "\t" ++ show (y::Int64)
  putStrLn "contours:"
  for_ [0..nCs-1] $ \i -> do
    let c = indexByteArray cs i
    putStrLn $ show (c::Int16)

-- | A two dimentional vector where each unit represents 1/64th of a
--   pixel.
data Vec = Vec !Int !Int
  deriving Show

-- | A single segment of a glyph.
data Segment
  = Linear !Vec
  | Quadratic !Vec !Vec
  | Cubic !Vec !Vec !Vec
  deriving Show

getGlyphOutline :: FontFace -> CharIndex -> IO Outline
getGlyphOutline ff charIndex = do
  -- putStrLn "loading glyph"
  -- charIndex <- ft_get_char_index ff (fromIntegral $ ord c)
  -- putStrLn $ "char index is " ++ show charIndex
  x <- load_glyph ff charIndex (LoadFlags 1)
  ft_error x
  -- print x
  -- putStrLn "loaded glyph"
  alloca $ \npsPtr ->
    alloca $ \ncsPtr -> do
      get_glyph_outline_sizes ff npsPtr ncsPtr
      nps <- peek npsPtr
      ncs <- peek ncsPtr
      -- putStrLn $ "num points   = " ++ show nps
      -- putStrLn $ "num contours = " ++ show ncs
      psMBA@(MutableByteArray psMBA') <- newByteArray (fromIntegral $ 8*2*nps)
      tsMBA@(MutableByteArray tsMBA') <- newByteArray (fromIntegral $ nps)
      csMBA@(MutableByteArray csMBA') <- newByteArray (fromIntegral $ 2*ncs)
      copy_glyph_outline ff psMBA' tsMBA' csMBA'
      psBA <- unsafeFreezeByteArray psMBA
      tsBA <- unsafeFreezeByteArray tsMBA
      csBA <- unsafeFreezeByteArray csMBA
      return $ Outline (fromIntegral nps) (fromIntegral ncs) psBA tsBA csBA

-- Glyph info ----------------------------------------------------------

-- data Glyph = Glyph
--   { glyphBitmap :: GlyphBitmap
--   , glyphMetrics :: GlyphMetrics
--   , glyphOutline :: GlyphOutline
--   }

------------------------------------------------------------------------
-- Harfbuzz
------------------------------------------------------------------------

foreign import ccall "string_advances"
  string_placements
    :: FontFace
    -> CString
    -> Ptr CInt -- length of string
    -> Ptr CInt -- width of rendered string
    -> MutableByteArray# RealWorld -- codepoints
    -> MutableByteArray# RealWorld -- advances
    -> IO ()

-- | The result of shaping a single lined string. Contains the total
--   length of the string (in 1/64th pixels), the number of characters in
--   the string, the codepoints for each character and the advances for
--   each character.
data Advances = Advances
  { _hbLength     :: !Int
  , _hbWidth      :: !Int
  , _hbCodepoints :: !ByteArray
  , _hbAdvances   :: !ByteArray
  }

showHB :: Advances -> IO ()
showHB (Advances n w cs as) = do
  putStrLn $ "length = " ++ show n
  putStrLn $ "width  = " ++ show w
  for_ [0..n-1] $ \i -> do
    let c = indexByteArray cs i :: Int32
        a = indexByteArray as i :: Int32
    putStrLn $ "codepoint: " ++ show c ++ "\t advance: " ++ show a

advancesCodepoints :: Advances -> [CharIndex]
advancesCodepoints (Advances l _ cs _) = go 0 where
  go i
    | i == l    = []
    | otherwise = CharIndex ((fromIntegral :: Int32 -> CUInt) (indexByteArray cs i))
                : go (i+1)

-- | Fold over each character along with the position that character
--   should be placed.
foldAdvances :: Monad m => (CharIndex -> Int -> m ()) -> Advances -> m ()
foldAdvances f (Advances l _ cs as) = go 0 0 where
  go i o
    | i == l    = return ()
    | otherwise = do
        f (CharIndex . fromC $ indexByteArray cs i) o
        go (i+1) (o + fromI (indexByteArray as i))

  fromI :: Int32 -> Int
  fromI = fromIntegral
  fromC :: Int32 -> CUInt
  fromC = fromIntegral
{-# INLINE foldAdvances #-}

advances :: FontFace -> String -> IO Advances
advances ff str =
  alloca $ \nPtr ->
  alloca $ \wPtr ->
  withCString str $ \cStr -> do
    csMBA@(MutableByteArray csMBA#) <- newByteArray (8*strN)
    asMBA@(MutableByteArray asMBA#) <- newByteArray (8*strN)
    string_placements ff cStr nPtr wPtr csMBA# asMBA#
    n <- peek nPtr
    w <- peek wPtr
    csBA <- unsafeFreezeByteArray csMBA
    asBA <- unsafeFreezeByteArray asMBA
    return $ Advances (fromIntegral n) (fromIntegral w) csBA asBA
  where strN = length str
{-# INLINE advances #-}

------------------------------------------------------------------------
-- Maths
------------------------------------------------------------------------

foreign import ccall unsafe "hb_ft_face_create_referenced"
  hb_ft_face_create :: FontFace -> IO HB_face

foreign import ccall unsafe "hb_ft_font_create_referenced"
  hb_ft_font_create :: FontFace -> IO HB_font

newtype HB_font = HB_font (Ptr ())
newtype HB_face = HB_face (Ptr ())

foreign import ccall "hb_ot_math_has_data"
  hasMath
    :: HB_face
    -> IO CInt

newtype HB_pos = HB_pos Int32
  deriving (Num, Enum, Real, Eq, Ord, Integral, Show, Storable)

foreign import ccall "hb_ot_math_get_constant"
  getMathConstant
    :: HB_font
    -> HB_math_constant
    -> IO HB_pos

foreign import ccall "hb_ot_math_get_glyph_italics_correction"
  italicsCorrection
    :: HB_font
    -> CharIndex
    -> IO HB_pos

foreign import ccall "hb_ot_math_get_glyph_accent_attachment"
  accentAttachment
    :: HB_font
    -> CharIndex
    -> IO HB_pos

newtype HB_math_constant = HB_math_constant Word32
  deriving (Num, Enum, Real, Eq, Ord, Integral, Show)

pattern HB_OT_MATH_CONSTANT_SCRIPT_PERCENT_SCALE_DOWN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_SCRIPT_PERCENT_SCALE_DOWN = 0

pattern HB_OT_MATH_CONSTANT_SCRIPT_SCRIPT_PERCENT_SCALE_DOWN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_SCRIPT_SCRIPT_PERCENT_SCALE_DOWN = 1

pattern HB_OT_MATH_CONSTANT_DELIMITED_SUB_FORMULA_MIN_HEIGHT :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_DELIMITED_SUB_FORMULA_MIN_HEIGHT = 2

pattern HB_OT_MATH_CONSTANT_DISPLAY_OPERATOR_MIN_HEIGHT :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_DISPLAY_OPERATOR_MIN_HEIGHT = 3

pattern HB_OT_MATH_CONSTANT_MATH_LEADING :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_MATH_LEADING = 4

pattern HB_OT_MATH_CONSTANT_AXIS_HEIGHT :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_AXIS_HEIGHT = 5

pattern HB_OT_MATH_CONSTANT_ACCENT_BASE_HEIGHT :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_ACCENT_BASE_HEIGHT = 6

pattern HB_OT_MATH_CONSTANT_FLATTENED_ACCENT_BASE_HEIGHT :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_FLATTENED_ACCENT_BASE_HEIGHT = 7

pattern HB_OT_MATH_CONSTANT_SUBSCRIPT_SHIFT_DOWN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_SUBSCRIPT_SHIFT_DOWN = 8

pattern HB_OT_MATH_CONSTANT_SUBSCRIPT_TOP_MAX :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_SUBSCRIPT_TOP_MAX = 9

pattern HB_OT_MATH_CONSTANT_SUBSCRIPT_BASELINE_DROP_MIN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_SUBSCRIPT_BASELINE_DROP_MIN = 10

pattern HB_OT_MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP = 11

pattern HB_OT_MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP_CRAMPED :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP_CRAMPED = 12

pattern HB_OT_MATH_CONSTANT_SUPERSCRIPT_BOTTOM_MIN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_SUPERSCRIPT_BOTTOM_MIN = 13

pattern HB_OT_MATH_CONSTANT_SUPERSCRIPT_BASELINE_DROP_MAX :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_SUPERSCRIPT_BASELINE_DROP_MAX = 14

pattern HB_OT_MATH_CONSTANT_SUB_SUPERSCRIPT_GAP_MIN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_SUB_SUPERSCRIPT_GAP_MIN = 15

pattern HB_OT_MATH_CONSTANT_SUPERSCRIPT_BOTTOM_MAX_WITH_SUBSCRIPT :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_SUPERSCRIPT_BOTTOM_MAX_WITH_SUBSCRIPT = 16

pattern HB_OT_MATH_CONSTANT_SPACE_AFTER_SCRIPT :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_SPACE_AFTER_SCRIPT = 17

pattern HB_OT_MATH_CONSTANT_UPPER_LIMIT_GAP_MIN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_UPPER_LIMIT_GAP_MIN = 18

pattern HB_OT_MATH_CONSTANT_UPPER_LIMIT_BASELINE_RISE_MIN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_UPPER_LIMIT_BASELINE_RISE_MIN = 19

pattern HB_OT_MATH_CONSTANT_LOWER_LIMIT_GAP_MIN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_LOWER_LIMIT_GAP_MIN = 20

pattern HB_OT_MATH_CONSTANT_LOWER_LIMIT_BASELINE_DROP_MIN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_LOWER_LIMIT_BASELINE_DROP_MIN = 21

pattern HB_OT_MATH_CONSTANT_STACK_TOP_SHIFT_UP :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_STACK_TOP_SHIFT_UP = 22

pattern HB_OT_MATH_CONSTANT_STACK_TOP_DISPLAY_STYLE_SHIFT_UP :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_STACK_TOP_DISPLAY_STYLE_SHIFT_UP = 23

pattern HB_OT_MATH_CONSTANT_STACK_BOTTOM_SHIFT_DOWN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_STACK_BOTTOM_SHIFT_DOWN = 24

pattern HB_OT_MATH_CONSTANT_STACK_BOTTOM_DISPLAY_STYLE_SHIFT_DOWN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_STACK_BOTTOM_DISPLAY_STYLE_SHIFT_DOWN = 25

pattern HB_OT_MATH_CONSTANT_STACK_GAP_MIN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_STACK_GAP_MIN = 26

pattern HB_OT_MATH_CONSTANT_STACK_DISPLAY_STYLE_GAP_MIN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_STACK_DISPLAY_STYLE_GAP_MIN = 27

pattern HB_OT_MATH_CONSTANT_STRETCH_STACK_TOP_SHIFT_UP :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_STRETCH_STACK_TOP_SHIFT_UP = 28

pattern HB_OT_MATH_CONSTANT_STRETCH_STACK_BOTTOM_SHIFT_DOWN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_STRETCH_STACK_BOTTOM_SHIFT_DOWN = 29

pattern HB_OT_MATH_CONSTANT_STRETCH_STACK_GAP_ABOVE_MIN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_STRETCH_STACK_GAP_ABOVE_MIN = 30

pattern HB_OT_MATH_CONSTANT_STRETCH_STACK_GAP_BELOW_MIN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_STRETCH_STACK_GAP_BELOW_MIN = 31

pattern HB_OT_MATH_CONSTANT_FRACTION_NUMERATOR_SHIFT_UP :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_FRACTION_NUMERATOR_SHIFT_UP = 32

pattern HB_OT_MATH_CONSTANT_FRACTION_NUMERATOR_DISPLAY_STYLE_SHIFT_UP :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_FRACTION_NUMERATOR_DISPLAY_STYLE_SHIFT_UP = 33

pattern HB_OT_MATH_CONSTANT_FRACTION_DENOMINATOR_SHIFT_DOWN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_FRACTION_DENOMINATOR_SHIFT_DOWN = 34

pattern HB_OT_MATH_CONSTANT_FRACTION_DENOMINATOR_DISPLAY_STYLE_SHIFT_DOWN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_FRACTION_DENOMINATOR_DISPLAY_STYLE_SHIFT_DOWN = 35

pattern HB_OT_MATH_CONSTANT_FRACTION_NUMERATOR_GAP_MIN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_FRACTION_NUMERATOR_GAP_MIN = 36

pattern HB_OT_MATH_CONSTANT_FRACTION_NUM_DISPLAY_STYLE_GAP_MIN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_FRACTION_NUM_DISPLAY_STYLE_GAP_MIN = 37

pattern HB_OT_MATH_CONSTANT_FRACTION_RULE_THICKNESS :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_FRACTION_RULE_THICKNESS = 38

pattern HB_OT_MATH_CONSTANT_FRACTION_DENOMINATOR_GAP_MIN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_FRACTION_DENOMINATOR_GAP_MIN = 39

pattern HB_OT_MATH_CONSTANT_FRACTION_DENOM_DISPLAY_STYLE_GAP_MIN :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_FRACTION_DENOM_DISPLAY_STYLE_GAP_MIN = 40

pattern HB_OT_MATH_CONSTANT_SKEWED_FRACTION_HORIZONTAL_GAP :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_SKEWED_FRACTION_HORIZONTAL_GAP = 41

pattern HB_OT_MATH_CONSTANT_SKEWED_FRACTION_VERTICAL_GAP :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_SKEWED_FRACTION_VERTICAL_GAP = 42

pattern HB_OT_MATH_CONSTANT_OVERBAR_VERTICAL_GAP :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_OVERBAR_VERTICAL_GAP = 43

pattern HB_OT_MATH_CONSTANT_OVERBAR_RULE_THICKNESS :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_OVERBAR_RULE_THICKNESS = 44

pattern HB_OT_MATH_CONSTANT_OVERBAR_EXTRA_ASCENDER :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_OVERBAR_EXTRA_ASCENDER = 45

pattern HB_OT_MATH_CONSTANT_UNDERBAR_VERTICAL_GAP :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_UNDERBAR_VERTICAL_GAP = 46

pattern HB_OT_MATH_CONSTANT_UNDERBAR_RULE_THICKNESS :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_UNDERBAR_RULE_THICKNESS = 47

pattern HB_OT_MATH_CONSTANT_UNDERBAR_EXTRA_DESCENDER :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_UNDERBAR_EXTRA_DESCENDER = 48

pattern HB_OT_MATH_CONSTANT_RADICAL_VERTICAL_GAP :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_RADICAL_VERTICAL_GAP = 49

pattern HB_OT_MATH_CONSTANT_RADICAL_DISPLAY_STYLE_VERTICAL_GAP :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_RADICAL_DISPLAY_STYLE_VERTICAL_GAP = 50

pattern HB_OT_MATH_CONSTANT_RADICAL_RULE_THICKNESS :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_RADICAL_RULE_THICKNESS = 51

pattern HB_OT_MATH_CONSTANT_RADICAL_EXTRA_ASCENDER :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_RADICAL_EXTRA_ASCENDER = 52

pattern HB_OT_MATH_CONSTANT_RADICAL_KERN_BEFORE_DEGREE :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_RADICAL_KERN_BEFORE_DEGREE = 53

pattern HB_OT_MATH_CONSTANT_RADICAL_KERN_AFTER_DEGREE :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_RADICAL_KERN_AFTER_DEGREE = 54

pattern HB_OT_MATH_CONSTANT_RADICAL_DEGREE_BOTTOM_RAISE_PERCENT :: HB_math_constant
pattern HB_OT_MATH_CONSTANT_RADICAL_DEGREE_BOTTOM_RAISE_PERCENT = 55

-- Glyph kerning -------------------------------------------------------

foreign import ccall "hb_ot_math_get_glyph_kerning"
  glyph_kerning
    :: HB_font
    -> CharIndex -- ^ glyph to find kerning for
    -> HB_math_kern -- ^ position of kern
    -> HB_pos -- ^ correction height
    -> IO HB_pos

newtype HB_math_kern = HB_math_kern Word32
  deriving (Num, Enum, Real, Eq, Ord, Integral, Show)

pattern HB_OT_MATH_KERN_TOP_RIGHT :: HB_math_kern
pattern HB_OT_MATH_KERN_TOP_RIGHT = 0

pattern HB_OT_MATH_KERN_TOP_LEFT :: HB_math_kern
pattern HB_OT_MATH_KERN_TOP_LEFT = 1

pattern HB_OT_MATH_KERN_BOTTOM_RIGHT :: HB_math_kern
pattern HB_OT_MATH_KERN_BOTTOM_RIGHT = 2

pattern HB_OT_MATH_KERN_BOTTOM_LEFT :: HB_math_kern
pattern HB_OT_MATH_KERN_BOTTOM_LEFT = 3

-- Glyph variants ------------------------------------------------------

foreign import ccall "hb_ot_math_get_glyph_variants"
  hb_glyph_variants
    :: HB_font
    -> CharIndex -- ^ glyph to stretch
    -> HB_direction -- ^ direction of stretching
    -> CUInt -- ^ start_offset
    -> Ptr CUInt -- ^ variants_count
    -> Ptr HB_glyph_variant -- ^ the variants
    -> IO CUInt

data HB_glyph_variant = HB_glyph_variant
  { glyph_variant_codepoint :: CharIndex
  , glyph_variant_position  :: HB_pos
  } deriving Show

instance Storable HB_glyph_variant where
  sizeOf _ = sizeOf (0 :: CUInt) + sizeOf (0 :: CUInt)
  alignment _ = alignment (0::CUInt)
  peek ptr = do
    idx <- peek (castPtr ptr)
    pos <- peekElemOff (castPtr ptr) 1
    return $ HB_glyph_variant (CharIndex idx) (HB_pos pos)
  poke ptr (HB_glyph_variant (CharIndex idx) (HB_pos pos)) = do
    poke (castPtr ptr) idx
    pokeElemOff (castPtr ptr) 1 pos

newtype HB_direction = HB_direction CInt
  deriving Show

pattern HB_DIRECTION_INVALID :: HB_direction
pattern HB_DIRECTION_INVALID = HB_direction 0

pattern HB_DIRECTION_LTR :: HB_direction
pattern HB_DIRECTION_LTR = HB_direction 4

pattern HB_DIRECTION_RTL :: HB_direction
pattern HB_DIRECTION_RTL = HB_direction 5

pattern HB_DIRECTION_TTB :: HB_direction
pattern HB_DIRECTION_TTB = HB_direction 6

pattern HB_DIRECTION_BTT :: HB_direction
pattern HB_DIRECTION_BTT = HB_direction 7

glyphVariants :: HB_font -> CharIndex -> HB_direction -> Int -> IO [HB_glyph_variant]
glyphVariants font cix dir off =
  alloca $ \nPtr ->
  allocaArray 16 $ \ptr -> do
    poke nPtr 16
    actualNum <- hb_glyph_variants font cix dir (fromIntegral off) nPtr ptr
    n <- peek nPtr

    -- I think 16 should be enough, but just incase there's more run it
    -- again to get all of them.
    if actualNum > n
      then
        allocaArray (fromIntegral n) $ \ptr' -> do
          poke nPtr n
          actualNum' <- hb_glyph_variants font cix dir (fromIntegral off) nPtr ptr
          n' <- peek nPtr
          when (actualNum' /= n') $ putStrLn ("glyphVariants: looks like something's wrong: " ++ show n' ++ " " ++ show actualNum')
          peekArray (fromIntegral n') ptr'

      else peekArray (fromIntegral n) ptr

-- assembly ------------------------------------------------------------

foreign import ccall "hb_ot_math_get_min_connector_overlap"
  minConnectorOverlap
    :: HB_font
    -> HB_direction -- ^ direction of assembly
    -> IO HB_pos

foreign import ccall "hb_ot_math_get_glyph_assembly"
  hb_glyph_assembly
    :: HB_font
    -> CharIndex -- ^ glyph to stretch
    -> HB_direction -- ^ direction of stretching
    -> CUInt -- ^ start_offset
    -> Ptr CUInt -- ^ parts_count
    -> Ptr HB_glyph_part -- ^ the parts
    -> Ptr HB_pos -- ^ italics_correction
    -> IO CUInt

glyphAssembly :: HB_font -> CharIndex -> HB_direction -> Int -> IO (HB_pos, [HB_glyph_part])
glyphAssembly font cix dir off =
  alloca $ \nPtr ->
  alloca $ \iPtr ->
  allocaArray 16 $ \ptr -> do
    poke nPtr 16
    actualNum <- hb_glyph_assembly font cix dir (fromIntegral off) nPtr ptr iPtr
    when (actualNum > 16) $ putStrLn "more than 16 parts in glyph assembly!"
    n <- peek nPtr
    parts <- peekArray (fromIntegral n) ptr
    i <- peek iPtr -- italics correction
    return (i, parts)

data HB_glyph_part = HB_glyph_part
  { glyph_part_code        :: CharIndex
  , start_connector_length :: HB_pos
  , end_connector_length   :: HB_pos
  , full_advance           :: HB_pos
  , glyph_is_extender      :: Bool
  } deriving Show

instance Storable HB_glyph_part where
  sizeOf _ = sizeOf (0 :: CUInt) + 3*sizeOf (0 :: CUInt) + sizeOf (0::CUInt)
  alignment _ = alignment (0::CUInt)
  peek ptr = do
    idx <- peek (castPtr ptr)
    l1 <- peekElemOff (castPtr ptr) 1
    l2 <- peekElemOff (castPtr ptr) 2
    ad <- peekElemOff (castPtr ptr) 3
    fl <- peekElemOff (castPtr ptr) 4
    return $ HB_glyph_part idx l1 l2 ad (fl == (1::CInt))

  poke ptr (HB_glyph_part idx l1 l2 ad fl) = do
    poke (castPtr ptr) idx
    pokeElemOff (castPtr ptr) 1 l1
    pokeElemOff (castPtr ptr) 2 l2
    pokeElemOff (castPtr ptr) 3 ad
    pokeElemOff (castPtr ptr) 4 (bool 0 1 fl :: CInt)

