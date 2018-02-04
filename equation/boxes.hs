{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}
import Codec.Picture
import Letters.Internal
import Graphics.Rasterific hiding (V2 (..), Point)
import qualified Graphics.Rasterific as R

import Data.Primitive.ByteArray
import Debug.Trace
import Graphics.Text.TrueType
import Graphics.Rasterific.Texture
import System.IO.Unsafe
import Data.Semigroup
import Data.Word
import Control.Lens
import Data.Traversable
import Data.Foldable

import Control.Monad.RWS hiding ((<>))
import Control.Monad.IO.Class
import Linear
import Control.Applicative

import Data.List (concat)

import Data.Foldable
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Linear
import Linear.Affine


type Distance  = F26_6

-- Boxes ---------------------------------------------------------------

data Box f a = Box
  { boxWidth  :: !Distance
    -- ^ The width of the box

  , boxHeight :: !Distance
    -- ^ the height of the box above the reference point

  , boxDepth  :: !Distance
    -- ^ the depth of the box below the reference point

  , boxShift  :: !Distance
    -- ^ how much this box should be lowered in horizontal list or how
    --   much it should be moved to the right in vertical list

  -- , boxGlue :: !BoxGlue
    -- The glue used for the box when it is part of a list.

  , boxContents :: Seq (f a)
  -- ^ The contents of the box. If 'Nothing', the box is empty.
  } deriving Show

emptyBox :: Box f a
emptyBox = Box
  { boxWidth = 0
  , boxHeight = 0
  , boxDepth = 0
  , boxShift = 0
  , boxContents = Seq.empty
  }

type HBox = Box HorizontalElement
type VBox = Box VerticalElement
-- type MBox = Box MathElement

-- data GlueDirection = Rigid | Shrink | Stretch
--   deriving Show

-- data GlueOrder = FiniteGlue | Fil | Fill | Filll
--   deriving Show

-- from the description of boxes, it makes it look like this is the
-- definition of glue for a box, but it may actually the definition for
-- Glue that I already have.
-- data BoxGlue = BoxGlue
--   { glueValue     :: Distance
--   , glueDirection :: GlueUnit
--   , glueOrder     :: GlueOrder
--   } deriving Show

-- Glue ----------------------------------------------------------------

data GlueUnit
  = FiniteGlue Distance
  | Fil Distance
  | Fill Distance
  | Filll Distance
  deriving Show

data Glue = Glue
  { glueSpace   :: Distance
  , glueStretch :: GlueUnit
  , glueShrink  :: GlueUnit
  } deriving Show

-- Rules ---------------------------------------------------------------

-- | A solid black box. (Tex also allows values to be 'running'. This
--   means it will run to the boundary of the innermost enclosing box).
data Rule = Rule
  { ruleWidth  :: !Distance
  , ruleHeight :: !Distance
  , ruleDepth  :: !Distance
  } deriving Show

-- Kern ----------------------------------------------------------------

-- A kern node can also appear in a vertical list, when its ‘width’
-- denotes additional spacing in the vertical direction. The subtype is
-- either normal (for kerns inserted from font information or math mode
-- calculations) or explicit (for kerns inserted from \kern and \/
-- commands) or acc kern (for kerns inserted from non-math accents) or
-- mu glue (for kerns inserted from \mkern specifications in math
-- formulas).

data Kern = Kern
  { kernAmmount :: !Distance
  , kernType    :: !KernType
  }
  deriving Show

data KernType
  = NormalKern
  | AccentKern
  | ExplicitKern
  | MathKern
  deriving Show

-- Horizontal list -----------------------------------------------------

data HorizontalElement a
  = HHBox !(HBox a)
  | HVBox !(VBox a)
  | HRule !Rule
  | HKern !Kern
  | HGlue !Glue
  | HString String
  | HCustom a
  deriving Show

type HList a = [HorizontalElement a]

-- Vertical list -------------------------------------------------------

data VerticalElement a
  = VVBox !(VBox a)
  | VHBox !(HBox a)
  | VRule !Rule
  | VKern !Kern
  | VGlue !Glue
  | VString String
  | VCustom a
  deriving Show

type VList a = [VerticalElement a]






sz :: Num a => a
sz = 14

myfont :: FilePath
myfont = xits

deja :: FilePath
deja = "/Users/christopher/Documents/truetype/DejaVuSerif.ttf"

ffFont :: Font
ffFont = unsafePerformIO $ do
  ef <- loadFontFile myfont
  case ef of
    Left err -> error err
    Right f  -> return f

makethatIMG :: FontFace -> CharIndex -> IO (Float,Float,Image PixelRGBA8)
makethatIMG ff c = do
  GlyphBitmap l t (Bitmap x y p img) <- bitmap ff c
  let f i j = PixelRGBA8 0 0 0 a' -- (255-a') (255-a') (255-a') a'
        where a = indexByteArray img (j*p + i) :: Pixel8
              a' = a -- round $ sqrt ((fromIntegral a)/255 :: Double) * 255

  return (fromIntegral l,fromIntegral t,generateImage f x y :: Image PixelRGBA8)

mydrawing :: FontFace -> Advances -> FontFace -> Advances -> Drawing PixelRGBA8 ()
mydrawing ff1 as1 ff2 as2 = do
  -- let as = singleFace myfont sz $ advances str
  let f c x = drawImage img 1 (fmap rounding $ R.V2 (30+(fromIntegral x/64)+l) y)
        where (l,t,img) = unsafePerformIO $ makethatIMG ff1 c
              y   = 200 - t

  let g c x = drawImage img 1 (fmap rounding $ R.V2 xx yy)
        where (l,t,img) = unsafePerformIO $ makethatIMG ff2 c
              yy  = 200 - 34 - t
              xx  = fromIntegral (_hbWidth as1 + x)/64 + 30 + l
  -- withTexture (uniformTexture $ PixelRGBA8 200 0 0 255) $
  --   printTextAt ffFont (PointSize sz) (V2 30 220) str
  foldAdvances f as1
  foldAdvances g as2
  -- foldAdvances g as
  -- foldAdvances g as
  -- drawImage (makethatIMG 'c') 0 (V2 5 20)
  -- drawImage (makethatIMG 'a') 0 (V2 40 5)
  -- drawImage (makethatIMG 'b') 0 (V2 65 5)

rounding :: Float -> Float
rounding = fromIntegral . round

renderedImg
  :: FontFace -> Advances
  -> FontFace -> Advances
  -> Image PixelRGBA8
renderedImg ff1 ads1 ff2 ads2 =
  renderDrawingAtDpi 1024 512 144 (PixelRGBA8 255 255 255 255) (mydrawing ff1 ads1 ff2 ads2)

doit :: String -> String -> IO ()
doit str1 str2 = do
  lib  <- newLibrary
  ff1 <- newFontFace lib xits (64*42) 144
  ff2 <- newFontFace lib xits (round $ 0.73*64*42) 144
  advs1 <- advances ff1 str1
  advs2 <- advances ff2 str2
  writePng "maths.png" (renderedImg ff1 advs1 ff2 advs2)

menlo :: FilePath
menlo = "/System/Library/Fonts/Menlo.ttc"

brush :: FilePath
brush = "/Library/Fonts/Brush Script.ttf"

zap :: FilePath
zap = "/Library/Fonts/Zapfino.ttf"

xits :: FilePath
xits = "/Users/christopher/Documents/truetype/letters/fonts/xits-math-master/xits-math.otf"

asana :: FilePath
asana = "/Users/christopher/Documents/truetype/letters/fonts/Asana-Math/Asana-Math.otf"

data Math
  = MNum String
  | MOp  String
  | MSup Math Math
  | MSub Math Math
  | MSupSub Math Math Math
  | Sqrt Math
  | MathIt String

data Faces = Faces
  { _mathFace            :: FontFace
  , _mathFont_hb         :: HB_font
  , _scriptFace          :: FontFace
  , _scriptFont_hb       :: HB_font
  , _scriptScriptFace    :: FontFace
  , _scriptScriptFont_hb :: HB_font
  , _normalFace          :: FontFace
  , _normalFont_hb       :: HB_font
  }

makeLenses ''Faces

data Layout = Layout
  { glyphs :: Seq Glyph
  , rules  :: Seq (Point V2 F26_6, Rule)
  }

data Glyph = Glyph
  { glyphFace      :: FontFace
  , glyphFont_hb   :: HB_font
  , glyphPosition  :: Point V2 F26_6
  -- , glyphUnicode   :: Char
  , glyphCodepoint :: CharIndex
  }


-- makeLenses ''Layout

-- data Rule = Rule
--   { rulePos   :: V2 F26_6
--   , ruleLine  :: F26_6
--   , ruleWidth :: F26_6
--   }

data Pen = Pen
  { _penPos :: Point V2 F26_6
  , _penSupDepth :: Int
  , _penSubDepth :: Int
  }

makeLenses ''Pen


instance Semigroup Layout where
  Layout g1 r1 <> Layout g2 r2 = Layout (g1<>g2) (r1<>r2)
instance Monoid Layout where
  mappend = (<>)
  mempty = Layout mempty mempty

newtype EqBuilder a = EqBuilders {unEq :: RWST Faces Layout Pen IO a}
  deriving (Functor, Applicative, Monad, MonadReader Faces, MonadWriter Layout, MonadState Pen, MonadIO)

buildMath :: Math -> EqBuilder ()
buildMath = \case
  MNum num -> mathStringRender num
  MOp  op  -> normalStringRender op -- probably needs spacing?
  MSup level super -> buildMath level >> raised (buildMath super)
  MSub level sub   -> buildMath level >> raised (buildMath sub)
  MSupSub level super sub -> do
    buildMath level
    p <- use penPos
    raised $ buildMath super
    penPos .= p
    lowered $ buildMath sub
  Sqrt math -> mathStringRender "√(" >> buildMath math >> mathStringRender ")"
  MathIt str  -> mathStringRender str -- map regular to math italic unicode

currentMathFace :: EqBuilder (FontFace, HB_font)
currentMathFace = do
  supDepth <- use penSupDepth :: EqBuilder Int
  subDepth <- use penSupDepth :: EqBuilder Int
  let maxDepth = max supDepth subDepth
  if | maxDepth == 0 -> liftA2 (,) (view mathFace) (view mathFont_hb)
     | maxDepth == 1 -> liftA2 (,) (view scriptFace) (view scriptFont_hb)
     | otherwise     -> liftA2 (,) (view scriptScriptFace) (view scriptScriptFont_hb)

mathStringRender :: String -> EqBuilder ()
mathStringRender str = do
  (face, hb_font) <- currentMathFace
  someStringRender face hb_font str

-- USE NORMAL FONT
normalStringRender :: String -> EqBuilder ()
normalStringRender str = do
  (face, hb_font) <- currentMathFace
  someStringRender face hb_font str

someStringRender :: FontFace -> HB_font -> String -> EqBuilder ()
someStringRender face hb_font str = do
  adv <- liftIO $ advances face str
  pos <- use penPos
  flip foldAdvances adv $ \c x -> do
    addGlyph face hb_font (pos & _x +~ fromIntegral x) c
  penPos._x += fromIntegral (_hbWidth adv)

addGlyph :: FontFace -> HB_font -> Point V2 F26_6 -> CharIndex -> EqBuilder ()
addGlyph ff hb_font pos i = tell $ Layout (Seq.singleton glyph) mempty
  where glyph = Glyph ff hb_font pos i

mathConstant :: HB_math_constant -> EqBuilder F26_6
mathConstant c = do
  hb_font  <- view mathFont_hb
  HB_pos x <- liftIO $ getMathConstant hb_font c
  return (F26_6 x)

raised :: EqBuilder () -> EqBuilder ()
raised eq = do
  raise_dy <- mathConstant HB_OT_MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP

  penSupDepth += 1
  penPos._y += raise_dy
  eq
  penPos._y -= raise_dy
  penSupDepth -= 1

lowered :: EqBuilder () -> EqBuilder ()
lowered eq = do
  lower_dy <- mathConstant HB_OT_MATH_CONSTANT_SUBSCRIPT_SHIFT_DOWN
  penSubDepth += 1
  penPos._y -= lower_dy
  eq
  penPos._y += lower_dy
  penSubDepth -= 1

drawGlyph :: Glyph -> Drawing PixelRGBA8 ()
drawGlyph (Glyph ff _ pos c) =
  drawImage img 1 (fmap rounding $ R.V2 (30+x+l) y)
  where (l,t,img) = unsafePerformIO $ makethatIMG ff c
        -- y   = 200 - t
        x = fromIntegral (pos^._x) / 64
        y = 200 - t - fromIntegral (pos^._y) / 64

------------------------------------------------------------------------
-- RENDERING HBOXES
------------------------------------------------------------------------

addToHBox :: Distance -> Distance -> Distance -> HorizontalElement a -> HBox a -> HBox a
addToHBox width height depth ele box =
  box { boxContents = boxContents box Seq.|> ele
      , boxWidth    = boxWidth box + width
      , boxDepth    = max (boxDepth box) depth
      , boxHeight   = max (boxHeight box) height
      }

mkHbox :: [HorizontalElement a] -> EqBuilder (HBox a)
mkHbox = foldlM f emptyBox where
  f box = \case
    HHBox hbox -> return $ addToHBox (boxWidth hbox) (boxHeight hbox) (boxDepth hbox) (HHBox hbox) box
    HVBox vbox -> return $ addToHBox (boxWidth vbox) (boxHeight vbox) (boxDepth vbox) (HVBox vbox) box
    HRule rule -> return $ addToHBox (ruleWidth rule) (ruleHeight rule) (ruleDepth rule) (HRule rule) box
    HKern kern -> return $ addToHBox (kernAmmount kern) 0 0 (HKern kern) box
    HGlue glue -> return box
    HString str -> do
      (face, hb_font) <- currentMathFace
      -- someStringRender face hb_font str
      adv <- liftIO $ advances face str
      -- pos <- use penPos
      -- penPos._x += fromIntegral (_hbWidth adv)
      return $ addToHBox (fromIntegral $ _hbWidth adv) (12*64) (2*64) (HString str) box
    HCustom a -> return box

addToVBox :: Distance -> Distance -> Distance -> VerticalElement a -> VBox a -> VBox a
addToVBox width height depth ele box =
  box { boxContents = boxContents box Seq.|> ele
      , boxWidth    = max (boxWidth box) width
      , boxDepth    = depth
      , boxHeight   = boxHeight box + boxDepth box + height
      }

mkVbox :: [VerticalElement a] -> EqBuilder (VBox a)
mkVbox = foldlM f emptyBox where
  f box = \case
    VHBox hbox -> return $ addToVBox (boxWidth hbox) (boxHeight hbox) (boxDepth hbox) (VHBox hbox) box
    VVBox vbox -> return $ addToVBox (boxWidth vbox) (boxHeight vbox) (boxDepth vbox) (VVBox vbox) box
    VRule rule -> return $ addToVBox (ruleWidth rule) (ruleHeight rule) (ruleDepth rule) (VRule rule) box
    VKern kern -> return $ addToVBox 0 (kernAmmount kern) 0 (VKern kern) box
    VGlue glue -> return box
    VString str -> do
      (face, hb_font) <- currentMathFace
      -- someStringRender face hb_font str
      adv <- liftIO $ advances face str
      -- pos <- use penPos
      -- penPos._x += fromIntegral (_hbWidth adv)
      return $ addToVBox (fromIntegral $ _hbWidth adv) (70*64) (8*64) (VString str) box
    VCustom a -> return box

renderHBox :: HBox () -> EqBuilder ()
renderHBox hb = traverse_ go (boxContents hb) where
  go = \case
    HHBox hbox -> do
      p <- use penPos
      renderHBox hbox
      penPos .= p .+^ V2 (boxWidth hbox) 0
    HVBox vbox -> do
      p <- use penPos
      penPos .= p .+^ V2 0 (boxHeight vbox)
      renderVBox vbox
      penPos .= p .+^ V2 (boxWidth vbox) 0
    HRule rule -> do
      p <- use penPos
      renderRule rule
      penPos .= p .+^ V2 (ruleWidth rule) 0

    HGlue glue -> renderGlue glue
    -- HKern glue -> renderGlue glue
    HKern kern -> penPos._x += kernAmmount kern
    HString str -> mathStringRender str
    HCustom ()  -> return ()

renderVBox :: VBox () -> EqBuilder ()
renderVBox hb = traverse_ go (boxContents hb) where
  go = \case
    VHBox hbox -> do
      p <- use penPos
      -- penPos .= p .-^ V2 0 (boxHeight hbox)
      renderHBox hbox
      penPos .= p .-^ V2 0 (boxDepth hbox)
    VVBox vbox -> do
      p <- use penPos
      penPos .= p .-^ V2 0 (boxHeight vbox)
      renderVBox vbox
      penPos .= p .-^ V2 0 (boxDepth vbox)
    VRule rule -> do
      p <- use penPos
      renderRule rule
      penPos .= p .-^ V2 0 (ruleHeight rule + ruleDepth rule)
    VGlue glue -> return () -- renderGlue glue
    VKern kern -> penPos._y -= kernAmmount kern
    VString str -> do
      p <- use penPos
      penPos .= p .-^ V2 0 (70*64)
      mathStringRender str
      penPos .= p .-^ V2 0 (78*64)
    VCustom ()  -> return ()

renderGlue :: Glue -> EqBuilder ()
renderGlue _ = return ()

renderRule :: Rule -> EqBuilder ()
renderRule rule = do
  p <- use penPos
  tell $ Layout mempty (Seq.singleton (p,rule))


-- RENDERING HBOXES END ------------------------------------------------

-- fixed point to floating point
f2f :: F26_6 -> Float
f2f f = fromIntegral f / 64

renderLayout
  :: Layout
  -> Image PixelRGBA8
renderLayout (Layout glyphs rules) =
  -- renderDrawingAtDpi 2024 8012 144 (PixelRGBA8 255 255 255 255) $ do
  renderDrawingAtDpi 1024 1024 144 (PixelRGBA8 255 255 255 255) $ do
    for_ glyphs drawGlyph
    for_ rules $ \(P (V2 x y), rule) ->
      withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) . fill $
        rectangle (f2f <$> R.V2 (64*30+x) (64*200 - (y + ruleHeight rule))) (f2f $ ruleWidth rule) (f2f $ ruleDepth rule + ruleHeight rule)

renderEq :: EqBuilder () -> IO ()
renderEq eq = do
  let size = 36*64 :: Num n => n

  lib <- newLibrary
  mathFace    <- newFontFace lib xits (size) 144
  mathFont_hb <- hb_ft_font_create mathFace

  scriptM <- getMathConstant mathFont_hb HB_OT_MATH_CONSTANT_SCRIPT_PERCENT_SCALE_DOWN
  scriptFace    <- newFontFace lib xits (round $ (fromIntegral scriptM / 100::Double) * fromIntegral size) 144
  scriptFont_hb <- hb_ft_font_create scriptFace

  scriptScriptM <- getMathConstant mathFont_hb HB_OT_MATH_CONSTANT_SCRIPT_SCRIPT_PERCENT_SCALE_DOWN
  scriptScriptFace    <- newFontFace lib xits (round $ (fromIntegral scriptScriptM / 100::Double) * fromIntegral size) 144
  scriptScriptFont_hb <- hb_ft_font_create scriptScriptFace

  let faces = Faces
        { _mathFace            = mathFace
        , _mathFont_hb         = mathFont_hb
        , _scriptFace          = scriptFace
        , _scriptFont_hb       = scriptFont_hb
        , _scriptScriptFace    = scriptScriptFace
        , _scriptScriptFont_hb = scriptScriptFont_hb
        , _normalFace          = mathFace
        , _normalFont_hb       = mathFont_hb
        }

  ((),layout) <- evalRWST (unEq eq) faces (Pen (P $ V2 0 0) 0 0)

  let img = renderLayout layout

  writePng "boxes.png" img

-- tests ---------------------------------------------------------------

basicbox1 :: HList ()
basicbox1 = [HString "woop", HKern (Kern (64*100) ExplicitKern), HString "WOOP"]

basicbox2 :: HList ()
basicbox2 = [HString "woop", HRule (Rule 5000 1000 0), HString "XX"]

kern :: F26_6 -> Kern
kern x = Kern (64*12) ExplicitKern

bb3 :: EqBuilder (HBox ())
bb3 = do
  myfrac <- mkVbox [VString "over", VKern $ kern 10, VRule (Rule 8000 300 0), VString "under"]
  mkHbox [HString "My fraction is ", HVBox myfrac, HString " yeah."]

-- variants ------------------------------------------------------------

getVariants :: HB_font -> IO [HB_glyph_variant]
getVariants hb = do
  xs <- for [0..10000] $ \i -> glyphVariants hb (CharIndex i) HB_DIRECTION_BTT 0
  return $ concat xs

varientsBuilder :: EqBuilder ()
varientsBuilder = do
  (ff, hb) <- currentMathFace
  vs <- liftIO $ getVariants hb

  liftIO $ print (length vs)

  penPos._y += (200*64)

  for_ [0,10..length vs] $ \xxx -> do
    liftIO $ putStrLn ("printing " ++ show xxx)

    ifor_ (take 10 (drop xxx vs)) $ \i v -> do
      pos <- use penPos
      addGlyph ff hb pos (glyph_variant_codepoint v)
      penPos._x += fromIntegral (3800*i)

    penPos._y -= (200*64)
    penPos._x .= 0

  -- ifor_ (take 10 (drop 10 vs)) $ \i v -> do
  --   pos <- use penPos
  --   addGlyph ff hb pos (glyph_variant_codepoint v)
  --   penPos._x += fromIntegral (1800*i)

  -- penPos._y += (100*64)
  -- penPos._x .= 0
  -- ifor_ (take 10 (drop 20 vs)) $ \i v -> do
  --   pos <- use penPos
  --   addGlyph ff hb pos (glyph_variant_codepoint v)
  --   penPos._x += fromIntegral (1800*i)

-- assembly ------------------------------------------------------------

char_assemble :: Char -> EqBuilder ()
char_assemble char = do
  (ff, hb) <- currentMathFace

  cix <- liftIO $ getCharIndex ff char

  (ic, parts) <- liftIO $ glyphAssembly hb cix HB_DIRECTION_BTT 0

  assemble parts

assemble :: [HB_glyph_part] -> EqBuilder ()
assemble glyphs = do
  (ff, hb) <- currentMathFace
  minOverlap <- liftIO $ minConnectorOverlap hb HB_DIRECTION_BTT

  penPos._y -= 64*600

  for_ glyphs $ \glyph -> do
    p <- use penPos
    -- full_advance

    let p' = p .+^ V2 0 (fromIntegral $ full_advance glyph)
    addGlyph ff hb p' (glyph_part_code glyph)
    penPos .= p'
    penPos._y -= fromIntegral minOverlap


