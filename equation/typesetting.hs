
-- Font parameters -----------------------------------------------------

newtype Percent = Percent Word8
type Distance = Double

data MathParam = MathParam
  { scriptPercentScaleDown                   :: Percent
  , scriptScriptPercentScaleDown             :: Percent
  , delimitedSubFormulaMinHeight             :: Distance
  , displayOperatorMinHeight                 :: Distance
  , mathLeading                              :: Distance
  , axisHeight                               :: Distance
  , accentBaseHeight                         :: Distance
  , flattenedAccentBaseHeight                :: Distance
  , subscriptShiftDown                       :: Distance
  , subscriptTopMax                          :: Distance
  , subscriptBaselineDropMin                 :: Distance
  , superscriptShiftUp                       :: Distance
  , superscriptShiftUpCramped                :: Distance
  , superscriptBottomMin                     :: Distance
  , superscriptBaselineDropMax               :: Distance
  , subSuperscriptGapMin                     :: Distance
  , superscriptBottomMaxWithSubscript        :: Distance
  , spaceAfterScript                         :: Distance
  , upperLimitGapMin                         :: Distance
  , upperLimitBaselineRiseMin                :: Distance
  , lowerLimitGapMin                         :: Distance
  , lowerLimitBaselineDropMin                :: Distance
  , stackTopShiftUp                          :: Distance
  , stackTopDisplayStyleShiftUp              :: Distance
  , stackBottomShiftDown                     :: Distance
  , stackBottomDisplayStyleShiftDown         :: Distance
  , stackGapMin                              :: Distance
  , stackDisplayStyleGapMin                  :: Distance
  , stretchStackTopShiftUp                   :: Distance
  , stretchStackBottomShiftDown              :: Distance
  , stretchStackGapAboveMin                  :: Distance
  , stretchStackGapBelowMin                  :: Distance
  , fractionNumeratorShiftUp                 :: Distance
  , fractionNumeratorDisplayStyleShiftUp     :: Distance
  , fractionDenominatorShiftDown             :: Distance
  , fractionDenominatorDisplayStyleShiftDown :: Distance
  , fractionNumeratorGapMin                  :: Distance
  , fractionNumDisplayStyleGapMin            :: Distance
  , fractionRuleThickness                    :: Distance
  , fractionDenominatorGapMin                :: Distance
  , fractionDenomDisplayStyleGapMin          :: Distance
  , skewedFractionHorizontalGap              :: Distance
  , skewedFractionVerticalGap                :: Distance
  , overbarVerticalGap                       :: Distance
  , overbarRuleThickness                     :: Distance
  , overbarExtraAscender                     :: Distance
  , underbarVerticalGap                      :: Distance
  , underbarRuleThickness                    :: Distance
  , underbarExtraDescender                   :: Distance
  , radicalVerticalGap                       :: Distance
  , radicalDisplayStyleVerticalGap           :: Distance
  , radicalRuleThickness                     :: Distance
  , radicalExtraAscender                     :: Distance
  , radicalKernBeforeDegree                  :: Distance
  , radicalKernAfterDegree                   :: Distance
  , radicalDegreeBottomRaisePercent          :: Distance
  } deriving Show

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

  , boxGlue :: !BoxGlue
    -- The glue used for the box when it is part of a list.

  , boxContents :: [f a]
  -- ^ The contents of the box. If 'Nothing', the box is empty.
  } deriving Show

type HBox = Box HorizontalElement
type VBox = Box VerticalElement
type MBox = Box MathElement

data GlueDirection = Rigid | Shrink | Stretch
  deriving Show

data GlueOrder = FiniteGlue | Fil | Fill | Filll
  deriving Show

-- from the description of boxes, it makes it look like this is the
-- definition of glue for a box, but it may actually the definition for
-- Glue that I already have.
data BoxGlue = BoxGlue
  { glueValue     :: Distance
  , glueDirection :: GlueUnit
  , glueOrder     :: GlueOrder
  } deriving Show

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

-- A kern node can also appear in a vertical list, when its ‘width’ denotes additional spacing in the vertical direction. The subtype is either normal (for kerns inserted from font information or math mode calculations) or explicit (for kerns inserted from \kern and \/ commands) or acc kern (for kerns inserted from non-math accents) or mu glue (for kerns inserted from \mkern specifications in math formulas).

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
  = HBox !(HBox a)
  | HRule !Rule
  | HKern !Kern
  | HGlue !Glue
  | HString String
  | HCustom a
  deriving Show

type HList a = [HorizontalElement a]

-- Vertical list -------------------------------------------------------

data Vertical a
  = VBox !(Box VerticalElement a)
  | VRule !Rule
  | VKern !Kern
  | VGlue !Glue
  | VString String
  | VCustom a
  deriving Show

-- Mathmatical list ----------------------------------------------------

data Field a
  = EmptyField
  | MathSymbol Char
  | MBox a
  deriving Show

data Atom a
  = Ordinary (Field a) (Field a) (Field a)
  | Operator Limits (Field a) (Field a) (Field a)
  | Binary (Field a) (Field a) (Field a)
  | Relation (Field a) (Field a) (Field a)
  | Open (Field a) (Field a) (Field a)
  | Close (Field a) (Field a) (Field a)
  | Punctuation (Field a) (Field a) (Field a)
  | Inner (Field a) (Field a) (Field a)
  | Over (Field a) (Field a) (Field a)
  | Under (Field a) (Field a) (Field a)
  | Accent Char (Field a) (Field a) (Field a)
  | Radical Char (Field a) (Field a) (Field a)
  | Fractional (Maybe Distance) Delimiter (MathList a) (MathList a) Delimiter
  | VerticalCentre (Box VerticalElement a)
  deriving Show

newtype Delimiter = Delimiter (Distance -> MBox a)

instance Show Delimiter where show = "DELIMITER"

data MathChoice a = MathChoice
  { displayChoice      :: MBox a
  , textChoice         :: MBox a
  , scriptChoice       :: MBox a
  , scriptScriptChoice :: MBox a
  } deriving Show

data MathElement a
  = MBox !(Box MathElement a)
  | MRule !Rule
  | MKern !Kern
  | MGlue !Glue
  | MAtom (Atom a)
  | MStyleChange Style
  | MBoundary Delimiter [MathElement a] Delimiter
  | MChoice (MathChoice a)

type MathList a = [MathElement a]

typesetMath :: MathParam -> Style -> MathList a -> HBox a
typesetMath params@MathParam {..} sty = step2 . step1 sty Nothing Nothing
  where

  -- This is the first step where we place all items in the correct
  -- place.
  step1 :: Style -> Bool -> [MathElement a] -> [MathElement a]
  step1 _ _revWasBinCompat []     = []
  step1 c prevWasBinCompat (m:ms) = case m of

    -- boundary elements
    MBoundary d1 m d2 = MBoundary d1 m' d2 : step1 c False ms
      where m' = step1 c False m

    -- spacing
    MKern kern -> M'Kern kern
    MGlue glue -> M'Glue glue

    MChoice d t s ss -> step1 c False m' ++ ms
      where
      m' = case c of
        D   -> d
        D'  -> d
        T   -> t
        T'  -> t
        S   -> s
        S'  -> s
        SS  -> ss
        SS' -> ss

    MAtom atom -> case atom of
      Ordinary n sup sub ->
      Operator Limits n sup sub
      Binary {}
        | prevWasBinCompat -> rule17 atom
        | otherwise        -> rule14 atom

      Relation {} -> rule17 atom
      Open {}     -> rule17 atom
      Close {}     -> rule17 atom
      Punctuation {}     -> rule17 atom
      Inner {}     -> rule17 atom

      Over n sup sub ->
        let box = mkVbox overbarRuleThickness
        rule17 [hrule overbarRuleThickness, kern overbarVerticalGap, n']
      Under n sup sub ->
        let box = [n', kern underbarVerticalGap, rule underbarRuleThickness
        rule16

      Accent accentChar n sup sub -> case n of
        let x = typsetMath params (set cramped True c) n
            s = 0 -- ment to be the kern ammount for the nucleus
        let u = boxWidth b
        let δ = min (boxHeight b) accentBaseHeight
        let y = VString [accentChar]
            yShift = s + 0.5*(u - boxWidth y)
        -- XXX deal with sup and sub
        let z = [VBox {boxShift = yShift}, vkern (-δ), VBox x]
        let z' | boxHeight z < boxHeight x -> kern (boxHeight x - boxHeight z) : z
        in  rule16 z'

      Radical radicalDelimiter n sup sub ->
        let x = typesetMath params (set cramped True c) n
            θ = radicalRuleThickness
            φ | c > T     = radicalDisplayStyleVerticalGap
              | otherwise = radicalVerticalGap
            ψ = θ + 0.25 * abs φ -- minimum allowed clearance between box and rule
        let y = radicalWithHeight radicalDelimiter (boxHeight x + boxDepth x + ψ + θ)
            θ' = boxHeight y
            let excess = boxHeight x + boxDepth x - boxHeight x - boxDepth x
            ψ' | boxDepth y > boxHeight x + boxDepth y + ψ =
                   0.5 * (ψ + boxDepth y - boxHeight x - boxDepth x)
               | otherwise = ψ
        let z = vbox [kern θ, hrule θ, kern ψ, x]
        in  rule16 z

      Fractional mDividerThickness d1 numerator denominator d2 ->
        let θ = case mDividerThickness of
                  Nothing -> fractionRuleThickness
                  Just t  -> t
        let λ = d1
        let ρ = d2

        let x = typesetMath params (numeratorStyle c) numerator
            z = typesetMath params (denominatorStyle c) denominator
            (x',z') | boxWidth x < boxWidth z = (x {boxWidth = boxWidth z}, z)
                    | otherwise               = (x, z {boxWidth = boxWidth x})

            (u,v)
              | c > T     = (fractionNumeratorDisplayStyleShiftUp,
                             fractionDenominatorDisplayStyleShiftDown)
              | θ == 0    = (fractionNumeratorShiftUp, fractionDenominatorShiftDown)
                -- ^ tex has this as another case but I don't know what
                --   σ_10 corresponds to
              | otherwise = (fractionNumeratorShiftUp, fractionDenominatorShiftDown)

           v | θ == 0 =
               let φ | c > T     = 7*ξ_8
                     | otherwise = 3*ξ_8 -- minimum
                   ψ = (u - boxDepth x) - (boxHeight z - v)
                   (u',v') | ψ < φ = (u + 0.5*(φ - ψ), v + 0.5*(φ - ψ))
               in  vbox [VBox x, vkern (u' + v'), VBox z]

             | otherwise =
               let φ = bool (c > T) θ (3*θ)
                   a = 0 -- currentAxisHeight?
                   uClearence = (u - boxDepth x) - (a + 0.5*θ)
                   vClearence = (a - 0.5*θ) - (boxHeight z - v)
                   u' = bool (uClearence < φ) u (u + φ - uClearence)
                   v' = bool (vClearence < φ) v (v + φ - vClearence)
               in vbox [VBox x, kern u', hrule θ, kern v', z]
                  -- kerns should be figured so that the bottom of the
                  -- hrule occurs at a=0.5θ above the baseline

          -- XXX handle delimiters
          --



  , fractionNumeratorShiftUp                 :: Distance
  , fractionNumeratorDisplayStyleShiftUp     :: Distance
  , fractionDenominatorShiftDown             :: Distance
  , fractionDenominatorDisplayStyleShiftDown :: Distance
  , fractionNumeratorGapMin                  :: Distance
  , fractionNumDisplayStyleGapMin            :: Distance
  , fractionRuleThickness                    :: Distance
  , fractionDenominatorGapMin                :: Distance
  , fractionDenomDisplayStyleGapMin          :: Distance


      VerticalCentre (Box VerticalElement a)



  step2 :: Style -> Bool -> [MathElement a] -> HBox a

