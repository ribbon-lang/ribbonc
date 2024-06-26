module Language.Ribbon.Lexical.Name where

import Data.Function
import Data.Foldable qualified as Fold

import Data.Char qualified as Char

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Data.Attr
import Data.Tag
import Data.Nil

import Control.Monad

import System.FilePath

import Text.Pretty

import Language.Ribbon.Util
import Language.Ribbon.Lexical.Fixity
import Language.Ribbon.Lexical.Associativity
import Language.Ribbon.Lexical.Precedence
import Language.Ribbon.Lexical.Category
import Language.Ribbon.Parsing.Text




-- | Class for name-likes to test if they need escaping with backticks
class NeedsEscape a where
    -- | Test if a name needs escaping with backticks
    needsEscape :: a -> Bool



-- | A variable name, without qualification.
--   Either a symbol or an identifier; never a reserved character sequence,
--   never a @FixName@, ie containing spaces indicating operator semantics
newtype SimpleName
    = SimpleName
    { value :: String }
    deriving (Eq, Ord, Show)

instance Pretty SimpleName where
    pPrint (SimpleName n) = text n

instance NeedsEscape SimpleName where
    needsEscape (SimpleName n) = not $ all Char.isAlphaNum n

-- | Try to extract a @SimpleName@ from a @FilePath@;
--   Fails in the case where the file name contains special characters
simpleNameFromFile :: FilePath -> Maybe SimpleName
simpleNameFromFile = compose takeBaseName \n ->
    SimpleName n <$ guard (isUserSymbol n)


-- | A variable name with operator semantics;
--   contains components that are either names or operand placeholders;
--   Note that @components@ should never be empty; however, it is not enforced
--   with @NonEmpty@ because a single operand with no names isn't valid either.
--   Additionally, it is not type-enforced that multiple operands are not
--   adjacent, but this is an invalid construction as well
newtype FixName
     = FixName
     { components :: Seq FixNameComponent }
    deriving (Eq, Ord, Show)

-- | Pattern alias for a @FixName@ with only a single @SimpleName@ component
pattern SimpleFixName :: SimpleName -> FixName
pattern SimpleFixName n = FixName (FixSimple n Seq.:<| Nil)

-- | Pattern alias for a @FixName@ with only a single @SimpleName@ component,
--   derived from @String@
pattern StringFixName :: String -> FixName
pattern StringFixName s = SimpleFixName (SimpleName s)

instance Pretty FixName where
    pPrint fn@(FixName cs) = maybeBackticks (needsEscape fn) $
        hcat $ pPrint <$> Fold.toList cs

instance NeedsEscape FixName where
    needsEscape (FixName cs) = any needsEscape cs

instance HasFixity FixName where
    getFixity (FixName Nil) = error "getFixity: FixName with no components"
    getFixity (FixName s@(h Seq.:<| _))
        | FixSimple _ <- h = case s of
            _ Seq.:|> q
                | FixSimple _ <- q -> Atom
                | FixOperand <- q -> Prefix
        | FixOperand <- h = case s of
            _ Seq.:|> q
                | FixSimple _ <- q -> Postfix
                | FixOperand <- q -> Infix

validateFixName :: FixName -> Maybe Doc
validateFixName (FixName cs) = do
    if not $ any isFixSimple cs
        then Just "fix name must contain at least one identifier or operator"
        else checkAdjacencies cs where
    checkAdjacencies Nil = Nothing
    checkAdjacencies (l Seq.:<| r)
        | FixOperand <- l, FixOperand Seq.:<| _ <- r =
            Just "fix name must not contain adjacent operands"
        | otherwise = checkAdjacencies r

-- | Compare two @FixName@s by their @OverloadFixity@
--   and then by their @SimpleNames@
fixNameCompare :: FixName -> FixName -> Ordering
fixNameCompare a b = compare
    (getFixity a, fixNameSimples a)
    (getFixity b, fixNameSimples b)

-- | Extract only the @SimpleNames@ from a @FixName@
fixNameSimples :: FixName -> [SimpleName]
fixNameSimples (FixName cs) = Fold.toList $ Fold.foldr f Nil cs where
    f (FixSimple n) ns = n Seq.<| ns
    f _ ns = ns

-- | Extract only the @SimpleNames@ from a @FixName@ and concatenate them
simplifyFixName :: FixName -> SimpleName
simplifyFixName = SimpleName . concatMap (.value) . fixNameSimples

-- | Test if a @FixName@ is a simple name,
--   ie contains only a single @SimpleName@
isSimpleFixName :: FixName -> Bool
isSimpleFixName = \case SimpleFixName _ -> True; _ -> False

-- | @not . isSimpleFixName@
isCompoundFixName :: FixName -> Bool
isCompoundFixName = not . isSimpleFixName


-- | A component of a variable name with operator semantics;
--   see @FixName@ for usage details
data FixNameComponent
    -- | A simple name (symbol or identifier), ie the + in \` + \`
    = FixSimple !SimpleName
    -- | An operand placeholder, ie the spaces in \` + \`
    | FixOperand
    deriving (Eq, Ord, Show)

pattern FixSimpleString :: String -> FixNameComponent
pattern FixSimpleString s = FixSimple (SimpleName s)

instance Pretty FixNameComponent where
    pPrint = \case
        FixSimple n -> pPrint n
        FixOperand -> " "

instance NeedsEscape FixNameComponent where
    needsEscape = \case
        FixSimple n -> needsEscape n
        FixOperand -> True

isFixSimple :: FixNameComponent -> Bool
isFixSimple = \case
    FixSimple _ -> True
    _ -> False

isFixOperand :: FixNameComponent -> Bool
isFixOperand = \case
    FixOperand -> True
    _ -> False



-- | A @FixName@ qualified with
--   a @Associativity@, and @Precedence@
data QualifiedName
    = QualifiedName
    { associativity :: !Associativity
    ,    precedence :: !Precedence
    ,         value :: !(ATag FixName)
    }
    deriving Show

pattern SimpleQualifiedName :: SimpleName -> QualifiedName
pattern SimpleQualifiedName n <-
    QualifiedName NonAssociative 0 (SimpleFixName n :@: _)

instance Eq QualifiedName where
    (==) = (== EQ) .: compare

instance Ord QualifiedName where
    compare = fixNameCompare `on` untag . (.value)

instance HasFixity QualifiedName where
    getFixity = getFixity . (.value)

instance Pretty QualifiedName where
    pPrintPrec lvl _ QualifiedName{..} =
        case getFixity value of
            Atom -> pPrintPrec lvl 0 value
            _ -> case associativity of
                NonAssociative ->
                    paren'd precedence <+> pPrintPrec lvl 0 value
                LeftAssociative ->
                    pPrint precedence <+> pPrintPrec lvl 0 value
                RightAssociative ->
                    pPrintPrec lvl 0 value <+> pPrint precedence

isSimpleQualifiedName :: QualifiedName -> Bool
isSimpleQualifiedName QualifiedName{..} =
    getFixity value == Atom
    && associativity == NonAssociative
    && precedence == 0
    && isSimpleFixName value.value



-- | A @FixName@ associated with an import that has not been resolved yet
data UnresolvedName
    = UnresolvedName
    { fixitySpecifics :: !(Maybe (Associativity, Precedence))
    ,           value :: !(ATag FixName)
    }
    deriving Show

instance Eq UnresolvedName where
    (==) = (== EQ) .: compare

instance Ord UnresolvedName where
    compare a b = compare
        (overloadedFixity $ getFixity a, a.value)
        (overloadedFixity $ getFixity b, b.value)

instance HasFixity UnresolvedName where
    getFixity = getFixity . (.value)

instance Pretty UnresolvedName where
    pPrintPrec lvl _ UnresolvedName{..} =
        hsep [ case getFixity value of
                Atom -> pPrintPrec lvl 0 value
                _ -> case fixitySpecifics of
                    Just (a, p) -> case a of
                        NonAssociative ->
                            paren'd p <+> pPrintPrec lvl 0 value
                        LeftAssociative ->
                            pPrint p <+> pPrintPrec lvl 0 value
                        RightAssociative ->
                            pPrintPrec lvl 0 value <+> pPrint p
                    _ -> pPrintPrec lvl 0 value
             ]

-- | Convert a @QualifiedName@ to an @UnresolvedName@,
--   given an optional @Category@;
--   Fails if:
--   + The @QualifiedName@ has an associativity other than @NonAssociative@,
--     with a @Fixity@ of @Atom@, @Prefix@, or @Postfix@
--   + The @QualifiedName@ has a non-zero precedence,
--     with a @Fixity@ of @Atom@
unresolvedFromQualified :: QualifiedName -> Maybe UnresolvedName
unresolvedFromQualified QualifiedName{..} =
    UnresolvedName
    { fixitySpecifics = Just (associativity, precedence)
    , value
    } <$ guard if
        | getFixity value == Atom
            -> associativity == NonAssociative
            && precedence == 0
        | getFixity value == Prefix || getFixity value == Postfix
            -> associativity == NonAssociative
        | otherwise -> True

-- | Convert an @UnresolvedName@ to a @QualifiedName@, using a target @Category@
--   Fails if:
--   + The @UnresolvedName@ has a @fixitySpecifics@ that is incompatible
--   + The @UnresolvedName@ has a @Fixity@ that is incompatible
--   + @Category@ is not @Namespace@ or @Instance@ and the @UnresolvedName@
--     has no @fixitySpecifics@
groupFromUnresolvedInCategory ::
    Category -> UnresolvedName -> Maybe QualifiedName
groupFromUnresolvedInCategory category UnresolvedName{..}
    | category == Namespace || category == Instance
    , maybe True (== (NonAssociative, 0)) fixitySpecifics
    , getFixity value == Atom =
        Just $ QualifiedName
            { associativity = NonAssociative
            ,    precedence = 0
            ,         value = value
            }

    | Just (a, p) <- fixitySpecifics =
        Just $ QualifiedName
            { associativity = a
            ,    precedence = p
            ,         value = value
            }

    | otherwise = Nothing
