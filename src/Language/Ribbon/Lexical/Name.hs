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

data FixNameError
    = FixNameMissingSimple
    | FixNameAdjacentOperands
    deriving (Eq, Ord, Show)

instance Pretty FixNameError where
    pPrint = \case
        FixNameMissingSimple ->
            "fix name must contain at least one identifier or operator"
        FixNameAdjacentOperands ->
            "fix name must not contain adjacent operands"

validateFixName :: FixName -> Maybe FixNameError
validateFixName (FixName cs) = do
    if not $ any isFixSimple cs
        then Just FixNameMissingSimple
        else checkAdjacencies cs where
    checkAdjacencies Nil = Nothing
    checkAdjacencies (l Seq.:<| r)
        | FixOperand <- l, FixOperand Seq.:<| _ <- r =
            Just FixNameAdjacentOperands
        | otherwise = checkAdjacencies r

-- | A component of a variable name with operator semantics;
--   see @FixName@ for usage details
data FixNameComponent
    -- | A simple name (symbol or identifier), ie the + in \` + \`
    = FixSimple !SimpleName
    -- | An operand placeholder, ie the spaces in \` + \`
    | FixOperand
    deriving (Eq, Ord, Show)

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
--   a @Visibility@, @Associativity@, and @Precedence@
data QualifiedName
    = QualifiedName
    { associativity :: !Associativity
    ,    precedence :: !Precedence
    ,          name :: !(ATag FixName)
    }
    deriving Show

instance Eq QualifiedName where
    (==) = (== EQ) .: compare

instance Ord QualifiedName where
    compare = fixNameCompare `on` untag . (.name)

instance HasFixity QualifiedName where
    getFixity = getFixity . (.name)

instance Pretty QualifiedName where
    pPrintPrec lvl _ QualifiedName{..} =
        case getFixity name of
            Atom -> pPrintPrec lvl 0 name
            _ -> case associativity of
                NonAssociative ->
                    paren'd precedence <+> pPrintPrec lvl 0 name
                LeftAssociative ->
                    pPrint precedence <+> pPrintPrec lvl 0 name
                RightAssociative ->
                    pPrintPrec lvl 0 name <+> pPrint precedence

-- | @Categorical FixName@
type SpecificName = Categorical FixName


-- | A component of a @Path@,
--   specifying a name to look up, at a particular fixity and category
data PathName
    = PathName
    { category :: !(Maybe OverloadCategory)
    , name :: !FixName
    }
    deriving (Eq, Ord, Show)

-- | Pattern alias for a @PathName@ with no @Category@
pattern FixPathName :: FixName -> PathName
pattern FixPathName n = PathName Nothing n

instance Pretty PathName where
    pPrintPrec lvl _ = \case
        PathName k n -> hsep
            [ maybeMEmpty (pPrintPrec lvl 0 <$> k)
            , pPrintPrec lvl 0 n
            ]

instance HasFixity PathName where
    getFixity (PathName _ n) = getFixity n

-- | A @FixName@ qualified with
--   a @Visibility@, @Category@, @Associativity@, and @Precedence@
--   used for binding elements in a @Group@
type GroupName = Categorical QualifiedName


-- | A @FixName@ associated with an import that has not been resolved yet
data UnresolvedName
    = UnresolvedName
    { category :: !(Maybe OverloadCategory)
    , fixitySpecifics :: !(Maybe (Associativity, Precedence))
    , name :: !(ATag FixName)
    }
    deriving Show

instance Eq UnresolvedName where
    (==) = (== EQ) .: compare

instance Ord UnresolvedName where
    compare a b = compare
        (a.category, overloadedFixity $ getFixity a, a.name)
        (b.category, overloadedFixity $ getFixity b, b.name)

instance HasFixity UnresolvedName where
    getFixity = getFixity . (.name)

instance Pretty UnresolvedName where
    pPrintPrec lvl _ UnresolvedName{..} =
        hsep [ maybeMEmpty (pPrintPrec lvl 0 <$> category)
             , case getFixity name of
                Atom -> pPrintPrec lvl 0 name
                _ -> case fixitySpecifics of
                    Just (a, p) -> case a of
                        NonAssociative ->
                            paren'd p <+> pPrintPrec lvl 0 name
                        LeftAssociative ->
                            pPrint p <+> pPrintPrec lvl 0 name
                        RightAssociative ->
                            pPrintPrec lvl 0 name <+> pPrint p
                    _ -> pPrintPrec lvl 0 name
             ]

-- | Convert a @QualifiedName@ to an @UnresolvedName@,
--   given an optional @OverloadCategory@;
--   Fails if:
--   + @OverloadCategory@ is @ONamespace@ or @OInstance@ and the @QualifiedName@
--     is not an atom with associativity @NonAssociative@ and precedence @0@
--   + The @QualifiedName@ has an associativity other than @NonAssociative@,
--     with a @Fixity@ of @Atom@, @Prefix@, or @Postfix@
--   + The @QualifiedName@ has a non-zero precedence,
--     with a @Fixity@ of @Atom@
unresolvedFromQualified ::
    Maybe OverloadCategory -> QualifiedName -> Maybe UnresolvedName
unresolvedFromQualified category QualifiedName{..} = UnresolvedName
    { category = category
    , fixitySpecifics = Just (associativity, precedence)
    , name = name
    } <$ guard if
        | category == Just ONamespace || category == Just OInstance
            -> getFixity name == Atom
            && associativity == NonAssociative
            && precedence == 0
        | getFixity name == Atom
            -> associativity == NonAssociative
            && precedence == 0
        | getFixity name == Prefix || getFixity name == Postfix
            -> associativity == NonAssociative
        | otherwise -> True

-- | Convert an @UnresolvedName@ to a @QualifiedName@, using a target @Category@
--   Fails if:
--   + Respective @Category@s are mismatched
--   + The @UnresolvedName@ has a @fixitySpecifics@ that is incompatible
--   + The @UnresolvedName@ has a @Fixity@ that is incompatible
--   + @Category@ is not @ONamespace@ or @OInstance@ and the @UnresolvedName@
--     has no @fixitySpecifics@
groupFromUnresolvedInCategory ::
    Category -> UnresolvedName -> Maybe GroupName
groupFromUnresolvedInCategory exactCategory UnresolvedName{..} =
    let category' = overloadedCategory exactCategory
    in if
        | category' == ONamespace || category' == OInstance
        , maybe True (== category') category
        , maybe True (== (NonAssociative, 0)) fixitySpecifics
        , getFixity name == Atom ->
            Just $ Categorical exactCategory QualifiedName
                { associativity = NonAssociative
                ,    precedence = 0
                ,          name = name
                }

        | Just (a, p) <- fixitySpecifics
        , maybe True (== category') category ->
            Just $ Categorical exactCategory QualifiedName
                { associativity = a
                ,    precedence = p
                ,          name = name
                }

        | otherwise -> Nothing
