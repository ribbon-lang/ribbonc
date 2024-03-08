module Language.Ribbon.Lexical.Name where

import Data.Char qualified as Char

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Data.Foldable qualified as Fold

import Data.Nil

import Text.Pretty

import Language.Ribbon.Lexical.Fixity
import Language.Ribbon.Lexical.Associativity
import Language.Ribbon.Lexical.Precedence
import Language.Ribbon.Lexical.Category
import Language.Ribbon.Util
import Data.Function
import Data.Attr
import Data.Tag




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
pattern SimpleNameFix :: SimpleName -> FixName
pattern SimpleNameFix n <- FixName (FixSimple n Seq.:<| Nil)

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

-- | Pattern alias for a @FixName@ with a single @SimpleName@
pattern SimpleFixName :: SimpleName -> FixName
pattern SimpleFixName n = FixName (FixSimple n Seq.:<| Nil)
{-# COMPLETE SimpleFixName #-}

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

-- | A @FixName@ and @Category@
data SpecificName
    = SpecificName
    { category :: !Category
    , name :: !(ATag FixName)
    }
    deriving (Eq, Ord, Show)

instance Pretty SpecificName where
    pPrintPrec lvl _ SpecificName{..} =
        hsep [ pPrintPrec lvl 0 category
             , pPrintPrec lvl 0 name
             ]


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
data GroupName
    = GroupName
    {  category :: !Category
    , qualified :: !QualifiedName
    }
    deriving (Eq, Ord, Show)

instance Pretty GroupName where
    pPrintPrec lvl _ GroupName{..} =
        hsep [ pPrintPrec lvl 0 qualified
             , pPrintPrec lvl 0 category
             ]

instance HasCategory GroupName where
    getCategory = (.category)

instance HasFixity GroupName where
    getFixity = getFixity . (.qualified)



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
        hsep [ pPrintPrec lvl 0 category
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
