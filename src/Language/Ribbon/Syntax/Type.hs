module Language.Ribbon.Syntax.Type where

import Data.Word(Word32)

import Data.Tag
import Data.Attr

import Text.Pretty

import Language.Ribbon.Syntax.Kind
import Language.Ribbon.Syntax.Data
import Language.Ribbon.Lexical.Name
import Language.Ribbon.Lexical.Path
import Language.Ribbon.Syntax.Ref
import qualified Data.Maybe as Maybe
import Language.Ribbon.Util




-- | Constrains a type, inside a @Qualifier@
data Constraint t
    = CEquality !(EqualityConstraint t)
    | CRow !(RowConstraint t)
    | CData !(DataConstraint t)
    | CClass !(ClassConstraint t)
    deriving (Eq, Ord, Show)

instance Pretty t => Pretty (Constraint t) where
    pPrintPrec lvl p = \case
        CEquality c -> pPrintPrec lvl p c
        CRow c -> pPrintPrec lvl p c
        CData c -> pPrintPrec lvl p c
        CClass c -> pPrintPrec lvl p c

data EqualityConstraint t
    = Ec !(ATag t) !(ATag t)
    deriving (Eq, Ord, Show)

instance Pretty t => Pretty (EqualityConstraint t) where
    pPrintPrec lvl p (Ec a b) = maybeParens (p > 0) do
        pPrintPrec lvl 0 a <+> "~" <+> pPrintPrec lvl 0 b

data RowConstraint t
    = RcSub !(RowSubConstraint t)
    | RcCat !(RowCatConstraint t)
    deriving (Eq, Ord, Show)

instance Pretty t => Pretty (RowConstraint t) where
    pPrintPrec lvl p = \case
        RcSub c -> pPrintPrec lvl p c
        RcCat c -> pPrintPrec lvl p c

data DataConstraint t
    = DcStruct !(StructConstraint t)
    | DcUnion !(UnionConstraint t)
    deriving (Eq, Ord, Show)

instance Pretty t => Pretty (DataConstraint t) where
    pPrintPrec lvl p = \case
        DcStruct c -> pPrintPrec lvl p c
        DcUnion c -> pPrintPrec lvl p c

data ClassConstraint t
    = CcType !(TypeClassConstraint t)
    | CcAssociate !(AssociateConstraint t)
    deriving (Eq, Ord, Show)

instance Pretty t => Pretty (ClassConstraint t) where
    pPrintPrec lvl p = \case
        CcType c -> pPrintPrec lvl p c
        CcAssociate c -> pPrintPrec lvl p c

data RowSubConstraint t
    = RowSubConstraint !(ATag t) !(ATag t)
    deriving (Eq, Ord, Show)

instance Pretty t => Pretty (RowSubConstraint t) where
    pPrintPrec lvl p (RowSubConstraint a b) = maybeParens (p > 0) do
        pPrintPrec lvl 0 a <+> "<" <+> pPrintPrec lvl 0 b

data RowCatConstraint t
    = RowCatConstraint !(ATag t) !(ATag t) !(ATag t)
    deriving (Eq, Ord, Show)

instance Pretty t => Pretty (RowCatConstraint t) where
    pPrintPrec lvl p (RowCatConstraint a b c) = maybeParens (p > 0) do
        pPrintPrec lvl 0 a <+> "<>" <+> pPrintPrec lvl 0 b
            <+> "~" <+> pPrintPrec lvl 0 c

data StructConstraint t
    = StructConstraint !(ATag t) !(ATag t)
    deriving (Eq, Ord, Show)

instance Pretty t => Pretty (StructConstraint t) where
    pPrintPrec lvl p (StructConstraint a b) = maybeParens (p > 0) do
        "struct" <+> pPrintPrec lvl 0 a <+> "as" <+> pPrintPrec lvl 0 b

data UnionConstraint t
    = UnionConstraint !(ATag t) !(ATag t)
    deriving (Eq, Ord, Show)

instance Pretty t => Pretty (UnionConstraint t) where
    pPrintPrec lvl p (UnionConstraint a b) = maybeParens (p > 0) do
        "union" <+> pPrintPrec lvl 0 a <+> "as" <+> pPrintPrec lvl 0 b

data TypeClassConstraint t
    = TypeClassConstraint !(ATag t) !(ATag t)
    deriving (Eq, Ord, Show)

instance Pretty t => Pretty (TypeClassConstraint t) where
    pPrintPrec lvl p (TypeClassConstraint a b) = maybeParens (p > 0) do
        pPrintPrec lvl 0 a <+> "type" <+> pPrintPrec lvl 0 b

data AssociateConstraint t
    = AssociateConstraint !(ATag t) !SimpleName !(ATag t)
    deriving (Eq, Ord, Show)

instance Pretty t => Pretty (AssociateConstraint t) where
    pPrintPrec lvl p (AssociateConstraint a b c) = maybeParens (p > 0) do
        pPrintPrec lvl 0 a <+> "has" <+> pPrintPrec lvl 0 b
            <+> "~" <+> pPrintPrec lvl 0 c




-- | A raw type expression ast, given by the parser.
--   Gives the type of a @Value@,
--   after kind inference performs various substitutions to form a @FinalType@
type UserType = TypeF UserTypeVar


-- | A final type expression ast.
--   Gives the type of a @Value@
type FinalType = TypeF TypeVar



-- | A type expression ast.
--   Gives the type of a @Value@, after instantiation with its recursive type
data TypeF v
    = TVar !v
    | TConstant !Constant
    | TConstructor !Constructor
    | TApp !(ATag (TypeF v)) !(ATag (TypeF v))
    | TEffects ![ATag (TypeF v)]
    | TData ![Field (TypeF v)]
    deriving (Eq, Ord, Show)

data Constructor
    = Constructor
    { ref :: !Ref
    , name :: !SimpleName
    , kind :: !Kind
    }
    deriving (Eq, Ord, Show)

instance Pretty Constructor where
    pPrintPrec lvl prec (Constructor r n k)
        | lvl > PrettyRich =
            parens do
                joinWith "↘" (pPrintPrec lvl 0 r) (pPrintPrec lvl 0 n)
                    <+> "::" <+> pPrintPrec lvl 0 k
        | lvl > PrettyNormal =
            parens do
                pPrintPrec lvl 0 n <+> "::" <+> pPrintPrec lvl 0 k
        | otherwise = pPrintPrec lvl prec n


data UserTypeVar
    = TvFree !(Maybe SimpleName) !(Maybe (ATag Kind))
    | TvPath !Path
    | TvInlineConstraint !(Constraint UserType)
    deriving (Eq, Ord, Show)


instance Pretty UserTypeVar where
    pPrintPrec lvl p = \case
        TvFree n k -> case n of
            Just s -> maybeParens (p > 0 && Maybe.isJust k) do
                pPrintPrec lvl 0 s
                    <+> maybeMEmpty (("of" <+>) . pPrintPrec lvl 0 <$> k)
            _ -> maybe (text "_") (("'of" <+>) . pPrintPrec lvl 0) k
        TvPath x -> pPrintPrec lvl p x
        TvInlineConstraint c -> pPrintPrec lvl p c

data TypeVar
    = TvBound !SimpleName !Kind
    | TvMeta !Int !Kind
    deriving (Eq, Ord, Show)

instance Pretty TypeVar where
    pPrintPrec lvl prec = \case
        TvBound n k
            | lvl > PrettyNormal ->
                maybeParens (prec > 0) do
                    pPrintPrec lvl 0 n <+> ":" <+> pPrintPrec lvl 0 k
            | otherwise ->
                pPrintPrec lvl 0 n
        TvMeta i k
            | lvl > PrettyNormal ->
                maybeParens (prec > 0) do
                    ("$" <> pPrintPrec lvl 0 i) <+> ":" <+> pPrintPrec lvl 0 k
            | otherwise ->
                ("$" <> pPrintPrec lvl 0 i)


pattern TUnitCon :: TypeF t
pattern TUnitCon
    = TConstructor Constructor
    { ref = Ref 0 0
    , name = SimpleName "()"
    , kind = KType
    }

pattern TArrowCon :: TypeF t
pattern TArrowCon
    = TConstructor Constructor
    { ref = Ref 0 0
    , name = SimpleName "->"
    , kind = KType :~>: KType :~>: KEffects :~>: KType
    }

pattern TTupleCon :: TypeF t
pattern TTupleCon
    = TConstructor Constructor
    { ref = Ref 0 0
    , name = SimpleName "Tuple"
    , kind = KData :~>: KType
    }

pattern TArrow :: ATag (TypeF v) -> ATag (TypeF v) -> ATag (TypeF v) -> TypeF v
pattern TArrow a b x <-
    TApp (TApp (TApp (TArrowCon :@: _) a :@: _) b :@: _) x where
    TArrow a b x =
        let abx = a.tag <> b.tag <> x.tag
        in TApp (TApp (TApp (TArrowCon :@: abx) a :@: abx) b :@: abx) x

instance Pretty v => Pretty (TypeF v) where
    pPrintPrec lvl p = \case
        TVar v -> pPrintPrec lvl p v
        TConstant a -> pPrintPrec lvl p a
        TConstructor a -> pPrintPrec lvl p a
        t@TApp{} -> let (f, xs) = forceUnTApp t in case untag f of
            TTupleCon | [TData ts :@: _] <- xs ->
                parens $ lsep (pPrintPrec lvl 0 . (.value) <$> ts)
            TArrowCon | [a, b, x] <- xs ->
                maybeParens (p > 10) do
                    pPrintPrec lvl 11 a <+> "->" <+> pPrintPrec lvl 10 b
                        <+> case untag x of
                            TEffects [] | lvl == PrettyNormal -> mempty
                            _ -> "in" <+> pPrintPrec lvl 0 x
            _ -> maybeParens (p > 70) do
                hsep $ pPrintPrec lvl 71 f : (pPrintPrec lvl 71 <$> xs)
        TEffects es -> brackets do
            lsep (pPrintPrec lvl 0 <$> es)
        TData fs -> braces do
            lsep (pPrintPrec lvl 0 <$> fs)

unTApp :: TypeF v -> Maybe (ATag (TypeF v), [ATag (TypeF v)])
unTApp = \case
    TApp a b -> case unTApp (untag a) of
        Just (f, xs) -> Just (f, xs <> [b])
        Nothing -> Just (a, [b])
    _ -> Nothing

forceUnTApp :: TypeF v -> (ATag (TypeF v), [ATag (TypeF v)])
forceUnTApp t = Maybe.fromMaybe
    (error "unTApp: not a TApp")
    (unTApp t)

data Constant
    = CInt !Word32
    | CString !String
    deriving (Eq, Ord, Show)

instance Pretty Constant where
    pPrintPrec lvl _ = \case
        CInt a -> pPrintPrec lvl 0 a
        CString a -> pPrintPrec lvl 0 a
