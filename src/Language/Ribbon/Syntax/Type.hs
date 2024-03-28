module Language.Ribbon.Syntax.Type where

import Data.Word(Word32)

import Data.Tag
import Data.Attr

import Text.Pretty

import Language.Ribbon.Syntax.Kind
import Language.Ribbon.Syntax.Data
import Language.Ribbon.Lexical.Name
import Language.Ribbon.Lexical.Path




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
--   after kind inference performs various substitutions to form a @FixType@
data UserType
    = UtFix !(TypeF UserType)
    | UtFree !(Maybe SimpleName) !(Maybe (ATag Kind))
    | UtPath !Path
    | UtInlineConstraint !(Constraint UserType)
    deriving (Eq, Ord, Show)



-- | A final type expression ast.
--   Gives the type of a @Value@
data FixType
    = FtFix !(TypeF FixType)
    | FtVar !TypeVar
    deriving (Eq, Ord, Show)



-- | A type expression ast.
--   Gives the type of a @Value@, after instantiation with its recursive type
data TypeF t
    = TConstant !Constant
    | TApp !(ATag t) !(ATag t)
    | TEffects ![ATag t]
    | TData ![Field t]
    deriving (Eq, Ord, Show)



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


instance Pretty UserType where
    pPrintPrec lvl p = \case
        UtFix t -> pPrintPrec lvl p t
        UtFree n k
            | lvl > PrettyNormal -> maybeParens (p > 0) do
                maybe (text "_") (pPrintPrec lvl 0) n
                    <+> ":" <+> maybe (text "_") (pPrintPrec lvl 0) k
            | otherwise -> maybe (text "_") (pPrintPrec lvl 0) n
        UtPath x -> pPrintPrec lvl p x
        UtInlineConstraint c -> pPrintPrec lvl p c

instance Pretty FixType where
    pPrintPrec lvl p = \case
        FtFix t -> pPrintPrec lvl p t
        FtVar v -> pPrintPrec lvl p v

pattern TUnitCon :: TypeF t
pattern TUnitCon
    = TConstant (CConstructor (SimpleName "()") KType)

pattern TArrowCon :: TypeF t
pattern TArrowCon
    = TConstant (CConstructor (SimpleName " -> in ")
        (KType :~>: KType :~>: KEffects :~>: KType))

pattern TTupleCon :: TypeF t
pattern TTupleCon
    = TConstant (CConstructor (SimpleName "Tuple") (KData :~>: KType))

pattern UtArrow :: ATag UserType -> ATag UserType -> ATag UserType -> UserType
pattern UtArrow a b x <-
    UtFix (TApp
        (UtFix (TApp
            (UtFix (TApp (UtFix TArrowCon :@: _) a) :@: _)
            b) :@: _)
        x)
    where
    UtArrow a b x =
        let abx = a.tag <> b.tag <> x.tag
        in UtFix (TApp
            (UtFix (TApp
                (UtFix (TApp (UtFix TArrowCon :@: abx) a) :@: abx)
                b) :@: abx)
            x)

pattern FixArrow :: ATag FixType -> ATag FixType -> ATag FixType -> FixType
pattern FixArrow a b x <-
    FtFix (TApp
        (FtFix (TApp
            (FtFix (TApp (FtFix TArrowCon :@: _) a) :@: _)
            b) :@: _)
        x)
    where
    FixArrow a b x =
        let abx = a.tag <> b.tag <> x.tag
        in FtFix (TApp
            (FtFix (TApp
                (FtFix (TApp (FtFix TArrowCon :@: abx) a) :@: abx)
                b) :@: abx)
            x)

instance Pretty t => Pretty (TypeF t) where
    pPrintPrec lvl p = \case
        TConstant a -> pPrintPrec lvl p a
        TApp a b -> maybeParens (p > 0) do
            pPrintPrec lvl p a <+> pPrintPrec lvl p b
        TEffects es -> brackets do
            lsep (pPrintPrec lvl 0 <$> es)
        TData fs -> braces do
            lsep (pPrintPrec lvl 0 <$> fs)




data Constant
    = CInt !Word32
    | CString !String
    | CConstructor !SimpleName !Kind
    deriving (Eq, Ord, Show)

instance Pretty Constant where
    pPrintPrec lvl _ = \case
        CInt a -> pPrintPrec lvl 0 a
        CString a -> pPrintPrec lvl 0 a
        CConstructor n k
            | lvl > PrettyNormal ->
                parens $ pPrintPrec lvl 0 n <+> ":" <+> pPrintPrec lvl 0 k
            | otherwise ->
                pPrintPrec lvl 0 n <+> ":" <+> pPrintPrec lvl 0 k
