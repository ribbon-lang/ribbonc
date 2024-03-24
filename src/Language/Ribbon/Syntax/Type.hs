module Language.Ribbon.Syntax.Type where

import Data.Word(Word32)

import Data.Tag
import Data.Attr

import Text.Pretty

import Language.Ribbon.Syntax.Kind
import Language.Ribbon.Lexical.Name




-- | A raw type expression ast, given by the parser.
--   Gives the type of a @Value@,
--   after kind inference performs various substitutions
data UserType
    = UtMono !(TypeF (ATag UserType))
    | UtFree !(Maybe SimpleName)
    deriving (Eq, Ord, Show)

instance Pretty UserType where
    pPrintPrec lvl p = \case
        UtMono t -> pPrintPrec lvl p t
        UtFree (Just n) -> pPrintPrec lvl p n
        UtFree Nothing -> "_"



-- | A final type expression ast.
--   Gives the type of a @Value@
newtype MonoType = Mono {inner :: TypeF (ATag MonoType)}
    deriving (Eq, Ord, Show)

instance Pretty MonoType where
    pPrintPrec lvl p (Mono t) = pPrintPrec lvl p t



-- | A type expression ast.
--   Gives the type of a @Value@, after instantiation with its recursive type
data TypeF t
    = TConstant !Constant
    | TApp !t !t
    | TEffects ![t]
    | T_FIXME -- placeholder
    deriving (Eq, Ord, Show)

pattern TArrowCon :: TypeF t
pattern TArrowCon
    = TConstant (CConstructor (SimpleName " -> in ")
        (KType :~>: KType :~>: KEffects :~>: KType))

pattern UtArrow :: ATag UserType -> ATag UserType -> ATag UserType -> UserType
pattern UtArrow a b x <-
    UtMono (TApp
        (UtMono (TApp
            (UtMono (TApp (UtMono TArrowCon :@: _) a) :@: _)
            b) :@: _)
        x)
    where
    UtArrow a b x =
        let abx = a.tag <> b.tag <> x.tag
        in UtMono (TApp
            (UtMono (TApp
                (UtMono (TApp (UtMono TArrowCon :@: abx) a) :@: abx)
                b) :@: abx)
            x)

pattern MonoArrow :: ATag MonoType -> ATag MonoType -> ATag MonoType -> MonoType
pattern MonoArrow a b x <-
    Mono (TApp
        (Mono (TApp
            (Mono (TApp (Mono TArrowCon :@: _) a) :@: _)
            b) :@: _)
        x)
    where
    MonoArrow a b x =
        let abx = a.tag <> b.tag <> x.tag
        in Mono (TApp
            (Mono (TApp
                (Mono (TApp (Mono TArrowCon :@: abx) a) :@: abx)
                b) :@: abx)
            x)

instance Pretty t => Pretty (TypeF t) where
    pPrintPrec lvl p = \case
        TConstant a -> pPrintPrec lvl p a
        TApp a b -> maybeParens (p > 0) do
            pPrintPrec lvl p a <+> pPrintPrec lvl p b
        _ -> error "FIXME"


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
