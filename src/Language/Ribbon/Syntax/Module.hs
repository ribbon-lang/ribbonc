module Language.Ribbon.Syntax.Module where

import Data.Sequence (Seq)
import Data.Map.Strict (Map)

import Data.Attr

-- FIXME: implement pretty printer
-- import Text.Pretty

import Language.Ribbon.Lexical

import Language.Ribbon.Syntax.Ref
import Language.Ribbon.Syntax.Group
import Language.Ribbon.Syntax.Data
import Language.Ribbon.Syntax.Scheme
import Language.Ribbon.Syntax.Type
import Language.Ribbon.Syntax.Value
import Language.Ribbon.Syntax.Raw




-- | An analysis context component, storing completed module's information.
--   Provides a lookup table from @(name string, version)@ pairs,
--   to their respective references
data ModuleContext
    = ModuleContext
    { modules :: !(RefMap FinalModule)
    , moduleLookup :: !(Map (String, Version) Ref)
    }
    deriving Show


-- | A module that has been fully parsed
type FinalModule
    = Module
        Type
        Value
        ResolvedBlobs

-- | A module that has been partially parsed, and is awaiting name resolution
type ProtoModule
    = Module
        (Seq [ATag Token])
        (Seq [ATag Token])
        UnresolvedImports


-- | A definition of some item in a module, with a parent.
--   the parent is either the namespace or type the item was defined in,
--   or the module if it is the root namespace
data Def v
    = Def
    { parent :: !Ref
    , inner :: !(ATag v)
    }
    deriving (Eq, Ord, Show)


-- | Parametric type storing all the elements of a module,
--   with their form depending on compilation phase.
--   A given @Ref@ may be bound to multiple rows of the module,
--   for example a value definition may have an entry in
--   @values@, @quantifiers@, @qualifiers@, and @types@, while
--   types with fields are stored in @groups@, @quantifiers@, and @qualifiers@.
--   The root namespace is always stored in @Ref Namespace modId 0@
data Module t v i
    = Module
    {       groups :: !(RefMap (Def Group))
    ,  quantifiers :: !(RefMap (Def Quantifier))
    ,   qualifiers :: !(RefMap (Def (Qualifier t)))
    ,       fields :: !(RefMap (Def (FieldType t)))
    ,        types :: !(RefMap (Def t))
    ,       values :: !(RefMap (Def v))
    ,      imports :: !(RefMap (Def i))

    ,        files :: !(Map FilePath Ref)
    , dependencies :: !(Map SimpleName Ref)
    ,         meta :: !MetaData
    }
    deriving Show
