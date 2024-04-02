Visibility = "pub"

SimpleName = identifier | operator

FixName = SimpleName | Prefix | Infix | Postfix where
    Prefix "`" (SimpleName space)+ "`"
    Postfix = "`" (space SimpleName)+ "`"
    Infix = "`" space (SimpleName space)+ "`"

FixNameDecl
    = SimpleName
    | FixName/Prefix uInt?
    | uInt? FixName/Postfix
    | uInt? FixName/Infix
    | FixName/Infix uInt?
    | ("(" uInt ")")? FixName/Infix

TypeHead = Qualifier Quantifier?
Qualifier = listSome<identifier (":" Kind)?>
Quantifier = "where" listSome<Type>

Field body = Label body where
    ;; FIXME: Cannot vary over both layout and name
    Label
        = ("#" Type | uInt) "\\" ("#" Type | SimpleName)
        | Int ("\\" Name)? | Name
    Int = "#I" Type | uInt
    Name = "#S" Type | SimpleName

Path
    = PlainBase ("/" FixName++"/")?
    | SlashBase FixName**"/"
    | FixName++"/" where
    PlainBase = "module" SimpleName | "file" string
    SlashBase = "~/" | "./" | "../"+

PathExt tail
    = Path/PlainBase ("/" FixName++"/")? "/" tail
    | Path/SlashBase (FixName++"/" "/" tail | tail)
    | FixName++"/" "/" tail
    | tail

WsList sep elem = wsBlock<wsBlock<elem>++(sep?) | elem (sep elem)*>


Module = ModuleHead Doc

ModuleHead = "module" string "@" Version wsBlock<Meta> where
    Meta
        = "sources" WsList<",", string>
        | "dependencies" WsList<",", Dependency>
        | identifier WsList<",", string>
    Dependency = string "@" Version ("as" SimpleName)?
    Version = $uInt "." $uInt "." $uInt
        if $1 > 0 or $2 > 0 or $3 > 0

Doc = Def*

Def = Visibility? (Use | Namespace | TypeDef | ValueDef) where
    Use = "use" Tree where
        Tree = Elem ("as" FixNameDecl)?
        Elem = Path | PathExt<"{" Tree**"," "}" | FixName | (".." Hiding?)>
        Hiding = "hiding" ("{" FixName**"," "}" | FixName)

    Namespace = "namespace" SimpleName "=" wsBlock<Doc>

    TypeDef = TypeBody where
        EffectDec = FixNameDecl ":" wsBlock<Type>
        FieldDec = Field<":" wsBlock<Type>>
        StructFields
            = WsList<",", wsBlock<Type>>
            | WsList<",", FieldDec>
        ClassDec
            = "type" SimpleName TypeHead?
            | FixNameDecl ":" ("for" TypeHead "=>")? Type
        InstanceDef
            = "type" SimpleName Quantifier? "=" Type
            | FixNameDecl "=" wsBlock<Value>
        TypeBody
            = "type" SimpleName Quantifier? "="
                wsBlock<Type>
            | "struct" SimpleName Quantifier? "="
                StructFields
            | "union" SimpleName Quantifier? "="
                WsList<",", FieldDec>
            | "effect" SimpleName TypeHead? "="
                WsList<",", EffectDec>
            | "class" SimpleName TypeHead? "="
                WsList<",", ClassDec>
            | "instance" SimpleName TypeHead? "for" Type "="
                WsList<",", InstanceDef>

    ValueDef = FixNameDecl (ValueType? ValueExpr | ValueType) where
        ValueType = ":" ("for" TypeHead "=>")? wsBlock<Type>
        ValueExpr = "=" wsBlock<Value>

Kind
    = "type" | "effect"
    | "int"  | "str"
    | "data" | "effects"
    | "constraint"
    | Kind "->" Kind
    | "(" Kind ")"

Type |=
    Var = identifier
    Con = Path
    Unit = "(" ")"
    Group = "(" Type ")"
    Tuple = "(" Type "," Type**"," ")"
    DataRow = "{" Field<":" Type>**"," "}"
    EffectRow = "[" Type**"," "]"
    Function = Type "->" Type ("in" Type)?
    App = Type Type
    QuantifiedInline
        = "_"
        | "'" identifier ("of" Kind)?
        | "'" "of" Kind
    Constraint |=
        IsStruct = "struct" Type ("as" Type)?
        IsUnion = "union" Type ("as" Type)?
        HasClass = Type? "with" Type
        HasAssoc = Type? "has" SimpleName "~" Type
        RowSub = Type "<" Type?
        RowCat = Type "<>" Type ("~" Type)?
        Equality = Type "~" Type

Value |=
    Var = identifier
    Literal = literal
    Global = Path
    Unit = "(" ")"
    Group = "(" Value ")"
    Tuple = "(" Value "," Value**"," ")"
    Struct = "{" Field<"=" Block>**"," "}"
    AnyUnion = "+/" SimpleName
    Select = Value "." SimpleName
    Concat = Value "<>" Value
    App = Value Block
    Ann = Value ":" wsBlock<Type>
    Function = "fun" listSome<Patt> "=>" Block
    Match = "match" Value wsBlock<Case+>
        where Case = ("|" Patt)+ "=>" Block
    Let = "let" WsList<",", Patt "=" Block>
    Continue = "continue" Block
    Return = "return" Block
    Sequence = Value ";" Value
    Handler = "with" Type "handler" wsBlock<Case+> "do" Block
        where Case
            = FixName "|" listSome<Patt> "=>" Block
            | "return" Patt "=>" Block
    User |=
        Infix = Value Path Value
        Prefix = Path Value
        Postfix = Value Path
    where Block = WsList<";", Value>

Patt |=
    Var = identifier
    Literal = literal
    Unit = "(" ")"
    Group = "(" Patt ")"
    Tuple = "(" Patt "," Patt**"," ")"
    Struct = "{" Field<"=" wsBlock<Patt>>**"," ".."? "}"
    AnyUnion = "+/" SimpleName Patt?
    App = Path Patt?
    Alias = Patt "as" identifier
    Ann = Patt ":" wsBlock<Type>
