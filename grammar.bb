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

TypeHead = listSome<identifier (":" Kind)?> ("where" listSome<Type>)?

Field body = Label body where
    Label = uInt ("\\" SimpleName)? | SimpleName

Path
    = PlainBase ("/" Component++"/")?
    | SlashBase Component**"/"
    | Component++"/" where
    PlainBase = "module" SimpleName | "file" string
    SlashBase = "/" | "./" | "../"+
    Component = ("namespace" | "instance" | "type" | "value")? FixName

PathExt tail
    = Path/PlainBase ("/" Path/Component++"/")? "/" tail
    | Path/SlashBase (Path/Component++"/" "/" tail | tail)
    | Path/Component++"/" "/" tail
    | tail

WsList sep elem = wsBlock<wsBlock<elem>++(sep?) | elem (sep elem)*>
WsLines elem = wsBlock<wsBlock<elem>*>


Module = ModuleHead Doc

ModuleHead = "module" string "@" version wsBlock<Meta> where
    Meta
        = "sources" WsList<",", string>
        | "dependencies" WsList<",", Dependency>
        | identifier WsList<",", string>
    Dependency = string "@" version ("as" SimpleName)?
    Version
        = $uInt "." $uInt "." $uInt
        if $1 > 0 or $2 > 0 or $3 > 0

Doc = Def*

Def = Visibility? (Use | Namespace | TypeDef | ValueDef) where
    Use = "use" Tree where
        Tree = Elem ("as" FixNameDecl)?
        Elem = Path | PathExt<"{" Tree**"," "}" | FixName | (".." Hiding?)>
        Hiding = "hiding" ("{" FixName**"," "}" | FixName)

    Namespace = SimpleName "=" "namespace" wsBlock<Doc>

    TypeDef = SimpleName "=" TypeBody where
        EffectDec = FixNameDecl ":" wsBlock<Type>
        FieldDec = Field<":" wsBlock<Type>>
        StructFields
            = WsBlock<",", wsBlock<Type>>
            | WsBlock<",", SimpleName ":" wsBlock<Type>>
        ClassDec
            = SimpleName ":" "type" TypeHead?
            | FixNameDecl ":" ("for" TypeHead "=>")? Type
        InstanceDef
            = SimpleName "=" "type" (TypeHead "=>")? Type
            | FixNameDecl "=" wsBlock<Value>
        TypeBody
            = "type" (TypeHead "=>")? wsBlock<Type>
            | "struct" (TypeHead "=>")? StructFields
            | "union" (TypeHead "=>")? WsBlock<",", FieldDec>
            | "effect" (TypeHead "=>")? WsLines<EffectDec>
            | "class" (TypeHead "=>")? WsLines<ClassDec>
            | "instance" TypeHead? "for" Type "=>" WsLines<InstanceDef>

    ValueDef = FixNameDecl (ValueType? ValueExpr | ValueType) where
        ValueType = ":" ("for" TypeHead "=>")? wsBlock<Type>
        ValueExpr = "=" wsBlock<Value>

Kind
    = "Type" | "Effect"
    | "Int"  | "String"
    | "Data" | "Effects"
    | "Constraint"
    | Kind "->" Kind
    | "(" Kind ")"

Type |=
    Var = identifier
    Free = "_"
    Con = Path
    Unit = "(" ")"
    Group = "(" Type ")"
    Tuple = "(" Type "," Type**"," ")"
    DataRow = "{" Field<":" Type>**"," "}"
    EffectRow = "[" Type**"," "]"
    Function = Type "->" Type ("in" Type)?
    App = Type Type
    QuantifiedInline = "'" identifier ("of" Kind)?
    Constraint |=
        IsStruct = "struct" Type ("as" Type)?
        IsUnion = "union" Type ("as" Type)?
        HasClass = Type? "with" Type
        HasAssoc = Type? "has" (FixName ("~" Type)? | "type" FixName)
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
    Struct = "{" Field<"=" WsBlock<Patt>>**"," ".."? "}"
    AnyUnion = "+/" SimpleName Patt?
    App = Path Patt?
    Alias = Patt "as" identifier
    Ann = Patt ":" wsBlock<Type>
