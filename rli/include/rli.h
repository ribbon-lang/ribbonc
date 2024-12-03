/* File generated from src/lib/librli.zig */

#pragma once

#ifndef LIBRLI_H
#define LIBRLI_H

#define BB_OPAQUE struct{}

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef enum BB_Error { BB_OKAY } BB_Error;

typedef enum BB_EnvName {
    BB_ALIST_ENV,
    BB_ARITHMETIC_ENV,
    BB_ATTR_ENV,
    BB_BINDING_ENV,
    BB_CONTROL_ENV,
    BB_CONVERSION_ENV,
    BB_EFFECT_ENV,
    BB_ENV_ENV,
    BB_IO_ENV,
    BB_LIST_ENV,
    BB_LOGICAL_ENV,
    BB_META_ENV,
    BB_PAIR_ENV,
    BB_PARSER_ENV,
    BB_PATTERN_ENV,
    BB_PROCEDURE_ENV,
    BB_STRING_ENV,
    BB_SYMBOL_ENV,
    BB_TEXT_ENV,
    BB_TYPE_ENV,
} BB_EnvName;

typedef enum BB_FunctionKind {
    BB_LAMBDA,
    BB_MACRO,
} BB_FunctionKind;

typedef enum BB_Message {
    BB_SIG_TERMINATE,
    BB_ERR_PANIC,
    BB_ERR_NOT_EVALUATABLE,
    BB_ERR_NOT_CALLABLE,
    BB_ERR_TYPE_ERROR,
    BB_ERR_RANGE_ERROR,
    BB_ERR_NOT_ENOUGH_ARGUMENTS,
    BB_ERR_TOO_MANY_ARGUMENTS,
    BB_ERR_DIVISION_BY_ZERO,
    BB_ERR_UNBOUND_SYMBOL,
    BB_ERR_INVALID_CONTEXT,
    BB_ERR_ENVIRONMENT_UNDERFLOW,
    BB_ERR_CALL_STACK_OVERFLOW,
    BB_ERR_MISSING_DYNAMIC,
    BB_ERR_UNEXPECTED_TERMINATE,
    BB_ERR_MISSING_TERMINATION_DATA,
    BB_ERR_OUT_OF_MEMORY,
    BB_ERR_BAD_ENCODING,
} BB_Message;

typedef enum BB_Ordering {
    BB_LESS,
    BB_EQUAL,
    BB_GREATER,
} BB_Ordering;

typedef enum BB_Tag {
    BB_NIL,
    BB_BOOL,
    BB_INT,
    BB_FLOAT,
    BB_CHAR,
    BB_SYMBOL,
    BB_STRING,
    BB_CONS,
    BB_FUNCTION,
    BB_BUILTIN,
    BB_EXTERN_DATA,
    BB_EXTERN_FUNCTION,
} BB_Tag;

typedef BB_OPAQUE BB_Attr;

typedef BB_OPAQUE BB_Arena;

typedef BB_OPAQUE BB_Context;

typedef BB_OPAQUE BB_Interpreter;

typedef BB_OPAQUE BB_Parser;

typedef char *BB_CStr;

typedef struct BB_UStr {
    uint8_t *ptr;
    uint64_t len;
} BB_UStr;

typedef struct BB_Unit {} BB_Unit;

typedef union BB_SExprData {
    BB_Unit nil;
    bool boolean;
    int64_t integral;
    double floating;
    uint32_t character;
} BB_SExprData;

typedef struct BB_SExpr {
    void *head;
    BB_SExprData data;
} BB_SExpr;

typedef struct BB_Obj_Symbol {
    BB_Attr *attr;
    BB_UStr buffer;
} BB_Obj_Symbol;

typedef struct BB_Obj_String {
    BB_Attr *attr;
    BB_UStr buffer;
} BB_Obj_String;

typedef struct BB_Obj_Cons {
    BB_Attr *attr;
    BB_SExpr car;
    BB_SExpr cdr;
} BB_Obj_Cons;

typedef struct BB_Obj_Function {
    BB_Attr *attr;
    BB_FunctionKind kind;
    BB_SExpr args;
    BB_SExpr env;
    BB_SExpr body;
    uint64_t id;
} BB_Obj_Function;

typedef BB_OPAQUE BB_Obj_Builtin;

typedef struct BB_Writer { void* inner; } BB_Writer;

typedef void (*BB_HasherProc) (uint32_t *state, uint8_t *bytes, uint64_t bytes_len);

typedef struct BB_Hasher {
    uint32_t state;
    BB_HasherProc proc;
} BB_Hasher;

typedef BB_Ordering (*BB_VCompareProc) (void*, void*);

typedef bool (*BB_VFormatProc) (void*, BB_Writer);

typedef void (*BB_VHasherProc) (void*, BB_Hasher*);

typedef void (*BB_VFinalizerProc) (void*);

typedef struct BB_VTable {
    BB_VCompareProc compare;
    BB_VFormatProc format;
    BB_VHasherProc hashWith;
    BB_VFinalizerProc finalizer;
} BB_VTable;

typedef struct BB_Obj_ExternData {
    BB_Attr *attr;
    void *ptr;
    BB_VTable *vtable;
    BB_UStr typeName;
} BB_Obj_ExternData;

typedef bool (*BB_Proc) (BB_Interpreter *interpreter, BB_Attr *attr, BB_Message *msg, BB_SExpr *out_result, BB_SExpr args);

typedef struct BB_Obj_ExternFunction {
    BB_Attr *attr;
    BB_UStr name;
    BB_Proc proc;
} BB_Obj_ExternFunction;

typedef struct BB_Allocator {
    void *ptr;
    void *vtable;
} BB_Allocator;

#ifdef __linux__
    typedef int BB_FileHandle;
#elif _WIN32
    typedef void* BB_FileHandle;
#endif

typedef void (*BB_Finalizer) (void* object, void* userdata);

typedef struct BB_Pos {
    uint32_t line;
    uint32_t column;
    uint32_t offset;
} BB_Pos;

typedef struct BB_Opt_Pos { bool isSome; BB_Pos some; } BB_Opt_Pos;

typedef struct BB_Range {
    BB_Opt_Pos start;
    BB_Opt_Pos end;
} BB_Range;

typedef struct BB_Opt_Range { bool isSome; BB_Range some; } BB_Opt_Range;

typedef struct BB_CAttr {
    BB_Context *context;
    BB_UStr filename;
    BB_Opt_Range range;
} BB_CAttr;

BB_CAttr BB_Attr_read (BB_Attr *attr);

void BB_Attr_write (BB_Attr *attr, BB_CAttr cAttr);

BB_Arena *BB_Arena_init (BB_Error *err_out);

void BB_Arena_deinit (BB_Arena *arena);

BB_Allocator BB_Arena_allocator (BB_Arena *arena);

BB_Allocator BB_GC_allocator ();

void *BB_Allocator_alloc (BB_Allocator allocator, size_t size, size_t alignment, BB_Error *err_out);

void BB_Allocator_free (BB_Allocator allocator, void *buf, size_t size, size_t alignment);

BB_Context *BB_Context_initAllocator (BB_Allocator allocator, BB_Error *err_out);

BB_Context *BB_Context_initGc (BB_Error *err_out);

void BB_Context_deinit (BB_Context *ctx);

void *BB_Context_new (BB_Context *ctx, void *value, size_t size, size_t alignment, BB_Error *err_out);

void *BB_Context_newBuffer (BB_Context *ctx, void *values, size_t count, size_t size, size_t alignment, BB_Error *err_out);

void BB_Context_setFinalizer (BB_Context *ctx, void *ptr, BB_Finalizer finalizer, void *userdata);

void BB_Context_collectGarbage (BB_Context *ctx);

uint64_t BB_Context_genId (BB_Context *ctx);

void BB_Context_genSymbol (BB_Context *ctx, BB_UStr *symbol_out, BB_Error *err_out);

void BB_Context_bindSymbolU (BB_Context *ctx, BB_UStr value, BB_UStr *symbol_out, BB_Error *err_out);

void BB_Context_bindSymbolC (BB_Context *ctx, BB_CStr value, BB_UStr *symbol_out, BB_Error *err_out);

void BB_Context_resetSymbolInterner (BB_Context *ctx);

BB_Attr *BB_Context_bindAttrU (BB_Context *ctx, BB_UStr fileName, BB_Range *range, BB_Error *err_out);

BB_Attr *BB_Context_bindAttrC (BB_Context *ctx, BB_CStr fileName, BB_Range *range, BB_Error *err_out);

BB_Attr *BB_Context_bindAttrExistingFileU (BB_Context *ctx, BB_UStr fileName, BB_Range *range, BB_Error *err_out);

BB_Attr *BB_Context_bindAttrExistingFileC (BB_Context *ctx, BB_CStr fileName, BB_Range *range, BB_Error *err_out);

BB_Interpreter *BB_Interpreter_init (BB_Context *ctx, BB_Error *err_out);

void BB_Interpreter_deinit (BB_Interpreter *interpreter);

BB_SExpr BB_Interpreter_eval (BB_Interpreter *interpreter, BB_SExpr sexpr, BB_Error *err_out);

BB_SExpr BB_Interpreter_getEnv (BB_Interpreter *interpreter);

void BB_Interpreter_setEnv (BB_Interpreter *interpreter, BB_SExpr env, BB_Error *err_out);

void BB_Interpreter_bindBuiltinEnvs (BB_Interpreter *interpreter, BB_SExpr output_env, BB_EnvName builtin_env, BB_Error *err_out);

BB_Parser *BB_Parser_init (BB_Context *ctx, BB_Error *err_out);

void BB_Parser_deinit (BB_Parser *parser);

void BB_Parser_setFileNameU (BB_Parser *parser, BB_UStr filename, BB_Error *err_out);

void BB_Parser_setFileNameC (BB_Parser *parser, BB_CStr filename, BB_Error *err_out);

void BB_Parser_setInputU (BB_Parser *parser, BB_UStr src, BB_Pos *offset);

void BB_Parser_setInputC (BB_Parser *parser, BB_CStr src, BB_Pos *offset);

bool BB_Parser_isEof (BB_Parser *parser);

BB_SExpr BB_Parser_sexprP (BB_Parser *parser, BB_Error *err_out);

BB_SExpr BB_SExpr_Nil (BB_Attr const*attr, BB_Error *err_out);

BB_SExpr BB_SExpr_Bool (BB_Attr const*attr, bool value, BB_Error *err_out);

BB_SExpr BB_SExpr_Int (BB_Attr const*attr, int64_t value, BB_Error *err_out);

BB_SExpr BB_SExpr_Float (BB_Attr const*attr, double value, BB_Error *err_out);

BB_SExpr BB_SExpr_Char (BB_Attr const*attr, uint32_t value, BB_Error *err_out);

BB_SExpr BB_SExpr_SymbolU (BB_Attr const*attr, BB_UStr value, BB_Error *err_out);

BB_SExpr BB_SExpr_SymbolC (BB_Attr const*attr, BB_CStr value, BB_Error *err_out);

BB_SExpr BB_SExpr_GenSymbol (BB_Attr const*attr, BB_Error *err_out);

BB_SExpr BB_SExpr_StringU (BB_Attr const*attr, BB_UStr value, BB_Error *err_out);

BB_SExpr BB_SExpr_StringC (BB_Attr const*attr, BB_CStr value, BB_Error *err_out);

BB_SExpr BB_SExpr_StringPreallocatedU (BB_Attr const*attr, BB_UStr buf, BB_Error *err_out);

BB_SExpr BB_SExpr_StringPreallocatedC (BB_Attr const*attr, BB_CStr buf, BB_Error *err_out);

BB_SExpr BB_SExpr_ListTail (BB_Attr const*attr, BB_SExpr const*values, size_t values_len, BB_SExpr tail, BB_Error *err_out);

BB_SExpr BB_SExpr_List (BB_Attr const*attr, BB_SExpr const*values, size_t values_len, BB_Error *err_out);

BB_SExpr BB_SExpr_Cons (BB_Attr const*attr, BB_SExpr car, BB_SExpr cdr, BB_Error *err_out);

BB_SExpr BB_SExpr_Function (BB_Attr const*attr, BB_FunctionKind kind, BB_SExpr args, BB_SExpr env, BB_SExpr body, BB_Error *err_out);

BB_SExpr BB_SExpr_ExternDataU (BB_Attr const*attr, void *ptr, BB_VTable const*vtable, BB_UStr typeName, BB_Error *err_out);

BB_SExpr BB_SExpr_ExternDataC (BB_Attr const*attr, void *ptr, BB_VTable const*vtable, BB_CStr typeName, BB_Error *err_out);

BB_SExpr BB_SExpr_ExternFunctionU (BB_Attr const*attr, BB_UStr name, BB_Proc proc, BB_Error *err_out);

BB_SExpr BB_SExpr_ExternFunctionC (BB_Attr const*attr, BB_CStr name, BB_Proc proc, BB_Error *err_out);

BB_Ordering BB_SExpr_compare (BB_SExpr a, BB_SExpr b);

BB_Tag BB_SExpr_getTag (BB_SExpr sexpr);

void *BB_SExpr_forcePtr (BB_SExpr sexpr);

bool BB_SExpr_isNil (BB_SExpr sexpr);

bool BB_SExpr_isBool (BB_SExpr sexpr);

bool BB_SExpr_isInt (BB_SExpr sexpr);

bool BB_SExpr_isFloat (BB_SExpr sexpr);

bool BB_SExpr_isChar (BB_SExpr sexpr);

bool BB_SExpr_isSymbol (BB_SExpr sexpr);

bool BB_SExpr_isExactSymbolU (BB_SExpr sexpr, BB_UStr value);

bool BB_SExpr_isExactSymbolC (BB_SExpr sexpr, BB_CStr value);

bool BB_SExpr_isString (BB_SExpr sexpr);

bool BB_SExpr_isExactStringU (BB_SExpr sexpr, BB_UStr value);

bool BB_SExpr_isExactStringC (BB_SExpr sexpr, BB_CStr value);

bool BB_SExpr_isCons (BB_SExpr sexpr);

bool BB_SExpr_isList (BB_SExpr sexpr);

bool BB_SExpr_isFunction (BB_SExpr sexpr);

bool BB_SExpr_isLambda (BB_SExpr sexpr);

bool BB_SExpr_isMacro (BB_SExpr sexpr);

bool BB_SExpr_isBuiltin (BB_SExpr sexpr);

bool BB_SExpr_isExternData (BB_SExpr sexpr);

bool BB_SExpr_isNamedExternDataU (BB_SExpr sexpr, BB_UStr typeName);

bool BB_SExpr_isNamedExternDataC (BB_SExpr sexpr, BB_CStr typeName);

bool BB_SExpr_isExternFunction (BB_SExpr sexpr);

bool BB_SExpr_isInContext (BB_SExpr sexpr, BB_Context const*context);

BB_Context *BB_SExpr_getContext (BB_SExpr sexpr);

BB_Attr const*BB_SExpr_getAttr (BB_SExpr sexpr);

BB_Unit BB_SExpr_forceNil (BB_SExpr sexpr);

bool BB_SExpr_forceBool (BB_SExpr sexpr);

int64_t BB_SExpr_forceInt (BB_SExpr sexpr);

double BB_SExpr_forceFloat (BB_SExpr sexpr);

uint32_t BB_SExpr_forceChar (BB_SExpr sexpr);

BB_Obj_Symbol *BB_SExpr_forceSymbol (BB_SExpr sexpr);

BB_UStr BB_SExpr_forceSymbolSlice (BB_SExpr sexpr);

BB_Obj_String *BB_SExpr_forceString (BB_SExpr sexpr);

BB_UStr BB_SExpr_forceStringSlice (BB_SExpr sexpr);

BB_Obj_Cons *BB_SExpr_forceCons (BB_SExpr sexpr);

BB_Obj_Function *BB_SExpr_forceFunction (BB_SExpr sexpr);

BB_Obj_Builtin *BB_SExpr_forceBuiltin (BB_SExpr sexpr);

BB_Obj_ExternData *BB_SExpr_forceExternData (BB_SExpr sexpr);

void *BB_SExpr_forceExternDataPtr (BB_SExpr sexpr);

void *BB_SExpr_forceExternDataNamedPtrU (BB_SExpr sexpr, BB_UStr typeName);

void *BB_SExpr_forceExternDataNamedPtrC (BB_SExpr sexpr, BB_CStr typeName);

BB_Obj_ExternFunction *BB_SExpr_forceExternFunction (BB_SExpr sexpr);

BB_Obj_ExternFunction *BB_SExpr_forceNamedExternFunctionU (BB_SExpr sexpr, BB_UStr name);

BB_Obj_ExternFunction *BB_SExpr_forceNamedExternFunctionC (BB_SExpr sexpr, BB_CStr name);

BB_Proc BB_SExpr_forceExternFunctionProc (BB_SExpr sexpr);

BB_Proc BB_SExpr_forceExternFunctionNamedProcU (BB_SExpr sexpr, BB_UStr name);

BB_Proc BB_SExpr_forceExternFunctionNamedProcC (BB_SExpr sexpr, BB_CStr name);

bool BB_SExpr_castNil (BB_SExpr sexpr, BB_Unit *out_unit);

bool BB_SExpr_castBool (BB_SExpr sexpr, bool *out_bool);

bool BB_SExpr_castInt (BB_SExpr sexpr, int64_t *out_int);

bool BB_SExpr_castFloat (BB_SExpr sexpr, double *out_float);

bool BB_SExpr_castChar (BB_SExpr sexpr, uint32_t *out_char);

bool BB_SExpr_castSymbol (BB_SExpr sexpr, BB_Obj_Symbol **out_symbol);

bool BB_SExpr_castSymbolSlice (BB_SExpr sexpr, BB_UStr *out_symbol);

bool BB_SExpr_castString (BB_SExpr sexpr, BB_Obj_String **out_string);

bool BB_SExpr_castStringSlice (BB_SExpr sexpr, BB_UStr *out_string);

bool BB_SExpr_castCons (BB_SExpr sexpr, BB_Obj_Cons **out_cons);

bool BB_SExpr_castFunction (BB_SExpr sexpr, BB_Obj_Function **out_function);

bool BB_SExpr_castBuiltin (BB_SExpr sexpr, BB_Obj_Builtin **out_builtin);

bool BB_SExpr_castExternData (BB_SExpr sexpr, BB_Obj_ExternData **out_extern_data);

bool BB_SExpr_castNamedExternDataU (BB_SExpr sexpr, BB_UStr typeName, BB_Obj_ExternData **out_extern_data);

bool BB_SExpr_castNamedExternDataC (BB_SExpr sexpr, BB_CStr typeName, BB_Obj_ExternData **out_extern_data);

bool BB_SExpr_castExternDataPtr (BB_SExpr sexpr, void **out_ptr);

bool BB_SExpr_castExternDataNamedPtrU (BB_SExpr sexpr, BB_UStr typeName, void **out_ptr);

bool BB_SExpr_castExternDataNamedPtrC (BB_SExpr sexpr, BB_CStr typeName, void **out_ptr);

bool BB_SExpr_castExternFunction (BB_SExpr sexpr, BB_Obj_ExternFunction **out_extern_function);

bool BB_SExpr_castNamedExternFunctionU (BB_SExpr sexpr, BB_UStr name, BB_Obj_ExternFunction **out_extern_function);

bool BB_SExpr_castNamedExternFunctionC (BB_SExpr sexpr, BB_CStr name, BB_Obj_ExternFunction **out_extern_function);

bool BB_SExpr_castExternFunctionProc (BB_SExpr sexpr, BB_Proc *out_proc);

bool BB_SExpr_castExternFunctionNamedProcU (BB_SExpr sexpr, BB_UStr name, BB_Proc *out_proc);

bool BB_SExpr_castExternFunctionNamedProcC (BB_SExpr sexpr, BB_CStr name, BB_Proc *out_proc);

bool BB_SExpr_coerceNativeBool (BB_SExpr sexpr);

bool BB_SExpr_coerceNativeInt (BB_SExpr sexpr, int64_t *out_int);

bool BB_SExpr_coerceNativeFloat (BB_SExpr sexpr, double *out_float);

bool BB_SExpr_coerceNativeChar (BB_SExpr sexpr, uint32_t *out_char);

bool BB_SExpr_listLen (BB_SExpr sexpr, size_t *out_len);

void BB_SExpr_hashWith (BB_SExpr sexpr, BB_Hasher *hasher);

void BB_SExpr_print (BB_SExpr sexpr, BB_FileHandle out, BB_Error *err_out);

BB_CStr BB_Error_name (BB_Error err);

#endif // LIBRLI_H
