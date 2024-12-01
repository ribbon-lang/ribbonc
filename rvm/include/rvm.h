/* File generated from src/lib/librvm.zig */

#pragma once

#ifndef LIBRVM_H
#define LIBRVM_H

#define BB_OPAQUE struct{}

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef enum BB_Error { BB_OKAY } BB_Error;

typedef enum BB_Ordering {
    BB_LESS,
    BB_EQUAL,
    BB_GREATER,
} BB_Ordering;

typedef BB_OPAQUE BB_Arena;

typedef char *BB_CStr;

typedef struct BB_UStr {
    uint8_t *ptr;
    uint64_t len;
} BB_UStr;

typedef struct BB_Unit {} BB_Unit;

typedef struct BB_Writer { void* inner; } BB_Writer;

typedef void (*BB_HasherProc) (uint32_t *state, uint8_t *bytes, uint64_t bytes_len);

typedef struct BB_Hasher {
    uint32_t state;
    BB_HasherProc proc;
} BB_Hasher;

typedef struct BB_Allocator {
    void *ptr;
    void *vtable;
} BB_Allocator;

#ifdef __linux__
    typedef int BB_FileHandle;
#elif _WIN32
    typedef void* BB_FileHandle;
#endif

BB_Arena *BB_Arena_init (BB_Error *err_out);

void BB_Arena_deinit (BB_Arena *arena);

BB_Allocator BB_Arena_allocator (BB_Arena *arena);

void *BB_Allocator_alloc (BB_Allocator allocator, size_t size, size_t alignment, BB_Error *err_out);

void BB_Allocator_free (BB_Allocator allocator, void *buf, size_t size, size_t alignment);

#endif // LIBRVM_H
