#include <inttypes.h>
#include <stdio.h>

#include <rli.h>

#ifdef _WIN32
    #include <windows.h>
#endif

#define TEST_PATH "test.bb"
#define TEST_CONTENT "(+ 1 2 3)"

int main (int argc, char** argv) {
    #ifdef __linux__
        printf("Linux\n");
        BB_FileHandle H_STDOUT = 1;
    #elif _WIN32
        printf("Windows\n");
        BB_FileHandle H_STDOUT = GetStdHandle(STD_OUTPUT_HANDLE);
    #elif
        #error "Unsupported platform"
    #endif
    BB_Error err = BB_OKAY;

    BB_Context* ctx = BB_Context_initGc(&err);
    if (err != BB_OKAY) {
        fprintf(stderr, "Failed to initialize context: %s\n", BB_Error_name(err));
        goto end0;
    }

    BB_Interpreter* interpreter = BB_Interpreter_init(ctx, &err);
    if (err != BB_OKAY) {
        fprintf(stderr, "Failed to initialize interpreter: %s\n", BB_Error_name(err));
        goto end1;
    }

    BB_Interpreter_bindBuiltinEnv(interpreter, BB_Interpreter_getEnv(interpreter), BB_ARITHMETIC_ENV, &err);
    if (err != BB_OKAY) {
        fprintf(stderr, "Failed to bind built-in environment: %s\n", BB_Error_name(err));
        goto end2;
    }

    BB_Parser* parser = BB_Parser_init(ctx, &err);
    if (err != BB_OKAY) {
        printf("Failed to initialize parser: %s\n", BB_Error_name(err));
        goto end2;
    }

    BB_Parser_setFileNameC(parser, TEST_PATH, &err);
    if (err != BB_OKAY) {
        fprintf(stderr, "Failed to set file name: %s\n", BB_Error_name(err));
        goto end3;
    }

    BB_Parser_setInputC(parser, TEST_CONTENT, NULL);

    BB_SExpr result = BB_Parser_sexprP(parser, &err);
    if (err != BB_OKAY) {
        fprintf(stderr, "Failed to parse S-expression: %s\n", BB_Error_name(err));
        goto end3;
    }

    BB_SExpr_print(result, H_STDOUT, &err);
    if (err != BB_OKAY) {
        fprintf(stderr, "Failed to print S-expression: %s\n", BB_Error_name(err));
        goto end3;
    }

    putchar('\n');

    BB_SExpr eExpr = BB_Interpreter_eval(interpreter, result, &err);
    if (err != BB_OKAY) {
        fprintf(stderr, "Failed to eval S-expression: %s\n", BB_Error_name(err));
        goto end3;
    }

    if (eExpr.data.integral != 6) {
        fprintf(stderr, "Unexpected result: %"PRId64"\n", eExpr.data.integral);
        goto end3;
    }

    printf("Result: %"PRId64"\n", eExpr.data.integral);

    end3: BB_Parser_deinit(parser);
    end2: BB_Interpreter_deinit(interpreter);
    end1: BB_Context_deinit(ctx);
    end0: return err;
}
