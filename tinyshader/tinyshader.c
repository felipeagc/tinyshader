/**
 * This file is part of the tinyshader library.
 * See tinyshader.h for license details.
 */
#include "tinyshader_internal.h"

void ts__addErr(TsCompiler *compiler, const Location *loc, const char *fmt, ...)
{
    va_list vl;

    va_start(vl, fmt);
    ts__sbReset(&compiler->sb);
    ts__sbVsprintf(&compiler->sb, fmt, vl);
    va_end(vl);

    char *msg = ts__sbBuild(&compiler->sb, &compiler->alloc);

    Error err = {0};
    err.loc = *loc;
    err.message = msg;
    arrPush(compiler, &compiler->errors, err);
}

TsCompiler *tsCompilerCreate()
{
    TsCompiler *compiler = malloc(sizeof(TsCompiler));
    memset(compiler, 0, sizeof(*compiler));

    ts__bumpInit(&compiler->alloc, 1 << 16);
    ts__sbInit(&compiler->sb);

    ts__hashInit(compiler, &compiler->keyword_table, 32);
    ts__hashInit(compiler, &compiler->builtin_function_table, 32);

    ts__hashSet(&compiler->keyword_table, "const", (void *)TOKEN_CONST);
    ts__hashSet(&compiler->keyword_table, "return", (void *)TOKEN_RETURN);
    ts__hashSet(&compiler->keyword_table, "switch", (void *)TOKEN_SWITCH);
    ts__hashSet(&compiler->keyword_table, "case", (void *)TOKEN_CASE);
    ts__hashSet(&compiler->keyword_table, "default", (void *)TOKEN_DEFAULT);
    ts__hashSet(&compiler->keyword_table, "break", (void *)TOKEN_BREAK);
    ts__hashSet(&compiler->keyword_table, "continue", (void *)TOKEN_CONTINUE);
    ts__hashSet(&compiler->keyword_table, "for", (void *)TOKEN_FOR);
    ts__hashSet(&compiler->keyword_table, "while", (void *)TOKEN_WHILE);
    ts__hashSet(&compiler->keyword_table, "do", (void *)TOKEN_DO);
    ts__hashSet(&compiler->keyword_table, "if", (void *)TOKEN_IF);
    ts__hashSet(&compiler->keyword_table, "else", (void *)TOKEN_ELSE);
    ts__hashSet(&compiler->keyword_table, "struct", (void *)TOKEN_STRUCT);
    ts__hashSet(&compiler->keyword_table, "in", (void *)TOKEN_IN);
    ts__hashSet(&compiler->keyword_table, "out", (void *)TOKEN_OUT);
    ts__hashSet(&compiler->keyword_table, "inout", (void *)TOKEN_INOUT);
    ts__hashSet(
        &compiler->keyword_table, "ConstantBuffer", (void *)TOKEN_CONSTANT_BUFFER);
    ts__hashSet(
        &compiler->keyword_table, "StructuredBuffer", (void *)TOKEN_STRUCTURED_BUFFER);
    ts__hashSet(
        &compiler->keyword_table,
        "RWStructuredBuffer",
        (void *)TOKEN_RW_STRUCTURED_BUFFER);
    ts__hashSet(&compiler->keyword_table, "SamplerState", (void *)TOKEN_SAMPLER_STATE);
    ts__hashSet(&compiler->keyword_table, "Texture1D", (void *)TOKEN_TEXTURE_1D);
    ts__hashSet(&compiler->keyword_table, "Texture2D", (void *)TOKEN_TEXTURE_2D);
    ts__hashSet(&compiler->keyword_table, "Texture3D", (void *)TOKEN_TEXTURE_3D);
    ts__hashSet(&compiler->keyword_table, "TextureCube", (void *)TOKEN_TEXTURE_CUBE);

    ts__hashSet(&compiler->keyword_table, "discard", (void *)TOKEN_DISCARD);

    ts__hashSet(&compiler->keyword_table, "uint", (void *)TOKEN_UINT);
    ts__hashSet(&compiler->keyword_table, "int", (void *)TOKEN_INT);
    ts__hashSet(&compiler->keyword_table, "float", (void *)TOKEN_FLOAT);
    ts__hashSet(&compiler->keyword_table, "bool", (void *)TOKEN_BOOL);
    ts__hashSet(&compiler->keyword_table, "true", (void *)TOKEN_TRUE);
    ts__hashSet(&compiler->keyword_table, "false", (void *)TOKEN_FALSE);
    ts__hashSet(&compiler->keyword_table, "void", (void *)TOKEN_VOID);

    ts__hashSet(&compiler->keyword_table, "static", (void *)TOKEN_STATIC);
    ts__hashSet(&compiler->keyword_table, "groupshared", (void *)TOKEN_GROUPSHARED);
    ts__hashSet(&compiler->keyword_table, "register", (void *)TOKEN_REGISTER);

    ts__hashSet(&compiler->builtin_function_table, "dot", (void *)AST_BUILTIN_FUNC_DOT);
    ts__hashSet(&compiler->builtin_function_table, "cross", (void *)AST_BUILTIN_FUNC_CROSS);
    ts__hashSet(&compiler->builtin_function_table, "length", (void *)AST_BUILTIN_FUNC_LENGTH);
    ts__hashSet(&compiler->builtin_function_table, "normalize", (void *)AST_BUILTIN_FUNC_NORMALIZE);
    ts__hashSet(&compiler->builtin_function_table, "mul", (void *)AST_BUILTIN_FUNC_MUL);
    ts__hashSet(&compiler->builtin_function_table, "distance", (void *)AST_BUILTIN_FUNC_DISTANCE);
    ts__hashSet(&compiler->builtin_function_table, "degrees", (void *)AST_BUILTIN_FUNC_DEGREES);
    ts__hashSet(&compiler->builtin_function_table, "radians", (void *)AST_BUILTIN_FUNC_RADIANS);

    ts__hashSet(&compiler->builtin_function_table, "sin", (void *)AST_BUILTIN_FUNC_SIN);
    ts__hashSet(&compiler->builtin_function_table, "cos", (void *)AST_BUILTIN_FUNC_COS);
    ts__hashSet(&compiler->builtin_function_table, "tan", (void *)AST_BUILTIN_FUNC_TAN);
    ts__hashSet(&compiler->builtin_function_table, "asin", (void *)AST_BUILTIN_FUNC_ASIN);
    ts__hashSet(&compiler->builtin_function_table, "acos", (void *)AST_BUILTIN_FUNC_ACOS);
    ts__hashSet(&compiler->builtin_function_table, "atan", (void *)AST_BUILTIN_FUNC_ATAN);
    ts__hashSet(&compiler->builtin_function_table, "sinh", (void *)AST_BUILTIN_FUNC_SINH);
    ts__hashSet(&compiler->builtin_function_table, "cosh", (void *)AST_BUILTIN_FUNC_COSH);
    ts__hashSet(&compiler->builtin_function_table, "tanh", (void *)AST_BUILTIN_FUNC_TANH);
    ts__hashSet(&compiler->builtin_function_table, "atan2", (void *)AST_BUILTIN_FUNC_ATAN2);

    ts__hashSet(&compiler->builtin_function_table, "sqrt", (void *)AST_BUILTIN_FUNC_SQRT);
    ts__hashSet(&compiler->builtin_function_table, "rsqrt", (void *)AST_BUILTIN_FUNC_RSQRT);

    ts__hashSet(&compiler->builtin_function_table, "reflect", (void *)AST_BUILTIN_FUNC_REFLECT);
    ts__hashSet(&compiler->builtin_function_table, "refract", (void *)AST_BUILTIN_FUNC_REFRACT);

    ts__hashSet(&compiler->builtin_function_table, "pow", (void *)AST_BUILTIN_FUNC_POW);
    ts__hashSet(&compiler->builtin_function_table, "exp", (void *)AST_BUILTIN_FUNC_EXP);
    ts__hashSet(&compiler->builtin_function_table, "exp2", (void *)AST_BUILTIN_FUNC_EXP2);
    ts__hashSet(&compiler->builtin_function_table, "log", (void *)AST_BUILTIN_FUNC_LOG);
    ts__hashSet(&compiler->builtin_function_table, "log2", (void *)AST_BUILTIN_FUNC_LOG2);

    ts__hashSet(&compiler->builtin_function_table, "abs", (void *)AST_BUILTIN_FUNC_ABS);
    ts__hashSet(&compiler->builtin_function_table, "min", (void *)AST_BUILTIN_FUNC_MIN);
    ts__hashSet(&compiler->builtin_function_table, "max", (void *)AST_BUILTIN_FUNC_MAX);
    ts__hashSet(&compiler->builtin_function_table, "frac", (void *)AST_BUILTIN_FUNC_FRAC);
    ts__hashSet(&compiler->builtin_function_table, "trunc", (void *)AST_BUILTIN_FUNC_TRUNC);
    ts__hashSet(&compiler->builtin_function_table, "ceil", (void *)AST_BUILTIN_FUNC_CEIL);
    ts__hashSet(&compiler->builtin_function_table, "floor", (void *)AST_BUILTIN_FUNC_FLOOR);
    ts__hashSet(&compiler->builtin_function_table, "lerp", (void *)AST_BUILTIN_FUNC_LERP);
    ts__hashSet(&compiler->builtin_function_table, "clamp", (void *)AST_BUILTIN_FUNC_CLAMP);
    ts__hashSet(&compiler->builtin_function_table, "step", (void *)AST_BUILTIN_FUNC_STEP);
    ts__hashSet(&compiler->builtin_function_table, "smoothstep", (void *)AST_BUILTIN_FUNC_SMOOTHSTEP);
    ts__hashSet(&compiler->builtin_function_table, "fmod", (void *)AST_BUILTIN_FUNC_FMOD);

    ts__hashSet(&compiler->builtin_function_table, "ddx", (void *)AST_BUILTIN_FUNC_DDX);
    ts__hashSet(&compiler->builtin_function_table, "ddy", (void *)AST_BUILTIN_FUNC_DDY);

    ts__hashSet(&compiler->builtin_function_table, "asuint", (void *)AST_BUILTIN_FUNC_ASUINT);
    ts__hashSet(&compiler->builtin_function_table, "asint", (void *)AST_BUILTIN_FUNC_ASINT);
    ts__hashSet(&compiler->builtin_function_table, "asfloat", (void *)AST_BUILTIN_FUNC_ASFLOAT);

    ts__hashSet(
        &compiler->builtin_function_table,
        "InterlockedAdd",
        (void *)AST_BUILTIN_FUNC_INTERLOCKED_ADD);
    ts__hashSet(
        &compiler->builtin_function_table,
        "InterlockedAnd",
        (void *)AST_BUILTIN_FUNC_INTERLOCKED_AND);
    ts__hashSet(
        &compiler->builtin_function_table,
        "InterlockedMin",
        (void *)AST_BUILTIN_FUNC_INTERLOCKED_MIN);
    ts__hashSet(
        &compiler->builtin_function_table,
        "InterlockedMax",
        (void *)AST_BUILTIN_FUNC_INTERLOCKED_MAX);
    ts__hashSet(
        &compiler->builtin_function_table, "InterlockedOr", (void *)AST_BUILTIN_FUNC_INTERLOCKED_OR);
    ts__hashSet(
        &compiler->builtin_function_table,
        "InterlockedXor",
        (void *)AST_BUILTIN_FUNC_INTERLOCKED_XOR);
    ts__hashSet(
        &compiler->builtin_function_table,
        "InterlockedExchange",
        (void *)AST_BUILTIN_FUNC_INTERLOCKED_EXCHANGE);
    ts__hashSet(
        &compiler->builtin_function_table,
        "InterlockedCompareExchange",
        (void *)AST_BUILTIN_FUNC_INTERLOCKED_COMPARE_EXCHANGE);
    ts__hashSet(
        &compiler->builtin_function_table,
        "InterlockedCompareStore",
        (void *)AST_BUILTIN_FUNC_INTERLOCKED_COMPARE_STORE);

    ts__hashSet(&compiler->builtin_function_table, "transpose", (void *)AST_BUILTIN_FUNC_TRANSPOSE);
    ts__hashSet(
        &compiler->builtin_function_table, "determinant", (void *)AST_BUILTIN_FUNC_DETERMINANT);

    ts__hashSet(
        &compiler->builtin_function_table, "AllMemoryBarrier", (void *)AST_BUILTIN_FUNC_ALL_MEMORY_BARRIER);
    ts__hashSet(
        &compiler->builtin_function_table,
        "AllMemoryBarrierWithGroupSync",
        (void *)AST_BUILTIN_FUNC_ALL_MEMORY_BARRIER_WITH_GROUP_SYNC);

    ts__hashSet(
        &compiler->builtin_function_table,
        "DeviceMemoryBarrier",
        (void *)AST_BUILTIN_FUNC_DEVICE_MEMORY_BARRIER);
    ts__hashSet(
        &compiler->builtin_function_table,
        "DeviceMemoryBarrierWithGroupSync",
        (void *)AST_BUILTIN_FUNC_DEVICE_MEMORY_BARRIER_WITH_GROUP_SYNC);

    ts__hashSet(
        &compiler->builtin_function_table,
        "GroupMemoryBarrier",
        (void *)AST_BUILTIN_FUNC_GROUP_MEMORY_BARRIER);
    ts__hashSet(
        &compiler->builtin_function_table,
        "GroupMemoryBarrierWithGroupSync",
        (void *)AST_BUILTIN_FUNC_GROUP_MEMORY_BARRIER_WITH_GROUP_SYNC);

    return compiler;
}

void tsCompilerDestroy(TsCompiler *compiler)
{
    ts__hashDestroy(&compiler->keyword_table);
    ts__hashDestroy(&compiler->builtin_function_table);
    ts__bumpDestroy(&compiler->alloc);
    ts__sbDestroy(&compiler->sb);
    free(compiler);
}

static void
moduleInit(Module *m, TsCompiler *compiler, const char *entry_point, TsShaderStage stage)
{
    memset(m, 0, sizeof(*m));
    m->compiler = compiler;
    m->entry_point = entry_point;
    m->stage = stage;

    ts__hashInit(compiler, &m->type_cache, 0);
}

static void moduleDestroy(Module *m)
{
    ts__hashDestroy(&m->type_cache);
}

static bool handleErrors(TsCompiler *compiler, TsCompilerOutput *output)
{
    if (arrLength(compiler->errors) > 0)
    {
        ts__sbReset(&compiler->sb);

        for (uint32_t i = 0; i < arrLength(compiler->errors); ++i)
        {
            Error *err = &compiler->errors.ptr[i];
            if (err->loc.path)
            {
                ts__sbAppend(&compiler->sb, err->loc.path);
                ts__sbAppend(&compiler->sb, ":");
            }
            ts__sbSprintf(
                &compiler->sb,
                "%u:%u: error: %s\n",
                err->loc.line,
                err->loc.col,
                err->message);
        }

        output->error = ts__sbBuildMalloc(&compiler->sb);

        return true;
    }

    return false;
}

File *ts__createFile(TsCompiler *compiler, const char *text, size_t text_size, const char *path)
{
    File *file = NEW(compiler, File);
    file->path = ts__getAbsolutePath(compiler, path);
    file->text = text;
    file->text_size = text_size;
    return file;
}

void tsCompile(TsCompiler *compiler, TsCompilerInput *input, TsCompilerOutput *output)
{
    assert(compiler);
    assert(input);
    assert(output);
    assert(input->entry_point);

    memset(output, 0, sizeof(*output));

    File *file = ts__createFile(compiler, input->input, input->input_size, input->path);

    Module *module = NEW(compiler, Module);
    moduleInit(module, compiler, input->entry_point, input->stage);

    size_t preprocessed_text_size = 0;
    const char *preprocessed_text = ts__preprocess(compiler, file, &preprocessed_text_size);
    if (handleErrors(compiler, output)) return;

    ArrayOfToken tokens =  ts__lex(compiler, file, preprocessed_text, preprocessed_text_size);
    if (handleErrors(compiler, output)) return;

    ArrayOfAstDeclPtr decls = ts__parse(compiler, tokens);
    if (handleErrors(compiler, output)) return;

    ts__analyze(compiler, module, decls.ptr, decls.len);
    if (handleErrors(compiler, output)) return;

    IRModule *ir_module = ts__irModuleCreate(compiler);
    ts__astModuleBuild(module, ir_module);
    if (handleErrors(compiler, output)) return;

    size_t word_count;
    uint32_t *words = ts__irModuleCodegen(ir_module, &word_count);
    if (handleErrors(compiler, output))
    {
        if (words) free(words);
        return;
    }

    output->spirv_byte_size = word_count * 4;
    output->spirv = (uint8_t *)words;

    ts__irModuleDestroy(ir_module);
    moduleDestroy(module);
}

void tsCompilerOutputDestroy(TsCompilerOutput *output)
{
    if (output->spirv) free(output->spirv);
    if (output->error) free(output->error);
}

static const char *TOKEN_STRINGS[TOKEN_MAX] = {
    [TOKEN_LPAREN] = "(",
    [TOKEN_RPAREN] = ")",
    [TOKEN_LBRACK] = "[",
    [TOKEN_RBRACK] = "]",
    [TOKEN_LCURLY] = "{",
    [TOKEN_RCURLY] = "}",
    [TOKEN_HASH] = "#",
    [TOKEN_SEMICOLON] = ";",
    [TOKEN_COLON] = ":",
    [TOKEN_COLON_COLON] = "::",
    [TOKEN_ADD] = "+",
    [TOKEN_SUB] = "-",
    [TOKEN_MUL] = "*",
    [TOKEN_DIV] = "/",
    [TOKEN_MOD] = "%",
    [TOKEN_ADDADD] = "++",
    [TOKEN_SUBSUB] = "--",
    [TOKEN_BITOR] = "|",
    [TOKEN_BITXOR] = "^",
    [TOKEN_BITAND] = "&",
    [TOKEN_BITNOT] = "~",
    [TOKEN_LSHIFT] = "<<",
    [TOKEN_RSHIFT] = ">>",
    [TOKEN_PERIOD] = ".",
    [TOKEN_COMMA] = ",",
    [TOKEN_QUESTION] = "?",
    [TOKEN_NOT] = "!",
    [TOKEN_ASSIGN] = "=",
    [TOKEN_EQUAL] = "==",
    [TOKEN_NOTEQ] = "!=",
    [TOKEN_LESS] = "<",
    [TOKEN_LESSEQ] = "<=",
    [TOKEN_GREATER] = ">",
    [TOKEN_GREATEREQ] = ">=",
    [TOKEN_ADD_ASSIGN] = "+=",
    [TOKEN_SUB_ASSIGN] = "-=",
    [TOKEN_MUL_ASSIGN] = "*=",
    [TOKEN_DIV_ASSIGN] = "/=",
    [TOKEN_MOD_ASSIGN] = "%=",
    [TOKEN_BITAND_ASSIGN] = "&=",
    [TOKEN_BITOR_ASSIGN] = "|=",
    [TOKEN_BITXOR_ASSIGN] = "^=",
    [TOKEN_LSHIFT_ASSIGN] = "<<=",
    [TOKEN_RSHIFT_ASSIGN] = ">>=",
    [TOKEN_AND] = "&&",
    [TOKEN_OR] = "||",
    [TOKEN_IDENT] = "<identifier>",
    [TOKEN_IN] = "in",
    [TOKEN_OUT] = "out",
    [TOKEN_INOUT] = "inout",
    [TOKEN_STRUCT] = "struct",
    [TOKEN_FOR] = "for",
    [TOKEN_WHILE] = "while",
    [TOKEN_DO] = "do",
    [TOKEN_SWITCH] = "switch",
    [TOKEN_CASE] = "case",
    [TOKEN_DEFAULT] = "default",
    [TOKEN_BREAK] = "break",
    [TOKEN_CONTINUE] = "continue",
    [TOKEN_IF] = "if",
    [TOKEN_ELSE] = "else",
    [TOKEN_RETURN] = "return",
    [TOKEN_CONST] = "const",
    [TOKEN_CONSTANT_BUFFER] = "ConstantBuffer",
    [TOKEN_STRUCTURED_BUFFER] = "StructuredBuffer",
    [TOKEN_RW_STRUCTURED_BUFFER] = "RWStructuredBuffer",
    [TOKEN_SAMPLER_STATE] = "SamplerState",
    [TOKEN_TEXTURE_1D] = "Texture1D",
    [TOKEN_TEXTURE_2D] = "Texture2D",
    [TOKEN_TEXTURE_3D] = "Texture3D",
    [TOKEN_TEXTURE_CUBE] = "TextureCube",
    [TOKEN_DISCARD] = "discard",
    [TOKEN_INT_LIT] = "<integer literal>",
    [TOKEN_FLOAT_LIT] = "<float literal>",
    [TOKEN_STRING_LIT] = "<string literal>",
    [TOKEN_BOOL] = "bool",
    [TOKEN_UINT] = "uint",
    [TOKEN_INT] = "int",
    [TOKEN_FLOAT] = "float",
    [TOKEN_VOID] = "void",
    [TOKEN_FALSE] = "false",
    [TOKEN_TRUE] = "true",
    [TOKEN_VECTOR_TYPE] = "<vector type>",
    [TOKEN_MATRIX_TYPE] = "<matrix type>",
    [TOKEN_STATIC] = "static",
    [TOKEN_GROUPSHARED] = "groupshared",
    [TOKEN_REGISTER] = "register",
};

const char *ts__getTokenString(TokenKind kind)
{
    return TOKEN_STRINGS[kind];
}
