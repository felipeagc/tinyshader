#include "tinyshader_internal.h"

void ts__addErr(TsCompiler *compiler, Location *loc, const char *msg)
{
    Error err = {0};
    err.loc = *loc;
    err.message = msg;
    arrPush(compiler->errors, err);
}

TsCompiler *tsCompilerCreate()
{
    TsCompiler *compiler = malloc(sizeof(TsCompiler));
    memset(compiler, 0, sizeof(*compiler));

    ts__bumpInit(&compiler->alloc, 1 << 16);
    ts__sbInit(&compiler->sb);

    ts__hashInit(&compiler->keyword_table, 32);

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

    ts__hashSet(&compiler->keyword_table, "dot", (void *)TOKEN_BUILTIN_DOT);
    ts__hashSet(&compiler->keyword_table, "cross", (void *)TOKEN_BUILTIN_CROSS);
    ts__hashSet(&compiler->keyword_table, "length", (void *)TOKEN_BUILTIN_LENGTH);
    ts__hashSet(&compiler->keyword_table, "normalize", (void *)TOKEN_BUILTIN_NORMALIZE);
    ts__hashSet(&compiler->keyword_table, "mul", (void *)TOKEN_BUILTIN_MUL);
    ts__hashSet(&compiler->keyword_table, "distance", (void *)TOKEN_BUILTIN_DISTANCE);
    ts__hashSet(&compiler->keyword_table, "degrees", (void *)TOKEN_BUILTIN_DEGREES);
    ts__hashSet(&compiler->keyword_table, "radians", (void *)TOKEN_BUILTIN_RADIANS);

    ts__hashSet(&compiler->keyword_table, "sin", (void *)TOKEN_BUILTIN_SIN);
    ts__hashSet(&compiler->keyword_table, "cos", (void *)TOKEN_BUILTIN_COS);
    ts__hashSet(&compiler->keyword_table, "tan", (void *)TOKEN_BUILTIN_TAN);
    ts__hashSet(&compiler->keyword_table, "asin", (void *)TOKEN_BUILTIN_ASIN);
    ts__hashSet(&compiler->keyword_table, "acos", (void *)TOKEN_BUILTIN_ACOS);
    ts__hashSet(&compiler->keyword_table, "atan", (void *)TOKEN_BUILTIN_ATAN);
    ts__hashSet(&compiler->keyword_table, "sinh", (void *)TOKEN_BUILTIN_SINH);
    ts__hashSet(&compiler->keyword_table, "cosh", (void *)TOKEN_BUILTIN_COSH);
    ts__hashSet(&compiler->keyword_table, "tanh", (void *)TOKEN_BUILTIN_TANH);
    ts__hashSet(&compiler->keyword_table, "atan2", (void *)TOKEN_BUILTIN_ATAN2);

    ts__hashSet(&compiler->keyword_table, "sqrt", (void *)TOKEN_BUILTIN_SQRT);
    ts__hashSet(&compiler->keyword_table, "rsqrt", (void *)TOKEN_BUILTIN_RSQRT);

    ts__hashSet(&compiler->keyword_table, "reflect", (void *)TOKEN_BUILTIN_REFLECT);
    ts__hashSet(&compiler->keyword_table, "refract", (void *)TOKEN_BUILTIN_REFRACT);

    ts__hashSet(&compiler->keyword_table, "pow", (void *)TOKEN_BUILTIN_POW);
    ts__hashSet(&compiler->keyword_table, "exp", (void *)TOKEN_BUILTIN_EXP);
    ts__hashSet(&compiler->keyword_table, "exp2", (void *)TOKEN_BUILTIN_EXP2);
    ts__hashSet(&compiler->keyword_table, "log", (void *)TOKEN_BUILTIN_LOG);
    ts__hashSet(&compiler->keyword_table, "log2", (void *)TOKEN_BUILTIN_LOG2);

    ts__hashSet(&compiler->keyword_table, "abs", (void *)TOKEN_BUILTIN_ABS);
    ts__hashSet(&compiler->keyword_table, "min", (void *)TOKEN_BUILTIN_MIN);
    ts__hashSet(&compiler->keyword_table, "max", (void *)TOKEN_BUILTIN_MAX);
    ts__hashSet(&compiler->keyword_table, "lerp", (void *)TOKEN_BUILTIN_LERP);
    ts__hashSet(&compiler->keyword_table, "clamp", (void *)TOKEN_BUILTIN_CLAMP);
    ts__hashSet(&compiler->keyword_table, "step", (void *)TOKEN_BUILTIN_STEP);
    ts__hashSet(&compiler->keyword_table, "smoothstep", (void *)TOKEN_BUILTIN_SMOOTHSTEP);

    ts__hashSet(&compiler->keyword_table, "ddx", (void *)TOKEN_BUILTIN_DDX);
    ts__hashSet(&compiler->keyword_table, "ddy", (void *)TOKEN_BUILTIN_DDY);

    ts__hashSet(&compiler->keyword_table, "InterlockedAdd", (void *)TOKEN_BUILTIN_INTERLOCKED_ADD);

    ts__hashSet(&compiler->keyword_table, "transpose", (void *)TOKEN_BUILTIN_TRANSPOSE);
    ts__hashSet(
        &compiler->keyword_table, "determinant", (void *)TOKEN_BUILTIN_DETERMINANT);

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

    ts__hashSet(
        &compiler->keyword_table, "AllMemoryBarrier", (void *)TOKEN_BARRIER_ALL_MEMORY);
    ts__hashSet(
        &compiler->keyword_table,
        "AllMemoryBarrierWithGroupSync",
        (void *)TOKEN_BARRIER_ALL_MEMORY_WITH_GROUP_SYNC);

    ts__hashSet(
        &compiler->keyword_table,
        "DeviceMemoryBarrier",
        (void *)TOKEN_BARRIER_DEVICE_MEMORY);
    ts__hashSet(
        &compiler->keyword_table,
        "DeviceMemoryBarrierWithGroupSync",
        (void *)TOKEN_BARRIER_DEVICE_MEMORY_WITH_GROUP_SYNC);

    ts__hashSet(
        &compiler->keyword_table,
        "GroupMemoryBarrier",
        (void *)TOKEN_BARRIER_GROUP_MEMORY);
    ts__hashSet(
        &compiler->keyword_table,
        "GroupMemoryBarrierWithGroupSync",
        (void *)TOKEN_BARRIER_GROUP_MEMORY_WITH_GROUP_SYNC);

    return compiler;
}

void tsCompilerDestroy(TsCompiler *compiler)
{
    ts__hashDestroy(&compiler->keyword_table);
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

    ts__hashInit(&m->type_cache, 0);
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
            Error *err = &compiler->errors[i];
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

File *ts__createFile(TsCompiler *compiler, char *text, size_t text_size, char *path)
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

    File *file = ts__createFile(compiler, input->input, input->input_size, input->path);

    Module *module = NEW(compiler, Module);
    moduleInit(module, compiler, input->entry_point, input->stage);

    Preprocessor p = {0};
    size_t text_size = 0;
    char *text = ts__preprocessRootFile(&p, compiler, file, &text_size);
    if (handleErrors(compiler, output)) return;

    Lexer lexer = {0};
    ts__lexerLex(&lexer, compiler, text, text_size);
    if (handleErrors(compiler, output)) return;

    Parser parser = {0};
    ts__parserParse(&parser, compiler, lexer.tokens, arrLength(lexer.tokens));
    if (handleErrors(compiler, output)) return;

    Analyzer analyzer = {0};
    ts__analyzerAnalyze(
        &analyzer, compiler, module, parser.decls, arrLength(parser.decls));
    if (handleErrors(compiler, output)) return;

    size_t word_count;
    uint32_t *words = ts__irModuleCodegen(module, &word_count);

    output->spirv_byte_size = word_count * 4;
    output->spirv = (uint8_t *)words;

    moduleDestroy(module);
}

void tsCompilerOutputDestroy(TsCompilerOutput *output)
{
    if (output->spirv) free(output->spirv);
    if (output->error) free(output->error);
}
