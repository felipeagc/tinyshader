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
    ts__hashSet(&compiler->keyword_table, "if", (void *)TOKEN_IF);
    ts__hashSet(&compiler->keyword_table, "else", (void *)TOKEN_ELSE);
    ts__hashSet(&compiler->keyword_table, "import", (void *)TOKEN_IMPORT);
    ts__hashSet(&compiler->keyword_table, "struct", (void *)TOKEN_STRUCT);
    ts__hashSet(&compiler->keyword_table, "in", (void *)TOKEN_IN);
    ts__hashSet(&compiler->keyword_table, "out", (void *)TOKEN_OUT);
    ts__hashSet(&compiler->keyword_table, "inout", (void *)TOKEN_INOUT);
    ts__hashSet(&compiler->keyword_table, "ConstantBuffer", (void *)TOKEN_CONSTANT_BUFFER);
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

    ts__hashSet(&compiler->keyword_table, "uint", (void *)TOKEN_UINT);
    ts__hashSet(&compiler->keyword_table, "int", (void *)TOKEN_INT);
    ts__hashSet(&compiler->keyword_table, "float", (void *)TOKEN_FLOAT);
    ts__hashSet(&compiler->keyword_table, "bool", (void *)TOKEN_BOOL);
    ts__hashSet(&compiler->keyword_table, "true", (void *)TOKEN_TRUE);
    ts__hashSet(&compiler->keyword_table, "false", (void *)TOKEN_FALSE);
    ts__hashSet(&compiler->keyword_table, "void", (void *)TOKEN_VOID);

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
    arrFree(m->files);
}

static bool handleErrors(TsCompiler *compiler, TsCompilerOutput *output)
{
    if (arrLength(compiler->errors) > 0)
    {
        ts__sbReset(&compiler->sb);

        for (uint32_t i = 0; i < arrLength(compiler->errors); ++i)
        {
            Error *err = &compiler->errors[i];
            if (err->loc.file && err->loc.file->path)
            {
                ts__sbAppend(&compiler->sb, err->loc.file->path);
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

void tsCompile(TsCompiler *compiler, TsCompilerInput *input, TsCompilerOutput *output)
{
    assert(compiler);
    assert(input);
    assert(output);

    assert(input->entry_point);

    {
        File *file = NEW(compiler, File);
        file->path = input->path;
        file->text = input->input;
        file->text_size = input->input_size;

        arrPush(compiler->file_queue, file);
    }

    Module *module = NEW(compiler, Module);
    moduleInit(module, compiler, input->entry_point, input->stage);

    while (arrLength(compiler->file_queue) > 0)
    {
        File *file = compiler->file_queue[arrLength(compiler->file_queue) - 1];
        arrPop(compiler->file_queue);

        Lexer lexer = {0};
        ts__lexerLex(&lexer, compiler, file);
        if (handleErrors(compiler, output)) return;

        Parser parser = {0};
        ts__parserParse(&parser, compiler, file);
        if (handleErrors(compiler, output)) return;

        arrPush(module->files, file);
    }

    Analyzer analyzer = {0};
    ts__analyzerAnalyze(&analyzer, compiler, module);
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
