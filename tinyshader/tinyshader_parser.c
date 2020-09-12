#include "tinyshader_internal.h"

static inline bool isLetter(char c)
{
    return (('z' >= c) && (c >= 'a')) || (('Z' >= c) && (c >= 'A')) || c == '_';
}

static inline bool isNumeric(char c)
{
    return ('0' <= c) && ('9' >= c);
}

static inline bool isAlphanum(char c)
{
    return isLetter(c) || isNumeric(c);
}

static inline bool isWhitespace(char c)
{
    return (c == ' ') || (c == '\t');
}

////////////////////////////////
//
// Preprocessor
//
////////////////////////////////

typedef struct PreprocessorFile
{
    TsCompiler *compiler;
    File *file;
    size_t pos;
    uint32_t line;
    uint32_t col;

    /*array*/ bool *if_stack;
} PreprocessorFile;

static inline bool preprocIsAtEnd(PreprocessorFile *f)
{
    return (f->pos >= f->file->text_size) || (f->file->text[f->pos] == '\0');
}

static inline char preprocNext(PreprocessorFile *f, size_t count)
{
    char c = f->file->text[f->pos];
    f->pos += count;
    f->col += (uint32_t)count;
    return c;
}

static inline char preprocPeek(PreprocessorFile *f, size_t offset)
{
    return f->file->text[f->pos + offset];
}

static Location preprocErrLoc(PreprocessorFile *f)
{
    Location err_loc = {0};
    err_loc.path = f->file->path;
    err_loc.pos = (uint32_t)f->pos;
    err_loc.line = f->line;
    err_loc.col = f->col;
    err_loc.length = 1;
    return err_loc;
}

static inline bool preprocConsume1(PreprocessorFile *f, char c)
{
    if (preprocPeek(f, 0) != c)
    {
        Location err_loc = preprocErrLoc(f);
        ts__addErr(f->compiler, &err_loc, "unexpected character");
        preprocNext(f, 1);
        return false;
    }
    preprocNext(f, 1);
    return true;
}

static inline bool preprocSkipWhitespace1(PreprocessorFile *f)
{
    if (!isWhitespace(preprocPeek(f, 0)))
    {
        Location err_loc = preprocErrLoc(f);
        ts__addErr(f->compiler, &err_loc, "expecting whitespace");
        preprocNext(f, 1);
        return false;
    }
    while (isWhitespace(preprocPeek(f, 0)))
    {
        preprocNext(f, 1);
    }
    return true;
}

static void preprocPrintLoc(Preprocessor *p, PreprocessorFile *f)
{
    if (f->file->path)
    {
        ts__sbSprintf(
            &p->sb,
            "# %zu \"%.*s\"\n",
            f->line,
            (int)strlen(f->file->path),
            f->file->path);
    }
    else
    {
        ts__sbSprintf(&p->sb, "# %zu \n", f->line);
    }
}

static bool preprocCanInsert(PreprocessorFile *f)
{
    bool can_insert = true;
    if (arrLength(f->if_stack) > 0)
    {
        can_insert = *arrLast(f->if_stack);
    }
    return can_insert;
}

static int preprocIsAtLineEnd(PreprocessorFile *f)
{
    if (preprocIsAtEnd(f)) return 0;

    if (preprocPeek(f, 0) == '\n')
    {
        return 1;
    }

    if (preprocPeek(f, 0) == '\r' && preprocPeek(f, 1) == '\n')
    {
        return 2;
    }

    return 0;
}

static void preprocessFile(Preprocessor *p, File *file)
{
    PreprocessorFile *f = NEW(p->compiler, PreprocessorFile);

    f->file = file;
    f->col = 1;
    f->line = 1;
    f->compiler = p->compiler;

    preprocPrintLoc(p, f);

    while (!preprocIsAtEnd(f))
    {
        char c;
        switch ((c = preprocPeek(f, 0)))
        {
        case '#': {
            char *curr = &f->file->text[f->pos];
            if (strncmp(curr, "#define", strlen("#define")) == 0)
            {
                if (preprocCanInsert(f))
                {
                    preprocNext(f, strlen("#define"));
                    if (!preprocSkipWhitespace1(f)) break;

                    size_t ident_start = f->pos;

                    while (!preprocIsAtEnd(f) && isAlphanum(preprocPeek(f, 0)))
                    {
                        preprocNext(f, 1);
                    }

                    size_t ident_length = f->pos - ident_start;

                    if (ident_length == 0)
                    {
                        Location err_loc = preprocErrLoc(f);
                        ts__addErr(
                            p->compiler, &err_loc, "expected idenfier for #define");
                        break;
                    }

                    char *ident = NEW_ARRAY(p->compiler, char, ident_length + 1);
                    memcpy(ident, &f->file->text[ident_start], ident_length);
                    ident[ident_length] = '\0';

                    char *defined_value = NULL;

                    if (!preprocIsAtEnd(f) && isWhitespace(preprocPeek(f, 0)))
                    {
                        preprocSkipWhitespace1(f);

                        ts__sbReset(&p->compiler->sb);

                        while (!preprocIsAtEnd(f) && !preprocIsAtLineEnd(f))
                        {
                            if (preprocPeek(f, 0) == '\\' && preprocIsAtLineEnd(f))
                            {
                                f->line++;
                                preprocNext(f, preprocIsAtLineEnd(f));
                            }
                            ts__sbAppendChar(&p->compiler->sb, preprocPeek(f, 0));
                            preprocNext(f, 1);
                        }

                        defined_value =
                            ts__sbBuild(&p->compiler->sb, &p->compiler->alloc);
                    }

                    ts__hashSet(&p->defines, ident, defined_value);
                }
            }
            else if (strncmp(curr, "#undef", strlen("#undef")) == 0)
            {
                if (preprocCanInsert(f))
                {
                    preprocNext(f, strlen("#undef"));
                    if (!preprocSkipWhitespace1(f)) break;

                    size_t ident_start = f->pos;

                    while (!preprocIsAtEnd(f) && isAlphanum(preprocPeek(f, 0)))
                    {
                        preprocNext(f, 1);
                    }

                    size_t ident_length = f->pos - ident_start;

                    if (ident_length == 0)
                    {
                        Location err_loc = preprocErrLoc(f);
                        ts__addErr(p->compiler, &err_loc, "expected idenfier for #undef");
                        break;
                    }

                    char *ident = NEW_ARRAY(p->compiler, char, ident_length + 1);
                    memcpy(ident, &f->file->text[ident_start], ident_length);
                    ident[ident_length] = '\0';

                    ts__hashRemove(&p->defines, ident);
                }
            }
            else if (strncmp(curr, "#ifdef", strlen("#ifdef")) == 0)
            {
                if (preprocCanInsert(f))
                {
                    preprocNext(f, strlen("#ifdef"));
                    if (!preprocSkipWhitespace1(f)) break;

                    size_t ident_start = f->pos;

                    while (!preprocIsAtEnd(f) && isAlphanum(preprocPeek(f, 0)))
                    {
                        preprocNext(f, 1);
                    }

                    size_t ident_length = f->pos - ident_start;

                    if (ident_length == 0)
                    {
                        Location err_loc = preprocErrLoc(f);
                        ts__addErr(p->compiler, &err_loc, "expected idenfier for #ifdef");
                        break;
                    }

                    char *ident = NEW_ARRAY(p->compiler, char, ident_length + 1);
                    memcpy(ident, &f->file->text[ident_start], ident_length);
                    ident[ident_length] = '\0';

                    void *result;
                    arrPush(f->if_stack, ts__hashGet(&p->defines, ident, &result));
                }
            }
            else if (strncmp(curr, "#ifndef", strlen("#ifndef")) == 0)
            {
                if (preprocCanInsert(f))
                {
                    preprocNext(f, strlen("#ifndef"));
                    if (!preprocSkipWhitespace1(f)) break;

                    size_t ident_start = f->pos;

                    while (!preprocIsAtEnd(f) && isAlphanum(preprocPeek(f, 0)))
                    {
                        preprocNext(f, 1);
                    }

                    size_t ident_length = f->pos - ident_start;

                    if (ident_length == 0)
                    {
                        Location err_loc = preprocErrLoc(f);
                        ts__addErr(
                            p->compiler, &err_loc, "expected idenfier for #ifndef");
                        break;
                    }

                    char *ident = NEW_ARRAY(p->compiler, char, ident_length + 1);
                    memcpy(ident, &f->file->text[ident_start], ident_length);
                    ident[ident_length] = '\0';

                    void *result;
                    arrPush(f->if_stack, !ts__hashGet(&p->defines, ident, &result));
                }
            }
            else if (strncmp(curr, "#else", strlen("#else")) == 0)
            {
                preprocNext(f, strlen("#else"));

                if (arrLength(f->if_stack) == 0)
                {
                    Location err_loc = preprocErrLoc(f);
                    ts__addErr(p->compiler, &err_loc, "unmatched #else");
                    break;
                }

                bool *insert = &f->if_stack[arrLength(f->if_stack)-1];
                *insert = !(*insert); // Invert the condition
            }
            else if (strncmp(curr, "#endif", strlen("#endif")) == 0)
            {
                preprocNext(f, strlen("#endif"));

                if (arrLength(f->if_stack) == 0)
                {
                    Location err_loc = preprocErrLoc(f);
                    ts__addErr(p->compiler, &err_loc, "unmatched #endif");
                    break;
                }
                arrPop(f->if_stack);
            }
            else if (strncmp(curr, "#include", strlen("#include")) == 0)
            {
                if (preprocCanInsert(f))
                {
                    preprocNext(f, strlen("#include"));
                    if (!preprocSkipWhitespace1(f)) break;

                    if (!preprocConsume1(f, '\"')) break;

                    size_t path_start = f->pos;

                    while (!preprocIsAtEnd(f) && (preprocPeek(f, 0) != '\"'))
                    {
                        preprocNext(f, 1);
                    }

                    size_t path_length = f->pos - path_start;

                    char *path = NEW_ARRAY(p->compiler, char, path_length + 1);
                    memcpy(path, &f->file->text[path_start], path_length);
                    path[path_length] = '\0';

                    if (!preprocConsume1(f, '\"')) break;

                    if (!f->file->path)
                    {
                        Location err_loc = preprocErrLoc(f);
                        ts__addErr(
                            f->compiler,
                            &err_loc,
                            "#include only works if you supply a path for the file "
                            "during compilation");
                        break;
                    }

                    char *this_path = ts__getAbsolutePath(f->compiler, f->file->path);
                    char *dir_path = ts__getPathDir(f->compiler, this_path);
                    char *full_path = ts__pathConcat(f->compiler, dir_path, path);
                    bool exists = ts__fileExists(f->compiler, full_path);

                    if (!this_path || !dir_path || !full_path || !exists)
                    {
                        Location err_loc = preprocErrLoc(f);
                        ts__addErr(
                            f->compiler, &err_loc, "#include'd file does not exist");
                        break;
                    }

                    FILE *cfile = fopen(full_path, "rb");
                    if (!cfile)
                    {
                        Location err_loc = preprocErrLoc(f);
                        ts__addErr(f->compiler, &err_loc, "could not open included file");
                        break;
                    }

                    fseek(cfile, 0, SEEK_END);
                    size_t text_size = ftell(cfile);
                    fseek(cfile, 0, SEEK_SET);

                    char *text = NEW_ARRAY_UNINIT(p->compiler, char, text_size);
                    fread(text, 1, text_size, cfile);

                    fclose(cfile);

                    File *new_file =
                        ts__createFile(f->compiler, text, text_size, full_path);

                    preprocessFile(p, new_file);
                }
            }
            else
            {
                Location err_loc = preprocErrLoc(f);
                ts__addErr(p->compiler, &err_loc, "unexpected preprocessor directive");
            }

            while (!preprocIsAtEnd(f) && !preprocIsAtLineEnd(f))
            {
                preprocNext(f, 1);
            }

            if (!preprocIsAtEnd(f) && preprocIsAtLineEnd(f))
            {
                preprocNext(f, preprocIsAtLineEnd(f));
                f->line++;
            }

            preprocPrintLoc(p, f);
            break;
        }

        default: {
            if (!preprocCanInsert(f))
            {
                preprocNext(f, 1);
                break;
            }

            if (preprocIsAtLineEnd(f))
            {
                f->col = 0;
                f->line++;

                if (preprocCanInsert(f))
                {
                    if (preprocIsAtLineEnd(f) == 1)
                    {
                        ts__sbAppendChar(&p->sb, '\n');
                    }
                    else
                    {
                        ts__sbAppendChar(&p->sb, '\r');
                        ts__sbAppendChar(&p->sb, '\n');
                    }
                }

                preprocNext(f, preprocIsAtLineEnd(f));
            }
            else if (isLetter(preprocPeek(f, 0)))
            {
                size_t ident_start = f->pos;

                while (!preprocIsAtEnd(f) && isAlphanum(preprocPeek(f, 0)))
                {
                    preprocNext(f, 1);
                }

                size_t ident_length = f->pos - ident_start;

                char *ident = calloc(1, ident_length + 1);
                memcpy(ident, &f->file->text[ident_start], ident_length);
                ident[ident_length] = '\0';

                char *value;
                if (ts__hashGet(&p->defines, ident, (void **)&value))
                {
                    if (value) ts__sbAppend(&p->sb, value);
                }
                else
                {
                    ts__sbAppend(&p->sb, ident);
                }

                free(ident);
            }
            else if (preprocPeek(f, 0) == '/' && preprocPeek(f, 1) == '/')
            {
                size_t comment_start = f->pos;

                preprocNext(f, 2);

                while (!preprocIsAtEnd(f) && !preprocIsAtLineEnd(f))
                {
                    preprocNext(f, 1);
                }

                size_t comment_length = f->pos - comment_start;

                char *comment = calloc(1, comment_length + 1);
                memcpy(comment, &f->file->text[comment_start], comment_length);
                comment[comment_length] = '\0';

                ts__sbAppend(&p->sb, comment);

                free(comment);
            }
            else if (preprocPeek(f, 0) == '/' && preprocPeek(f, 1) == '*')
            {
                size_t comment_start = f->pos;

                // Multiline comment
                preprocNext(f, 2);

                while ((preprocPeek(f, 0) != '*' || preprocPeek(f, 1) != '/') &&
                       !preprocIsAtEnd(f))
                {
                    if (preprocIsAtLineEnd(f))
                    {
                        ++f->line;
                        f->col = 0;
                        preprocNext(f, preprocIsAtLineEnd(f));
                    }
                    else
                    {
                        preprocNext(f, 1);
                    }
                }

                if (!preprocIsAtEnd(f))
                {
                    preprocNext(f, 2);
                }
                else
                {
                    Location err_loc = preprocErrLoc(f);
                    ts__addErr(f->compiler, &err_loc, "unclosed comment");
                }

                size_t comment_length = f->pos - comment_start;

                char *comment = calloc(1, comment_length + 1);
                memcpy(comment, &f->file->text[comment_start], comment_length);
                comment[comment_length] = '\0';

                ts__sbAppend(&p->sb, comment);

                free(comment);
            }
            else
            {
                preprocNext(f, 1);

                if (preprocCanInsert(f))
                {
                    ts__sbAppendChar(&p->sb, c);
                }
            }

            break;
        }
        }
    }

    if (arrLength(f->if_stack) > 0)
    {
        Location err_loc = preprocErrLoc(f);
        ts__addErr(p->compiler, &err_loc, "expected #endif");
    }

    arrFree(f->if_stack);
}

char *ts__preprocessRootFile(
    Preprocessor *p, TsCompiler *compiler, File *file, size_t *out_length)
{
    p->compiler = compiler;

    ts__hashInit(&p->defines, 0);

    ts__sbInit(&p->sb);

    preprocessFile(p, file);
    char *buffer = ts__sbBuild(&p->sb, &compiler->alloc);
    *out_length = strlen(buffer);

    ts__hashDestroy(&p->defines);
    ts__sbDestroy(&p->sb);
    return buffer;
}

////////////////////////////////
//
// Lexer
//
////////////////////////////////

static inline bool lexerIsAtEnd(Lexer *l)
{
    return (l->pos >= l->text_size) || (l->text[l->pos] == '\0');
}

static inline char lexerNext(Lexer *l, size_t count)
{
    char c = l->text[l->pos];
    l->pos += count;
    l->col += (uint32_t)count;
    return c;
}

static inline char lexerPeek(Lexer *l, size_t offset)
{
    return l->text[l->pos + offset];
}

static int lexerIsAtLineEnd(Lexer *l)
{
    if (lexerIsAtEnd(l)) return 0;

    if (lexerPeek(l, 0) == '\n')
    {
        return 1;
    }

    if (lexerPeek(l, 0) == '\r' && lexerPeek(l, 1) == '\n')
    {
        return 2;
    }

    return 0;
}

static inline bool lexerConsume(Lexer *l, char c)
{
    if (lexerPeek(l, 0) != c)
    {
        Location err_loc = {0};
        err_loc.length = 1;
        err_loc.line = l->line;
        err_loc.col = l->col;
        err_loc.pos = (uint32_t)l->pos;
        ts__addErr(l->compiler, &err_loc, "unexpected character");
        lexerNext(l, 1);
        return false;
    }
    lexerNext(l, 1);
    return true;
}

static inline void lexerAddSimpleToken(Lexer *l, TokenKind kind, size_t length)
{
    lexerNext(l, length);
    l->token.kind = kind;
    l->token.loc.length = (uint32_t)length;
}

void ts__lexerLex(Lexer *l, TsCompiler *compiler, char *text, size_t text_size)
{
    memset(l, 0, sizeof(*l));

    l->text = text;
    l->text_size = text_size;
    l->compiler = compiler;
    l->col = 1;
    l->line = 1;

    while (!lexerIsAtEnd(l))
    {
        memset(&l->token, 0, sizeof(Token));
        l->token.loc.path = l->file_path;
        l->token.loc.pos = (uint32_t)l->pos;
        l->token.loc.length = 0;
        l->token.loc.line = l->line;
        l->token.loc.col = l->col;

        switch (lexerPeek(l, 0))
        {
        case '#': {
            lexerNext(l, 1);

            if (!lexerConsume(l, ' ')) break;
            while (lexerPeek(l, 0) == ' ')
                lexerNext(l, 1);

            {
                // Parse line number
                char *hex_start = &l->text[l->pos];
                size_t num_length = 0;

                while (isNumeric(lexerPeek(l, 0)))
                {
                    num_length++;
                    lexerNext(l, 1);
                }

                char *str = NEW_ARRAY(compiler, char, num_length + 1);
                memcpy(str, hex_start, num_length);

                l->line = (size_t)strtol(str, NULL, 10);
                assert(l->line > 0);
            }

            while (lexerPeek(l, 0) == ' ')
                lexerNext(l, 1);

            if (lexerPeek(l, 0) == '"')
            {
                lexerNext(l, 1);

                ts__sbReset(&compiler->sb);
                while (!lexerIsAtEnd(l) && lexerPeek(l, 0) != '"')
                {
                    ts__sbAppendChar(&compiler->sb, lexerPeek(l, 0));
                    lexerNext(l, 1);
                }

                if (!lexerConsume(l, '"')) break;

                l->file_path = ts__sbBuild(&compiler->sb, &compiler->alloc);
            }

            while (!lexerIsAtEnd(l) && !lexerIsAtLineEnd(l))
            {
                lexerNext(l, 1);
            }

            assert(l->line > 0);
            l->line--;

            break;
        }

        case '\t':
        case ' ': {
            lexerNext(l, 1);
            break;
        }

        case '(': {
            lexerAddSimpleToken(l, TOKEN_LPAREN, 1);
            break;
        }
        case ')': {
            lexerAddSimpleToken(l, TOKEN_RPAREN, 1);
            break;
        }
        case '[': {
            if (lexerPeek(l, 1) == '[')
                lexerAddSimpleToken(l, TOKEN_ATTR_LBRACK, 2);
            else
                lexerAddSimpleToken(l, TOKEN_LBRACK, 1);
            break;
        }
        case ']': {
            if (lexerPeek(l, 1) == ']')
                lexerAddSimpleToken(l, TOKEN_ATTR_RBRACK, 2);
            else
                lexerAddSimpleToken(l, TOKEN_RBRACK, 1);
            break;
        }
        case '{': {
            lexerAddSimpleToken(l, TOKEN_LCURLY, 1);
            break;
        }
        case '}': {
            lexerAddSimpleToken(l, TOKEN_RCURLY, 1);
            break;
        }

        case '.': {
            lexerAddSimpleToken(l, TOKEN_PERIOD, 1);
            break;
        }

        case ',': {
            lexerAddSimpleToken(l, TOKEN_COMMA, 1);
            break;
        }

        case '+': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_ADDEQ, 2);
            else if (lexerPeek(l, 1) == '+')
                lexerAddSimpleToken(l, TOKEN_ADDADD, 2);
            else
                lexerAddSimpleToken(l, TOKEN_ADD, 1);
            break;
        }
        case '-': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_SUBEQ, 2);
            else if (lexerPeek(l, 1) == '-')
                lexerAddSimpleToken(l, TOKEN_SUBSUB, 2);
            else
                lexerAddSimpleToken(l, TOKEN_SUB, 1);
            break;
        }
        case '*': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_MULEQ, 2);
            else
                lexerAddSimpleToken(l, TOKEN_MUL, 1);
            break;
        }
        case '/': {
            if (lexerPeek(l, 1) == '/')
            {
                lexerNext(l, 2);

                while (!lexerIsAtLineEnd(l) && !lexerIsAtEnd(l))
                {
                    lexerNext(l, 1);
                }
            }
            else if (lexerPeek(l, 1) == '*')
            {
                // Multiline comment
                lexerNext(l, 2);

                while ((lexerPeek(l, 0) != '*' || lexerPeek(l, 1) != '/') &&
                       !lexerIsAtEnd(l))
                {
                    if (lexerIsAtLineEnd(l))
                    {
                        ++l->line;
                        l->col = 0;
                        lexerNext(l, lexerIsAtLineEnd(l));
                    }
                    else
                    {
                        lexerNext(l, 1);
                    }
                }

                if (!lexerIsAtEnd(l))
                {
                    lexerNext(l, 2);
                }
                else
                {
                    Location err_loc = l->token.loc;
                    err_loc.length = 1;
                    err_loc.path = l->file_path;
                    err_loc.pos = (uint32_t)l->pos;
                    err_loc.line = l->line;
                    err_loc.col = l->col;
                    ts__addErr(l->compiler, &err_loc, "unclosed comment");
                }
            }
            else if (lexerPeek(l, 1) == '=')
            {
                lexerAddSimpleToken(l, TOKEN_DIVEQ, 2);
            }
            else
            {
                lexerAddSimpleToken(l, TOKEN_DIV, 1);
            }
            break;
        }
        case '%': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_MODEQ, 2);
            else
                lexerAddSimpleToken(l, TOKEN_MOD, 1);
            break;
        }

        case '&': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_BITANDEQ, 2);
            else
                lexerAddSimpleToken(l, TOKEN_BITAND, 1);
            break;
        }
        case '|': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_BITOREQ, 2);
            else
                lexerAddSimpleToken(l, TOKEN_BITOR, 1);
            break;
        }
        case '^': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_BITXOREQ, 2);
            else
                lexerAddSimpleToken(l, TOKEN_BITXOR, 1);
            break;
        }
        case '~': {
            lexerAddSimpleToken(l, TOKEN_BITNOT, 1);
            break;
        }

        case ':': {
            if (lexerPeek(l, 1) == ':')
                lexerAddSimpleToken(l, TOKEN_COLON_COLON, 2);
            else
                lexerAddSimpleToken(l, TOKEN_COLON, 1);
            break;
        }
        case ';': {
            lexerAddSimpleToken(l, TOKEN_SEMICOLON, 1);
            break;
        }

        case '!': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_NOTEQ, 2);
            else
                lexerAddSimpleToken(l, TOKEN_NOT, 1);
            break;
        }

        case '=': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_EQUAL, 2);
            else
                lexerAddSimpleToken(l, TOKEN_ASSIGN, 1);
            break;
        }

        case '>': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_GREATEREQ, 2);
            else
                lexerAddSimpleToken(l, TOKEN_GREATER, 1);
            break;
        }
        case '<': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_LESSEQ, 2);
            else
                lexerAddSimpleToken(l, TOKEN_LESS, 1);
            break;
        }

        case '"': {
            lexerNext(l, 1);

            ts__sbReset(&compiler->sb);

            while (!lexerIsAtEnd(l) && lexerPeek(l, 0) != '"')
            {
                if (lexerPeek(l, 0) == '\\')
                {
                    lexerNext(l, 1);
                    switch (lexerPeek(l, 0))
                    {
                    case 'a':
                        ts__sbAppendChar(&compiler->sb, '\a');
                        lexerNext(l, 1);
                        break;
                    case 'b':
                        ts__sbAppendChar(&compiler->sb, '\b');
                        lexerNext(l, 1);
                        break;
                    case 'f':
                        ts__sbAppendChar(&compiler->sb, '\f');
                        lexerNext(l, 1);
                        break;
                    case 'n':
                        ts__sbAppendChar(&compiler->sb, '\n');
                        lexerNext(l, 1);
                        break;
                    case 'r':
                        ts__sbAppendChar(&compiler->sb, '\r');
                        lexerNext(l, 1);
                        break;
                    case 't':
                        ts__sbAppendChar(&compiler->sb, '\t');
                        lexerNext(l, 1);
                        break;
                    case 'v':
                        ts__sbAppendChar(&compiler->sb, '\v');
                        lexerNext(l, 1);
                        break;
                    case '0':
                        ts__sbAppendChar(&compiler->sb, '\0');
                        lexerNext(l, 1);
                        break;
                    case '?':
                        ts__sbAppendChar(&compiler->sb, '\?');
                        lexerNext(l, 1);
                        break;
                    case '\'':
                        ts__sbAppendChar(&compiler->sb, '\'');
                        lexerNext(l, 1);
                        break;
                    case '\"':
                        ts__sbAppendChar(&compiler->sb, '\"');
                        lexerNext(l, 1);
                        break;
                    case '\\':
                        ts__sbAppendChar(&compiler->sb, '\\');
                        lexerNext(l, 1);
                        break;
                    default:
                        ts__sbAppendChar(&compiler->sb, '\\');
                        ts__sbAppendChar(&compiler->sb, lexerPeek(l, 0));
                        lexerNext(l, 1);
                        break;
                    }
                }
                else
                {
                    ts__sbAppendChar(&compiler->sb, lexerPeek(l, 0));
                    lexerNext(l, 1);
                }
            }

            if (lexerIsAtEnd(l) || lexerPeek(l, 0) != '"')
            {
                Location err_loc = l->token.loc;
                err_loc.length = 1;
                err_loc.path = l->file_path;
                err_loc.pos = (uint32_t)l->pos;
                err_loc.line = l->line;
                err_loc.col = l->col;
                ts__addErr(l->compiler, &err_loc, "unclosed string");
                break;
            }

            lexerNext(l, 1);

            l->token.kind = TOKEN_STRING_LIT;
            l->token.str = ts__sbBuild(&compiler->sb, &compiler->alloc);
            l->token.loc.length = (uint32_t)(l->pos - l->token.loc.pos);
            break;
        }

        default: {
            if (lexerIsAtLineEnd(l))
            {
                l->col = 0;
                l->line++;
                lexerNext(l, lexerIsAtLineEnd(l));
            }
            else if (isLetter(lexerPeek(l, 0)))
            {
                while (!lexerIsAtEnd(l) && isAlphanum(lexerPeek(l, 0)))
                {
                    lexerNext(l, 1);
                }

                size_t ident_length = l->pos - l->token.loc.pos;
                char *ident_start = &l->text[l->token.loc.pos];

                char *ident = NEW_ARRAY(compiler, char, ident_length + 1);
                memcpy(ident, ident_start, ident_length);
                ident[ident_length] = '\0';

                l->token.loc.length = (uint32_t)ident_length;
                l->token.str = ident;

                void *result = NULL;
                bool found_keyword =
                    ts__hashGet(&compiler->keyword_table, ident, &result);
                if (found_keyword)
                {
                    l->token.kind = (TokenKind)result;
                }
                else
                {
                    l->token.kind = TOKEN_IDENT;

                    char *type_prefixes[3] = {"float", "uint", "int"};
                    TokenKind type_kinds[3] = {TOKEN_FLOAT, TOKEN_UINT, TOKEN_INT};
                    char *type_str = l->token.str;
                    size_t type_str_len = strlen(type_str);

                    for (uint32_t i = 0; i < 3; ++i)
                    {
                        size_t prefix_len = strlen(type_prefixes[i]);
                        if (strncmp(type_str, type_prefixes[i], prefix_len) == 0)
                        {
                            if ((prefix_len + 1) == type_str_len)
                            {
                                uint8_t dim = (uint8_t)(type_str[prefix_len] - '0');
                                if (dim > 1 && dim <= 4)
                                {
                                    l->token.kind = TOKEN_VECTOR_TYPE;
                                    l->token.vector_type.elem_type = type_kinds[i];
                                    l->token.vector_type.dim = dim;
                                }
                            }

                            if ((prefix_len + 3) == type_str_len &&
                                type_str[prefix_len + 1] == 'x')
                            {
                                // Matrix type
                                uint8_t dim1 = (uint8_t)(type_str[prefix_len] - '0');
                                uint8_t dim2 = (uint8_t)(type_str[prefix_len + 2] - '0');
                                if (dim1 > 1 && dim1 <= 4 && dim2 > 1 && dim2 <= 4)
                                {
                                    l->token.kind = TOKEN_MATRIX_TYPE;
                                    l->token.matrix_type.elem_type = type_kinds[i];
                                    l->token.matrix_type.dim1 = dim1;
                                    l->token.matrix_type.dim2 = dim2;
                                }
                            }

                            break;
                        }
                    }
                }
            }
            else if (isNumeric(lexerPeek(l, 0)))
            {
                char *dot_ptr = NULL;

                if (lexerPeek(l, 0) == '0' && lexerPeek(l, 1) == 'x')
                {
                    // Hexadecimal
                    l->token.kind = TOKEN_INT_LIT;
                    lexerNext(l, 2);
                    l->token.loc.length += 2;

                    char *hex_start = &l->text[l->pos];

                    while (isNumeric(lexerPeek(l, 0)) ||
                           (lexerPeek(l, 0) >= 'a' && lexerPeek(l, 0) <= 'f') ||
                           (lexerPeek(l, 0) >= 'A' && lexerPeek(l, 0) <= 'F'))
                    {
                        l->token.loc.length++;
                        l->col++;
                        lexerNext(l, 1);
                    }

                    char *str = NEW_ARRAY(compiler, char, l->token.loc.length - 2 + 1);
                    memcpy(str, hex_start, l->token.loc.length - 2);

                    l->token.int_ = strtol(str, NULL, 16);
                }
                else
                {
                    char *number_start = &l->text[l->pos];

                    while (isNumeric(lexerPeek(l, 0)) || lexerPeek(l, 0) == '.')
                    {
                        if (lexerPeek(l, 0) == '.')
                        {
                            if (!isNumeric(lexerPeek(l, 1))) break;
                            assert(!dot_ptr);
                            dot_ptr = &l->text[l->pos];
                        }

                        l->token.loc.length++;
                        l->col++;
                        lexerNext(l, 1);
                    }

                    char *str = NEW_ARRAY(compiler, char, l->token.loc.length + 1);
                    memcpy(str, number_start, l->token.loc.length);

                    if (dot_ptr)
                    {
                        l->token.kind = TOKEN_FLOAT_LIT;
                        l->token.double_ = strtod(str, NULL);
                    }
                    else
                    {
                        l->token.kind = TOKEN_INT_LIT;
                        l->token.int_ = strtol(str, NULL, 10);
                    }
                }
            }
            else
            {
                Location err_loc = l->token.loc;
                err_loc.length = 1;
                err_loc.path = l->file_path;
                err_loc.pos = (uint32_t)l->pos;
                err_loc.line = l->line;
                err_loc.col = l->col;
                ts__addErr(l->compiler, &err_loc, "unknown token");
                lexerNext(l, 1);
            }

            break;
        }
        }

        if (l->token.loc.length > 0)
        {
            arrPush(l->tokens, l->token);
        }
    }

    if (arrLength(l->tokens) == 0)
    {
        Location err_loc = {0};
        err_loc.path = l->file_path;
        ts__addErr(l->compiler, &err_loc, "no tokens found for file");
    }
}

////////////////////////////////
//
// Parser
//
////////////////////////////////

static AstExpr *parseExpr(Parser *p);

static inline bool parserIsAtEnd(Parser *p)
{
    return p->pos >= arrLength(p->tokens);
}

static inline Token *parserPeek(Parser *p, size_t offset)
{
    if (p->pos + offset >= arrLength(p->tokens))
    {
        return &p->tokens[p->token_count - 1];
    }
    return &p->tokens[p->pos + offset];
}

static inline Token *parserNext(Parser *p, size_t count)
{
    if (parserIsAtEnd(p)) return NULL;

    Token *tok = &p->tokens[p->pos];
    p->pos += count;
    return tok;
}

static inline Token *parserConsume(Parser *p, TokenKind kind)
{
    Token *tok = parserPeek(p, 0);
    if (tok->kind != kind)
    {
        ts__addErr(p->compiler, &tok->loc, "unexpected token");
        return NULL;
    }
    parserNext(p, 1);
    return tok;
}

static inline Location parserBeginLoc(Parser *p)
{
    return parserPeek(p, 0)->loc;
}

static inline void parserEndLoc(Parser *p, Location *loc)
{
    loc->length = (parserPeek(p, 0)->loc.pos + parserPeek(p, 0)->loc.length) - loc->pos;
}

static AstExpr *parseIdentExpr(Parser *p)
{
    TsCompiler *compiler = p->compiler;

    Location loc = parserBeginLoc(p);

    switch (parserPeek(p, 0)->kind)
    {
    case TOKEN_IDENT: {
        AstExpr *expr = NEW(compiler, AstExpr);
        expr->kind = EXPR_IDENT;
        expr->ident.name = parserNext(p, 1)->str;

        parserEndLoc(p, &loc);
        expr->loc = loc;

        return expr;
    }

    default: {
        ts__addErr(
            p->compiler, &parserNext(p, 1)->loc, "expecting identifier expression");
        break;
    }
    }

    return NULL;
}

static AstExpr *parsePrimaryExpr(Parser *p)
{
    TsCompiler *compiler = p->compiler;

    Location loc = parserBeginLoc(p);

    switch (parserPeek(p, 0)->kind)
    {
    case TOKEN_IDENT: {
        return parseIdentExpr(p);
    }

    case TOKEN_INT_LIT:
    case TOKEN_FLOAT_LIT:
    case TOKEN_STRING_LIT:
    case TOKEN_BOOL:
    case TOKEN_VOID:
    case TOKEN_TRUE:
    case TOKEN_FALSE:
    case TOKEN_INT:
    case TOKEN_UINT:
    case TOKEN_FLOAT:
    case TOKEN_VECTOR_TYPE:
    case TOKEN_MATRIX_TYPE: {
        AstExpr *expr = NEW(compiler, AstExpr);
        expr->kind = EXPR_PRIMARY;
        expr->primary.token = parserNext(p, 1);

        parserEndLoc(p, &loc);
        expr->loc = loc;

        return expr;
    }

    case TOKEN_LPAREN: {
        parserNext(p, 1);
        AstExpr *expr = parseExpr(p);
        if (!parserConsume(p, TOKEN_RPAREN)) return NULL;
        return expr;
    }

    default: {
        ts__addErr(p->compiler, &parserNext(p, 1)->loc, "expecting primary expression");
        break;
    }
    }

    return NULL;
}

static AstExpr *parseAccessFuncCall(Parser *p)
{
    TsCompiler *compiler = p->compiler;
    Location loc = parserBeginLoc(p);

    AstExpr *expr = NULL;

    bool is_builtin = true;
    IRBuiltinInstKind builtin_kind = {0};

    // Check if it's a builtin function
    switch (parserPeek(p, 0)->kind)
    {
    case TOKEN_BUILTIN_DOT: builtin_kind = IR_BUILTIN_DOT; break;
    case TOKEN_BUILTIN_CROSS: builtin_kind = IR_BUILTIN_CROSS; break;
    case TOKEN_BUILTIN_LENGTH: builtin_kind = IR_BUILTIN_LENGTH; break;
    case TOKEN_BUILTIN_NORMALIZE: builtin_kind = IR_BUILTIN_NORMALIZE; break;
    case TOKEN_BUILTIN_MUL: builtin_kind = IR_BUILTIN_MUL; break;
    case TOKEN_BUILTIN_DISTANCE: builtin_kind = IR_BUILTIN_DISTANCE; break;
    case TOKEN_BUILTIN_DEGREES: builtin_kind = IR_BUILTIN_DEGREES; break;
    case TOKEN_BUILTIN_RADIANS: builtin_kind = IR_BUILTIN_RADIANS; break;

    case TOKEN_BUILTIN_SIN: builtin_kind = IR_BUILTIN_SIN; break;
    case TOKEN_BUILTIN_COS: builtin_kind = IR_BUILTIN_COS; break;
    case TOKEN_BUILTIN_TAN: builtin_kind = IR_BUILTIN_TAN; break;
    case TOKEN_BUILTIN_ASIN: builtin_kind = IR_BUILTIN_ASIN; break;
    case TOKEN_BUILTIN_ACOS: builtin_kind = IR_BUILTIN_ACOS; break;
    case TOKEN_BUILTIN_ATAN: builtin_kind = IR_BUILTIN_ATAN; break;
    case TOKEN_BUILTIN_SINH: builtin_kind = IR_BUILTIN_SINH; break;
    case TOKEN_BUILTIN_COSH: builtin_kind = IR_BUILTIN_COSH; break;
    case TOKEN_BUILTIN_TANH: builtin_kind = IR_BUILTIN_TANH; break;
    case TOKEN_BUILTIN_ATAN2: builtin_kind = IR_BUILTIN_ATAN2; break;

    case TOKEN_BUILTIN_SQRT: builtin_kind = IR_BUILTIN_SQRT; break;
    case TOKEN_BUILTIN_RSQRT: builtin_kind = IR_BUILTIN_RSQRT; break;

    case TOKEN_BUILTIN_REFLECT: builtin_kind = IR_BUILTIN_REFLECT; break;
    case TOKEN_BUILTIN_REFRACT: builtin_kind = IR_BUILTIN_REFRACT; break;

    case TOKEN_BUILTIN_POW: builtin_kind = IR_BUILTIN_POW; break;
    case TOKEN_BUILTIN_EXP: builtin_kind = IR_BUILTIN_EXP; break;
    case TOKEN_BUILTIN_EXP2: builtin_kind = IR_BUILTIN_EXP2; break;
    case TOKEN_BUILTIN_LOG: builtin_kind = IR_BUILTIN_LOG; break;
    case TOKEN_BUILTIN_LOG2: builtin_kind = IR_BUILTIN_LOG2; break;

    case TOKEN_BUILTIN_ABS: builtin_kind = IR_BUILTIN_ABS; break;
    case TOKEN_BUILTIN_MIN: builtin_kind = IR_BUILTIN_MIN; break;
    case TOKEN_BUILTIN_MAX: builtin_kind = IR_BUILTIN_MAX; break;
    case TOKEN_BUILTIN_LERP: builtin_kind = IR_BUILTIN_LERP; break;
    case TOKEN_BUILTIN_CLAMP: builtin_kind = IR_BUILTIN_CLAMP; break;
    case TOKEN_BUILTIN_STEP: builtin_kind = IR_BUILTIN_STEP; break;
    case TOKEN_BUILTIN_SMOOTHSTEP: builtin_kind = IR_BUILTIN_SMOOTHSTEP; break;

    case TOKEN_BUILTIN_TRANSPOSE: builtin_kind = IR_BUILTIN_TRANSPOSE; break;
    case TOKEN_BUILTIN_DETERMINANT: builtin_kind = IR_BUILTIN_DETERMINANT; break;

    case TOKEN_BUILTIN_DDX: builtin_kind = IR_BUILTIN_DDX; break;
    case TOKEN_BUILTIN_DDY: builtin_kind = IR_BUILTIN_DDY; break;

    default: is_builtin = false; break;
    }

    if (is_builtin)
    {
        expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_BUILTIN_CALL;
        expr->builtin_call.kind = builtin_kind;

        parserNext(p, 1); // skip name token

        if (!parserConsume(p, TOKEN_LPAREN)) return NULL;

        while (!parserIsAtEnd(p) && parserPeek(p, 0)->kind != TOKEN_RPAREN)
        {
            AstExpr *param = parseExpr(p);
            if (!param) return NULL;
            arrPush(expr->builtin_call.params, param);

            if (parserPeek(p, 0)->kind != TOKEN_RPAREN)
            {
                if (!parserConsume(p, TOKEN_COMMA)) return NULL;
            }
        }

        if (!parserConsume(p, TOKEN_RPAREN)) return NULL;

        parserEndLoc(p, &loc);
        expr->loc = loc;
    }
    else
    {
        expr = parsePrimaryExpr(p);
        if (!expr) return NULL;
    }

    // Not a builtin function, must be an access or function call

    while (!parserIsAtEnd(p) && (parserPeek(p, 0)->kind == TOKEN_LPAREN ||
                                 parserPeek(p, 0)->kind == TOKEN_PERIOD))
    {
        if (parserPeek(p, 0)->kind == TOKEN_LPAREN)
        {
            // Function call expression
            parserNext(p, 1);

            AstExpr *func_call = NEW(p->compiler, AstExpr);
            func_call->kind = EXPR_FUNC_CALL;
            func_call->func_call.func_expr = expr;

            while (!parserIsAtEnd(p) && parserPeek(p, 0)->kind != TOKEN_RPAREN)
            {
                AstExpr *param = parseExpr(p);
                if (!param) return NULL;
                arrPush(func_call->func_call.params, param);

                if (parserPeek(p, 0)->kind != TOKEN_RPAREN)
                {
                    if (!parserConsume(p, TOKEN_COMMA)) return NULL;
                }
            }

            if (!parserConsume(p, TOKEN_RPAREN)) return NULL;

            parserEndLoc(p, &loc);
            func_call->loc = loc;

            expr = func_call;
        }
        else if (parserPeek(p, 0)->kind == TOKEN_PERIOD)
        {
            // Access expression

            AstExpr *base_expr = expr;
            expr = NEW(compiler, AstExpr);
            expr->kind = EXPR_ACCESS;
            expr->access.base = base_expr;

            while (parserPeek(p, 0)->kind == TOKEN_PERIOD)
            {
                parserNext(p, 1);

                AstExpr *ident = parseIdentExpr(p);
                if (!ident) return NULL;

                arrPush(expr->access.chain, ident);
            }

            parserEndLoc(p, &loc);
            expr->loc = loc;
        }
        else
        {
            assert(0);
        }
    }

    return expr;
}

static AstExpr *parsePostfixedUnaryExpr(Parser *p)
{
    AstExpr *expr = parseAccessFuncCall(p);

    while (parserPeek(p, 0)->kind == TOKEN_ADDADD ||
           parserPeek(p, 0)->kind == TOKEN_SUBSUB)
    {
        Token *op_tok = parserNext(p, 1);

        AstUnaryOp op = {0};

        switch (op_tok->kind)
        {
        case TOKEN_ADDADD: op = UNOP_POST_INC; break;
        case TOKEN_SUBSUB: op = UNOP_POST_DEC; break;
        default: assert(0); break;
        }

        AstExpr *new_expr = NEW(p->compiler, AstExpr);
        new_expr->kind = EXPR_UNARY;
        new_expr->unary.right = expr;
        new_expr->unary.op = op;

        expr = new_expr;
    }

    return expr;
}

static AstExpr *parsePrefixedUnaryExpr(Parser *p)
{
    if (parserPeek(p, 0)->kind == TOKEN_SUB || parserPeek(p, 0)->kind == TOKEN_NOT ||
        parserPeek(p, 0)->kind == TOKEN_ADDADD || parserPeek(p, 0)->kind == TOKEN_SUBSUB)
    {
        Token *op_tok = parserNext(p, 1);

        AstUnaryOp op = {0};

        switch (op_tok->kind)
        {
        case TOKEN_SUB: op = UNOP_NEG; break;
        case TOKEN_NOT: op = UNOP_NOT; break;
        case TOKEN_ADDADD: op = UNOP_PRE_INC; break;
        case TOKEN_SUBSUB: op = UNOP_PRE_DEC; break;
        default: assert(0); break;
        }

        AstExpr *right = parsePrefixedUnaryExpr(p);

        AstExpr *expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_UNARY;
        expr->unary.right = right;
        expr->unary.op = op;

        return expr;
    }

    return parsePostfixedUnaryExpr(p);
}

static AstExpr *parseMuliplication(Parser *p)
{
    AstExpr *expr = parsePrefixedUnaryExpr(p);
    if (!expr) return NULL;

    while (!parserIsAtEnd(p) &&
           (parserPeek(p, 0)->kind == TOKEN_MUL || parserPeek(p, 0)->kind == TOKEN_DIV))
    {
        Token *op_tok = parserNext(p, 1);

        AstBinaryOp op = {0};

        switch (op_tok->kind)
        {
        case TOKEN_MUL: op = BINOP_MUL; break;
        case TOKEN_DIV: op = BINOP_DIV; break;
        default: assert(0); break;
        }

        AstExpr *left = expr;
        AstExpr *right = parsePrefixedUnaryExpr(p);
        if (!right) return NULL;

        expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_BINARY;
        expr->binary.left = left;
        expr->binary.right = right;
        expr->binary.op = op;
    }

    return expr;
}

static AstExpr *parseAddition(Parser *p)
{
    AstExpr *expr = parseMuliplication(p);
    if (!expr) return NULL;

    while (!parserIsAtEnd(p) &&
           (parserPeek(p, 0)->kind == TOKEN_ADD || parserPeek(p, 0)->kind == TOKEN_SUB))
    {
        Token *op_tok = parserNext(p, 1);

        AstBinaryOp op = {0};

        switch (op_tok->kind)
        {
        case TOKEN_ADD: op = BINOP_ADD; break;
        case TOKEN_SUB: op = BINOP_SUB; break;
        default: assert(0); break;
        }

        AstExpr *left = expr;
        AstExpr *right = parseMuliplication(p);
        if (!right) return NULL;

        expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_BINARY;
        expr->binary.left = left;
        expr->binary.right = right;
        expr->binary.op = op;
    }

    return expr;
}

static AstExpr *parseComparison(Parser *p)
{
    AstExpr *expr = parseAddition(p);
    if (!expr) return NULL;

    while (!parserIsAtEnd(p) && (parserPeek(p, 0)->kind == TOKEN_EQUAL ||
                                 parserPeek(p, 0)->kind == TOKEN_NOTEQ ||
                                 parserPeek(p, 0)->kind == TOKEN_LESS ||
                                 parserPeek(p, 0)->kind == TOKEN_LESSEQ ||
                                 parserPeek(p, 0)->kind == TOKEN_GREATER ||
                                 parserPeek(p, 0)->kind == TOKEN_GREATEREQ))
    {
        Token *op_tok = parserNext(p, 1);

        AstBinaryOp op = {0};

        switch (op_tok->kind)
        {
        case TOKEN_EQUAL: op = BINOP_EQ; break;
        case TOKEN_NOTEQ: op = BINOP_NOTEQ; break;
        case TOKEN_LESS: op = BINOP_LESS; break;
        case TOKEN_LESSEQ: op = BINOP_LESSEQ; break;
        case TOKEN_GREATER: op = BINOP_GREATER; break;
        case TOKEN_GREATEREQ: op = BINOP_GREATEREQ; break;
        default: assert(0); break;
        }

        AstExpr *left = expr;
        AstExpr *right = parseAddition(p);
        if (!right) return NULL;

        expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_BINARY;
        expr->binary.left = left;
        expr->binary.right = right;
        expr->binary.op = op;
    }

    return expr;
}

static AstExpr *parseExpr(Parser *p)
{
    return parseComparison(p);
}

static AstStmt *parseStmt(Parser *p)
{
    TsCompiler *compiler = p->compiler;

    switch (parserPeek(p, 0)->kind)
    {
    case TOKEN_RETURN: {
        parserNext(p, 1);

        AstStmt *stmt = NEW(compiler, AstStmt);
        stmt->kind = STMT_RETURN;

        if (parserPeek(p, 0)->kind != TOKEN_SEMICOLON)
        {
            AstExpr *return_expr = parseExpr(p);
            if (!return_expr) return NULL;

            stmt->return_.value = return_expr;
        }

        if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

        return stmt;
    }

    case TOKEN_DISCARD: {
        parserNext(p, 1);

        AstStmt *stmt = NEW(compiler, AstStmt);
        stmt->kind = STMT_DISCARD;

        if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

        return stmt;
    }

    case TOKEN_CONTINUE: {
        parserNext(p, 1);

        AstStmt *stmt = NEW(compiler, AstStmt);
        stmt->kind = STMT_CONTINUE;

        if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

        return stmt;
    }

    case TOKEN_BREAK: {
        parserNext(p, 1);

        AstStmt *stmt = NEW(compiler, AstStmt);
        stmt->kind = STMT_BREAK;

        if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

        return stmt;
    }

    case TOKEN_IF: {
        parserNext(p, 1);
        AstStmt *stmt = NEW(compiler, AstStmt);
        stmt->kind = STMT_IF;

        if (!parserConsume(p, TOKEN_LPAREN)) return NULL;

        stmt->if_.cond = parseExpr(p);
        if (!stmt->if_.cond) return NULL;

        if (!parserConsume(p, TOKEN_RPAREN)) return NULL;

        stmt->if_.if_stmt = parseStmt(p);
        if (!stmt->if_.if_stmt) return NULL;

        if (parserPeek(p, 0)->kind == TOKEN_ELSE)
        {
            parserNext(p, 1);

            stmt->if_.else_stmt = parseStmt(p);
            if (!stmt->if_.else_stmt) return NULL;
        }

        return stmt;
    }

    case TOKEN_WHILE: {
        parserNext(p, 1);
        AstStmt *stmt = NEW(compiler, AstStmt);
        stmt->kind = STMT_WHILE;

        if (!parserConsume(p, TOKEN_LPAREN)) return NULL;

        stmt->while_.cond = parseExpr(p);
        if (!stmt->while_.cond) return NULL;

        if (!parserConsume(p, TOKEN_RPAREN)) return NULL;

        stmt->while_.stmt = parseStmt(p);
        if (!stmt->while_.stmt) return NULL;

        return stmt;
    }

    case TOKEN_DO: {
        parserNext(p, 1);
        AstStmt *stmt = NEW(compiler, AstStmt);
        stmt->kind = STMT_DO_WHILE;

        stmt->do_while.stmt = parseStmt(p);
        if (!stmt->do_while.stmt) return NULL;

        if (!parserConsume(p, TOKEN_WHILE)) return NULL;

        if (!parserConsume(p, TOKEN_LPAREN)) return NULL;

        stmt->do_while.cond = parseExpr(p);
        if (!stmt->do_while.cond) return NULL;

        if (!parserConsume(p, TOKEN_RPAREN)) return NULL;

        if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

        return stmt;
    }

    case TOKEN_FOR: {
        parserNext(p, 1);
        AstStmt *stmt = NEW(compiler, AstStmt);
        stmt->kind = STMT_FOR;

        if (!parserConsume(p, TOKEN_LPAREN)) return NULL;

        stmt->for_.init = NULL;
        if (parserPeek(p, 0)->kind != TOKEN_SEMICOLON)
        {
            stmt->for_.init = parseStmt(p);
            if (!stmt->for_.init) return NULL;
        }
        else
        {
            parserNext(p, 1);
        }

        stmt->for_.cond = NULL;
        if (parserPeek(p, 0)->kind != TOKEN_SEMICOLON)
        {
            stmt->for_.cond = parseExpr(p);
            if (!stmt->for_.cond) return NULL;
        }

        if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

        stmt->for_.inc = NULL;
        if (parserPeek(p, 0)->kind != TOKEN_RPAREN)
        {
            stmt->for_.inc = parseExpr(p);
            if (!stmt->for_.inc) return NULL;
        }

        if (!parserConsume(p, TOKEN_RPAREN)) return NULL;

        stmt->for_.stmt = parseStmt(p);
        if (!stmt->for_.stmt) return NULL;

        return stmt;
    }

    case TOKEN_LCURLY: {
        parserNext(p, 1);

        AstStmt *stmt = NEW(compiler, AstStmt);
        stmt->kind = STMT_BLOCK;

        while (parserPeek(p, 0)->kind != TOKEN_RCURLY)
        {
            AstStmt *sub_stmt = parseStmt(p);
            if (sub_stmt)
            {
                arrPush(stmt->block.stmts, sub_stmt);
            }
        }

        if (!parserConsume(p, TOKEN_RCURLY)) return NULL;

        return stmt;
    }

    default: {
        AstExpr *expr = parseExpr(p);
        if (!expr) return NULL;

        if (parserPeek(p, 0)->kind == TOKEN_SEMICOLON)
        {
            // Expression statement
            parserNext(p, 1);
            AstStmt *stmt = NEW(compiler, AstStmt);
            stmt->kind = STMT_EXPR;
            stmt->expr = expr;

            return stmt;
        }
        else if (parserPeek(p, 0)->kind == TOKEN_ASSIGN)
        {
            // Variable assignment
            parserNext(p, 1);

            AstExpr *value_expr = parseExpr(p);
            if (!value_expr) return NULL;

            AstStmt *stmt = NEW(compiler, AstStmt);
            stmt->kind = STMT_VAR_ASSIGN;
            stmt->var_assign.assigned_expr = expr;
            stmt->var_assign.value_expr = value_expr;

            if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

            return stmt;
        }
        else if (parserPeek(p, 0)->kind == TOKEN_IDENT)
        {
            // Variable declaration
            Token *name_tok = parserNext(p, 1);

            AstDecl *decl = NEW(compiler, AstDecl);
            decl->kind = DECL_VAR;
            decl->name = name_tok->str;
            decl->var.type_expr = expr;

            if (parserPeek(p, 0)->kind == TOKEN_ASSIGN)
            {
                parserNext(p, 1);

                AstExpr *value_expr = parseExpr(p);
                if (!value_expr) return NULL;

                decl->var.value_expr = value_expr;
            }

            AstStmt *stmt = NEW(compiler, AstStmt);
            stmt->kind = STMT_DECL;
            stmt->decl = decl;

            if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

            return stmt;
        }
        else
        {
            ts__addErr(
                compiler,
                &parserNext(p, 1)->loc,
                "unexpected token, expecting ';' or identifier");
        }

        break;
    }
    }

    return NULL;
}

static AstDecl *parseTopLevel(Parser *p)
{
    TsCompiler *compiler = p->compiler;

    /*array*/ AstAttribute *attributes = NULL;

    if (parserPeek(p, 0)->kind == TOKEN_ATTR_LBRACK)
    {
        parserNext(p, 1);

        Token *namespace = NULL;
        Token *attr_name = NULL;

        namespace = parserConsume(p, TOKEN_IDENT);
        if (!namespace) return NULL;

        if (parserPeek(p, 0)->kind == TOKEN_COLON_COLON)
        {
            parserNext(p, 1);
            attr_name = parserConsume(p, TOKEN_IDENT);
            if (!attr_name) return NULL;
        }
        else
        {
            attr_name = namespace;
            namespace = NULL;
        }

        assert(attr_name);

        ts__sbReset(&compiler->sb);
        if (namespace)
        {
            ts__sbAppend(&compiler->sb, namespace->str);
            ts__sbAppend(&compiler->sb, "::");
        }

        ts__sbAppend(&compiler->sb, attr_name->str);

        AstAttribute attr = {0};
        attr.name = ts__sbBuild(&compiler->sb, &compiler->alloc);

        if (parserPeek(p, 0)->kind == TOKEN_LPAREN)
        {
            parserNext(p, 1);

            while (parserPeek(p, 0)->kind != TOKEN_RPAREN)
            {
                AstExpr *value = parseExpr(p);
                if (!value) return NULL;

                arrPush(attr.values, value);

                if (parserPeek(p, 0)->kind != TOKEN_RPAREN)
                {
                    if (!parserConsume(p, TOKEN_COMMA)) return NULL;
                }
            }

            if (!parserConsume(p, TOKEN_RPAREN)) return NULL;
        }

        arrPush(attributes, attr);

        if (!parserConsume(p, TOKEN_ATTR_RBRACK)) return NULL;
    }

    switch (parserPeek(p, 0)->kind)
    {
    case TOKEN_CONST: {
        parserNext(p, 1);
        AstDecl *decl = NEW(compiler, AstDecl);
        decl->kind = DECL_CONST;
        decl->attributes = attributes;

        AstExpr *type_expr = parsePrefixedUnaryExpr(p);
        if (!type_expr) return NULL;

        Token *name_tok = parserConsume(p, TOKEN_IDENT);
        if (!name_tok) return NULL;

        decl->name = name_tok->str;
        decl->constant.type_expr = type_expr;

        if (!parserConsume(p, TOKEN_ASSIGN)) return NULL;

        AstExpr *value_expr = parseExpr(p);
        if (!value_expr) return NULL;
        decl->constant.value_expr = value_expr;

        if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

        return decl;
    }

    case TOKEN_STRUCT: {
        parserNext(p, 1);
        AstDecl *decl = NEW(compiler, AstDecl);
        decl->kind = DECL_STRUCT;
        decl->attributes = attributes;

        Token *name_tok = parserConsume(p, TOKEN_IDENT);
        if (!name_tok) return NULL;
        decl->name = name_tok->str;

        if (!parserConsume(p, TOKEN_LCURLY)) return NULL;

        while (parserPeek(p, 0)->kind != TOKEN_RCURLY)
        {
            AstExpr *type_expr = parsePrefixedUnaryExpr(p);
            if (!type_expr) return NULL;

            Token *name_tok = parserConsume(p, TOKEN_IDENT);
            if (!name_tok) return NULL;

            AstDecl *field_decl = NEW(compiler, AstDecl);
            field_decl->kind = DECL_STRUCT_FIELD;
            field_decl->name = name_tok->str;
            field_decl->struct_field.type_expr = type_expr;

            if (parserPeek(p, 0)->kind == TOKEN_COLON)
            {
                parserNext(p, 1);
                Token *semantic_tok = parserConsume(p, TOKEN_IDENT);
                if (!semantic_tok) return NULL;
                field_decl->struct_field.semantic = semantic_tok->str;
            }

            if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

            arrPush(decl->struct_.fields, field_decl);
        }

        if (!parserConsume(p, TOKEN_RCURLY)) return NULL;

        if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

        return decl;
    }

    case TOKEN_CONSTANT_BUFFER:
    case TOKEN_TEXTURE_1D:
    case TOKEN_TEXTURE_2D:
    case TOKEN_TEXTURE_3D:
    case TOKEN_TEXTURE_CUBE:
    case TOKEN_SAMPLER_STATE: {
        AstExpr *type_expr = NULL;
        AstVarKind var_kind = {0};

        switch (parserPeek(p, 0)->kind)
        {
        case TOKEN_CONSTANT_BUFFER: {
            parserNext(p, 1);

            var_kind = VAR_UNIFORM;

            if (!parserConsume(p, TOKEN_LESS)) return NULL;

            type_expr = parsePrefixedUnaryExpr(p);
            if (!type_expr) return NULL;

            if (!parserConsume(p, TOKEN_GREATER)) return NULL;

            break;
        }

        case TOKEN_TEXTURE_1D:
        case TOKEN_TEXTURE_2D:
        case TOKEN_TEXTURE_3D:
        case TOKEN_TEXTURE_CUBE: {
            Token *texture_kind_tok = parserNext(p, 1);

            var_kind = VAR_UNIFORM;

            type_expr = NEW(compiler, AstExpr);
            type_expr->loc = parserPeek(p, 0)->loc;

            switch (texture_kind_tok->kind)
            {
            case TOKEN_TEXTURE_1D:
                type_expr->kind = EXPR_TEXTURE_TYPE;
                type_expr->texture.dim = SpvDim1D;
                break;
            case TOKEN_TEXTURE_2D:
                type_expr->kind = EXPR_TEXTURE_TYPE;
                type_expr->texture.dim = SpvDim2D;
                break;
            case TOKEN_TEXTURE_3D:
                type_expr->kind = EXPR_TEXTURE_TYPE;
                type_expr->texture.dim = SpvDim3D;
                break;
            case TOKEN_TEXTURE_CUBE:
                type_expr->kind = EXPR_TEXTURE_TYPE;
                type_expr->texture.dim = SpvDimCube;
                break;

            default: assert(0); break;
            }

            if (!parserConsume(p, TOKEN_LESS)) return NULL;

            type_expr->texture.sampled_type_expr = parsePrefixedUnaryExpr(p);
            if (!type_expr->texture.sampled_type_expr) return NULL;

            if (!parserConsume(p, TOKEN_GREATER)) return NULL;

            break;
        }

        case TOKEN_SAMPLER_STATE: {
            type_expr = NEW(compiler, AstExpr);
            type_expr->kind = EXPR_SAMPLER_TYPE;
            type_expr->loc = parserPeek(p, 0)->loc;

            var_kind = VAR_UNIFORM;

            parserNext(p, 1);

            break;
        }

        default: assert(0); break;
        }

        Token *name_tok = parserConsume(p, TOKEN_IDENT);
        if (!name_tok) return NULL;

        if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

        AstDecl *decl = NEW(compiler, AstDecl);
        decl->kind = DECL_VAR;
        decl->name = name_tok->str;
        decl->var.type_expr = type_expr;
        decl->var.kind = var_kind;
        decl->attributes = attributes;

        return decl;
    }

    default: {
        AstExpr *type_expr = parsePrefixedUnaryExpr(p);
        if (!type_expr) return NULL;

        Token *name_tok = parserConsume(p, TOKEN_IDENT);
        if (!name_tok) return NULL;

        if (parserPeek(p, 0)->kind == TOKEN_LPAREN)
        {
            // Function declaration

            AstDecl *decl = NEW(compiler, AstDecl);
            decl->kind = DECL_FUNC;
            decl->name = name_tok->str;
            decl->func.return_type = type_expr;
            decl->attributes = attributes;

            if (!parserConsume(p, TOKEN_LPAREN)) return NULL;

            while (parserPeek(p, 0)->kind != TOKEN_RPAREN)
            {
                AstVarKind var_kind = VAR_PLAIN;

                if (parserPeek(p, 0)->kind == TOKEN_IN)
                {
                    parserNext(p, 1);
                    var_kind = VAR_IN_PARAM;
                }
                else if (parserPeek(p, 0)->kind == TOKEN_OUT)
                {
                    parserNext(p, 1);
                    var_kind = VAR_OUT_PARAM;
                }
                else if (parserPeek(p, 0)->kind == TOKEN_INOUT)
                {
                    parserNext(p, 1);
                    var_kind = VAR_INOUT_PARAM;
                }

                AstExpr *type_expr = parsePrefixedUnaryExpr(p);
                if (!type_expr) return NULL;

                Token *param_name_tok = parserConsume(p, TOKEN_IDENT);
                if (!param_name_tok) return NULL;

                AstDecl *param_decl = NEW(compiler, AstDecl);
                param_decl->kind = DECL_VAR;
                param_decl->name = param_name_tok->str;
                param_decl->var.type_expr = type_expr;
                param_decl->var.kind = var_kind;

                if (parserPeek(p, 0)->kind == TOKEN_COLON)
                {
                    parserNext(p, 1);
                    Token *semantic_tok = parserConsume(p, TOKEN_IDENT);
                    if (!semantic_tok) return NULL;
                    param_decl->var.semantic = semantic_tok->str;
                }

                arrPush(decl->func.all_params, param_decl);

                if (parserPeek(p, 0)->kind != TOKEN_RPAREN)
                {
                    if (!parserConsume(p, TOKEN_COMMA)) return NULL;
                }
            }

            if (!parserConsume(p, TOKEN_RPAREN)) return NULL;

            if (!parserConsume(p, TOKEN_LCURLY)) return NULL;

            while (parserPeek(p, 0)->kind != TOKEN_RCURLY)
            {
                AstStmt *stmt = parseStmt(p);
                if (stmt)
                {
                    arrPush(decl->func.stmts, stmt);
                }
            }

            if (!parserConsume(p, TOKEN_RCURLY)) return NULL;

            return decl;
        }
        else
        {
            ts__addErr(
                compiler, &parserPeek(p, 0)->loc, "expecting top level declaration");
            parserNext(p, 1);
        }

        break;
    }
    }

    return NULL;
}

void ts__parserParse(Parser *p, TsCompiler *compiler, Token *tokens, size_t token_count)
{
    memset(p, 0, sizeof(*p));
    p->compiler = compiler;
    p->tokens = tokens;
    p->token_count = token_count;

    while (!parserIsAtEnd(p))
    {
        AstDecl *decl = parseTopLevel(p);
        if (decl)
        {
            arrPush(p->decls, decl);
        }
    }
}
