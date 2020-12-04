/**
 * This file is part of the tinyshader library.
 * See tinyshader.h for license details.
 */
#include "tinyshader_internal.h"

typedef struct Preprocessor
{
    TsCompiler *compiler;
    StringBuilder sb;
    StringBuilder tmp_sb;
    HashMap defines;

    const char *input;
    size_t input_size;

    size_t pos;
    size_t line;
    size_t col;
} Preprocessor;

typedef struct PreprocessorFile
{
    File *file;

    size_t pos;
    size_t line;
    size_t col;
} PreprocessorFile;

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

static inline ptrdiff_t preprocessorLengthLeft(PreprocessorFile *f, size_t offset)
{
    return (ptrdiff_t)(f->file->text_size) - (ptrdiff_t)(f->pos + offset);
}

static inline const char *preprocessorPeek(PreprocessorFile *f, size_t offset)
{
    return &f->file->text[f->pos + offset];
}

static inline char preprocessorNext(PreprocessorFile *f, size_t count)
{
    char c = f->file->text[f->pos];
    f->pos += count;
    return c;
}

static size_t preprocessorSkipWhitespace(const char *text, size_t text_size)
{
    const char *curr = text;
    while ((curr != text + text_size) && isWhitespace(curr[0]))
    {
        curr++;
    }
    return (size_t)(curr - text);
}

static size_t preprocessorSkipWhitespaceNewline(const char *text, size_t text_size)
{
    const char *curr = text;
    while ((curr != text + text_size) &&
           (isWhitespace(curr[0]) ||
            curr[0] == '\r' ||
            curr[0] == '\n'))
    {
        curr++;
    }
    return (size_t)(curr - text);
}

static Location preprocessorGetLoc(PreprocessorFile *f)
{
    Location loc = {0};
    loc.length = 1;
    loc.line = f->line;
    loc.col = f->col;
    loc.pos = (uint32_t)f->pos;
    loc.path = f->file->path;
    loc.buffer = f->file->text;
    return loc;
}

static const char *preprocessorGetIdentifier(
    Preprocessor *p,
    const char* text,
    size_t text_size,
    size_t *out_length)
{
    *out_length = 0;
    if (text_size == 0) return NULL;

    const char *curr = text;

    if (!isLetter(*curr))
    {
        return NULL;
    }

    while (curr != text + text_size && isAlphanum(*curr))
    {
        curr++;
    }

    *out_length = (size_t)(curr - text);
    char *result = NEW_ARRAY_UNINIT(p->compiler, char, (*out_length)+1);
    memcpy(result, text, *out_length);
    result[*out_length] = '\0';
    return result;
}

static const char *preprocessorExpandMacro(
    Preprocessor *p,
    const char *ident,
    size_t ident_size,
    const char *from_define, /* can be null */
    size_t *out_length)
{
    size_t expanded_subtext_size = 0;
    const char *expanded_subtext = NULL;

    if (from_define && strcmp(from_define, ident) == 0)
    {
        // Recursive define, don't expand the identifier
        expanded_subtext = ident;
        expanded_subtext_size = ident_size;
    }
    else
    {
        char *define_value = NULL;
        if (ts__hashGet(&p->defines, ident, (void**)&define_value))
        {
            if (define_value)
            {
                expanded_subtext = preprocessorExpandMacro(
                    p,
                    define_value,
                    strlen(define_value),
                    ident, /* can be null */
                    &expanded_subtext_size);
            }
        }
        else
        {
            expanded_subtext = ident;
            expanded_subtext_size = ident_size;
        }
    }

    ts__sbReset(&p->tmp_sb);
    if (expanded_subtext_size > 0)
    {
        ts__sbAppendLen(&p->tmp_sb, expanded_subtext, expanded_subtext_size);
    }

    const char *result = ts__sbBuild(&p->tmp_sb, &p->compiler->alloc);
    *out_length = strlen(result);
    return result;
}

static void ts__preprocessFile(Preprocessor *p, PreprocessorFile *f)
{
    bool at_bol = true;

    f->col = 1;
    f->line = 1;

    while (preprocessorLengthLeft(f, 0) > 0)
    {
        switch (*preprocessorPeek(f, 0))
        {
        case '\n':
        {
            f->col = 1;
            f->line++;
            ts__sbAppendChar(&p->sb, '\n');
            at_bol = true;
            preprocessorNext(f, 1);
            break;
        }

        case '\r':
        {
            if (preprocessorLengthLeft(f, 1) > 0 && *preprocessorPeek(f, 1) == '\n')
            {
                f->col = 1;
                f->line++;
                ts__sbAppend(&p->sb, "\r\n");
                at_bol = true;
                preprocessorNext(f, 2);
            }
            break;
        }

        case '#':
        {
            preprocessorNext(f, 1); // Eat the hash

            size_t whitespace_len;

            whitespace_len = preprocessorSkipWhitespace(
                preprocessorPeek(f, 0),
                preprocessorLengthLeft(f, 0));
            preprocessorNext(f, whitespace_len);

            size_t ident_length = 0;
            const char *ident = preprocessorGetIdentifier(
                p,
                preprocessorPeek(f, 0),
                preprocessorLengthLeft(f, 0),
                &ident_length);
            preprocessorNext(f, ident_length);
            if (!ident)
            {
                break;
            }

            whitespace_len = preprocessorSkipWhitespace(
                preprocessorPeek(f, 0),
                preprocessorLengthLeft(f, 0));
            preprocessorNext(f, whitespace_len);

            ts__sbReset(&p->tmp_sb);

            while (preprocessorLengthLeft(f, 0) > 0)
            {
                if (preprocessorLengthLeft(f, 0) >= 2 &&
                    *preprocessorPeek(f, 0) == '\\' &&
                    *preprocessorPeek(f, 1) == '\n')
                {
                    // Skip newline
                    preprocessorNext(f, 2);
                    f->line++;
                    f->col = 1;

                    ts__sbAppendChar(&p->tmp_sb, '\n');
                    continue;
                }

                if (preprocessorLengthLeft(f, 0) >= 3 &&
                    *preprocessorPeek(f, 0) == '\\' &&
                    *preprocessorPeek(f, 1) == '\r' &&
                    *preprocessorPeek(f, 2) == '\n')
                {
                    // Skip newline
                    preprocessorNext(f, 3);
                    ts__sbAppendChar(&p->tmp_sb, '\n');
                    f->line++;
                    f->col = 1;
                    continue;
                }

                if (preprocessorLengthLeft(f, 0) >= 1 &&
                    *preprocessorPeek(f, 0) == '\n')
                {
                    // Found line break, end here
                    break;
                }

                if (preprocessorLengthLeft(f, 0) >= 2 &&
                    *preprocessorPeek(f, 0) == '\r' &&
                    *preprocessorPeek(f, 1) == '\n')
                {
                    // Found line break, end here
                    break;
                }

                char c = preprocessorNext(f, 1);
                ts__sbAppendChar(&p->tmp_sb, c);
            }

            const char *content = ts__sbBuild(&p->tmp_sb, &p->compiler->alloc);
            size_t content_length = strlen(content);

            whitespace_len = preprocessorSkipWhitespaceNewline(
                content,
                content_length);
            content_length -= whitespace_len;
            content += whitespace_len;

            if (strcmp(ident, "define") == 0)
            {
                size_t define_name_length = 0;
                const char *define_name = preprocessorGetIdentifier(
                    p, content, content_length, &define_name_length);

                if (define_name_length == 0)
                {
                    Location loc = preprocessorGetLoc(f);
                    ts__addErr(p->compiler, &loc, "missing define name");
                    break;
                }

                const char *value = content + define_name_length;
                size_t value_length = content_length - define_name_length;

                whitespace_len = preprocessorSkipWhitespaceNewline(
                    value,
                    value_length);
                value_length -= whitespace_len;
                value += whitespace_len;


                if (value_length > 0)
                {
                    ts__hashSet(&p->defines, define_name, (void*)value);
                }
                else
                {
                    ts__hashSet(&p->defines, define_name, (void*)NULL);
                }
            }
            else if (strcmp(ident, "undef") == 0)
            {
                size_t define_name_length = 0;
                const char *define_name = preprocessorGetIdentifier(
                    p, content, content_length, &define_name_length);

                if (define_name_length == 0)
                {
                    Location loc = preprocessorGetLoc(f);
                    ts__addErr(p->compiler, &loc, "missing define name");
                    break;
                }

                ts__hashRemove(&p->defines, define_name);
            }
            else if (strcmp(ident, "ifdef") == 0)
            {
                Location loc = preprocessorGetLoc(f);
                ts__addErr(p->compiler, &loc, "#ifdef not implemented");
            }
            else if (strcmp(ident, "ifndef") == 0)
            {
                Location loc = preprocessorGetLoc(f);
                ts__addErr(p->compiler, &loc, "#ifndef not implemented");
            }
            else if (strcmp(ident, "if") == 0)
            {
                Location loc = preprocessorGetLoc(f);
                ts__addErr(p->compiler, &loc, "#if not implemented");
            }
            else if (strcmp(ident, "else") == 0)
            {
                Location loc = preprocessorGetLoc(f);
                ts__addErr(p->compiler, &loc, "#else not implemented");
            }
            else if (strcmp(ident, "endif") == 0)
            {
                Location loc = preprocessorGetLoc(f);
                ts__addErr(p->compiler, &loc, "#endif not implemented");
            }
            else if (strcmp(ident, "include") == 0)
            {
                Location loc = preprocessorGetLoc(f);
                ts__addErr(p->compiler, &loc, "#include not implemented");
            }
            else if (strcmp(ident, "pragma") == 0)
            {
            }
            else
            {
                Location loc = preprocessorGetLoc(f);
                ts__addErr(
                    p->compiler, &loc, "invalid preprocessing directive: '%s'", ident);
            }

            break;
        }

        default:
        {
            const char *curr = preprocessorPeek(f, 0);
            size_t curr_size = preprocessorLengthLeft(f, 0);

            if (!isWhitespace(*curr) && at_bol)
            {
                at_bol = false;
            }

            size_t ident_size = 0;
            const char *ident = preprocessorGetIdentifier(
                p,
                curr,
                curr_size,
                &ident_size);

            if (ident)
            {
                assert(ident_size > 0);
                size_t expanded_size = 0;
                const char *expanded = preprocessorExpandMacro(
                    p, ident, ident_size, NULL, &expanded_size);

                ts__sbAppendLen(&p->sb, expanded, expanded_size);
                preprocessorNext(f, ident_size);
            }
            else
            {
                ts__sbAppendChar(&p->sb, *curr);
                preprocessorNext(f, 1);
            }

            break;
        }
        }
    }
}

const char *ts__preprocess(
    TsCompiler *compiler, File *base_file, size_t *out_size)
{
    Preprocessor *p = NEW(compiler, Preprocessor);
    memset(p, 0, sizeof(*p));
    p->compiler = compiler;

    ts__hashInit(compiler, &p->defines, 0);
    ts__sbInit(&p->sb);
    ts__sbInit(&p->tmp_sb);

    ts__sbReset(&p->sb);

    PreprocessorFile *preproc_file = NEW(compiler, PreprocessorFile);
    preproc_file->file = base_file;
    ts__preprocessFile(p, preproc_file);

    const char* final_text = ts__sbBuild(&p->sb, &p->compiler->alloc);

    ts__sbDestroy(&p->sb);
    ts__sbDestroy(&p->tmp_sb);
    ts__hashDestroy(&p->defines);

    *out_size = strlen(final_text);
    return final_text;
}
