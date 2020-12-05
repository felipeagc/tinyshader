/**
 * This file is part of the tinyshader library.
 * See tinyshader.h for license details.
 */
#include "tinyshader_internal.h"

typedef struct Lexer
{
    TsCompiler *compiler;

    File *file;
    const char *text;
    size_t text_size;
    Token token;

    ArrayOfToken tokens;

    char *file_path;
    size_t pos;
    uint32_t line;
    uint32_t col;
} Lexer;

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
        err_loc.path = l->file_path;
        err_loc.buffer = l->text;
        ts__addErr(l->compiler, &err_loc, "unexpected character: '%c'", lexerPeek(l, 0));
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

ArrayOfToken ts__lex(TsCompiler *compiler, File *file, const char *text, size_t text_size)
{
    Lexer *l = NEW(compiler, Lexer);
    memset(l, 0, sizeof(*l));

    if (file)
    {
        l->file_path = file->path;
    }
    l->text = text;
    l->text_size = text_size;
    l->compiler = compiler;
    l->col = 1;
    l->line = 1;

    while (!lexerIsAtEnd(l))
    {
        memset(&l->token, 0, sizeof(Token));
        l->token.loc.path = l->file_path;
        l->token.loc.buffer = l->text;
        l->token.loc.pos = (uint32_t)l->pos;
        l->token.loc.length = 0;
        l->token.loc.line = l->line;
        l->token.loc.col = l->col;

        if (l->tokens.len == 0)
        {
            l->token.at_bol = true;
        }
        else if (l->tokens.ptr[l->tokens.len-1].loc.line < l->token.loc.line)
        {
            l->token.at_bol = true;
        }

        switch (lexerPeek(l, 0))
        {
        case '\t':
        case ' ': {
            lexerNext(l, 1);
            break;
        }

        case '#': {
            lexerAddSimpleToken(l, TOKEN_HASH, 1);
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
            lexerAddSimpleToken(l, TOKEN_LBRACK, 1);
            break;
        }
        case ']': {
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
        case '?': {
            lexerAddSimpleToken(l, TOKEN_QUESTION, 1);
            break;
        }

        case '+': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_ADD_ASSIGN, 2);
            else if (lexerPeek(l, 1) == '+')
                lexerAddSimpleToken(l, TOKEN_ADDADD, 2);
            else
                lexerAddSimpleToken(l, TOKEN_ADD, 1);
            break;
        }
        case '-': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_SUB_ASSIGN, 2);
            else if (lexerPeek(l, 1) == '-')
                lexerAddSimpleToken(l, TOKEN_SUBSUB, 2);
            else
                lexerAddSimpleToken(l, TOKEN_SUB, 1);
            break;
        }
        case '*': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_MUL_ASSIGN, 2);
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
                    err_loc.buffer = l->text;
                    err_loc.pos = (uint32_t)l->pos;
                    err_loc.line = l->line;
                    err_loc.col = l->col;
                    ts__addErr(l->compiler, &err_loc, "unclosed comment");
                }
            }
            else if (lexerPeek(l, 1) == '=')
            {
                lexerAddSimpleToken(l, TOKEN_DIV_ASSIGN, 2);
            }
            else
            {
                lexerAddSimpleToken(l, TOKEN_DIV, 1);
            }
            break;
        }
        case '%': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_MOD_ASSIGN, 2);
            else
                lexerAddSimpleToken(l, TOKEN_MOD, 1);
            break;
        }

        case '&': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_BITAND_ASSIGN, 2);
            else if (lexerPeek(l, 1) == '&')
                lexerAddSimpleToken(l, TOKEN_AND, 2);
            else
                lexerAddSimpleToken(l, TOKEN_BITAND, 1);
            break;
        }
        case '|': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_BITOR_ASSIGN, 2);
            else if (lexerPeek(l, 1) == '|')
                lexerAddSimpleToken(l, TOKEN_OR, 2);
            else
                lexerAddSimpleToken(l, TOKEN_BITOR, 1);
            break;
        }
        case '^': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_BITXOR_ASSIGN, 2);
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
            else if (lexerPeek(l, 1) == '>')
                lexerAddSimpleToken(l, TOKEN_RSHIFT, 2);
            else
                lexerAddSimpleToken(l, TOKEN_GREATER, 1);
            break;
        }
        case '<': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_LESSEQ, 2);
            else if (lexerPeek(l, 1) == '<')
                lexerAddSimpleToken(l, TOKEN_LSHIFT, 2);
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
                err_loc.buffer = l->text;
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
            else if (!lexerIsAtEnd(l) && isLetter(lexerPeek(l, 0)))
            {
                while (!lexerIsAtEnd(l) && isAlphanum(lexerPeek(l, 0)))
                {
                    lexerNext(l, 1);
                }

                size_t ident_length = l->pos - l->token.loc.pos;
                const char *ident_start = &l->text[l->token.loc.pos];

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
                                uint8_t dim1 = (uint8_t)(type_str[prefix_len + 2] - '0');
                                uint8_t dim2 = (uint8_t)(type_str[prefix_len] - '0');
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
            else if (!lexerIsAtEnd(l) && isNumeric(lexerPeek(l, 0)))
            {
                if (lexerPeek(l, 0) == '0' && lexerPeek(l, 1) == 'x')
                {
                    // Hexadecimal
                    l->token.kind = TOKEN_INT_LIT;
                    lexerNext(l, 2);
                    l->token.loc.length += 2;

                    const char *hex_start = &l->text[l->pos];

                    while (isNumeric(lexerPeek(l, 0)) ||
                           (lexerPeek(l, 0) >= 'a' && lexerPeek(l, 0) <= 'f') ||
                           (lexerPeek(l, 0) >= 'A' && lexerPeek(l, 0) <= 'F'))
                    {
                        l->token.loc.length++;
                        lexerNext(l, 1);
                    }

                    char *str = NEW_ARRAY(compiler, char, l->token.loc.length - 2 + 1);
                    memcpy(str, hex_start, l->token.loc.length - 2);

                    if (lexerPeek(l, 0) == 'u' || lexerPeek(l, 0) == 'U')
                    {
                        l->token.loc.length++;
                        lexerNext(l, 1);
                    }

                    l->token.int_ = strtol(str, NULL, 16);
                }
                else
                {
                    const char *number_start = &l->text[l->pos];
                    bool is_float = false;

                    while (isNumeric(lexerPeek(l, 0)))
                    {
                        l->token.loc.length++;
                        lexerNext(l, 1);
                    }

                    if (lexerPeek(l, 0) == 'u' || lexerPeek(l, 0) == 'U')
                    {
                        l->token.loc.length++;
                        lexerNext(l, 1);
                    }
                    else
                    {
                        if (lexerPeek(l, 0) == '.')
                        {
                            is_float = true;
                            l->token.loc.length++;
                            lexerNext(l, 1);
                        }

                        while (isNumeric(lexerPeek(l, 0)))
                        {
                            l->token.loc.length++;
                            lexerNext(l, 1);
                        }

                        if (lexerPeek(l, 0) == 'e' || lexerPeek(l, 0) == 'E')
                        {
                            is_float = true;
                            l->token.loc.length++;
                            lexerNext(l, 1);

                            if (lexerPeek(l, 0) == '-')
                            {
                                l->token.loc.length++;
                                lexerNext(l, 1);
                            }

                            while (isNumeric(lexerPeek(l, 0)))
                            {
                                l->token.loc.length++;
                                lexerNext(l, 1);
                            }
                        }

                        if (lexerPeek(l, 0) == 'f')
                        {
                            is_float = true;
                            l->token.loc.length++;
                            lexerNext(l, 1);
                        }
                    }

                    char *str = NEW_ARRAY(compiler, char, l->token.loc.length + 1);
                    memcpy(str, number_start, l->token.loc.length);

                    if (is_float)
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
            else if (!lexerIsAtEnd(l))
            {
                Location err_loc = l->token.loc;
                err_loc.length = 1;
                err_loc.path = l->file_path;
                err_loc.buffer = l->text;
                err_loc.pos = (uint32_t)l->pos;
                err_loc.line = l->line;
                err_loc.col = l->col;

                char tok_char = lexerPeek(l, 0);
                if (tok_char > 0)
                {
                    ts__addErr(l->compiler, &err_loc, "unknown token: '%c'", tok_char);
                }
                else
                {
                    ts__addErr(l->compiler, &err_loc, "unknown token: '%d'", (int)tok_char);
                }
                lexerNext(l, 1);
            }

            break;
        }
        }

        if (l->token.loc.length > 0)
        {
            arrPush(l->compiler, &l->tokens, l->token);
        }
    }

    if (l->tokens.len == 0)
    {
        Location err_loc = {0};
        err_loc.path = l->file_path;
        err_loc.buffer = l->text;
        ts__addErr(l->compiler, &err_loc, "no input, reached end of file");
    }

    return l->tokens;
}
