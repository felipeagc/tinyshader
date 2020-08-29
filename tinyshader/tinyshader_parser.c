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

////////////////////////////////
//
// Lexer
//
////////////////////////////////

static inline bool lexerIsAtEnd(Lexer *l)
{
    return (l->pos >= l->file->text_size) || (l->file->text[l->pos] == '\0');
}

static inline char lexerNext(Lexer *l, size_t count)
{
    char c = l->file->text[l->pos];
    l->pos += count;
    l->col += count;
    return c;
}

static inline char lexerPeek(Lexer *l, size_t offset)
{
    return l->file->text[l->pos + offset];
}

static inline void lexerAddSimpleToken(Lexer *l, TokenKind kind, size_t length)
{
    lexerNext(l, length);
    l->token.kind = kind;
    l->token.loc.length = length;
}

void ts__lexerLex(Lexer *l, TsCompiler *compiler, File *file)
{
    l->file = file;
    l->compiler = compiler;
    l->pos = 0;
    l->col = 1;
    l->line = 1;

    while (!lexerIsAtEnd(l))
    {
        memset(&l->token, 0, sizeof(Token));
        l->token.loc.pos = l->pos;
        l->token.loc.length = 0;
        l->token.loc.line = l->line;
        l->token.loc.col = l->col;

        switch (lexerPeek(l, 0))
        {
        case '\t':
        case ' ': {
            lexerNext(l, 1);
            break;
        }

        case '\n': {
            l->col = 0;
            l->line++;
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
            else
                lexerAddSimpleToken(l, TOKEN_ADD, 1);
            break;
        }
        case '-': {
            if (lexerPeek(l, 1) == '=')
                lexerAddSimpleToken(l, TOKEN_SUBEQ, 2);
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

                while (lexerPeek(l, 0) != '\n' && !lexerIsAtEnd(l))
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
                    if (lexerPeek(l, 0) == '\n')
                    {
                        ++l->line;
                        l->col = 0;
                    }
                    lexerNext(l, 1);
                }

                if (!lexerIsAtEnd(l))
                {
                    lexerNext(l, 2);
                }
                else
                {
                    Location err_loc = l->token.loc;
                    err_loc.length = 1;
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
                ts__addErr(l->compiler, &err_loc, "unclosed string");
                break;
            }

            lexerNext(l, 1);

            l->token.kind = TOKEN_STRING_LIT;
            l->token.str = ts__sbBuild(&compiler->sb, &compiler->alloc);
            l->token.loc.length = l->pos - l->token.loc.pos;
            break;
        }

        default: {
            if (isLetter(lexerPeek(l, 0)))
            {
                while (!lexerIsAtEnd(l) && isAlphanum(lexerPeek(l, 0)))
                {
                    lexerNext(l, 1);
                }

                size_t ident_length = l->pos - l->token.loc.pos;
                char *ident_start = &l->file->text[l->token.loc.pos];

                char *ident = NEW_ARRAY(compiler, char, ident_length + 1);
                memcpy(ident, ident_start, ident_length);
                ident[ident_length] = '\0';

                l->token.loc.length = ident_length;
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

                    char *hex_start = &l->file->text[l->pos];

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
                    char *number_start = &l->file->text[l->pos];

                    while (isNumeric(lexerPeek(l, 0)) || lexerPeek(l, 0) == '.')
                    {
                        if (lexerPeek(l, 0) == '.')
                        {
                            if (!isNumeric(lexerPeek(l, 1))) break;
                            assert(!dot_ptr);
                            dot_ptr = &l->file->text[l->pos];
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
                ts__addErr(l->compiler, &err_loc, "unknown token");
                lexerNext(l, 1);
            }

            break;
        }
        }

        if (l->token.loc.length > 0)
        {
            arrPush(l->file->tokens, l->token);
        }
    }

    if (arrLength(l->file->tokens) == 0)
    {
        Location err_loc = {0};
        err_loc.file = l->file;
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
    return p->pos >= arrLength(p->file->tokens);
}

static inline Token *parserPeek(Parser *p, size_t offset)
{
    if (p->pos + offset >= arrLength(p->file->tokens))
    {
        return &p->file->tokens[arrLength(p->file->tokens) - 1];
    }
    return &p->file->tokens[p->pos + offset];
}

static inline Token *parserNext(Parser *p, size_t count)
{
    if (parserIsAtEnd(p)) return NULL;

    Token *tok = &p->file->tokens[p->pos];
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

static AstExpr *parseUnaryExpr(Parser *p)
{
    if (parserPeek(p, 0)->kind == TOKEN_SUB || parserPeek(p, 0)->kind == TOKEN_NOT)
    {
        Token *op_tok = parserNext(p, 1);

        AstUnaryOp op = {0};

        switch (op_tok->kind)
        {
        case TOKEN_SUB: op = UNOP_NEG; break;
        case TOKEN_NOT: op = UNOP_NOT; break;
        default: assert(0); break;
        }

        AstExpr *right = parseUnaryExpr(p);

        AstExpr *expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_UNARY;
        expr->unary.right = right;
        expr->unary.op = op;

        return expr;
    }

    return parseAccessFuncCall(p);
}

static AstExpr *parseMuliplication(Parser *p)
{
    AstExpr *expr = parseUnaryExpr(p);
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
        AstExpr *right = parseUnaryExpr(p);
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

        AstExpr *type_expr = parseUnaryExpr(p);
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
            AstExpr *type_expr = parseUnaryExpr(p);
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

            type_expr = parseUnaryExpr(p);
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

            type_expr->texture.sampled_type_expr = parseUnaryExpr(p);
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
        AstExpr *type_expr = parseUnaryExpr(p);
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

                AstExpr *type_expr = parseUnaryExpr(p);
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

void ts__parserParse(Parser *p, TsCompiler *compiler, File *file)
{
    p->pos = 0;
    p->compiler = compiler;
    p->file = file;

    while (!parserIsAtEnd(p))
    {
        AstDecl *decl = parseTopLevel(p);
        if (decl)
        {
            arrPush(p->file->decls, decl);
        }
    }
}
