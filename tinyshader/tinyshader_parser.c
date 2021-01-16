/**
 * This file is part of the tinyshader library.
 * See tinyshader.h for license details.
 */
#include "tinyshader_internal.h"

typedef struct Parser
{
    TsCompiler *compiler;
    Token *tokens;
    size_t token_count;

    ArrayOfAstDeclPtr decls;

    size_t pos;
} Parser;

static AstExpr *parseExpr(Parser *p);

static inline ptrdiff_t parserLengthLeft(Parser *p)
{
    return (ptrdiff_t)(p->token_count) - (ptrdiff_t)(p->pos);
}

static inline bool parserIsAtEnd(Parser *p)
{
    return p->pos >= p->token_count;
}

static inline Token *parserPeek(Parser *p, size_t offset)
{
    if (parserLengthLeft(p) <= (ptrdiff_t)offset)
    {
        return &p->tokens[p->token_count - 1];
    }
    return &p->tokens[p->pos + offset];
}

static inline Token *parserNext(Parser *p, size_t count)
{
    if (parserLengthLeft(p) <= 0) return NULL;

    Token *tok = &p->tokens[p->pos];
    p->pos += count;
    return tok;
}

static inline Token *parserConsume(Parser *p, TokenKind kind)
{
    Token *tok = parserPeek(p, 0);
    if (tok->kind != kind)
    {
        ts__addErr(
            p->compiler,
            &tok->loc,
            "unexpected token: '%.*s', expected: '%s'",
            (int)tok->loc.length,
            &tok->loc.buffer[tok->loc.pos],
            ts__getTokenString(kind));
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
            p->compiler, &parserPeek(p, 0)->loc, "expecting identifier expression");
        parserNext(p, 1);
        break;
    }
    }

    return NULL;
}

static AstExpr *parsePrefixedUnaryExpr(Parser *p);

static AstExpr *parsePrimaryExpr(Parser *p)
{
    TsCompiler *compiler = p->compiler;

    Location loc = parserBeginLoc(p);

    if (parserLengthLeft(p) == 0)
    {
        parserEndLoc(p, &loc);
        ts__addErr(
            p->compiler,
            &loc,
            "expecting primary expression, reached end of file");
        return NULL;
    }

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

    case TOKEN_CONSTANT_BUFFER: {
        parserNext(p, 1);

        AstExpr *type_expr = NEW(compiler, AstExpr);
        type_expr->loc = parserPeek(p, 0)->loc;
        type_expr->kind = EXPR_CONSTANT_BUFFER_TYPE;

        if (!parserConsume(p, TOKEN_LESS)) return NULL;

        type_expr->buffer.sub_expr = parsePrefixedUnaryExpr(p);
        if (!type_expr->buffer.sub_expr) return NULL;

        if (!parserConsume(p, TOKEN_GREATER)) return NULL;

        return type_expr;
    }

    case TOKEN_STRUCTURED_BUFFER: {
        parserNext(p, 1);

        AstExpr *type_expr = NEW(compiler, AstExpr);
        type_expr->loc = parserPeek(p, 0)->loc;
        type_expr->kind = EXPR_STRUCTURED_BUFFER_TYPE;

        if (!parserConsume(p, TOKEN_LESS)) return NULL;

        type_expr->buffer.sub_expr = parsePrefixedUnaryExpr(p);
        if (!type_expr->buffer.sub_expr) return NULL;

        if (!parserConsume(p, TOKEN_GREATER)) return NULL;

        return type_expr;
    }

    case TOKEN_RW_STRUCTURED_BUFFER: {
        parserNext(p, 1);

        AstExpr *type_expr = NEW(compiler, AstExpr);
        type_expr->loc = parserPeek(p, 0)->loc;
        type_expr->kind = EXPR_RW_STRUCTURED_BUFFER_TYPE;

        if (!parserConsume(p, TOKEN_LESS)) return NULL;

        type_expr->buffer.sub_expr = parsePrefixedUnaryExpr(p);
        if (!type_expr->buffer.sub_expr) return NULL;

        if (!parserConsume(p, TOKEN_GREATER)) return NULL;

        return type_expr;
    }

    case TOKEN_TEXTURE_1D:
    case TOKEN_TEXTURE_2D:
    case TOKEN_TEXTURE_3D:
    case TOKEN_TEXTURE_CUBE: {
        Token *texture_kind_tok = parserNext(p, 1);

        AstExpr *type_expr = NEW(compiler, AstExpr);
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

        if (parserPeek(p, 0)->kind == TOKEN_LESS)
        {
            if (!parserConsume(p, TOKEN_LESS)) return NULL;

            type_expr->texture.sampled_type_expr = parsePrefixedUnaryExpr(p);
            if (!type_expr->texture.sampled_type_expr) return NULL;

            if (!parserConsume(p, TOKEN_GREATER)) return NULL;
        }
        else
        {
            type_expr->texture.sampled_type_expr = NULL;
        }

        return type_expr;
    }

    case TOKEN_SAMPLER:
    case TOKEN_SAMPLER_STATE: {
        AstExpr *type_expr = NEW(compiler, AstExpr);
        type_expr->kind = EXPR_SAMPLER_TYPE;
        type_expr->loc = parserPeek(p, 0)->loc;

        parserNext(p, 1);

        return type_expr;
    }

    default: {
        ts__addErr(
            p->compiler,
            &parserPeek(p, 0)->loc,
            "expecting primary expression, instead got: '%s'",
            ts__getTokenString(parserPeek(p, 0)->kind));
        parserNext(p, 1);
        break;
    }
    }

    return NULL;
}

static AstExpr *parseAccessFuncCall(Parser *p)
{
    TsCompiler *compiler = p->compiler;
    Location loc = parserBeginLoc(p);

    AstExpr *expr = parsePrimaryExpr(p);
    if (!expr) return NULL;

    while (!parserIsAtEnd(p) && (parserPeek(p, 0)->kind == TOKEN_LPAREN ||
                                 parserPeek(p, 0)->kind == TOKEN_PERIOD ||
                                 parserPeek(p, 0)->kind == TOKEN_LBRACK))
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
                arrPush(p->compiler, &func_call->func_call.params, param);

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

                arrPush(p->compiler, &expr->access.chain, ident);
            }

            parserEndLoc(p, &loc);
            expr->loc = loc;
        }
        else if (parserPeek(p, 0)->kind == TOKEN_LBRACK)
        {
            // Subscript expression

            while (parserPeek(p, 0)->kind == TOKEN_LBRACK)
            {
                parserNext(p, 1);

                AstExpr *left_expr = expr;
                expr = NEW(compiler, AstExpr);
                expr->kind = EXPR_SUBSCRIPT;
                expr->subscript.left = left_expr;
                expr->subscript.right = parseExpr(p);
                if (!expr->subscript.right) return NULL;

                if (!parserConsume(p, TOKEN_RBRACK)) return NULL;
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

    Location loc = parserBeginLoc(p);

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

        parserEndLoc(p, &loc);
        expr->loc = loc;

        expr = new_expr;
    }

    return expr;
}

static AstExpr *parsePrefixedUnaryExpr(Parser *p)
{
    Location loc = parserBeginLoc(p);

    if (parserPeek(p, 0)->kind == TOKEN_SUB || parserPeek(p, 0)->kind == TOKEN_NOT ||
        parserPeek(p, 0)->kind == TOKEN_ADDADD || parserPeek(p, 0)->kind == TOKEN_SUBSUB|| parserPeek(p, 0)->kind == TOKEN_BITNOT)
    {
        Token *op_tok = parserNext(p, 1);

        AstUnaryOp op = {0};

        switch (op_tok->kind)
        {
        case TOKEN_SUB: op = UNOP_NEG; break;
        case TOKEN_NOT: op = UNOP_NOT; break;
        case TOKEN_ADDADD: op = UNOP_PRE_INC; break;
        case TOKEN_SUBSUB: op = UNOP_PRE_DEC; break;
        case TOKEN_BITNOT: op = UNOP_BITNOT; break;
        default: assert(0); break;
        }

        AstExpr *right = parsePrefixedUnaryExpr(p);

        AstExpr *expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_UNARY;
        expr->unary.right = right;
        expr->unary.op = op;

        parserEndLoc(p, &loc);
        expr->loc = loc;

        return expr;
    }

    return parsePostfixedUnaryExpr(p);
}

static AstExpr *parseMuliplication(Parser *p)
{
    Location loc = parserBeginLoc(p);

    AstExpr *expr = parsePrefixedUnaryExpr(p);
    if (!expr) return NULL;

    while (!parserIsAtEnd(p) &&
           (parserPeek(p, 0)->kind == TOKEN_MUL ||
            parserPeek(p, 0)->kind == TOKEN_DIV ||
            parserPeek(p, 0)->kind == TOKEN_MOD))
    {
        Token *op_tok = parserNext(p, 1);

        AstBinaryOp op = {0};

        switch (op_tok->kind)
        {
        case TOKEN_MUL: op = BINOP_MUL; break;
        case TOKEN_DIV: op = BINOP_DIV; break;
        case TOKEN_MOD: op = BINOP_MOD; break;
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

        parserEndLoc(p, &loc);
        expr->loc = loc;
    }

    return expr;
}

static AstExpr *parseAddition(Parser *p)
{
    Location loc = parserBeginLoc(p);

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

        parserEndLoc(p, &loc);
        expr->loc = loc;
    }

    return expr;
}

static AstExpr *parseBitShift(Parser *p)
{
    Location loc = parserBeginLoc(p);

    AstExpr *expr = parseAddition(p);
    if (!expr) return NULL;

    while (!parserIsAtEnd(p) &&
           (parserPeek(p, 0)->kind == TOKEN_LSHIFT || parserPeek(p, 0)->kind == TOKEN_RSHIFT))
    {
        Token *op_tok = parserNext(p, 1);

        AstBinaryOp op = {0};

        switch (op_tok->kind)
        {
        case TOKEN_LSHIFT: op = BINOP_LSHIFT; break;
        case TOKEN_RSHIFT: op = BINOP_RSHIFT; break;
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

        parserEndLoc(p, &loc);
        expr->loc = loc;
    }

    return expr;
}

static AstExpr *parseComparison(Parser *p)
{
    Location loc = parserBeginLoc(p);

    AstExpr *expr = parseBitShift(p);
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
        AstExpr *right = parseBitShift(p);
        if (!right) return NULL;

        expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_BINARY;
        expr->binary.left = left;
        expr->binary.right = right;
        expr->binary.op = op;

        parserEndLoc(p, &loc);
        expr->loc = loc;
    }

    return expr;
}

static AstExpr *parseBitAnd(Parser *p)
{
    Location loc = parserBeginLoc(p);

    AstExpr *expr = parseComparison(p);
    if (!expr) return NULL;

    while (!parserIsAtEnd(p) && (parserPeek(p, 0)->kind == TOKEN_BITAND))
    {
        Token *op_tok = parserNext(p, 1);

        AstBinaryOp op = {0};

        switch (op_tok->kind)
        {
        case TOKEN_BITAND: op = BINOP_BITAND; break;
        default: assert(0); break;
        }

        AstExpr *left = expr;
        AstExpr *right = parseComparison(p);
        if (!right) return NULL;

        expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_BINARY;
        expr->binary.left = left;
        expr->binary.right = right;
        expr->binary.op = op;

        parserEndLoc(p, &loc);
        expr->loc = loc;
    }

    return expr;
}

static AstExpr *parseBitXor(Parser *p)
{
    Location loc = parserBeginLoc(p);

    AstExpr *expr = parseBitAnd(p);
    if (!expr) return NULL;

    while (!parserIsAtEnd(p) && (parserPeek(p, 0)->kind == TOKEN_BITXOR))
    {
        Token *op_tok = parserNext(p, 1);

        AstBinaryOp op = {0};

        switch (op_tok->kind)
        {
        case TOKEN_BITXOR: op = BINOP_BITXOR; break;
        default: assert(0); break;
        }

        AstExpr *left = expr;
        AstExpr *right = parseBitAnd(p);
        if (!right) return NULL;

        expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_BINARY;
        expr->binary.left = left;
        expr->binary.right = right;
        expr->binary.op = op;

        parserEndLoc(p, &loc);
        expr->loc = loc;
    }

    return expr;
}

static AstExpr *parseBitOr(Parser *p)
{
    Location loc = parserBeginLoc(p);

    AstExpr *expr = parseBitXor(p);
    if (!expr) return NULL;

    while (!parserIsAtEnd(p) && (parserPeek(p, 0)->kind == TOKEN_BITOR))
    {
        Token *op_tok = parserNext(p, 1);

        AstBinaryOp op = {0};

        switch (op_tok->kind)
        {
        case TOKEN_BITOR: op = BINOP_BITOR; break;
        default: assert(0); break;
        }

        AstExpr *left = expr;
        AstExpr *right = parseBitXor(p);
        if (!right) return NULL;

        expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_BINARY;
        expr->binary.left = left;
        expr->binary.right = right;
        expr->binary.op = op;

        parserEndLoc(p, &loc);
        expr->loc = loc;
    }

    return expr;
}

static AstExpr *parseLogicalAnd(Parser *p)
{
    Location loc = parserBeginLoc(p);

    AstExpr *expr = parseBitOr(p);
    if (!expr) return NULL;

    while (!parserIsAtEnd(p) && (parserPeek(p, 0)->kind == TOKEN_AND))
    {
        parserNext(p, 1);

        AstExpr *left = expr;
        AstExpr *right = parseBitOr(p);
        if (!right) return NULL;

        expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_BINARY;
        expr->binary.left = left;
        expr->binary.right = right;
        expr->binary.op = BINOP_LOGICAL_AND;

        parserEndLoc(p, &loc);
        expr->loc = loc;
    }

    return expr;
}

static AstExpr *parseLogicalOr(Parser *p)
{
    Location loc = parserBeginLoc(p);

    AstExpr *expr = parseLogicalAnd(p);
    if (!expr) return NULL;

    while (!parserIsAtEnd(p) && (parserPeek(p, 0)->kind == TOKEN_OR))
    {
        parserNext(p, 1);

        AstExpr *left = expr;
        AstExpr *right = parseLogicalAnd(p);
        if (!right) return NULL;

        expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_BINARY;
        expr->binary.left = left;
        expr->binary.right = right;
        expr->binary.op = BINOP_LOGICAL_OR;

        parserEndLoc(p, &loc);
        expr->loc = loc;
    }

    return expr;
}

static AstExpr *parseTernaryExpr(Parser *p)
{
    Location loc = parserBeginLoc(p);

    AstExpr *expr = parseLogicalOr(p);
    if (!expr) return NULL;

    while (!parserIsAtEnd(p) && (parserPeek(p, 0)->kind == TOKEN_QUESTION))
    {
        parserNext(p, 1);

        AstExpr *cond = expr;

        AstExpr *true_expr  = parseTernaryExpr(p);
        if (!true_expr) return NULL;

        if (!parserConsume(p, TOKEN_COLON)) return NULL;

        AstExpr *false_expr = parseTernaryExpr(p);
        if (!false_expr) return NULL;

        expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_TERNARY;
        expr->ternary.cond = cond;
        expr->ternary.true_expr = true_expr;
        expr->ternary.false_expr = false_expr;

        parserEndLoc(p, &loc);
        expr->loc = loc;
    }

    return expr;
}

static AstExpr *parseAssignExpr(Parser *p)
{
    Location loc = parserBeginLoc(p);

    AstExpr *expr = parseTernaryExpr(p);
    if (!expr) return NULL;

    while (!parserIsAtEnd(p) && (
               parserPeek(p, 0)->kind == TOKEN_ASSIGN
               || parserPeek(p, 0)->kind == TOKEN_ADD_ASSIGN
               || parserPeek(p, 0)->kind == TOKEN_SUB_ASSIGN
               || parserPeek(p, 0)->kind == TOKEN_MUL_ASSIGN
               || parserPeek(p, 0)->kind == TOKEN_DIV_ASSIGN
               || parserPeek(p, 0)->kind == TOKEN_MOD_ASSIGN
               || parserPeek(p, 0)->kind == TOKEN_BITAND_ASSIGN
               || parserPeek(p, 0)->kind == TOKEN_BITOR_ASSIGN
               || parserPeek(p, 0)->kind == TOKEN_BITXOR_ASSIGN))
    {
        TokenKind op_kind = parserPeek(p, 0)->kind;
        parserNext(p, 1);

        AstExpr *left = expr;
        AstExpr *right = parseTernaryExpr(p);
        if (!right) return NULL;

        expr = NEW(p->compiler, AstExpr);
        expr->kind = EXPR_VAR_ASSIGN;
        expr->var_assign.assigned_expr = left;

        if (op_kind == TOKEN_ASSIGN)
        {
            expr->var_assign.value_expr = right;

            parserEndLoc(p, &loc);
            expr->loc = loc;
        }
        else
        {
            AstBinaryOp binop;
            switch (op_kind)
            {
            case TOKEN_ADD_ASSIGN: binop = BINOP_ADD; break;
            case TOKEN_SUB_ASSIGN: binop = BINOP_SUB; break;
            case TOKEN_MUL_ASSIGN: binop = BINOP_MUL; break;
            case TOKEN_DIV_ASSIGN: binop = BINOP_DIV; break;
            case TOKEN_MOD_ASSIGN: binop = BINOP_MOD; break;
            case TOKEN_BITAND_ASSIGN: binop = BINOP_BITAND; break;
            case TOKEN_BITOR_ASSIGN: binop = BINOP_BITOR; break;
            case TOKEN_BITXOR_ASSIGN: binop = BINOP_BITXOR; break;
            default: assert(0); break;
            }

            AstExpr *subexpr = NEW(p->compiler, AstExpr);
            subexpr->kind = EXPR_BINARY;
            subexpr->binary.op = binop;
            subexpr->binary.left = left;
            subexpr->binary.right = right;

            expr->var_assign.value_expr = subexpr;

            parserEndLoc(p, &loc);
            expr->loc = loc;
            subexpr->loc = loc;
        }
    }

    return expr;
}

static AstExpr *parseExpr(Parser *p)
{
    return parseAssignExpr(p);
}

static AstDecl *parseVarDecl(
    Parser *p,
    bool parse_default_value,
    ArrayOfAstAttribute *attributes,
    bool parse_type_modifiers,
    bool parse_storage_class,
    bool parse_semantic)
{
    TsCompiler *compiler = p->compiler;

    Location decl_loc = parserBeginLoc(p);

    AstVarStorageClass storage_class = 0;
    AstTypeModifier type_modifiers = 0;

    Token *current = parserPeek(p, 0);

    while (parserLengthLeft(p) > 0 &&
            ((parse_storage_class && (
                current->kind == TOKEN_GROUPSHARED ||
                current->kind == TOKEN_UNIFORM)) ||
            (parse_type_modifiers &&
                (current->kind == TOKEN_CONST))))
    {
        switch (current->kind)
        {
        case TOKEN_GROUPSHARED:
        {
            if (storage_class == VAR_STORAGE_CLASS_NONE ||
                storage_class == VAR_STORAGE_CLASS_UNIFORM)
            {
                storage_class = VAR_STORAGE_CLASS_GROUPSHARED;
            }
            break;
        }
        case TOKEN_UNIFORM:
        {
            if (storage_class == VAR_STORAGE_CLASS_NONE)
            {
                storage_class = VAR_STORAGE_CLASS_UNIFORM;
            }
            break;
        }
        case TOKEN_CONST:
        {
            type_modifiers |= TYPE_MODIFIER_CONST;
            break;
        }
        default: assert(0);
        }

        parserNext(p, 1);
        current = parserPeek(p, 0);
    }

    AstExpr *type_expr = parsePrefixedUnaryExpr(p);
    if (!type_expr) return NULL;

    Token *name_tok = parserConsume(p, TOKEN_IDENT);
    if (!name_tok) return NULL;

    AstDecl *decl = NEW(compiler, AstDecl);
    decl->kind = DECL_VAR;
    decl->name = name_tok->str;
    decl->var.type_expr = type_expr;
    decl->var.storage_class = storage_class;
    decl->var.type_modifiers = type_modifiers;
    if (attributes)
    {
        decl->attributes = *attributes;
    }

    if (parse_semantic)
    {
        if (parserPeek(p, 0)->kind == TOKEN_COLON)
        {
            parserNext(p, 1);
            Token *semantic_tok = parserConsume(p, TOKEN_IDENT);
            if (!semantic_tok) return NULL;
            decl->semantic = semantic_tok->str;
        }
    }

    if (parse_default_value)
    {
        if (parserPeek(p, 0)->kind == TOKEN_ASSIGN)
        {
            parserNext(p, 1);

            AstExpr *value_expr = parseExpr(p);
            if (!value_expr) return NULL;

            decl->var.value_expr = value_expr;
        }
    }

    parserEndLoc(p, &decl_loc);
    decl->loc = decl_loc;

    return decl;
}

static AstStmt *parseStmt(Parser *p)
{
    TsCompiler *compiler = p->compiler;

    switch (parserPeek(p, 0)->kind)
    {
    case TOKEN_SEMICOLON: {
        parserNext(p, 1);
        return NULL;
    }

    case TOKEN_RETURN: {
        Location stmt_loc = parserBeginLoc(p);

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

        parserEndLoc(p, &stmt_loc);
        stmt->loc = stmt_loc;

        return stmt;
    }

    case TOKEN_DISCARD: {
        Location stmt_loc = parserBeginLoc(p);

        parserNext(p, 1);

        AstStmt *stmt = NEW(compiler, AstStmt);
        stmt->kind = STMT_DISCARD;

        if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

        parserEndLoc(p, &stmt_loc);
        stmt->loc = stmt_loc;

        return stmt;
    }

    case TOKEN_CONTINUE: {
        Location stmt_loc = parserBeginLoc(p);

        parserNext(p, 1);

        AstStmt *stmt = NEW(compiler, AstStmt);
        stmt->kind = STMT_CONTINUE;

        if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

        parserEndLoc(p, &stmt_loc);
        stmt->loc = stmt_loc;

        return stmt;
    }

    case TOKEN_BREAK: {
        Location stmt_loc = parserBeginLoc(p);

        parserNext(p, 1);

        AstStmt *stmt = NEW(compiler, AstStmt);
        stmt->kind = STMT_BREAK;

        if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

        parserEndLoc(p, &stmt_loc);
        stmt->loc = stmt_loc;

        return stmt;
    }

    case TOKEN_IF: {
        Location stmt_loc = parserBeginLoc(p);

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

        parserEndLoc(p, &stmt_loc);
        stmt->loc = stmt_loc;

        return stmt;
    }

    case TOKEN_WHILE: {
        Location stmt_loc = parserBeginLoc(p);

        parserNext(p, 1);
        AstStmt *stmt = NEW(compiler, AstStmt);
        stmt->kind = STMT_WHILE;

        if (!parserConsume(p, TOKEN_LPAREN)) return NULL;

        stmt->while_.cond = parseExpr(p);
        if (!stmt->while_.cond) return NULL;

        if (!parserConsume(p, TOKEN_RPAREN)) return NULL;

        stmt->while_.stmt = parseStmt(p);
        if (!stmt->while_.stmt) return NULL;

        parserEndLoc(p, &stmt_loc);
        stmt->loc = stmt_loc;

        return stmt;
    }

    case TOKEN_DO: {
        Location stmt_loc = parserBeginLoc(p);

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

        parserEndLoc(p, &stmt_loc);
        stmt->loc = stmt_loc;

        return stmt;
    }

    case TOKEN_FOR: {
        Location stmt_loc = parserBeginLoc(p);

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

        parserEndLoc(p, &stmt_loc);
        stmt->loc = stmt_loc;

        return stmt;
    }

    case TOKEN_LCURLY: {
        Location stmt_loc = parserBeginLoc(p);

        parserNext(p, 1);

        AstStmt *stmt = NEW(compiler, AstStmt);
        stmt->kind = STMT_BLOCK;

        while (parserPeek(p, 0)->kind != TOKEN_RCURLY)
        {
            AstStmt *sub_stmt = parseStmt(p);
            if (sub_stmt)
            {
                arrPush(p->compiler, &stmt->block.stmts, sub_stmt);
            }
        }

        if (!parserConsume(p, TOKEN_RCURLY)) return NULL;

        parserEndLoc(p, &stmt_loc);
        stmt->loc = stmt_loc;

        return stmt;
    }

    case TOKEN_CONST:
    {
        Location stmt_loc = parserBeginLoc(p);

        AstDecl *decl = parseVarDecl(p, true, NULL, true, false, false);
        if (!decl) return NULL;

        assert(decl->var.storage_class == VAR_STORAGE_CLASS_NONE);
        decl->var.storage_class = VAR_STORAGE_CLASS_FUNCTION;

        if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

        AstStmt *stmt = NEW(compiler, AstStmt);
        stmt->kind = STMT_DECL;
        stmt->decl = decl;

        parserEndLoc(p, &stmt_loc);
        stmt->loc = stmt_loc;

        return stmt;
    }

    default: {
        Location stmt_loc = parserBeginLoc(p);

        size_t checkpoint = p->pos;

        AstExpr *expr = parseExpr(p);
        if (!expr) return NULL;

        if (parserPeek(p, 0)->kind == TOKEN_SEMICOLON)
        {
            // Expression statement
            parserNext(p, 1);
            AstStmt *stmt = NEW(compiler, AstStmt);
            stmt->kind = STMT_EXPR;
            stmt->expr = expr;

            parserEndLoc(p, &stmt_loc);
            stmt->loc = stmt_loc;

            return stmt;
        }
        else if (parserPeek(p, 0)->kind == TOKEN_IDENT)
        {
            // Variable declaration
            p->pos = checkpoint;

            AstDecl *decl = parseVarDecl(p, true, NULL, true, false, false);
            if (!decl) return NULL;

            assert(decl->var.storage_class == VAR_STORAGE_CLASS_NONE);
            decl->var.storage_class = VAR_STORAGE_CLASS_FUNCTION;

            if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

            AstStmt *stmt = NEW(compiler, AstStmt);
            stmt->kind = STMT_DECL;
            stmt->decl = decl;

            parserEndLoc(p, &stmt_loc);
            stmt->loc = stmt_loc;

            return stmt;
        }
        else
        {
            ts__addErr(
                compiler,
                &parserPeek(p, 0)->loc,
                "unexpected token, expecting ';' or identifier");
            parserNext(p, 1);
        }

        break;
    }
    }

    return NULL;
}

static AstDecl *parseTopLevel(Parser *p)
{
    assert(p);
    TsCompiler *compiler = p->compiler;

    ArrayOfAstAttribute attributes = {0};

    int attr_start = 0;
    if (parserPeek(p, 0)->kind == TOKEN_LBRACK)
    {
        parserNext(p, 1);
        attr_start++;
    }

    if (parserPeek(p, 0)->kind == TOKEN_LBRACK)
    {
        parserNext(p, 1);
        attr_start++;
    }

    if (attr_start > 0)
    {
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

                arrPush(p->compiler, &attr.values, value);

                if (parserPeek(p, 0)->kind != TOKEN_RPAREN)
                {
                    if (!parserConsume(p, TOKEN_COMMA)) return NULL;
                }
            }

            if (!parserConsume(p, TOKEN_RPAREN)) return NULL;
        }

        arrPush(p->compiler, &attributes, attr);

        for (int i = 0; i < attr_start; ++i)
        {
            if (!parserConsume(p, TOKEN_RBRACK)) return NULL;
        }
    }

    switch (parserPeek(p, 0)->kind)
    {
    case TOKEN_SEMICOLON: {
        parserNext(p, 1);
        return NULL;
    }

    case TOKEN_UNIFORM:
    case TOKEN_GROUPSHARED:
    case TOKEN_CONST: {
        AstDecl *decl = parseVarDecl(p, false, &attributes, true, true, false);
        if (!decl) return NULL;

        if (decl->var.storage_class == VAR_STORAGE_CLASS_NONE)
        {
            decl->var.storage_class = VAR_STORAGE_CLASS_UNIFORM;
        }

        arrPush(p->compiler, &p->decls, decl);

        if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

        return decl;
    }

    case TOKEN_STRUCT: {
        Location decl_loc = parserBeginLoc(p);

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
            AstDecl *field_decl = parseVarDecl(p, false, NULL, false, false, true);
            field_decl->kind = DECL_STRUCT_FIELD;

            if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

            arrPush(p->compiler, &decl->struct_.fields, field_decl);
        }

        if (!parserConsume(p, TOKEN_RCURLY)) return NULL;

        if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

        parserEndLoc(p, &decl_loc);
        decl->loc = decl_loc;

        arrPush(p->compiler, &p->decls, decl);
        return decl;
    }

    case TOKEN_CBUFFER: {
        Location decl_loc = parserBeginLoc(p);

        parserNext(p, 1);

        Token *name_tok = parserConsume(p, TOKEN_IDENT);
        if (!name_tok) return NULL;

        if (parserPeek(p, 0)->kind == TOKEN_COLON)
        {
            // Parse register
            parserNext(p, 1);

            if (!parserConsume(p, TOKEN_REGISTER)) return NULL;

            if (!parserConsume(p, TOKEN_LPAREN)) return NULL;

            while (!parserIsAtEnd(p) && parserPeek(p, 0)->kind != TOKEN_RPAREN)
            {
                AstExpr *param = parseExpr(p);
                if (!param) return NULL;

                if (parserPeek(p, 0)->kind != TOKEN_RPAREN)
                {
                    if (!parserConsume(p, TOKEN_COMMA)) return NULL;
                }
            }

            if (!parserConsume(p, TOKEN_RPAREN)) return NULL;
        }

        ts__sbReset(&p->compiler->sb);
        ts__sbAppend(&p->compiler->sb, name_tok->str);
        ts__sbAppend(&p->compiler->sb, "#cbuffer_struct");
        char *struct_name = ts__sbBuild(&p->compiler->sb, &p->compiler->alloc);

        AstDecl *struct_decl = NEW(compiler, AstDecl);
        struct_decl->kind = DECL_STRUCT;
        struct_decl->attributes = attributes;
        struct_decl->name = struct_name;

        if (!parserConsume(p, TOKEN_LCURLY)) return NULL;

        while (parserPeek(p, 0)->kind != TOKEN_RCURLY)
        {
            AstDecl *field_decl = parseVarDecl(p, false, NULL, false, false, true);
            field_decl->kind = DECL_STRUCT_FIELD;

            if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

            arrPush(p->compiler, &struct_decl->struct_.fields, field_decl);
        }

        if (!parserConsume(p, TOKEN_RCURLY)) return NULL;

        arrPush(p->compiler, &p->decls, struct_decl); // Push anonymous struct declaration

        AstExpr *struct_name_expr = NEW(compiler, AstExpr);
        struct_name_expr->kind = EXPR_IDENT;
        struct_name_expr->ident.name = struct_name;

        AstExpr *type_expr = NEW(compiler, AstExpr);
        type_expr->loc = parserPeek(p, 0)->loc;
        type_expr->kind = EXPR_CONSTANT_BUFFER_TYPE;
        type_expr->buffer.sub_expr = struct_name_expr;

        AstDecl *decl = NEW(compiler, AstDecl);
        decl->kind = DECL_VAR;
        decl->name = NULL;
        decl->var.storage_class = VAR_STORAGE_CLASS_UNIFORM;
        decl->var.type_expr = type_expr;
        decl->attributes = attributes;

        arrPush(p->compiler, &p->decls, decl);

        for (size_t i = 0; i < struct_decl->struct_.fields.len; ++i)
        {
            AstDecl *field = struct_decl->struct_.fields.ptr[i];

            AstDecl *alias_decl = NEW(compiler, AstDecl);
            alias_decl->kind = DECL_ALIAS;
            alias_decl->loc = field->loc;
            alias_decl->name = field->name;
            alias_decl->alias.accessed = decl;

            // Push field access alias declaration
            arrPush(p->compiler, &p->decls, alias_decl);
        }

        parserEndLoc(p, &decl_loc);
        decl->loc = decl_loc;

        return decl;
    }

    default: {
        Location decl_loc = parserBeginLoc(p);

        size_t checkpoint = p->pos;

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
                Location param_decl_loc = parserBeginLoc(p);

                AstParameterKind param_kind = PARAM_IN;

                if (parserPeek(p, 0)->kind == TOKEN_IN)
                {
                    parserNext(p, 1);
                    param_kind = PARAM_IN;
                }
                else if (parserPeek(p, 0)->kind == TOKEN_OUT)
                {
                    parserNext(p, 1);
                    param_kind = PARAM_OUT;
                }
                else if (parserPeek(p, 0)->kind == TOKEN_INOUT)
                {
                    parserNext(p, 1);
                    param_kind = PARAM_OUT;
                }

                AstDecl *param_decl = parseVarDecl(p, true, NULL, true, false, true);
                if (!param_decl) return NULL;

                param_decl->var.parameter_kind = param_kind;

                if (parserPeek(p, 0)->kind == TOKEN_COLON)
                {
                    parserNext(p, 1);
                    Token *semantic_tok = parserConsume(p, TOKEN_IDENT);
                    if (!semantic_tok) return NULL;
                    param_decl->semantic = semantic_tok->str;
                }

                parserEndLoc(p, &param_decl_loc);
                param_decl->loc = param_decl_loc;

                arrPush(p->compiler, &decl->func.params, param_decl);

                if (parserPeek(p, 0)->kind != TOKEN_RPAREN)
                {
                    if (!parserConsume(p, TOKEN_COMMA)) return NULL;
                }
            }

            if (!parserConsume(p, TOKEN_RPAREN)) return NULL;

            if (parserPeek(p, 0)->kind == TOKEN_COLON)
            {
                parserNext(p, 1);
                Token *semantic_tok = parserConsume(p, TOKEN_IDENT);
                if (!semantic_tok) return NULL;
                decl->semantic = semantic_tok->str;
            }

            if (!parserConsume(p, TOKEN_LCURLY)) return NULL;

            while (parserPeek(p, 0)->kind != TOKEN_RCURLY)
            {
                AstStmt *stmt = parseStmt(p);
                if (stmt)
                {
                    arrPush(p->compiler, &decl->func.stmts, stmt);
                }
            }

            if (!parserConsume(p, TOKEN_RCURLY)) return NULL;

            parserEndLoc(p, &decl_loc);
            decl->loc = decl_loc;

            arrPush(p->compiler, &p->decls, decl);
            return decl;
        }
        else if (parserPeek(p, 0)->kind == TOKEN_SEMICOLON ||
                 parserPeek(p, 0)->kind == TOKEN_COLON)
        {
            // Parse top level uniform variable declaration

            p->pos = checkpoint;

            AstDecl *decl = parseVarDecl(p, false, &attributes, true, true, false);
            if (!decl) return NULL;

            if (decl->var.storage_class == VAR_STORAGE_CLASS_NONE)
            {
                decl->var.storage_class = VAR_STORAGE_CLASS_UNIFORM;
            }

            if (parserPeek(p, 0)->kind == TOKEN_COLON)
            {
                parserNext(p, 1);

                if (!parserConsume(p, TOKEN_REGISTER)) return NULL;

                if (!parserConsume(p, TOKEN_LPAREN)) return NULL;

                while (!parserIsAtEnd(p) && parserPeek(p, 0)->kind != TOKEN_RPAREN)
                {
                    AstExpr *param = parseExpr(p);
                    if (!param) return NULL;

                    if (parserPeek(p, 0)->kind != TOKEN_RPAREN)
                    {
                        if (!parserConsume(p, TOKEN_COMMA)) return NULL;
                    }
                }

                if (!parserConsume(p, TOKEN_RPAREN)) return NULL;
            }

            if (!parserConsume(p, TOKEN_SEMICOLON)) return NULL;

            arrPush(p->compiler, &p->decls, decl);
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

ArrayOfAstDeclPtr ts__parse(TsCompiler *compiler, ArrayOfToken tokens)
{
    Parser *p = NEW(compiler, Parser);
    assert(compiler);
    assert(p);

    memset(p, 0, sizeof(*p));
    p->compiler = compiler;
    p->tokens = tokens.ptr;
    p->token_count = tokens.len;

    while (!parserIsAtEnd(p))
    {
        AstDecl *decl = parseTopLevel(p);
        if (!decl)
        {
            // Just stop trying to parse more declarations
            // if we already found errors
            assert(compiler->errors.len > 0);
            break;
        }
    }

    return p->decls;
}
