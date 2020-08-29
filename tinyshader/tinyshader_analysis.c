#include "tinyshader_internal.h"

static void scopeInit(Scope *scope, Scope *parent, AstDecl *owner)
{
    memset(scope, 0, sizeof(*scope));
    ts__hashInit(&scope->map, 0);
    scope->parent = parent;
    scope->owner = owner;
}

static void scopeDestroy(Scope *scope)
{
    ts__hashDestroy(&scope->map);
}

static AstDecl *scopeGetLocal(Scope *scope, const char *name)
{
    AstDecl *symbol = NULL;
    bool found = ts__hashGet(&scope->map, name, (void **)&symbol);
    if (found) return symbol;
    return NULL;
}

static AstDecl *scopeGetGlobal(Scope *scope, const char *name)
{
    if (scope->parent)
    {
        AstDecl *sym = scopeGetGlobal(scope->parent, name);
        if (sym) return sym;
    }

    return scopeGetLocal(scope, name);
}

static bool scopeAdd(Scope *scope, const char *name, AstDecl *decl)
{
    assert(scope);

    if (scopeGetLocal(scope, name)) return false;

    ts__hashSet(&scope->map, name, decl);

    return true;
}

static void scopeClone(Scope *new_scope, Scope *old_scope, AstDecl *new_owner)
{
    memcpy(new_scope, old_scope, sizeof(Scope));
    new_scope->owner = new_owner;
}

static char *typeToString(TsCompiler *compiler, AstType *type)
{
    if (type->string) return type->string;

    char *prefix = NULL;
    char *storage_class = NULL;
    char *sub = NULL;
    char *postfix = NULL;

    switch (type->kind)
    {
    case TYPE_VOID: prefix = "void"; break;

    case TYPE_TYPE: prefix = "type"; break;

    case TYPE_BOOL: prefix = "bool"; break;

    case TYPE_FLOAT:
        ts__sbReset(&compiler->sb);
        ts__sbSprintf(&compiler->sb, "float%u", type->float_.bits);
        prefix = ts__sbBuild(&compiler->sb, &compiler->alloc);
        break;

    case TYPE_INT:
        ts__sbReset(&compiler->sb);

        if (type->int_.is_signed)
            ts__sbAppend(&compiler->sb, "int");
        else
            ts__sbAppend(&compiler->sb, "uint");

        ts__sbSprintf(&compiler->sb, "%u", type->int_.bits);
        prefix = ts__sbBuild(&compiler->sb, &compiler->alloc);
        break;

    case TYPE_VECTOR:
        ts__sbReset(&compiler->sb);
        ts__sbSprintf(&compiler->sb, "vec%u", type->vector.size);
        prefix = ts__sbBuild(&compiler->sb, &compiler->alloc);

        sub = typeToString(compiler, type->vector.elem_type);
        break;
    case TYPE_MATRIX:
        ts__sbReset(&compiler->sb);
        ts__sbSprintf(&compiler->sb, "mat%u", type->matrix.col_count);
        prefix = ts__sbBuild(&compiler->sb, &compiler->alloc);

        sub = typeToString(compiler, type->matrix.col_type);
        break;

    case TYPE_POINTER: {
        prefix = "ptr";

        switch (type->ptr.storage_class)
        {
        case SpvStorageClassInput: storage_class = "input"; break;
        case SpvStorageClassOutput: storage_class = "output"; break;
        case SpvStorageClassUniformConstant: storage_class = "constant"; break;
        case SpvStorageClassUniform: storage_class = "uniform"; break;
        case SpvStorageClassStorageBuffer: storage_class = "storage"; break;
        case SpvStorageClassFunction: storage_class = "function"; break;

        default: assert(0); break;
        }

        assert(type->ptr.sub);
        sub = typeToString(compiler, type->ptr.sub);
        break;
    }

    case TYPE_FUNC: {
        prefix = "func";

        char *return_type = typeToString(compiler, type->func.return_type);
        char **params = NEW_ARRAY(compiler, char *, type->func.param_count);
        for (uint32_t i = 0; i < type->func.param_count; ++i)
        {
            params[i] = typeToString(compiler, type->func.params[i]);
        }

        ts__sbReset(&compiler->sb);
        ts__sbAppend(&compiler->sb, return_type);
        ts__sbAppend(&compiler->sb, "$");
        ts__sbSprintf(&compiler->sb, "%u", type->func.param_count);
        for (uint32_t i = 0; i < type->func.param_count; ++i)
        {
            if (i != 0) ts__sbAppend(&compiler->sb, "_");
            ts__sbAppend(&compiler->sb, params[i]);
        }
        sub = ts__sbBuild(&compiler->sb, &compiler->alloc);

        break;
    }

    case TYPE_STRUCT: {
        ts__sbReset(&compiler->sb);
        ts__sbSprintf(
            &compiler->sb, "struct%u%s", strlen(type->struct_.name), type->struct_.name);
        prefix = ts__sbBuild(&compiler->sb, &compiler->alloc);
        break;
    }

    case TYPE_SAMPLER: {
        prefix = "sampler";
        break;
    }

    case TYPE_IMAGE: {
        prefix = "image";

        sub = typeToString(compiler, type->image.sampled_type);

        switch (type->image.dim)
        {
        case SpvDimCube: postfix = "Cube"; break;
        case SpvDim1D: postfix = "1D"; break;
        case SpvDim2D: postfix = "2D"; break;
        case SpvDim3D: postfix = "3D"; break;
        default: assert(0); break;
        }

        break;
    }

    case TYPE_SAMPLED_IMAGE: {
        prefix = "sampledImage";
        sub = typeToString(compiler, type->sampled_image.image_type);
        break;
    }
    }

    ts__sbReset(&compiler->sb);

    assert(prefix);
    ts__sbAppend(&compiler->sb, prefix);

    if (storage_class)
    {
        ts__sbAppend(&compiler->sb, "_");
        ts__sbAppend(&compiler->sb, storage_class);
    }

    if (sub)
    {
        ts__sbAppend(&compiler->sb, "_");
        ts__sbAppend(&compiler->sb, sub);
    }

    if (postfix)
    {
        ts__sbAppend(&compiler->sb, "_");
        ts__sbAppend(&compiler->sb, postfix);
    }

    type->string = ts__sbBuild(&compiler->sb, &compiler->alloc);
    return type->string;
}

static uint32_t padToAlignment(uint32_t current, uint32_t align)
{
    assert(align >= 1);

    uint32_t minum = current & (align - 1);
    if (minum)
    {
        assert((current % align) != 0);
        current += align - minum;
    }

    return current;
}

static uint32_t typeAlignOf(Module *m, AstType *type)
{
    if (type->align > 0) return type->align;

    uint32_t align = 1;
    switch (type->kind)
    {
    case TYPE_INT: align = type->int_.bits / 8; break;
    case TYPE_FLOAT: align = type->float_.bits / 8; break;

    case TYPE_VECTOR: {
        switch (type->vector.size)
        {
        case 2: align = typeAlignOf(m, type->vector.elem_type) * 2; break;
        case 3:
        case 4: align = typeAlignOf(m, type->vector.elem_type) * 4; break;
        default: assert(0); break;
        }

        break;
    }

    case TYPE_MATRIX: {
        align = typeAlignOf(m, type->matrix.col_type);
        break;
    }

    case TYPE_STRUCT: {
        for (AstType **field = type->struct_.fields;
             field != type->struct_.fields + type->struct_.field_count;
             ++field)
        {
            uint32_t field_align = typeAlignOf(m, *field);
            if (field_align > align) align = field_align;
        }

        break;
    }

    case TYPE_IMAGE:
    case TYPE_SAMPLER:
    case TYPE_SAMPLED_IMAGE:
    case TYPE_POINTER:
    case TYPE_BOOL:
    case TYPE_FUNC:
    case TYPE_VOID:
    case TYPE_TYPE: align = 0; break;
    }

    type->align = align;

    return type->align;
}

static uint32_t typeSizeOf(Module *m, AstType *type)
{
    if (type->size > 0) return type->size;

    uint32_t size = 0;

    switch (type->kind)
    {
    case TYPE_INT: size = type->int_.bits / 8; break;
    case TYPE_FLOAT: size = type->float_.bits / 8; break;

    case TYPE_VECTOR: {
        switch (type->vector.size)
        {
        case 2: size = typeSizeOf(m, type->vector.elem_type) * 2; break;
        case 3: size = typeSizeOf(m, type->vector.elem_type) * 3; break;
        case 4: size = typeSizeOf(m, type->vector.elem_type) * 4; break;
        default: assert(0); break;
        }
        break;
    }

    case TYPE_MATRIX: {
        size = typeSizeOf(m, type->matrix.col_type) * type->matrix.col_count;
        break;
    }

    case TYPE_STRUCT: {
        for (size_t i = 0; i < type->struct_.field_count; ++i)
        {
            AstType *field = type->struct_.fields[i];
            uint32_t field_align = typeAlignOf(m, field);
            size = padToAlignment(size, field_align); // Add padding

            IRMemberDecoration member_dec = {0};
            member_dec.kind = SpvDecorationOffset;
            member_dec.member_index = i;
            member_dec.value = size;
            arrPush(type->struct_.field_decorations, member_dec);

            size += typeSizeOf(m, field);
        }

        break;
    }

    case TYPE_IMAGE:
    case TYPE_SAMPLER:
    case TYPE_SAMPLED_IMAGE:
    case TYPE_POINTER:
    case TYPE_BOOL:
    case TYPE_FUNC:
    case TYPE_VOID:
    case TYPE_TYPE: size = 0; break;
    }

    type->size = size;
    return type->size;
}

static AstType *getCachedType(Module *m, AstType *type)
{
    char *type_string = typeToString(m->compiler, type);
    assert(type_string);
    assert(strlen(type_string) > 0);

    AstType *found_type = NULL;
    if (ts__hashGet(&m->type_cache, type_string, (void **)&found_type))
    {
        assert(found_type);
        return found_type;
    }

    ts__hashSet(&m->type_cache, type_string, type);

    typeSizeOf(m, type);

    return type;
}

static AstType *newBasicType(Module *m, AstTypeKind kind)
{
    AstType *ty = NEW(m->compiler, AstType);
    ty->kind = kind;
    return getCachedType(m, ty);
}

static AstType *newPointerType(Module *m, SpvStorageClass storage_class, AstType *sub)
{
    AstType *ty = NEW(m->compiler, AstType);
    ty->kind = TYPE_POINTER;
    ty->ptr.storage_class = storage_class;
    ty->ptr.sub = sub;
    return getCachedType(m, ty);
}

static AstType *newVectorType(Module *m, AstType *elem_type, uint32_t size)
{
    AstType *ty = NEW(m->compiler, AstType);
    ty->kind = TYPE_VECTOR;
    ty->vector.elem_type = elem_type;
    ty->vector.size = size;
    return getCachedType(m, ty);
}

static AstType *newMatrixType(Module *m, AstType *col_type, uint32_t col_count)
{
    AstType *ty = NEW(m->compiler, AstType);
    ty->kind = TYPE_MATRIX;
    ty->matrix.col_type = col_type;
    ty->matrix.col_count = col_count;
    return getCachedType(m, ty);
}

static AstType *newFloatType(Module *m, uint32_t bits)
{
    AstType *ty = NEW(m->compiler, AstType);
    ty->kind = TYPE_FLOAT;
    ty->float_.bits = bits;
    return getCachedType(m, ty);
}

static AstType *newIntType(Module *m, uint32_t bits, bool is_signed)
{
    AstType *ty = NEW(m->compiler, AstType);
    ty->kind = TYPE_INT;
    ty->int_.bits = bits;
    ty->int_.is_signed = is_signed;
    return getCachedType(m, ty);
}

static AstType *
newFuncType(Module *m, AstType *return_type, AstType **params, uint32_t param_count)
{
    AstType *ty = NEW(m->compiler, AstType);
    ty->kind = TYPE_FUNC;
    ty->func.return_type = return_type;
    if (param_count > 0)
    {
        ty->func.param_count = param_count;
        ty->func.params = NEW_ARRAY(m->compiler, AstType *, param_count);
        memcpy(ty->func.params, params, sizeof(AstType *) * param_count);
    }
    return getCachedType(m, ty);
}

static AstType *newStructType(
    Module *m, char *name, AstType **fields, AstDecl **field_decls, uint32_t field_count)
{
    AstType *ty = NEW(m->compiler, AstType);
    ty->kind = TYPE_STRUCT;
    ty->struct_.name = name;

    if (field_count > 0)
    {
        ty->struct_.field_count = field_count;

        ty->struct_.fields = NEW_ARRAY(m->compiler, AstType *, field_count);
        memcpy(ty->struct_.fields, fields, sizeof(AstType *) * field_count);

        ty->struct_.field_decls = NEW_ARRAY(m->compiler, AstDecl *, field_count);
        memcpy(ty->struct_.field_decls, field_decls, sizeof(AstDecl *) * field_count);
    }

    return getCachedType(m, ty);
}

static AstType *newImageType(Module *m, AstType *sampled_type, SpvDim dim)
{
    AstType *ty = NEW(m->compiler, AstType);
    ty->kind = TYPE_IMAGE;
    ty->image.sampled_type = sampled_type;
    ty->image.dim = dim;
    ty->image.depth = 0;
    ty->image.arrayed = 0;
    ty->image.multisampled = 0;
    ty->image.sampled = 1;
    ty->image.format = SpvImageFormatUnknown;
    return getCachedType(m, ty);
}

static AstType *newSampledImageType(Module *m, AstType *image_type)
{
    AstType *ty = NEW(m->compiler, AstType);
    ty->kind = TYPE_SAMPLED_IMAGE;
    ty->sampled_image.image_type = image_type;
    return getCachedType(m, ty);
}

static bool isTypeCastable(AstType *src_type, AstType *dst_type)
{
    if (src_type == dst_type)
    {
        return true;
    }
    else if (src_type->kind == TYPE_INT)
    {
        if (src_type->int_.is_signed)
        {
            if (dst_type->kind == TYPE_INT)
            {
                if (dst_type->int_.is_signed)
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
            else if (dst_type->kind == TYPE_FLOAT)
            {
                return true;
            }
            else
            {
                return false;
            }
        }
        else
        {
            if (dst_type->kind == TYPE_INT)
            {
                if (dst_type->int_.is_signed)
                {
                    return false;
                }
                else
                {
                    return true;
                }
            }
            else if (dst_type->kind == TYPE_FLOAT)
            {
                return true;
            }
            else
            {
                return false;
            }
        }
    }
    else if (src_type->kind == TYPE_FLOAT)
    {
        if (dst_type->kind == TYPE_INT)
        {
            if (dst_type->int_.is_signed)
            {
                return true;
            }
            else
            {
                return true;
            }
        }
        else if (dst_type->kind == TYPE_FLOAT)
        {
            return true;
        }
        else
        {
            return false;
        }
    }
    else
    {
        return false;
    }

    assert(0);
}

AstType *ts__getScalarType(AstType *type)
{
    switch (type->kind)
    {
    case TYPE_FLOAT:
    case TYPE_INT: {
        return type;
    }

    case TYPE_VECTOR: {
        return type->vector.elem_type;
    }

    default: break;
    }

    return NULL;
}

// Returns a type that can be used in a comparison operation
AstType *ts__getComparableType(AstType *type)
{
    switch (type->kind)
    {
    case TYPE_FLOAT:
    case TYPE_BOOL:
    case TYPE_INT: {
        return type;
    }

    default: break;
    }

    return NULL;
}

// Returns a type that can be used in a logical operation
AstType *ts__getLogicalType(AstType *type)
{
    switch (type->kind)
    {
    case TYPE_BOOL:
    case TYPE_INT: {
        return type;
    }

    default: break;
    }

    return NULL;
}

AstType *ts__getElemType(AstType *type)
{
    switch (type->kind)
    {
    case TYPE_VECTOR: {
        return type->vector.elem_type;
    }

    default: break;
    }

    return type;
}

static bool canCoerceExprToScalarType(Analyzer *a, AstExpr *expr)
{
    switch (expr->kind)
    {
    case EXPR_PRIMARY: {
        switch (expr->primary.token->kind)
        {
        case TOKEN_INT_LIT: {
            return true;
        }

        default: break;
        }
        break;
    }

    case EXPR_BINARY: {
        switch (expr->binary.op)
        {
        case BINOP_ADD:
        case BINOP_SUB:
        case BINOP_MUL:
        case BINOP_DIV:
        case BINOP_MOD: {
            return canCoerceExprToScalarType(a, expr->binary.left) &&
                   canCoerceExprToScalarType(a, expr->binary.right);
        }

        default: break;
        }

        break;
    }

    case EXPR_UNARY: {
        switch (expr->unary.op)
        {
        case UNOP_NEG: {
            return canCoerceExprToScalarType(a, expr->unary.right);
        }

        default: break;
        }

        break;
    }

    default: break;
    }

    return false;
}

static void tryCoerceExprToScalarType(Analyzer *a, AstExpr *expr, AstType *scalar_type)
{
    switch (expr->kind)
    {
    case EXPR_PRIMARY: {
        if (canCoerceExprToScalarType(a, expr))
        {
            expr->type = scalar_type;
        }
        break;
    }

    case EXPR_BINARY: {
        if (canCoerceExprToScalarType(a, expr))
        {
            expr->binary.left->type = scalar_type;
            expr->binary.right->type = scalar_type;
            expr->type = scalar_type;
        }
        break;
    }

    case EXPR_UNARY: {
        if (canCoerceExprToScalarType(a, expr))
        {
            expr->unary.right->type = scalar_type;
            expr->type = scalar_type;
        }
        break;
    }

    default: break;
    }
}

static void analyzerAnalyzeExpr(Analyzer *a, AstExpr *expr, AstType *expected_type);
static void analyzerAnalyzeStmt(Analyzer *a, AstStmt *stmt);
static void analyzerAnalyzeDecl(Analyzer *a, AstDecl *decl);

static void analyzerPushScope(Analyzer *a, Scope *scope)
{
    assert(scope);
    arrPush(a->scope_stack, scope);
    if (scope->owner && scope->owner->kind == DECL_FUNC)
    {
        a->scope_func = scope->owner;
    }
}

static void analyzerPopScope(Analyzer *a, Scope *scope)
{
    assert(scope);
    assert(arrLength(a->scope_stack) > 0);

    Scope *last_scope = a->scope_stack[arrLength(a->scope_stack) - 1];
    assert(last_scope == scope);

    arrPop(a->scope_stack);

    a->scope_func = NULL;
    for (uint32_t i = 0; i < arrLength(a->scope_stack); ++i)
    {
        if (a->scope_stack[i]->owner && a->scope_stack[i]->owner->kind == DECL_FUNC)
        {
            a->scope_func = a->scope_stack[i]->owner;
        }
    }
}

static Scope *analyzerCurrentScope(Analyzer *a)
{
    if (arrLength(a->scope_stack) > 0)
    {
        return a->scope_stack[arrLength(a->scope_stack) - 1];
    }
    return NULL;
}

static void analyzerTryRegisterDecl(Analyzer *a, AstDecl *decl)
{
    if (!decl->name) return;
    if (strcmp(decl->name, "_") == 0) return; // We don't register underscore names

    Scope *scope = analyzerCurrentScope(a);

    if (scopeGetLocal(scope, decl->name))
    {
        ts__addErr(a->compiler, &decl->loc, "duplicate declaration");
    }
    else
    {
        scopeAdd(scope, decl->name, decl);
    }
}

static void analyzerAnalyzeExpr(Analyzer *a, AstExpr *expr, AstType *expected_type)
{
    TsCompiler *compiler = a->compiler;
    Scope *scope = analyzerCurrentScope(a);
    Module *m = a->module;

    expr->inhabited_scope = scope;

    switch (expr->kind)
    {
    case EXPR_PRIMARY: {
        switch (expr->primary.token->kind)
        {
        case TOKEN_VOID: {
            expr->as_type = newBasicType(a->module, TYPE_VOID);
            expr->type = newBasicType(a->module, TYPE_TYPE);
            break;
        }

        case TOKEN_BOOL: {
            expr->as_type = newBasicType(a->module, TYPE_BOOL);
            expr->type = newBasicType(a->module, TYPE_TYPE);
            break;
        }

        case TOKEN_FLOAT: {
            expr->as_type = newFloatType(a->module, 32);
            expr->type = newBasicType(a->module, TYPE_TYPE);
            break;
        }

        case TOKEN_INT: {
            expr->as_type = newIntType(a->module, 32, true);
            expr->type = newBasicType(a->module, TYPE_TYPE);
            break;
        }

        case TOKEN_UINT: {
            expr->as_type = newIntType(a->module, 32, false);
            expr->type = newBasicType(a->module, TYPE_TYPE);
            break;
        }

        case TOKEN_VECTOR_TYPE: {
            AstType *elem_type = NULL;

            switch (expr->primary.token->vector_type.elem_type)
            {
            case TOKEN_FLOAT: {
                elem_type = newFloatType(a->module, 32);
                break;
            }

            case TOKEN_INT: {
                elem_type = newIntType(a->module, 32, true);
                break;
            }

            case TOKEN_UINT: {
                elem_type = newIntType(a->module, 32, false);
                break;
            }

            default: assert(0); break;
            }

            assert(elem_type);

            expr->as_type = newVectorType(
                a->module, elem_type, (uint32_t)expr->primary.token->vector_type.dim);
            expr->type = newBasicType(a->module, TYPE_TYPE);
            break;
        }

        case TOKEN_MATRIX_TYPE: {
            AstType *elem_type = NULL;

            switch (expr->primary.token->matrix_type.elem_type)
            {
            case TOKEN_FLOAT: {
                elem_type = newFloatType(a->module, 32);
                break;
            }

            case TOKEN_INT: {
                elem_type = newIntType(a->module, 32, true);
                break;
            }

            case TOKEN_UINT: {
                elem_type = newIntType(a->module, 32, false);
                break;
            }

            default: assert(0); break;
            }

            assert(elem_type);

            AstType *col_type = newVectorType(
                a->module, elem_type, (uint32_t)expr->primary.token->matrix_type.dim1);
            expr->as_type = newMatrixType(
                a->module, col_type, (uint32_t)expr->primary.token->matrix_type.dim2);
            expr->type = newBasicType(a->module, TYPE_TYPE);
            break;
        }

        case TOKEN_FALSE:
        case TOKEN_TRUE: {
            expr->type = newBasicType(a->module, TYPE_BOOL);
            break;
        }

        case TOKEN_FLOAT_LIT: {
            if (expected_type && expected_type->kind == TYPE_FLOAT)
            {
                expr->type = expected_type;
            }
            else
            {
                expr->type = newFloatType(a->module, 32);
            }
            break;
        }

        case TOKEN_INT_LIT: {
            if (expected_type &&
                (expected_type->kind == TYPE_INT || expected_type->kind == TYPE_FLOAT))
            {
                expr->type = expected_type;
            }
            else
            {
                expr->type = newIntType(a->module, 32, true);
            }

            expr->resolved_int = NEW(compiler, int64_t);
            *expr->resolved_int = expr->primary.token->int_;

            break;
        }

        default: assert(0); break;
        }
        break;
    }

    case EXPR_IDENT: {
        AstDecl *decl = scopeGetGlobal(scope, expr->ident.name);
        if (!decl)
        {
            ts__addErr(compiler, &expr->loc, "unknown identifier");
            break;
        }

        if (decl->kind == DECL_FUNC)
        {
            decl->func.called = true;
        }

        if (decl->kind == DECL_VAR)
        {
            expr->assignable = true;
        }

        expr->ident.decl = decl;

        expr->type = decl->type;
        expr->as_type = decl->as_type;
        expr->scope = decl->scope;
        expr->resolved_int = decl->resolved_int;
        break;
    }

    case EXPR_ACCESS: {
        AstExpr *left = expr->access.base;
        analyzerAnalyzeExpr(a, left, NULL);

        assert(arrLength(expr->access.chain) > 0);

        for (uint32_t i = 0; i < arrLength(expr->access.chain); ++i)
        {
            AstExpr *right = expr->access.chain[i];

            if (!left->type)
            {
                assert(arrLength(compiler->errors) > 0);
                break;
            }

            if (left->type->kind == TYPE_STRUCT)
            {
                assert(left->scope);

                analyzerPushScope(a, left->scope);
                analyzerAnalyzeExpr(a, right, NULL);
                analyzerPopScope(a, left->scope);
            }
            else if (left->type->kind == TYPE_VECTOR)
            {
                assert(right->kind == EXPR_IDENT);

                char *selector = right->ident.name;
                uint32_t new_vec_dim = strlen(selector);
                if (new_vec_dim > 4)
                {
                    ts__addErr(
                        compiler,
                        &right->loc,
                        "vector shuffle must select at most 4 elements");
                    break;
                }

                uint32_t *positions = NEW_ARRAY(compiler, uint32_t, new_vec_dim);

                for (uint32_t j = 0; j < new_vec_dim; ++j)
                {
                    bool valid = true;
                    switch (selector[j])
                    {
                    case 'r':
                    case 'x': positions[j] = 0; break;
                    case 'g':
                    case 'y': positions[j] = 1; break;
                    case 'b':
                    case 'z': positions[j] = 2; break;
                    case 'a':
                    case 'w': positions[j] = 3; break;

                    default:
                        ts__addErr(compiler, &right->loc, "invalid vector shuffle");
                        valid = false;
                        break;
                    }

                    if (positions[j] >= left->type->vector.size)
                    {
                        ts__addErr(compiler, &right->loc, "invalid vector shuffle");
                        break;
                        valid = false;
                    }

                    if (!valid) break;
                }

                right->ident.shuffle_indices = positions;
                right->ident.shuffle_index_count = new_vec_dim;

                if (new_vec_dim == 1)
                {
                    right->type = left->type->vector.elem_type;
                }
                else
                {
                    right->type =
                        newVectorType(m, left->type->vector.elem_type, new_vec_dim);
                }
            }
            else
            {
                ts__addErr(compiler, &left->loc, "expression is not accessible");
                break;
            }

            if (i == (arrLength(expr->access.chain) - 1))
            {
                expr->type = right->type;
                expr->as_type = right->as_type;
                expr->scope = right->scope;
                expr->resolved_int = right->resolved_int;
            }

            left = right;
        }

        expr->assignable = expr->access.base->assignable;

        break;
    }

    case EXPR_FUNC_CALL: {
        AstExpr *func_expr = expr->func_call.func_expr;

        // Builtin method call
        if (func_expr->kind == EXPR_ACCESS)
        {
            AstExpr *method_name_expr =
                func_expr->access.chain[arrLength(func_expr->access.chain) - 1];
            assert(method_name_expr->kind == EXPR_IDENT);
            char *method_name = method_name_expr->ident.name;

            arrPop(func_expr->access
                       .chain); // Remove last element from access (the method name)

            if (arrLength(func_expr->access.chain) == 0)
            {
                func_expr = func_expr->access.base;
            }

            expr->func_call.self_param = func_expr;
            expr->func_call.func_expr = method_name_expr;

            analyzerAnalyzeExpr(a, expr->func_call.self_param, NULL);
            AstType *self_type = expr->func_call.self_param->type;
            if (!self_type)
            {
                break;
            }

            if (self_type->kind == TYPE_IMAGE && strcmp(method_name, "Sample") == 0)
            {
                AstType *texture_component_type = self_type->image.sampled_type;

                uint32_t func_param_count = 2;
                AstType **func_param_types =
                    NEW_ARRAY(compiler, AstType *, func_param_count);

                func_param_types[0] = newBasicType(m, TYPE_SAMPLER);

                AstType *float_type = newFloatType(m, 32);
                switch (self_type->image.dim)
                {
                case SpvDim1D:
                    func_param_types[1] = newVectorType(m, float_type, 1);
                    break;
                case SpvDim2D:
                    func_param_types[1] = newVectorType(m, float_type, 2);
                    break;
                case SpvDim3D:
                case SpvDimCube:
                    func_param_types[1] = newVectorType(m, float_type, 3);
                    break;

                default: assert(0); break;
                }

                if (func_param_count != arrLength(expr->func_call.params))
                {
                    ts__addErr(
                        compiler,
                        &expr->loc,
                        "wrong amount of parameters for function call");
                    break;
                }

                for (uint32_t i = 0; i < arrLength(expr->func_call.params); ++i)
                {
                    AstExpr *param = expr->func_call.params[i];
                    analyzerAnalyzeExpr(a, param, func_param_types[i]);
                }

                expr->type = texture_component_type;
            }
            else
            {
                ts__addErr(compiler, &expr->loc, "invalid method call");
            }

            break;
        }

        analyzerAnalyzeExpr(a, func_expr, NULL);
        AstType *func_type = func_expr->type;
        if (!func_type)
        {
            break;
        }

        if (func_type->kind == TYPE_TYPE)
        {
            // Type constructor

            AstType *constructed_type = func_expr->as_type;
            assert(constructed_type);

            expr->type = constructed_type;

            uint32_t param_count = arrLength(expr->func_call.params);
            AstExpr **params = expr->func_call.params;

            if (constructed_type->kind == TYPE_VECTOR)
            {
                if (param_count == constructed_type->vector.size)
                {
                    for (uint32_t i = 0; i < param_count; ++i)
                    {
                        analyzerAnalyzeExpr(
                            a, params[i], constructed_type->vector.elem_type);
                    }
                }
                else if (param_count == 1)
                {
                    analyzerAnalyzeExpr(a, params[0], constructed_type->vector.elem_type);
                }
                else
                {
                    ts__addErr(
                        compiler, &expr->loc, "invalid parameter count for constructor");
                    break;
                }
            }
            else if (param_count == 1)
            {
                analyzerAnalyzeExpr(a, params[0], NULL);
                if (!params[0]->type) break;

                if (!isTypeCastable(params[0]->type, constructed_type))
                {
                    ts__addErr(
                        compiler, &params[0]->loc, "value is not castable to this type");
                    break;
                }
            }
            else
            {
                ts__addErr(compiler, &expr->loc, "invalid constructor");
                break;
            }
        }
        else if (func_type->kind == TYPE_FUNC)
        {
            // Actual function call

            if (func_type->func.param_count != arrLength(expr->func_call.params))
            {
                ts__addErr(
                    compiler, &expr->loc, "wrong amount of parameters for function call");
                break;
            }

            for (uint32_t i = 0; i < func_type->func.param_count; ++i)
            {
                AstExpr *param = expr->func_call.params[i];
                AstType *param_expected = func_type->func.params[i];
                if (param_expected->kind == TYPE_POINTER)
                {
                    param_expected = param_expected->ptr.sub;
                }
                analyzerAnalyzeExpr(a, param, param_expected);
            }

            expr->type = func_type->func.return_type;
        }
        else
        {
            ts__addErr(
                compiler,
                &expr->func_call.func_expr->loc,
                "expression does not represent a function");
        }

        break;
    }

    case EXPR_BUILTIN_CALL: {
        uint32_t param_count = arrLength(expr->builtin_call.params);
        AstExpr **params = expr->builtin_call.params;

        bool got_param_types = true;

        for (uint32_t i = 0; i < param_count; ++i)
        {
            AstExpr *param = params[i];
            analyzerAnalyzeExpr(a, param, NULL);
            if (!param->type)
            {
                got_param_types = false;
                continue;
            }
        }

        if (!got_param_types) break;

        switch (expr->builtin_call.kind)
        {
        case IR_BUILTIN_DOT: {
            if (param_count != 2)
            {
                ts__addErr(compiler, &expr->loc, "dot needs 2 parameters");
                break;
            }

            AstExpr *a = params[0];
            AstExpr *b = params[1];

            if ((a->type->kind != TYPE_VECTOR) || (b->type->kind != TYPE_VECTOR))
            {
                ts__addErr(compiler, &expr->loc, "dot operates on vectors");
                break;
            }

            if (a->type != b->type)
            {
                ts__addErr(compiler, &expr->loc, "dot cannot operate on different types");
                break;
            }

            expr->type = a->type->vector.elem_type;
            break;
        }

        case IR_BUILTIN_CROSS: {
            if (param_count != 2)
            {
                ts__addErr(compiler, &expr->loc, "cross needs 2 parameters");
                break;
            }

            AstExpr *a = params[0];
            AstExpr *b = params[1];

            if ((a->type->kind != TYPE_VECTOR) || (b->type->kind != TYPE_VECTOR))
            {
                ts__addErr(compiler, &expr->loc, "cross operates on vectors");
                break;
            }

            if (a->type != b->type)
            {
                ts__addErr(
                    compiler, &expr->loc, "cross cannot operate on different types");
                break;
            }

            if (a->type->vector.size != 3)
            {
                ts__addErr(compiler, &expr->loc, "cross operates on 3D vectors");
                break;
            }

            expr->type = a->type;
            break;
        }

        case IR_BUILTIN_LENGTH: {
            if (param_count != 1)
            {
                ts__addErr(compiler, &expr->loc, "length needs 1 parameter");
                break;
            }

            AstExpr *a = params[0];

            if (a->type->kind != TYPE_VECTOR)
            {
                ts__addErr(compiler, &expr->loc, "length operates on a vector");
                break;
            }

            expr->type = a->type->vector.elem_type;
            break;
        }

        case IR_BUILTIN_NORMALIZE: {
            if (param_count != 1)
            {
                ts__addErr(compiler, &expr->loc, "normalize needs 1 parameter");
                break;
            }

            AstExpr *a = params[0];

            if (a->type->kind != TYPE_VECTOR)
            {
                ts__addErr(compiler, &expr->loc, "normalize operates on a vector");
                break;
            }

            expr->type = a->type;
            break;
        }

        case IR_BUILTIN_MUL: {
            if (param_count != 2)
            {
                ts__addErr(compiler, &expr->loc, "dot needs 2 parameters");
                break;
            }

            AstExpr *a = params[0];
            AstExpr *b = params[1];

            if (!((a->type->kind == TYPE_VECTOR && b->type->kind == TYPE_MATRIX) ||
                  (a->type->kind == TYPE_MATRIX && b->type->kind == TYPE_VECTOR) ||
                  (a->type->kind == TYPE_VECTOR && b->type->kind == TYPE_VECTOR) ||
                  (a->type->kind == TYPE_MATRIX && b->type->kind == TYPE_MATRIX)))
            {
                ts__addErr(compiler, &expr->loc, "invalid parameters for mul");
                break;
            }

            if (a->type->kind == TYPE_VECTOR && b->type->kind == TYPE_MATRIX)
            {
                // Matrix times vector, yes, it's backwards
                if (a->type != b->type->matrix.col_type)
                {
                    ts__addErr(
                        compiler,
                        &expr->loc,
                        "mismatched matrix columns with vector type");
                    break;
                }

                expr->type = a->type;
            }
            else if (a->type->kind == TYPE_MATRIX && b->type->kind == TYPE_VECTOR)
            {
                // Vector times matrix, yes, it's backwards
                if (b->type != a->type->matrix.col_type)
                {
                    ts__addErr(
                        compiler,
                        &expr->loc,
                        "mismatched matrix columns with vector type");
                    break;
                }

                expr->type = b->type;
            }
            else if (a->type->kind == TYPE_VECTOR && b->type->kind == TYPE_VECTOR)
            {
                // Vector dot product
                if (b->type != a->type)
                {
                    ts__addErr(compiler, &expr->loc, "mismatched vector types");
                    break;
                }

                expr->type = a->type->vector.elem_type;
            }
            else if (a->type->kind == TYPE_MATRIX && b->type->kind == TYPE_MATRIX)
            {
                // Matrix times matrix
                if (b->type != a->type)
                {
                    ts__addErr(compiler, &expr->loc, "mismatched matrix types");
                    break;
                }

                expr->type = a->type;
            }
            else
            {
                assert(0);
            }

            break;
        }

        case IR_BUILTIN_DISTANCE: {
            if (param_count != 2)
            {
                ts__addErr(compiler, &expr->loc, "distance needs 2 parameters");
                break;
            }

            tryCoerceExprToScalarType(a, params[0], newFloatType(m, 32));
            tryCoerceExprToScalarType(a, params[1], newFloatType(m, 32));

            AstExpr *a = params[0];
            AstExpr *b = params[1];

            if ((a->type->kind != TYPE_VECTOR) || (b->type->kind != TYPE_VECTOR))
            {
                ts__addErr(compiler, &expr->loc, "distance operates on vectors");
                break;
            }

            if (a->type != b->type)
            {
                ts__addErr(
                    compiler, &expr->loc, "distance cannot operate on different types");
                break;
            }

            expr->type = ts__getElemType(a->type);
            break;
        }

        case IR_BUILTIN_RADIANS:
        case IR_BUILTIN_DEGREES: {
            if (param_count != 1)
            {
                ts__addErr(
                    compiler, &expr->loc, "degrees/radians call needs 1 parameter");
                break;
            }

            tryCoerceExprToScalarType(a, params[0], newFloatType(m, 32));

            AstExpr *a = params[0];
            if (!a->type) break;

            if (a->type->kind != TYPE_FLOAT)
            {
                ts__addErr(
                    compiler, &expr->loc, "degrees/radians call needs a float parameter");
                break;
            }

            expr->type = a->type;

            break;
        }

        case IR_BUILTIN_SIN:
        case IR_BUILTIN_COS:
        case IR_BUILTIN_TAN:
        case IR_BUILTIN_ASIN:
        case IR_BUILTIN_ACOS:
        case IR_BUILTIN_ATAN:
        case IR_BUILTIN_SINH:
        case IR_BUILTIN_COSH:
        case IR_BUILTIN_TANH: {
            if (param_count != 1)
            {
                ts__addErr(
                    compiler, &expr->loc, "trigonometric functions take 1 parameter");
                break;
            }

            tryCoerceExprToScalarType(a, params[0], newFloatType(m, 32));

            AstExpr *a = params[0];

            if (a->kind == EXPR_PRIMARY && a->type->kind == TYPE_INT)
            {
                a->type = newFloatType(m, 32);
            }

            if (a->type->kind != TYPE_FLOAT)
            {
                ts__addErr(
                    compiler, &expr->loc, "trigonometric functions operate on floats");
                break;
            }

            expr->type = a->type;
            break;
        }

        case IR_BUILTIN_ATAN2: {
            if (param_count != 2)
            {
                ts__addErr(compiler, &expr->loc, "atan2 takes 2 parameters");
                break;
            }

            tryCoerceExprToScalarType(a, params[0], newFloatType(m, 32));
            tryCoerceExprToScalarType(a, params[1], newFloatType(m, 32));

            AstExpr *a = params[0];
            AstExpr *b = params[1];

            if (a->kind == EXPR_PRIMARY && a->type->kind == TYPE_INT)
            {
                a->type = newFloatType(m, 32);
            }

            if (b->kind == EXPR_PRIMARY && b->type->kind == TYPE_INT)
            {
                b->type = newFloatType(m, 32);
            }

            if (a->type != b->type)
            {
                ts__addErr(
                    compiler,
                    &expr->loc,
                    "atan2 requires 2 parameters of identical types");
                break;
            }

            if (a->type->kind != TYPE_FLOAT)
            {
                ts__addErr(compiler, &expr->loc, "atan2 operates on floats");
                break;
            }

            expr->type = a->type;
            break;
        }

        case IR_BUILTIN_SQRT:
        case IR_BUILTIN_RSQRT: {
            if (param_count != 1)
            {
                ts__addErr(compiler, &expr->loc, "sqrt/rsqrt takes 1 parameter");
                break;
            }

            tryCoerceExprToScalarType(a, params[0], newFloatType(m, 32));

            AstExpr *a = params[0];
            if (!a->type) break;

            AstType *scalar_type = ts__getScalarType(a->type);
            if (!scalar_type || scalar_type->kind != TYPE_FLOAT)
            {
                ts__addErr(
                    compiler,
                    &expr->loc,
                    "sqrt/rsqrt operates on vectors or scalars of float type");
                break;
            }

            expr->type = a->type;

            break;
        }

        case IR_BUILTIN_REFLECT: {
            if (param_count != 2)
            {
                ts__addErr(compiler, &expr->loc, "reflect needs 2 parameters");
                break;
            }

            AstExpr *a = params[0];
            AstExpr *b = params[1];

            if ((a->type->kind != TYPE_VECTOR) || (b->type->kind != TYPE_VECTOR))
            {
                ts__addErr(compiler, &expr->loc, "reflect operates on vectors");
                break;
            }

            if (a->type != b->type)
            {
                ts__addErr(
                    compiler,
                    &expr->loc,
                    "reflect cannot operate on different vector types");
                break;
            }

            expr->type = a->type;

            break;
        }

        case IR_BUILTIN_REFRACT: {
            if (param_count != 3)
            {
                ts__addErr(compiler, &expr->loc, "refract needs 2 parameters");
                break;
            }

            tryCoerceExprToScalarType(a, params[2], newFloatType(m, 32));

            AstExpr *a = params[0];
            AstExpr *b = params[1];
            AstExpr *c = params[2];

            if (!a->type || !b->type || !c->type) break;

            if ((a->type->kind != TYPE_VECTOR) || (b->type->kind != TYPE_VECTOR))
            {
                ts__addErr(compiler, &expr->loc, "refract operates on vectors");
                break;
            }

            if (a->type != b->type)
            {
                ts__addErr(
                    compiler,
                    &expr->loc,
                    "refract cannot operate on different vector types");
                break;
            }

            if (c->type->kind != TYPE_FLOAT)
            {
                ts__addErr(
                    compiler,
                    &expr->loc,
                    "refract takes a scalar as the third parameter");
                break;
            }

            expr->type = a->type;

            break;
        }

        case IR_BUILTIN_POW: {
            if (param_count != 2)
            {
                ts__addErr(compiler, &expr->loc, "pow takes 2 parameters");
                break;
            }

            tryCoerceExprToScalarType(a, params[0], newFloatType(m, 32));
            tryCoerceExprToScalarType(a, params[1], newFloatType(m, 32));

            AstExpr *a = params[0];
            AstExpr *b = params[1];
            if (!a->type || !b->type) break;

            if (a->type != b->type)
            {
                ts__addErr(
                    compiler, &expr->loc, "pow operates on parameters of the same type");
                break;
            }

            AstType *scalar_type = ts__getScalarType(a->type);
            if (!scalar_type || scalar_type->kind != TYPE_FLOAT)
            {
                ts__addErr(
                    compiler,
                    &expr->loc,
                    "pow operates on vectors or scalars of float type");
                break;
            }

            expr->type = a->type;

            break;
        }

        case IR_BUILTIN_EXP:
        case IR_BUILTIN_EXP2: {
            if (param_count != 1)
            {
                ts__addErr(compiler, &expr->loc, "exp/exp2 takes 1 parameter");
                break;
            }

            tryCoerceExprToScalarType(a, params[0], newFloatType(m, 32));

            AstExpr *a = params[0];
            if (!a->type) break;

            AstType *scalar_type = ts__getScalarType(a->type);
            if (!scalar_type || scalar_type->kind != TYPE_FLOAT)
            {
                ts__addErr(
                    compiler,
                    &expr->loc,
                    "exp/exp2 operates on vectors or scalars of float type");
                break;
            }

            expr->type = a->type;

            break;
        }

        case IR_BUILTIN_LOG:
        case IR_BUILTIN_LOG2: {
            if (param_count != 1)
            {
                ts__addErr(compiler, &expr->loc, "log/log2 takes 1 parameter");
                break;
            }

            tryCoerceExprToScalarType(a, params[0], newFloatType(m, 32));

            AstExpr *a = params[0];
            if (!a->type) break;

            AstType *scalar_type = ts__getScalarType(a->type);
            if (!scalar_type || scalar_type->kind != TYPE_FLOAT)
            {
                ts__addErr(
                    compiler,
                    &expr->loc,
                    "log/log2 operates on vectors or scalars of float type");
                break;
            }

            expr->type = a->type;

            break;
        }

        case IR_BUILTIN_ABS: {
            if (param_count != 1)
            {
                ts__addErr(compiler, &expr->loc, "abs takes 1 parameter");
                break;
            }

            if (expected_type)
            {
                tryCoerceExprToScalarType(a, params[0], expected_type);
            }

            AstExpr *a = params[0];
            if (!a->type) break;

            AstType *scalar_type = ts__getScalarType(a->type);
            if (!scalar_type ||
                !(scalar_type->kind == TYPE_INT || scalar_type->kind == TYPE_FLOAT))
            {
                ts__addErr(compiler, &expr->loc, "abs operates on vectors or scalars");
                break;
            }

            expr->type = a->type;

            break;
        }

        case IR_BUILTIN_MIN:
        case IR_BUILTIN_MAX: {
            if (param_count != 2)
            {
                ts__addErr(compiler, &expr->loc, "min/max takes 2 parameters");
                break;
            }

            if (expected_type && canCoerceExprToScalarType(a, params[0]) &&
                canCoerceExprToScalarType(a, params[1]))
            {
                tryCoerceExprToScalarType(a, params[0], expected_type);
                tryCoerceExprToScalarType(a, params[1], expected_type);
            }

            AstExpr *a = params[0];
            AstExpr *b = params[1];
            if (!a->type || !b->type) break;

            if (a->type != b->type)
            {
                ts__addErr(
                    compiler, &expr->loc, "min/max operates parameters of equal types");
                break;
            }

            AstType *scalar_type = ts__getScalarType(a->type);
            if (!scalar_type ||
                !(scalar_type->kind == TYPE_INT || scalar_type->kind == TYPE_FLOAT))
            {
                ts__addErr(
                    compiler, &expr->loc, "min/max operates on vectors or scalars");
                break;
            }

            expr->type = a->type;

            break;
        }

        case IR_BUILTIN_CREATE_SAMPLED_IMAGE: assert(0); break;
        }

        break;
    }

    case EXPR_SAMPLER_TYPE: {
        expr->type = newBasicType(m, TYPE_TYPE);
        expr->as_type = newBasicType(m, TYPE_SAMPLER);
        break;
    }

    case EXPR_TEXTURE_TYPE: {
        AstType *type_type = newBasicType(m, TYPE_TYPE);
        analyzerAnalyzeExpr(a, expr->texture.sampled_type_expr, type_type);
        if (!expr->texture.sampled_type_expr->type)
        {
            break;
        }

        AstType *sampled_type = expr->texture.sampled_type_expr->as_type;
        assert(sampled_type);

        if (!(sampled_type->kind == TYPE_VECTOR || sampled_type->kind == TYPE_INT ||
              sampled_type->kind == TYPE_FLOAT))
        {
            ts__addErr(
                compiler, &expr->loc, "invalid scalar type for sampled type for texture");
            break;
        }

        expr->type = type_type;
        expr->as_type = newImageType(m, sampled_type, expr->texture.dim);
        break;
    }

    case EXPR_UNARY: {
        switch (expr->unary.op)
        {
        case UNOP_NEG: {
            analyzerAnalyzeExpr(a, expr->unary.right, NULL);
            if (!expr->unary.right->type) break;
            AstType *right_type = expr->unary.right->type;
            AstType *scalar_type = ts__getScalarType(right_type);

            if (!scalar_type)
            {
                ts__addErr(
                    compiler,
                    &expr->unary.right->loc,
                    "\'negation\' expression does not work on this type");
                break;
            }

            expr->type = right_type;

            break;
        }

        case UNOP_NOT: {
            analyzerAnalyzeExpr(a, expr->unary.right, NULL);
            if (!expr->unary.right->type) break;
            AstType *right_type = expr->unary.right->type;
            AstType *logical_type = ts__getLogicalType(right_type);

            if (!logical_type)
            {
                ts__addErr(
                    compiler,
                    &expr->unary.right->loc,
                    "\'not\' expression does not work on this type");
                break;
            }

            expr->type = right_type;

            break;
        }
        }

        break;
    }

    case EXPR_BINARY: {
        if (expr->binary.left->kind == EXPR_PRIMARY)
        {
            analyzerAnalyzeExpr(a, expr->binary.right, NULL);
            analyzerAnalyzeExpr(a, expr->binary.left, expr->binary.right->type);
        }
        else if (expr->binary.right->kind == EXPR_PRIMARY)
        {
            analyzerAnalyzeExpr(a, expr->binary.left, NULL);
            analyzerAnalyzeExpr(a, expr->binary.right, expr->binary.left->type);
        }
        else
        {
            analyzerAnalyzeExpr(a, expr->binary.left, NULL);
            analyzerAnalyzeExpr(a, expr->binary.right, NULL);
        }

        switch (expr->binary.op)
        {
        case BINOP_ADD:
        case BINOP_SUB:
        case BINOP_MUL:
        case BINOP_DIV:
        case BINOP_MOD: {
            if (!expr->binary.left->type || !expr->binary.right->type) break;

            AstType *left_type = expr->binary.left->type;
            AstType *right_type = expr->binary.right->type;

            AstType *left_scalar = ts__getScalarType(left_type);
            AstType *right_scalar = ts__getScalarType(right_type);
            if ((!left_scalar) || (!right_scalar) || (left_type != right_type))
            {
                ts__addErr(
                    compiler,
                    &expr->loc,
                    "invalid types for binary arithmentic operation");
            }

            expr->type = left_type;

            break;
        }

        case BINOP_EQ:
        case BINOP_NOTEQ:
        case BINOP_LESS:
        case BINOP_LESSEQ:
        case BINOP_GREATER:
        case BINOP_GREATEREQ: {
            if (!expr->binary.left->type || !expr->binary.right->type) break;

            AstType *left_type = expr->binary.left->type;
            AstType *right_type = expr->binary.right->type;

            AstType *left_comparable = ts__getComparableType(left_type);
            AstType *right_comparable = ts__getComparableType(right_type);

            if ((!left_comparable) || (!right_comparable) || (left_type != right_type))
            {
                ts__addErr(
                    compiler,
                    &expr->loc,
                    "invalid types for binary comparison operation");
            }

            expr->type = newBasicType(m, TYPE_BOOL);

            break;
        }
        }

        break;
    }
    }

    if (expected_type)
    {
        if (!expr->type)
        {
            ts__addErr(compiler, &expr->loc, "could not resolve type for expression");
        }
        else if (expr->type != expected_type)
        {
            ts__addErr(compiler, &expr->loc, "unmatched types");
        }
    }
}

static void analyzerAnalyzeStmt(Analyzer *a, AstStmt *stmt)
{
    TsCompiler *compiler = a->compiler;

    switch (stmt->kind)
    {
    case STMT_DECL: {
        analyzerTryRegisterDecl(a, stmt->decl);
        analyzerAnalyzeDecl(a, stmt->decl);
        break;
    }

    case STMT_EXPR: {
        analyzerAnalyzeExpr(a, stmt->expr, NULL);
        break;
    }

    case STMT_RETURN: {
        assert(a->scope_func);
        AstType *return_type = a->scope_func->type->func.return_type;

        if (return_type->kind == TYPE_VOID && stmt->return_.value)
        {
            ts__addErr(compiler, &stmt->loc, "function does not return a value");
        }

        if (return_type->kind != TYPE_VOID && !stmt->return_.value)
        {
            ts__addErr(compiler, &stmt->loc, "function needs a return value");
        }

        if (stmt->return_.value)
        {
            analyzerAnalyzeExpr(a, stmt->return_.value, return_type);
        }
        break;
    }

    case STMT_VAR_ASSIGN: {
        analyzerAnalyzeExpr(a, stmt->var_assign.assigned_expr, NULL);
        if (!stmt->var_assign.assigned_expr->assignable)
        {
            ts__addErr(
                compiler,
                &stmt->var_assign.assigned_expr->loc,
                "expression is not assignable");
        }

        analyzerAnalyzeExpr(
            a, stmt->var_assign.value_expr, stmt->var_assign.assigned_expr->type);

        break;
    }

    case STMT_BLOCK: {
        Scope *scope = analyzerCurrentScope(a);

        stmt->block.scope = NEW(compiler, Scope);
        scopeInit(stmt->block.scope, scope, NULL);

        analyzerPushScope(a, stmt->block.scope);
        for (uint32_t i = 0; i < arrLength(stmt->block.stmts); ++i)
        {
            AstStmt *sub_stmt = stmt->block.stmts[i];
            analyzerAnalyzeStmt(a, sub_stmt);
        }
        analyzerPopScope(a, stmt->block.scope);
        break;
    }

    case STMT_IF: {
        analyzerAnalyzeExpr(a, stmt->if_.cond, NULL);
        if (stmt->if_.cond->type && !ts__getComparableType(stmt->if_.cond->type))
        {
            ts__addErr(compiler, &stmt->if_.cond->loc, "expression is not comparable");
        }

        analyzerAnalyzeStmt(a, stmt->if_.if_stmt);
        if (stmt->if_.else_stmt)
        {
            analyzerAnalyzeStmt(a, stmt->if_.else_stmt);
        }

        break;
    }

    case STMT_WHILE: {
        analyzerAnalyzeExpr(a, stmt->while_.cond, NULL);
        if (stmt->while_.cond->type && !ts__getComparableType(stmt->while_.cond->type))
        {
            ts__addErr(compiler, &stmt->while_.cond->loc, "expression is not comparable");
        }

        analyzerAnalyzeStmt(a, stmt->while_.stmt);
        break;
    }
    }
}

static void analyzerAnalyzeDecl(Analyzer *a, AstDecl *decl)
{
    TsCompiler *compiler = a->compiler;
    Module *m = a->module;
    Scope *scope = analyzerCurrentScope(a);

    for (uint32_t i = 0; i < arrLength(decl->attributes); ++i)
    {
        AstAttribute *attr = &decl->attributes[i];
        for (uint32_t j = 0; j < arrLength(attr->values); ++j)
        {
            analyzerAnalyzeExpr(a, attr->values[j], NULL);
        }
    }

    switch (decl->kind)
    {
    case DECL_FUNC: {
        if (strcmp(m->entry_point, decl->name) == 0)
        {
            decl->func.execution_model = NEW(compiler, SpvExecutionModel);
            decl->func.called = true;

            switch (m->stage)
            {
            case TS_SHADER_STAGE_VERTEX:
                *decl->func.execution_model = SpvExecutionModelVertex;
                break;
            case TS_SHADER_STAGE_FRAGMENT:
                *decl->func.execution_model = SpvExecutionModelFragment;
                break;
            }
        }

        for (uint32_t i = 0; i < arrLength(decl->func.all_params); ++i)
        {
            AstDecl *param_decl = decl->func.all_params[i];

            if (decl->func.execution_model)
            {
                if (!param_decl->var.semantic)
                {
                    ts__addErr(
                        compiler,
                        &param_decl->loc,
                        "entry point parameter needs a semantic string");
                }

                if (param_decl->var.kind == VAR_IN_PARAM ||
                    param_decl->var.kind == VAR_PLAIN)
                {
                    arrPush(decl->func.inputs, param_decl);
                }
                else if (
                    param_decl->var.kind == VAR_OUT_PARAM ||
                    param_decl->var.kind == VAR_INOUT_PARAM)
                {
                    arrPush(decl->func.outputs, param_decl);
                }
            }
            else
            {
                arrPush(decl->func.func_params, param_decl);
            }
        }

        if (decl->func.execution_model)
        {
            assert(arrLength(decl->func.func_params) == 0);
        }

        decl->scope = NEW(compiler, Scope);
        scopeInit(decl->scope, scope, decl);

        analyzerAnalyzeExpr(a, decl->func.return_type, newBasicType(m, TYPE_TYPE));
        AstType *return_type = decl->func.return_type->as_type;

        if (!return_type)
        {
            ts__addErr(
                compiler, &decl->loc, "could not resolve return type for function");
            break;
        }

        bool param_types_valid = true;
        AstType **param_types = NULL;
        if (arrLength(decl->func.func_params) > 0)
        {
            param_types =
                NEW_ARRAY(compiler, AstType *, arrLength(decl->func.func_params));
        }

        uint32_t input_loc = 0;
        uint32_t output_loc = 0;

        analyzerPushScope(a, decl->scope);
        for (uint32_t i = 0; i < arrLength(decl->func.inputs); ++i)
        {
            AstDecl *param_decl = decl->func.inputs[i];

            if (param_decl->var.semantic)
            {
                if (strcmp(param_decl->var.semantic, "SV_Position") == 0 &&
                    *decl->func.execution_model == SpvExecutionModelFragment)
                {
                    IRDecoration dec = {0};
                    dec.kind = SpvDecorationBuiltIn;
                    dec.value = SpvBuiltInFragCoord;
                    arrPush(param_decl->decorations, dec);
                }
                else if (
                    strcmp(param_decl->var.semantic, "SV_InstanceID") == 0 &&
                    *decl->func.execution_model == SpvExecutionModelVertex)
                {
                    IRDecoration dec = {0};
                    dec.kind = SpvDecorationBuiltIn;
                    dec.value = SpvBuiltInInstanceIndex;
                    arrPush(param_decl->decorations, dec);
                }
                else if (
                    strcmp(param_decl->var.semantic, "SV_VertexID") == 0 &&
                    *decl->func.execution_model == SpvExecutionModelVertex)
                {
                    IRDecoration dec = {0};
                    dec.kind = SpvDecorationBuiltIn;
                    dec.value = SpvBuiltInVertexIndex;
                    arrPush(param_decl->decorations, dec);
                }
                else
                {
                    IRDecoration dec = {0};
                    dec.kind = SpvDecorationLocation;
                    dec.value = input_loc++;
                    arrPush(param_decl->decorations, dec);
                }
            }

            analyzerTryRegisterDecl(a, param_decl);
            analyzerAnalyzeDecl(a, param_decl);
        }

        for (uint32_t i = 0; i < arrLength(decl->func.outputs); ++i)
        {
            AstDecl *param_decl = decl->func.outputs[i];

            if (param_decl->var.semantic)
            {
                if (strcmp(param_decl->var.semantic, "SV_Position") == 0 &&
                    *decl->func.execution_model == SpvExecutionModelVertex)
                {
                    IRDecoration dec = {0};
                    dec.kind = SpvDecorationBuiltIn;
                    dec.value = SpvBuiltInPosition;
                    arrPush(param_decl->decorations, dec);
                }
                else
                {
                    IRDecoration dec = {0};
                    dec.kind = SpvDecorationLocation;
                    dec.value = output_loc++;
                    arrPush(param_decl->decorations, dec);
                }
            }

            analyzerTryRegisterDecl(a, param_decl);
            analyzerAnalyzeDecl(a, param_decl);
        }

        for (uint32_t i = 0; i < arrLength(decl->func.func_params); ++i)
        {
            AstDecl *param_decl = decl->func.func_params[i];
            assert(param_decl->kind == DECL_VAR);

            analyzerTryRegisterDecl(a, param_decl);
            analyzerAnalyzeDecl(a, param_decl);
            param_types[i] = param_decl->type;
            if (!param_types[i])
            {
                ts__addErr(
                    compiler,
                    &param_decl->loc,
                    "could not resolve type for function parameter");
                param_types_valid = false;
                continue;
            }

            if (param_decl->var.kind == VAR_IN_PARAM ||
                param_decl->var.kind == VAR_OUT_PARAM ||
                param_decl->var.kind == VAR_INOUT_PARAM)
            {
                param_types[i] =
                    newPointerType(m, SpvStorageClassFunction, param_types[i]);
            }
        }

        if (param_types_valid)
        {
            decl->type = newFuncType(
                m, return_type, param_types, arrLength(decl->func.func_params));
        }

        for (uint32_t i = 0; i < arrLength(decl->func.stmts); ++i)
        {
            AstStmt *stmt = decl->func.stmts[i];
            analyzerAnalyzeStmt(a, stmt);
        }
        analyzerPopScope(a, decl->scope);

        break;
    }

    case DECL_VAR: {
        if (decl->var.kind == VAR_PLAIN && !a->scope_func)
        {
            ts__addErr(
                compiler, &decl->loc, "variable declaration must be inside a function");
            break;
        }

        analyzerAnalyzeExpr(a, decl->var.type_expr, newBasicType(m, TYPE_TYPE));
        if (!decl->var.type_expr->as_type)
        {
            break;
        }

        if (decl->var.value_expr)
        {
            analyzerAnalyzeExpr(a, decl->var.value_expr, decl->var.type_expr->as_type);
        }

        decl->type = decl->var.type_expr->as_type;

        if (decl->var.kind == VAR_PLAIN && a->scope_func)
        {
            arrPush(a->scope_func->func.var_decls, decl);
        }

        if (decl->type->kind == TYPE_STRUCT)
        {
            decl->scope = NEW(compiler, Scope);
            scopeInit(decl->scope, NULL, decl);
            for (uint32_t i = 0; i < decl->type->struct_.field_count; ++i)
            {
                AstDecl *field_decl = decl->type->struct_.field_decls[i];
                scopeAdd(decl->scope, field_decl->name, field_decl);
            }
        }

        if (decl->var.kind == VAR_UNIFORM && decl->type->kind == TYPE_STRUCT)
        {
            IRDecoration dec = {0};
            dec.kind = SpvDecorationBlock;
            arrPush(decl->type->struct_.decorations, dec);
        }

        // Add decorations
        for (uint32_t i = 0; i < arrLength(decl->attributes); ++i)
        {
            AstAttribute *attr = &decl->attributes[i];

            if (strcmp(attr->name, "vk::binding") == 0)
            {
                if (arrLength(attr->values) >= 1)
                {
                    if (!attr->values[0]->resolved_int)
                    {
                        ts__addErr(
                            compiler,
                            &attr->values[0]->loc,
                            "could not resolve integer from expression");
                    }
                    else
                    {
                        IRDecoration dec = {0};
                        dec.kind = SpvDecorationBinding;
                        dec.value = *attr->values[0]->resolved_int;
                        arrPush(decl->decorations, dec);
                    }
                }

                if (arrLength(attr->values) >= 2)
                {
                    if (!attr->values[1]->resolved_int)
                    {
                        ts__addErr(
                            compiler,
                            &attr->values[1]->loc,
                            "could not resolve integer from expression");
                    }
                    else
                    {
                        IRDecoration dec = {0};
                        dec.kind = SpvDecorationDescriptorSet;
                        dec.value = *attr->values[1]->resolved_int;
                        arrPush(decl->decorations, dec);
                    }
                }
            }
        }

        break;
    }

    case DECL_CONST: {
        analyzerAnalyzeExpr(a, decl->constant.type_expr, newBasicType(m, TYPE_TYPE));
        if (!decl->constant.type_expr->as_type)
        {
            ts__addErr(
                compiler,
                &decl->loc,
                "constant type expression does not represent a type");
        }

        analyzerAnalyzeExpr(
            a, decl->constant.value_expr, decl->constant.type_expr->as_type);

        decl->type = decl->constant.type_expr->as_type;
        decl->resolved_int = decl->constant.value_expr->resolved_int;

        break;
    }

    case DECL_STRUCT_FIELD: {
        analyzerAnalyzeExpr(a, decl->struct_field.type_expr, newBasicType(m, TYPE_TYPE));
        if (!decl->struct_field.type_expr->as_type)
        {
            ts__addErr(
                compiler,
                &decl->loc,
                "struct field type expression does not represent a type");
        }

        decl->type = decl->struct_field.type_expr->as_type;

        if (decl->type->kind == TYPE_STRUCT)
        {
            decl->scope = NEW(compiler, Scope);
            scopeInit(decl->scope, NULL, decl);
            for (uint32_t i = 0; i < decl->type->struct_.field_count; ++i)
            {
                AstDecl *field_decl = decl->type->struct_.field_decls[i];
                scopeAdd(decl->scope, field_decl->name, field_decl);
            }
        }

        break;
    }

    case DECL_STRUCT: {
        uint32_t field_count = arrLength(decl->struct_.fields);
        AstType **field_types = NEW_ARRAY(compiler, AstType *, field_count);

        decl->scope = NEW(compiler, Scope);
        scopeInit(decl->scope, scope, NULL);

        analyzerPushScope(a, decl->scope);
        for (uint32_t i = 0; i < arrLength(decl->struct_.fields); ++i)
        {
            AstDecl *field = decl->struct_.fields[i];
            field->struct_field.index = i;
            analyzerTryRegisterDecl(a, field);
            analyzerAnalyzeDecl(a, field);

            field_types[i] = field->type;
        }
        analyzerPopScope(a, decl->scope);

        decl->type = newBasicType(m, TYPE_TYPE);
        decl->as_type =
            newStructType(m, decl->name, field_types, decl->struct_.fields, field_count);

        for (uint32_t i = 0; i < arrLength(decl->struct_.fields); ++i)
        {
            if (field_types[i]->kind == TYPE_MATRIX)
            {
                IRMemberDecoration member_dec = {0};
                member_dec.kind = SpvDecorationRowMajor;
                member_dec.member_index = i;
                arrPush(decl->as_type->struct_.field_decorations, member_dec);

                uint32_t row_stride =
                    field_types[i]->matrix.col_count *
                    typeSizeOf(m, field_types[i]->matrix.col_type->vector.elem_type);

                member_dec.kind = SpvDecorationMatrixStride;
                member_dec.member_index = i;
                member_dec.value = row_stride;
                arrPush(decl->as_type->struct_.field_decorations, member_dec);
            }
        }

        break;
    }
    }
}

void ts__analyzerAnalyze(Analyzer *a, TsCompiler *compiler, Module *module)
{
    memset(a, 0, sizeof(*a));
    a->compiler = compiler;
    a->module = module;

    assert(!module->scope);

    module->scope = NEW(compiler, Scope);
    scopeInit(module->scope, NULL, NULL);

    for (uint32_t i = 0; i < arrLength(a->module->files); ++i)
    {
        File *file = a->module->files[i];

        analyzerPushScope(a, a->module->scope);

        for (uint32_t j = 0; j < arrLength(file->decls); ++j)
        {
            AstDecl *decl = file->decls[j];
            analyzerTryRegisterDecl(a, decl);
        }

        analyzerPopScope(a, a->module->scope);
    }

    for (uint32_t i = 0; i < arrLength(a->module->files); ++i)
    {
        File *file = a->module->files[i];

        analyzerPushScope(a, a->module->scope);

        for (uint32_t j = 0; j < arrLength(file->decls); ++j)
        {
            AstDecl *decl = file->decls[j];
            analyzerAnalyzeDecl(a, decl);
        }

        analyzerPopScope(a, a->module->scope);
    }
}
