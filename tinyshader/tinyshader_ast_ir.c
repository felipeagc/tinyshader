/**
 * This file is part of the tinyshader library.
 * See tinyshader.h for license details.
 */
#include "tinyshader_internal.h"

////////////////////////////////
//
// IR generation from AST
//
////////////////////////////////

static void astBuildDecl(Module *ast_mod, IRModule *ir_mod, AstDecl *decl);
static void astBuildStmt(Module *ast_mod, IRModule *ir_mod, AstStmt *stmt);
static void astBuildExpr(Module *ast_mod, IRModule *ir_mod, AstExpr *expr);

static IRInst *boolVal(IRModule *m, IRInst *value)
{
    IRType *bool_type = ts__irNewBasicType(m, IR_TYPE_BOOL);

    switch (value->type->kind)
    {
    case IR_TYPE_INT: {
        return ts__irBuildBinary(
            m, SpvOpINotEqual, bool_type, value, ts__irBuildConstInt(m, value->type, 0));
    }

    case IR_TYPE_FLOAT: {
        return ts__irBuildBinary(
            m,
            SpvOpFOrdNotEqual,
            bool_type,
            value,
            ts__irBuildConstFloat(m, value->type, 0.0));
    }

    case IR_TYPE_BOOL: {
        return value;
    }

    default: assert(0); break;
    }

    return NULL;
}

static bool isLvalue(IRInst *value)
{
    return (
        value->kind == IR_INST_VARIABLE || value->kind == IR_INST_ACCESS_CHAIN ||
        (value->kind == IR_INST_FUNC_PARAM && value->func_param.is_by_reference));
}

static IRInst *loadVal(IRModule *m, IRInst *value)
{
    assert(value);
    if (isLvalue(value))
    {
        return ts__irBuildLoad(m, value);
    }

    return value;
}

static IRType *convertTypeToIR(Module *module, IRModule *ir_module, AstType *type)
{
    switch (type->kind)
    {
    case TYPE_TYPE: assert(0); break;

    case TYPE_VOID: {
        return ts__irNewBasicType(ir_module, IR_TYPE_VOID);
    }

    case TYPE_BOOL: {
        return ts__irNewBasicType(ir_module, IR_TYPE_BOOL);
    }

    case TYPE_FLOAT: {
        return ts__irNewFloatType(ir_module, type->float_.bits);
    }

    case TYPE_INT: {
        return ts__irNewIntType(ir_module, type->int_.bits, type->int_.is_signed);
    }

    case TYPE_CONSTANT_BUFFER: {
        IRType *struct_type = convertTypeToIR(module, ir_module, type->buffer.sub);

        IRDecoration struct_dec = {0};
        struct_dec.kind = SpvDecorationBlock;

        ts__irDecorateType(ir_module, struct_type, &struct_dec);

        return struct_type;
    }

    case TYPE_RW_STRUCTURED_BUFFER: {
        IRType *subtype = convertTypeToIR(module, ir_module, type->buffer.sub);
        IRType *array_type = ts__irNewRuntimeArrayType(ir_module, subtype);

        assert(type->buffer.sub->size > 0);

        IRDecoration array_dec = {0};
        array_dec.kind = SpvDecorationArrayStride;
        array_dec.value = type->buffer.sub->size;

        ts__irDecorateType(ir_module, array_type, &array_dec);

        ts__sbReset(&module->compiler->sb);
        ts__sbSprintf(
            &module->compiler->sb, "__tmp_struct%u", module->compiler->counter++);
        char *struct_name = ts__sbBuild(&module->compiler->sb, &module->compiler->alloc);

        IRMemberDecoration member_decs[1] = {0};
        member_decs[0].kind = SpvDecorationOffset;
        member_decs[0].member_index = 0;
        member_decs[0].value = 0;

        IRType *struct_wrapper =
            ts__irNewStructType(ir_module, struct_name, &array_type, 1, member_decs, 1);

        IRDecoration struct_dec = {0};
        struct_dec.kind = SpvDecorationBufferBlock;

        ts__irDecorateType(ir_module, struct_wrapper, &struct_dec);

        return struct_wrapper;
    }

    case TYPE_STRUCTURED_BUFFER: {
        IRType *subtype = convertTypeToIR(module, ir_module, type->buffer.sub);
        IRType *array_type = ts__irNewRuntimeArrayType(ir_module, subtype);

        assert(type->buffer.sub->size > 0);

        IRDecoration array_dec = {0};
        array_dec.kind = SpvDecorationArrayStride;
        array_dec.value = type->buffer.sub->size;

        ts__irDecorateType(ir_module, array_type, &array_dec);

        ts__sbReset(&module->compiler->sb);
        ts__sbSprintf(
            &module->compiler->sb, "__tmp_struct%u", module->compiler->counter++);
        char *struct_name = ts__sbBuild(&module->compiler->sb, &module->compiler->alloc);

        IRMemberDecoration member_decs[2] = {0};
        member_decs[0].kind = SpvDecorationOffset;
        member_decs[0].member_index = 0;
        member_decs[0].value = 0;

        member_decs[1].kind = SpvDecorationNonWritable;
        member_decs[1].member_index = 0;

        IRType *struct_wrapper =
            ts__irNewStructType(ir_module, struct_name, &array_type, 1, member_decs, 2);

        IRDecoration struct_dec = {0};
        struct_dec.kind = SpvDecorationBufferBlock;

        ts__irDecorateType(ir_module, struct_wrapper, &struct_dec);

        return struct_wrapper;
    }

    case TYPE_STRUCT: {
        IRType **field_types =
            NEW_ARRAY(module->compiler, IRType *, type->struct_.field_count);
        for (uint32_t i = 0; i < type->struct_.field_count; ++i)
        {
            field_types[i] = convertTypeToIR(module, ir_module, type->struct_.fields[i]);
        }
        IRType *struct_type = ts__irNewStructType(
            ir_module,
            type->struct_.name,
            field_types,
            type->struct_.field_count,
            type->struct_.field_decorations.ptr,
            arrLength(type->struct_.field_decorations));

        return struct_type;
    }

    case TYPE_VECTOR: {
        IRType *elem_type = convertTypeToIR(module, ir_module, type->vector.elem_type);
        return ts__irNewVectorType(ir_module, elem_type, type->vector.size);
    }

    case TYPE_RUNTIME_ARRAY: {
        IRType *elem_type = convertTypeToIR(module, ir_module, type->array.sub);
        return ts__irNewRuntimeArrayType(ir_module, elem_type);
    }

    case TYPE_ARRAY: {
        assert(type->array.size > 0);

        IRType *size_type = ts__irNewIntType(ir_module, 32, false);
        IRInst *size = ts__irBuildConstInt(ir_module, size_type, type->array.size);
        IRType *elem_type = convertTypeToIR(module, ir_module, type->array.sub);
        return ts__irNewArrayType(ir_module, elem_type, size);
    }

    case TYPE_IMAGE: {
        IRType *sampled_type = convertTypeToIR(
            module, ir_module, ts__getScalarType(type->image.sampled_type));
        return ts__irNewImageType(ir_module, sampled_type, type->image.dim);
    }

    case TYPE_SAMPLER: {
        return ts__irNewBasicType(ir_module, IR_TYPE_SAMPLER);
    }

    case TYPE_FUNC: {
        IRType *return_type = convertTypeToIR(module, ir_module, type->func.return_type);
        IRType **param_types =
            NEW_ARRAY(module->compiler, IRType *, type->func.param_count);
        for (uint32_t i = 0; i < type->func.param_count; ++i)
        {
            param_types[i] = convertTypeToIR(module, ir_module, type->func.params[i]);
        }
        return ts__irNewFuncType(ir_module, return_type, param_types, type->func.param_count);
    }

    case TYPE_MATRIX: {
        IRType *col_type = convertTypeToIR(module, ir_module, type->matrix.col_type);
        return ts__irNewMatrixType(ir_module, col_type, type->matrix.col_count);
    }

    case TYPE_POINTER: {
        IRType *elem_type = convertTypeToIR(module, ir_module, type->ptr.sub);
        return ts__irNewPointerType(ir_module, type->ptr.storage_class, elem_type);
    }
    }

    return NULL;
}

static void astBuildExpr(Module *ast_mod, IRModule *ir_mod, AstExpr *expr)
{
    TsCompiler *compiler = ast_mod->compiler;

    assert(expr->type);

    switch (expr->kind)
    {
    case EXPR_PRIMARY: {

        switch (expr->primary.token->kind)
        {
        case TOKEN_FLOAT_LIT: {
            AstType *elem_type = ts__getElemType(expr->type);
            IRType *ir_elem_type = convertTypeToIR(ast_mod, ir_mod, elem_type);
            switch (elem_type->kind)
            {
            case TYPE_FLOAT: {
                expr->value =
                    ts__irBuildConstFloat(ir_mod, ir_elem_type, (double)expr->primary.token->double_);
                break;
            }

            case TYPE_BOOL: {
                expr->value = ts__irBuildConstBool(
                        ir_mod, expr->primary.token->double_ ? true : false);
                break;
            }

            default: assert(0); break;
            }

            break;
        }

        case TOKEN_INT_LIT: {
            AstType *elem_type = ts__getElemType(expr->type);
            IRType *ir_elem_type = convertTypeToIR(ast_mod, ir_mod, elem_type);
            switch (elem_type->kind)
            {
            case TYPE_FLOAT: {
                expr->value =
                    ts__irBuildConstFloat(ir_mod, ir_elem_type, (double)expr->primary.token->int_);
                break;
            }

            case TYPE_INT: {
                expr->value =
                    ts__irBuildConstInt(ir_mod, ir_elem_type, (uint64_t)expr->primary.token->int_);
                break;
            }

            case TYPE_BOOL: {
                expr->value = ts__irBuildConstBool(
                        ir_mod, expr->primary.token->int_ ? true : false);
                break;
            }

            default: assert(0); break;
            }

            break;
        }

        case TOKEN_TRUE: {
            expr->value = ts__irBuildConstBool(ir_mod, true);
            break;
        }

        case TOKEN_FALSE: {
            expr->value = ts__irBuildConstBool(ir_mod, false);
            break;
        }

        default: assert(0);
        }
        break;
    }

    case EXPR_VAR_ASSIGN: {
        astBuildExpr(ast_mod, ir_mod, expr->var_assign.value_expr);
        IRInst *to_store = expr->var_assign.value_expr->value;
        assert(to_store);
        to_store = loadVal(ir_mod, to_store);

        astBuildExpr(ast_mod, ir_mod, expr->var_assign.assigned_expr);
        IRInst *assigned_value = expr->var_assign.assigned_expr->value;

        ts__irBuildStore(ir_mod, assigned_value, to_store);

        break;
    }

    case EXPR_IDENT: {
        AstDecl *decl = expr->ident.decl;
        switch (decl->kind)
        {
        case DECL_ALIAS:
        {
            AstDecl *accessed = decl->alias.accessed;
            assert(accessed->type);
            switch (accessed->type->kind)
            {
            case TYPE_CONSTANT_BUFFER:
            {
                AstDecl *field_decl = decl->alias.field_decl;

                uint32_t field_index = field_decl->var.field_index;
                IRType *index_type = ts__irNewIntType(ir_mod, 32, false);
                IRInst *index = ts__irBuildConstInt(ir_mod, index_type, (uint64_t)field_index);

                IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, decl->type);

                assert(decl->alias.accessed->value);
                expr->value = ts__irBuildAccessChain(
                    ir_mod,
                    ir_type,
                    decl->alias.accessed->value,
                    &index,
                    1);

                break;
            }

            default: break;
            }

            break;
        }

        default:
        {
            assert(decl);
            expr->value = decl->value;
            break;
        }
        }
        break;
    }

    case EXPR_SUBSCRIPT: {
        astBuildExpr(ast_mod, ir_mod, expr->subscript.left);
        astBuildExpr(ast_mod, ir_mod, expr->subscript.right);
        assert(expr->subscript.left->value);
        assert(expr->subscript.right->value);

        IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, expr->type);

        uint32_t index_count = 0;
        IRInst **indices = NULL;

        IRType *index_type = ts__irNewIntType(ir_mod, 32, false);

        switch (expr->subscript.left->type->kind)
        {
        case TYPE_RW_STRUCTURED_BUFFER:
        case TYPE_STRUCTURED_BUFFER: {
            index_count = 2;
            indices = NEW_ARRAY(compiler, IRInst *, index_count);

            indices[0] = ts__irBuildConstInt(ir_mod, index_type, 0);
            indices[1] = loadVal(ir_mod, expr->subscript.right->value);
            break;
        }

        case TYPE_VECTOR: {
            index_count = 1;
            indices = NEW_ARRAY(compiler, IRInst *, index_count);
            indices[0] = loadVal(ir_mod, expr->subscript.right->value);
            break;
        }

        case TYPE_MATRIX: {
            index_count = 1;
            indices = NEW_ARRAY(compiler, IRInst *, index_count);
            indices[0] = loadVal(ir_mod, expr->subscript.right->value);
            break;
        }

        case TYPE_RUNTIME_ARRAY:
        case TYPE_ARRAY:
        {
            index_count = 1;
            indices = NEW_ARRAY(compiler, IRInst *, index_count);
            indices[0] = loadVal(ir_mod, expr->subscript.right->value);
            break;
        }

        default: assert(0); break;
        }

        assert(index_count > 0);

        expr->value = ts__irBuildAccessChain(
            ir_mod, ir_type, expr->subscript.left->value, indices, index_count);

        break;
    }

    case EXPR_ACCESS: {
        IRType *index_type = ts__irNewIntType(ir_mod, 32, false);

        AstExpr *base = expr->access.base;
        assert(base->type);

        astBuildExpr(ast_mod, ir_mod, base);
        IRInst *value = base->value;
        assert(value);

        uint32_t index_count = 0;
        IRInst **indices = NEW_ARRAY(compiler, IRInst *, expr->access.chain.len);

        AstType *struct_type = ts__getStructType(base->type);

        if (struct_type)
        {
            assert(struct_type->kind == TYPE_STRUCT);

            for (uint32_t i = 0; i < expr->access.chain.len; ++i)
            {
                AstExpr *field_ident = expr->access.chain.ptr[i];
                assert(field_ident->kind == EXPR_IDENT);

                if (!field_ident->ident.decl) break;

                index_count++;

                AstDecl *field_decl = field_ident->ident.decl;
                assert(field_decl->kind == DECL_STRUCT_FIELD);

                indices[i] =
                    ts__irBuildConstInt(ir_mod, index_type, field_decl->var.field_index);
            }

            AstType *last_type = expr->access.chain.ptr[index_count - 1]->type;
            IRType *ir_last_type = convertTypeToIR(ast_mod, ir_mod, last_type);
            value =
                ts__irBuildAccessChain(ir_mod, ir_last_type, base->value, indices, index_count);
        }

        for (uint32_t i = index_count; i < expr->access.chain.len; ++i)
        {
            // Must be a vector swizzle

            AstExpr *field_ident = expr->access.chain.ptr[i];
            assert(field_ident->kind == EXPR_IDENT);
            assert(!field_ident->ident.decl);
            assert(field_ident->ident.shuffle_indices);
            assert(field_ident->ident.shuffle_index_count > 0);

            uint32_t *shuffle_indices = field_ident->ident.shuffle_indices;
            uint32_t shuffle_index_count = field_ident->ident.shuffle_index_count;

            if (shuffle_index_count == 1)
            {
                if (!isLvalue(value))
                {
                    // It's a temporary value
                    IRInst *vec_value = loadVal(ir_mod, value);
                    value = ts__irBuildCompositeExtract(
                        ir_mod, vec_value, shuffle_indices, shuffle_index_count);
                }
                else
                {
                    // It's a variable
                    IRInst **ir_indices = NEW_ARRAY(compiler, IRInst *, 1);
                    ir_indices[0] = ts__irBuildConstInt(ir_mod, index_type, shuffle_indices[0]);
                    IRType *accessed_type = convertTypeToIR(ast_mod, ir_mod, field_ident->type);
                    value = ts__irBuildAccessChain(ir_mod, accessed_type, value, ir_indices, 1);
                }
            }
            else
            {
                IRInst *vec_value = loadVal(ir_mod, value);

                value = ts__irBuildVectorShuffle(
                    ir_mod, vec_value, vec_value, shuffle_indices, shuffle_index_count);
            }
        }

        expr->value = value;

        break;
    }

    case EXPR_FUNC_CALL: {
        AstExpr *func_expr = expr->func_call.func_expr;

        bool is_builtin_func = false;
        AstBuiltinFunction builtin_func_kind;

        // Builtin function
        if (func_expr->kind == EXPR_IDENT)
        {
            void *result;
            is_builtin_func = ts__hashGet(
                &compiler->builtin_function_table,
                func_expr->ident.name,
                &result);
            if (is_builtin_func)
            {
                builtin_func_kind = (AstBuiltinFunction)result;
            }
        }

        if (is_builtin_func)
        {
            IRType *result_type = convertTypeToIR(ast_mod, ir_mod, expr->type);
            uint32_t param_count = expr->func_call.params.len;
            IRInst **param_values = NEW_ARRAY(compiler, IRInst *, param_count);

            for (uint32_t i = 0; i < param_count; ++i)
            {
                AstExpr *param = expr->func_call.params.ptr[i];
                astBuildExpr(ast_mod, ir_mod, param);
                param_values[i] = param->value;
                assert(param_values[i]);
            }

            switch (builtin_func_kind)
            {
            case AST_BUILTIN_FUNC_INTERLOCKED_ADD:
            case AST_BUILTIN_FUNC_INTERLOCKED_AND:
            case AST_BUILTIN_FUNC_INTERLOCKED_MIN:
            case AST_BUILTIN_FUNC_INTERLOCKED_MAX:
            case AST_BUILTIN_FUNC_INTERLOCKED_OR:
            case AST_BUILTIN_FUNC_INTERLOCKED_XOR: {
                uint32_t ir_param_count = 4;
                IRInst **ir_param_values = NEW_ARRAY(compiler, IRInst *, ir_param_count);

                IRType *uint_type = ts__irNewIntType(ir_mod, 32, false);

                ir_param_values[0] = param_values[0];
                ir_param_values[1] = ts__irBuildConstInt(ir_mod, uint_type, SpvScopeDevice);
                ir_param_values[2] =
                    ts__irBuildConstInt(ir_mod, uint_type, SpvMemorySemanticsMaskNone);
                ir_param_values[3] = param_values[1];

                IRBuiltinInstKind ir_builtin_kind;
                switch (builtin_func_kind)
                {
                case AST_BUILTIN_FUNC_INTERLOCKED_ADD: ir_builtin_kind = IR_BUILTIN_INTERLOCKED_ADD; break;
                case AST_BUILTIN_FUNC_INTERLOCKED_AND: ir_builtin_kind = IR_BUILTIN_INTERLOCKED_AND; break;
                case AST_BUILTIN_FUNC_INTERLOCKED_MIN: ir_builtin_kind = IR_BUILTIN_INTERLOCKED_MIN; break;
                case AST_BUILTIN_FUNC_INTERLOCKED_MAX: ir_builtin_kind = IR_BUILTIN_INTERLOCKED_MAX; break;
                case AST_BUILTIN_FUNC_INTERLOCKED_OR: ir_builtin_kind = IR_BUILTIN_INTERLOCKED_OR; break;
                case AST_BUILTIN_FUNC_INTERLOCKED_XOR: ir_builtin_kind = IR_BUILTIN_INTERLOCKED_XOR; break;
                default: assert(0); break;
                }

                expr->value = ts__irBuildBuiltinCall(
                    ir_mod, ir_builtin_kind, result_type, ir_param_values, ir_param_count);
                break;
            }

            case AST_BUILTIN_FUNC_INTERLOCKED_EXCHANGE: {
                uint32_t ir_param_count = 5;
                IRInst **ir_param_values = NEW_ARRAY(compiler, IRInst *, ir_param_count);

                IRType *uint_type = ts__irNewIntType(ir_mod, 32, false);

                ir_param_values[0] = param_values[0];
                ir_param_values[1] = ts__irBuildConstInt(ir_mod, uint_type, SpvScopeDevice);
                ir_param_values[2] = ts__irBuildConstInt(ir_mod, uint_type, SpvMemorySemanticsMaskNone);
                ir_param_values[3] = param_values[1];
                ir_param_values[4] = param_values[2];

                expr->value = ts__irBuildBuiltinCall(
                    ir_mod, IR_BUILTIN_INTERLOCKED_EXCHANGE, result_type, ir_param_values, ir_param_count);
                break;
            }

            case IR_BUILTIN_INTERLOCKED_COMPARE_EXCHANGE: {
                uint32_t ir_param_count = 7;
                IRInst **ir_param_values = NEW_ARRAY(compiler, IRInst *, ir_param_count);

                IRType *uint_type = ts__irNewIntType(ir_mod, 32, false);

                ir_param_values[0] = param_values[0];
                ir_param_values[1] = ts__irBuildConstInt(ir_mod, uint_type, SpvScopeDevice);
                ir_param_values[2] = ts__irBuildConstInt(ir_mod, uint_type, SpvMemorySemanticsMaskNone);
                ir_param_values[3] = ts__irBuildConstInt(ir_mod, uint_type, SpvMemorySemanticsMaskNone);
                ir_param_values[4] = param_values[1];
                ir_param_values[5] = param_values[2];
                ir_param_values[6] = param_values[3];

                expr->value = ts__irBuildBuiltinCall(
                    ir_mod, IR_BUILTIN_INTERLOCKED_COMPARE_EXCHANGE, result_type, ir_param_values, ir_param_count);
                break;
            }

            case AST_BUILTIN_FUNC_INTERLOCKED_COMPARE_STORE: {
                uint32_t ir_param_count = 6;
                IRInst **ir_param_values = NEW_ARRAY(compiler, IRInst *, ir_param_count);

                IRType *uint_type = ts__irNewIntType(ir_mod, 32, false);

                ir_param_values[0] = param_values[0];
                ir_param_values[1] = ts__irBuildConstInt(ir_mod, uint_type, SpvScopeDevice);
                ir_param_values[2] = ts__irBuildConstInt(ir_mod, uint_type, SpvMemorySemanticsMaskNone);
                ir_param_values[3] = ts__irBuildConstInt(ir_mod, uint_type, SpvMemorySemanticsMaskNone);
                ir_param_values[4] = param_values[1];
                ir_param_values[5] = param_values[2];

                expr->value = ts__irBuildBuiltinCall(
                    ir_mod, IR_BUILTIN_INTERLOCKED_COMPARE_STORE, result_type, ir_param_values, ir_param_count);
                break;
            }

            case AST_BUILTIN_FUNC_ALL_MEMORY_BARRIER:
            case AST_BUILTIN_FUNC_ALL_MEMORY_BARRIER_WITH_GROUP_SYNC:
            case AST_BUILTIN_FUNC_DEVICE_MEMORY_BARRIER:
            case AST_BUILTIN_FUNC_DEVICE_MEMORY_BARRIER_WITH_GROUP_SYNC:
            case AST_BUILTIN_FUNC_GROUP_MEMORY_BARRIER:
            case AST_BUILTIN_FUNC_GROUP_MEMORY_BARRIER_WITH_GROUP_SYNC: {
                bool with_group_sync = false;
                uint32_t barrier_execution_scope = 0;
                uint32_t barrier_memory_scope = 0;
                uint32_t barrier_semantics = 0;

                switch (builtin_func_kind)
                {
                case AST_BUILTIN_FUNC_ALL_MEMORY_BARRIER: {
                    with_group_sync = false;
                    barrier_memory_scope = SpvScopeDevice;
                    barrier_semantics =
                        SpvMemorySemanticsAcquireReleaseMask | SpvMemorySemanticsUniformMemoryMask |
                        SpvMemorySemanticsWorkgroupMemoryMask | SpvMemorySemanticsImageMemoryMask;
                    break;
                }
                case AST_BUILTIN_FUNC_ALL_MEMORY_BARRIER_WITH_GROUP_SYNC: {
                    with_group_sync = true;
                    barrier_execution_scope = SpvScopeWorkgroup;
                    barrier_memory_scope = SpvScopeDevice;
                    barrier_semantics =
                        SpvMemorySemanticsAcquireReleaseMask | SpvMemorySemanticsUniformMemoryMask |
                        SpvMemorySemanticsWorkgroupMemoryMask | SpvMemorySemanticsImageMemoryMask;
                    break;
                }
                case AST_BUILTIN_FUNC_DEVICE_MEMORY_BARRIER: {
                    with_group_sync = false;
                    barrier_memory_scope = SpvScopeDevice;
                    barrier_semantics = SpvMemorySemanticsAcquireReleaseMask |
                        SpvMemorySemanticsUniformMemoryMask |
                        SpvMemorySemanticsImageMemoryMask;
                    break;
                }
                case AST_BUILTIN_FUNC_DEVICE_MEMORY_BARRIER_WITH_GROUP_SYNC: {
                    with_group_sync = true;
                    barrier_execution_scope = SpvScopeWorkgroup;
                    barrier_memory_scope = SpvScopeDevice;
                    barrier_semantics = SpvMemorySemanticsAcquireReleaseMask |
                        SpvMemorySemanticsUniformMemoryMask |
                        SpvMemorySemanticsImageMemoryMask;
                    break;
                }
                case AST_BUILTIN_FUNC_GROUP_MEMORY_BARRIER: {
                    with_group_sync = false;
                    barrier_memory_scope = SpvScopeWorkgroup;
                    barrier_semantics =
                        SpvMemorySemanticsAcquireReleaseMask | SpvMemorySemanticsWorkgroupMemoryMask;
                    break;
                }
                case AST_BUILTIN_FUNC_GROUP_MEMORY_BARRIER_WITH_GROUP_SYNC: {
                    with_group_sync = true;
                    barrier_execution_scope = SpvScopeWorkgroup;
                    barrier_memory_scope = SpvScopeWorkgroup;
                    barrier_semantics =
                        SpvMemorySemanticsAcquireReleaseMask | SpvMemorySemanticsWorkgroupMemoryMask;
                    break;
                }

                default: assert(0); break;
                }

                expr->value = ts__irBuildBarrier(
                    ir_mod,
                    with_group_sync,
                    barrier_execution_scope,
                    barrier_memory_scope,
                    barrier_semantics);

                break;
            }

            default: {
                IRBuiltinInstKind ir_builtin_kind;
                switch (builtin_func_kind)
                {
                case AST_BUILTIN_FUNC_ABS: ir_builtin_kind = IR_BUILTIN_ABS; break;
                case AST_BUILTIN_FUNC_ACOS: ir_builtin_kind = IR_BUILTIN_ACOS; break;
                case AST_BUILTIN_FUNC_ASFLOAT: ir_builtin_kind = IR_BUILTIN_ASFLOAT; break;
                case AST_BUILTIN_FUNC_ASIN: ir_builtin_kind = IR_BUILTIN_ASIN; break;
                case AST_BUILTIN_FUNC_ASINT: ir_builtin_kind = IR_BUILTIN_ASINT; break;
                case AST_BUILTIN_FUNC_ASUINT: ir_builtin_kind = IR_BUILTIN_ASUINT; break;
                case AST_BUILTIN_FUNC_ATAN: ir_builtin_kind = IR_BUILTIN_ATAN; break;
                case AST_BUILTIN_FUNC_ATAN2: ir_builtin_kind = IR_BUILTIN_ATAN2; break;
                case AST_BUILTIN_FUNC_CEIL: ir_builtin_kind = IR_BUILTIN_CEIL; break;
                case AST_BUILTIN_FUNC_CLAMP: ir_builtin_kind = IR_BUILTIN_CLAMP; break;
                case AST_BUILTIN_FUNC_COS: ir_builtin_kind = IR_BUILTIN_COS; break;
                case AST_BUILTIN_FUNC_COSH: ir_builtin_kind = IR_BUILTIN_COSH; break;
                case AST_BUILTIN_FUNC_CROSS: ir_builtin_kind = IR_BUILTIN_CROSS; break;
                case AST_BUILTIN_FUNC_DDX: ir_builtin_kind = IR_BUILTIN_DDX; break;
                case AST_BUILTIN_FUNC_DDY: ir_builtin_kind = IR_BUILTIN_DDY; break;
                case AST_BUILTIN_FUNC_DEGREES: ir_builtin_kind = IR_BUILTIN_DEGREES; break;
                case AST_BUILTIN_FUNC_DETERMINANT: ir_builtin_kind = IR_BUILTIN_DETERMINANT; break;
                case AST_BUILTIN_FUNC_DISTANCE: ir_builtin_kind = IR_BUILTIN_DISTANCE; break;
                case AST_BUILTIN_FUNC_DOT: ir_builtin_kind = IR_BUILTIN_DOT; break;
                case AST_BUILTIN_FUNC_EXP: ir_builtin_kind = IR_BUILTIN_EXP; break;
                case AST_BUILTIN_FUNC_EXP2: ir_builtin_kind = IR_BUILTIN_EXP2; break;
                case AST_BUILTIN_FUNC_FLOOR: ir_builtin_kind = IR_BUILTIN_FLOOR; break;
                case AST_BUILTIN_FUNC_FMOD: ir_builtin_kind = IR_BUILTIN_FMOD; break;
                case AST_BUILTIN_FUNC_FRAC: ir_builtin_kind = IR_BUILTIN_FRAC; break;
                case AST_BUILTIN_FUNC_LENGTH: ir_builtin_kind = IR_BUILTIN_LENGTH; break;
                case AST_BUILTIN_FUNC_LERP: ir_builtin_kind = IR_BUILTIN_LERP; break;
                case AST_BUILTIN_FUNC_LOG: ir_builtin_kind = IR_BUILTIN_LOG; break;
                case AST_BUILTIN_FUNC_LOG2: ir_builtin_kind = IR_BUILTIN_LOG2; break;
                case AST_BUILTIN_FUNC_MAX: ir_builtin_kind = IR_BUILTIN_MAX; break;
                case AST_BUILTIN_FUNC_MIN: ir_builtin_kind = IR_BUILTIN_MIN; break;
                case AST_BUILTIN_FUNC_MUL: ir_builtin_kind = IR_BUILTIN_MUL; break;
                case AST_BUILTIN_FUNC_NORMALIZE: ir_builtin_kind = IR_BUILTIN_NORMALIZE; break;
                case AST_BUILTIN_FUNC_POW: ir_builtin_kind = IR_BUILTIN_POW; break;
                case AST_BUILTIN_FUNC_RADIANS: ir_builtin_kind = IR_BUILTIN_RADIANS; break;
                case AST_BUILTIN_FUNC_REFLECT: ir_builtin_kind = IR_BUILTIN_REFLECT; break;
                case AST_BUILTIN_FUNC_REFRACT: ir_builtin_kind = IR_BUILTIN_REFRACT; break;
                case AST_BUILTIN_FUNC_RSQRT: ir_builtin_kind = IR_BUILTIN_RSQRT; break;
                case AST_BUILTIN_FUNC_SIN: ir_builtin_kind = IR_BUILTIN_SIN; break;
                case AST_BUILTIN_FUNC_SINH: ir_builtin_kind = IR_BUILTIN_SINH; break;
                case AST_BUILTIN_FUNC_SMOOTHSTEP: ir_builtin_kind = IR_BUILTIN_SMOOTHSTEP; break;
                case AST_BUILTIN_FUNC_SQRT: ir_builtin_kind = IR_BUILTIN_SQRT; break;
                case AST_BUILTIN_FUNC_STEP: ir_builtin_kind = IR_BUILTIN_STEP; break;
                case AST_BUILTIN_FUNC_TAN: ir_builtin_kind = IR_BUILTIN_TAN; break;
                case AST_BUILTIN_FUNC_TANH: ir_builtin_kind = IR_BUILTIN_TANH; break;
                case AST_BUILTIN_FUNC_TRANSPOSE: ir_builtin_kind = IR_BUILTIN_TRANSPOSE; break;
                case AST_BUILTIN_FUNC_TRUNC: ir_builtin_kind = IR_BUILTIN_TRUNC; break;

                case AST_BUILTIN_FUNC_INTERLOCKED_ADD:
                case AST_BUILTIN_FUNC_INTERLOCKED_AND:
                case AST_BUILTIN_FUNC_INTERLOCKED_COMPARE_EXCHANGE:
                case AST_BUILTIN_FUNC_INTERLOCKED_COMPARE_STORE:
                case AST_BUILTIN_FUNC_INTERLOCKED_EXCHANGE:
                case AST_BUILTIN_FUNC_INTERLOCKED_MAX:
                case AST_BUILTIN_FUNC_INTERLOCKED_MIN:
                case AST_BUILTIN_FUNC_INTERLOCKED_OR:
                case AST_BUILTIN_FUNC_INTERLOCKED_XOR:
                case AST_BUILTIN_FUNC_ALL_MEMORY_BARRIER:
                case AST_BUILTIN_FUNC_ALL_MEMORY_BARRIER_WITH_GROUP_SYNC:
                case AST_BUILTIN_FUNC_DEVICE_MEMORY_BARRIER:
                case AST_BUILTIN_FUNC_DEVICE_MEMORY_BARRIER_WITH_GROUP_SYNC:
                case AST_BUILTIN_FUNC_GROUP_MEMORY_BARRIER:
                case AST_BUILTIN_FUNC_GROUP_MEMORY_BARRIER_WITH_GROUP_SYNC:
                    assert(0);
                    break;
                }

                uint32_t ir_param_count = param_count;
                IRInst **ir_param_values = NEW_ARRAY(compiler, IRInst *, ir_param_count);

                for (uint32_t i = 0; i < param_count; ++i)
                {
                    ir_param_values[i] = loadVal(ir_mod, param_values[i]);
                    assert(ir_param_values[i]);
                }

                expr->value = ts__irBuildBuiltinCall(
                    ir_mod, ir_builtin_kind, result_type, ir_param_values, ir_param_count);
                break;
            }
            }

            assert(expr->value);
            break;
        }

        if (expr->func_call.self_param)
        {
            // Method call
            AstExpr *method_name_expr = expr->func_call.func_expr;
            assert(method_name_expr->kind == EXPR_IDENT);
            char *method_name = method_name_expr->ident.name;

            AstType *self_type = expr->func_call.self_param->type;

            astBuildExpr(ast_mod, ir_mod, expr->func_call.self_param);
            assert(expr->func_call.self_param->value);

            if (self_type->kind == TYPE_IMAGE && strcmp(method_name, "Sample") == 0)
            {
                uint32_t param_count = arrLength(expr->func_call.params);
                IRInst **param_values = NEW_ARRAY(compiler, IRInst *, param_count);

                IRInst *self_value = loadVal(ir_mod, expr->func_call.self_param->value);

                for (uint32_t i = 0; i < arrLength(expr->func_call.params); ++i)
                {
                    AstExpr *param = expr->func_call.params.ptr[i];
                    astBuildExpr(ast_mod, ir_mod, param);
                    assert(param->value);
                    param_values[i] = loadVal(ir_mod, param->value);
                }

                IRInst **sampled_image_params = NEW_ARRAY(compiler, IRInst *, 2);
                sampled_image_params[0] = self_value;
                sampled_image_params[1] = param_values[0];

                IRInst *sampled_image = ts__irBuildBuiltinCall(
                    ir_mod,
                    IR_BUILTIN_CREATE_SAMPLED_IMAGE,
                    ts__irNewSampledImageType(ir_mod, self_value->type),
                    sampled_image_params,
                    2);

                IRType *result_type = convertTypeToIR(ast_mod, ir_mod, expr->type);
                IRInst *coords = param_values[1];
                expr->value =
                    ts__irBuildSampleImplicitLod(ir_mod, result_type, sampled_image, coords);
            }
            else if (
                self_type->kind == TYPE_IMAGE && strcmp(method_name, "SampleLevel") == 0)
            {
                uint32_t param_count = arrLength(expr->func_call.params);
                IRInst **param_values = NEW_ARRAY(compiler, IRInst *, param_count);

                IRInst *self_value = loadVal(ir_mod, expr->func_call.self_param->value);

                for (uint32_t i = 0; i < arrLength(expr->func_call.params); ++i)
                {
                    AstExpr *param = expr->func_call.params.ptr[i];
                    astBuildExpr(ast_mod, ir_mod, param);
                    assert(param->value);
                    param_values[i] = loadVal(ir_mod, param->value);
                }

                IRInst **sampled_image_params = NEW_ARRAY(compiler, IRInst *, 2);
                sampled_image_params[0] = self_value;
                sampled_image_params[1] = param_values[0];

                IRInst *sampled_image = ts__irBuildBuiltinCall(
                    ir_mod,
                    IR_BUILTIN_CREATE_SAMPLED_IMAGE,
                    ts__irNewSampledImageType(ir_mod, self_value->type),
                    sampled_image_params,
                    2);

                IRType *result_type = convertTypeToIR(ast_mod, ir_mod, expr->type);
                IRInst *coords = param_values[1];
                IRInst *lod = param_values[2];
                expr->value =
                    ts__irBuildSampleExplicitLod(ir_mod, result_type, sampled_image, coords, lod);
            }
            else if (
                self_type->kind == TYPE_IMAGE &&
                strcmp(method_name, "GetDimensions") == 0)
            {
                uint32_t param_count = expr->func_call.params.len;
                IRInst **param_values = NEW_ARRAY(compiler, IRInst *, param_count);

                IRInst *image = loadVal(ir_mod, expr->func_call.self_param->value);

                for (uint32_t i = 0; i < expr->func_call.params.len; ++i)
                {
                    AstExpr *param = expr->func_call.params.ptr[i];
                    astBuildExpr(ast_mod, ir_mod, param);
                    assert(param->value);
                    param_values[i] = param->value;
                }

                IRInst *lod = loadVal(ir_mod, param_values[0]);
                IRInst *size = ts__irBuildQuerySizeLod(ir_mod, image, lod);
                IRInst *mip_levels = ts__irBuildQueryLevels(ir_mod, image);

                IRType *float_type = ts__irNewFloatType(ir_mod, 32);

                assert(image->type->kind == IR_TYPE_IMAGE);
                switch (image->type->image.dim)
                {
                case SpvDim1D: {
                    uint32_t index;

                    index = 0;
                    IRInst *width = ts__irBuildCompositeExtract(ir_mod, size, &index, 1);
                    width = ts__irBuildCast(ir_mod, float_type, width);

                    IRInst *mip_levels_float = ts__irBuildCast(ir_mod, float_type, mip_levels);

                    ts__irBuildStore(ir_mod, param_values[1], width);
                    ts__irBuildStore(ir_mod, param_values[2], mip_levels_float);

                    break;
                }
                case SpvDim2D: {
                    uint32_t index;

                    index = 0;
                    IRInst *width = ts__irBuildCompositeExtract(ir_mod, size, &index, 1);
                    width = ts__irBuildCast(ir_mod, float_type, width);

                    index = 1;
                    IRInst *height = ts__irBuildCompositeExtract(ir_mod, size, &index, 1);
                    height = ts__irBuildCast(ir_mod, float_type, height);

                    IRInst *mip_levels_float = ts__irBuildCast(ir_mod, float_type, mip_levels);

                    ts__irBuildStore(ir_mod, param_values[1], width);
                    ts__irBuildStore(ir_mod, param_values[2], height);
                    ts__irBuildStore(ir_mod, param_values[3], mip_levels_float);
                    break;
                }
                case SpvDim3D: {
                    uint32_t index;

                    index = 0;
                    IRInst *width = ts__irBuildCompositeExtract(ir_mod, size, &index, 1);
                    width = ts__irBuildCast(ir_mod, float_type, width);

                    index = 1;
                    IRInst *height = ts__irBuildCompositeExtract(ir_mod, size, &index, 1);
                    height = ts__irBuildCast(ir_mod, float_type, height);

                    index = 2;
                    IRInst *depth = ts__irBuildCompositeExtract(ir_mod, size, &index, 1);
                    depth = ts__irBuildCast(ir_mod, float_type, depth);

                    IRInst *mip_levels_float = ts__irBuildCast(ir_mod, float_type, mip_levels);

                    ts__irBuildStore(ir_mod, param_values[1], width);
                    ts__irBuildStore(ir_mod, param_values[2], height);
                    ts__irBuildStore(ir_mod, param_values[3], depth);
                    ts__irBuildStore(ir_mod, param_values[4], mip_levels_float);
                    break;
                }
                case SpvDimCube: {
                    uint32_t index;

                    index = 0;
                    IRInst *width = ts__irBuildCompositeExtract(ir_mod, size, &index, 1);
                    width = ts__irBuildCast(ir_mod, float_type, width);

                    index = 1;
                    IRInst *height = ts__irBuildCompositeExtract(ir_mod, size, &index, 1);
                    height = ts__irBuildCast(ir_mod, float_type, height);

                    IRInst *mip_levels_float = ts__irBuildCast(ir_mod, float_type, mip_levels);

                    ts__irBuildStore(ir_mod, param_values[1], width);
                    ts__irBuildStore(ir_mod, param_values[2], height);
                    ts__irBuildStore(ir_mod, param_values[3], mip_levels_float);
                    break;
                }

                default: assert(0); break;
                }
            }
            else
            {
                assert(0);
            }
            break;
        }

        AstType *func_type = expr->func_call.func_expr->type;
        assert(func_type);

        if (func_type->kind == TYPE_TYPE)
        {
            // Type constructor
            AstType *constructed_type = expr->func_call.func_expr->as_type;
            assert(constructed_type);
            IRType *ir_constructed_type = convertTypeToIR(ast_mod, ir_mod, constructed_type);

            uint32_t param_count = arrLength(expr->func_call.params);
            ArrayOfAstExprPtr params = expr->func_call.params;

            switch (constructed_type->kind)
            {
            case TYPE_VECTOR: {
                uint32_t elem_count = constructed_type->vector.size;
                IRInst **elems = NEW_ARRAY(compiler, IRInst *, elem_count);

                uint32_t elem_index = 0;
                for (uint32_t i = 0; i < param_count; ++i)
                {
                    astBuildExpr(ast_mod, ir_mod, params.ptr[i]);
                    assert(params.ptr[i]->value);
                    IRInst *param_val = loadVal(ir_mod, params.ptr[i]->value);

                    if (params.ptr[i]->type->kind == TYPE_VECTOR)
                    {
                        for (uint32_t j = 0;
                            j < ts__getTypeElemCount(params.ptr[i]->type);
                            ++j)
                        {
                            elems[elem_index++] =
                                ts__irBuildCompositeExtract(ir_mod, param_val, &j, 1);
                        }
                    }
                    else
                    {
                        elems[elem_index++] = param_val;
                    }
                }

                assert(elem_index == elem_count);

                expr->value = ts__irBuildCompositeConstruct(
                    ir_mod, ir_constructed_type, elems, elem_count);
                break;
            }
            case TYPE_MATRIX: {
                AstType *col_type = constructed_type->matrix.col_type;
                assert(col_type->kind == TYPE_VECTOR);

                uint32_t col_count = constructed_type->matrix.col_count;
                uint32_t col_size = col_type->vector.size;

                uint32_t matrix_elem_count = col_size * col_count;
                assert(matrix_elem_count == param_count);

                IRInst **columns = NEW_ARRAY(compiler, IRInst *, col_count);

                for (uint32_t i = 0; i < col_count; ++i)
                {
                    IRInst **col_fields = NEW_ARRAY(compiler, IRInst *, col_size);
                    for (uint32_t j = 0; j < col_size; ++j)
                    {
                        AstExpr *elem = params.ptr[i * col_size + j];

                        astBuildExpr(ast_mod, ir_mod, elem);
                        assert(elem->value);
                        col_fields[j] = loadVal(ir_mod, elem->value);
                    }

                    columns[i] = ts__irBuildCompositeConstruct(
                        ir_mod, ir_constructed_type->matrix.col_type, col_fields, col_size);
                }

                expr->value =
                    ts__irBuildCompositeConstruct(ir_mod, ir_constructed_type, columns, col_count);
                break;
            }
            case TYPE_INT:
            case TYPE_FLOAT: {
                assert(param_count == 1);

                astBuildExpr(ast_mod, ir_mod, params.ptr[0]);
                assert(params.ptr[0]->value);
                expr->value = ts__irBuildCast(
                    ir_mod, ir_constructed_type, loadVal(ir_mod, params.ptr[0]->value));
                break;
            }
            default: {
                assert(0);
                break;
            }
            }
        }
        else
        {
            // Actual function call
            assert(func_type->kind == TYPE_FUNC);

            astBuildExpr(ast_mod, ir_mod, expr->func_call.func_expr);
            IRInst *func_val = expr->func_call.func_expr->value;
            assert(func_val);

            uint32_t param_count = arrLength(expr->func_call.params);
            IRInst **param_values = NEW_ARRAY(compiler, IRInst *, param_count);

            for (uint32_t i = 0; i < param_count; ++i)
            {
                AstExpr *param = expr->func_call.params.ptr[i];
                astBuildExpr(ast_mod, ir_mod, param);
                assert(param->value);
                param_values[i] = param->value;
                if (func_type->func.params[i]->kind == TYPE_POINTER)
                {
                    if (!isLvalue(param_values[i]))
                    {
                        ts__addErr(
                            compiler,
                            &param->loc,
                            "function parameter needs to be an lvalue");
                    }
                }
                else
                {
                    param_values[i] = loadVal(ir_mod, param_values[i]);
                }
            }

            expr->value = ts__irBuildFuncCall(ir_mod, func_val, param_values, param_count);
        }

        break;
    }

    case EXPR_UNARY: {
        switch (expr->unary.op)
        {
        case UNOP_NEG: {
            astBuildExpr(ast_mod, ir_mod, expr->unary.right);
            IRInst *right_val = loadVal(ir_mod, expr->unary.right->value);

            AstType *scalar_type = ts__getScalarType(expr->type);
            SpvOp op = {0};

            switch (scalar_type->kind)
            {
            case TYPE_FLOAT: op = SpvOpFNegate; break;
            case TYPE_INT: op = SpvOpSNegate; break;
            default: assert(0); break;
            }

            IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, expr->type);
            expr->value = ts__irBuildUnary(ir_mod, op, ir_type, right_val);
            break;
        }

        case UNOP_NOT: {
            assert(0); // TODO: broken: use SpvINotEqual + SpvOpLogicalNot

            astBuildExpr(ast_mod, ir_mod, expr->unary.right);
            IRInst *right_val = loadVal(ir_mod, expr->unary.right->value);

            IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, expr->type);
            expr->value = ts__irBuildUnary(ir_mod, SpvOpNot, ir_type, right_val);
            break;
        }

        case UNOP_PRE_INC: {
            astBuildExpr(ast_mod, ir_mod, expr->unary.right);
            IRInst *right_val_ptr = expr->unary.right->value;
            IRInst *right_val = loadVal(ir_mod, right_val_ptr);

            IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, expr->type);

            SpvOp op_kind;
            IRInst *one_val = NULL;

            switch (expr->type->kind)
            {
            case TYPE_FLOAT:
                op_kind = SpvOpFAdd;
                one_val = ts__irBuildConstFloat(ir_mod, ir_type, 1.0);
                break;
            case TYPE_INT:
                op_kind = SpvOpIAdd;
                one_val = ts__irBuildConstInt(ir_mod, ir_type, 1);
                break;
            default: assert(0); break;
            }

            expr->value = ts__irBuildBinary(ir_mod, op_kind, ir_type, right_val, one_val);

            if (isLvalue(right_val_ptr))
            {
                ts__irBuildStore(ir_mod, right_val_ptr, expr->value);
            }
            break;
        }

        case UNOP_PRE_DEC: {
            astBuildExpr(ast_mod, ir_mod, expr->unary.right);
            IRInst *right_val_ptr = expr->unary.right->value;
            IRInst *right_val = loadVal(ir_mod, right_val_ptr);

            IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, expr->type);

            SpvOp op_kind;
            IRInst *one_val = NULL;

            switch (expr->type->kind)
            {
            case TYPE_FLOAT:
                op_kind = SpvOpFSub;
                one_val = ts__irBuildConstFloat(ir_mod, ir_type, 1.0);
                break;
            case TYPE_INT:
                op_kind = SpvOpISub;
                one_val = ts__irBuildConstInt(ir_mod, ir_type, 1);
                break;
            default: assert(0); break;
            }

            expr->value = ts__irBuildBinary(ir_mod, op_kind, ir_type, right_val, one_val);

            if (isLvalue(right_val_ptr))
            {
                ts__irBuildStore(ir_mod, right_val_ptr, expr->value);
            }
            break;
        }

        case UNOP_POST_INC: {
            astBuildExpr(ast_mod, ir_mod, expr->unary.right);
            IRInst *right_val_ptr = expr->unary.right->value;
            IRInst *right_val = loadVal(ir_mod, right_val_ptr);

            IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, expr->type);

            SpvOp op_kind;
            IRInst *one_val = NULL;

            switch (expr->type->kind)
            {
            case TYPE_FLOAT:
                op_kind = SpvOpFAdd;
                one_val = ts__irBuildConstFloat(ir_mod, ir_type, 1.0);
                break;
            case TYPE_INT:
                op_kind = SpvOpIAdd;
                one_val = ts__irBuildConstInt(ir_mod, ir_type, 1);
                break;
            default: assert(0); break;
            }

            if (isLvalue(right_val_ptr))
            {
                ts__irBuildStore(
                    ir_mod,
                    right_val_ptr,
                    ts__irBuildBinary(ir_mod, op_kind, ir_type, right_val, one_val));
            }

            expr->value = right_val;
            break;
        }

        case UNOP_POST_DEC: {
            astBuildExpr(ast_mod, ir_mod, expr->unary.right);
            IRInst *right_val_ptr = expr->unary.right->value;
            IRInst *right_val = loadVal(ir_mod, right_val_ptr);

            IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, expr->type);

            SpvOp op_kind;
            IRInst *one_val = NULL;

            switch (expr->type->kind)
            {
            case TYPE_FLOAT:
                op_kind = SpvOpFSub;
                one_val = ts__irBuildConstFloat(ir_mod, ir_type, 1.0);
                break;
            case TYPE_INT:
                op_kind = SpvOpISub;
                one_val = ts__irBuildConstInt(ir_mod, ir_type, 1);
                break;
            default: assert(0); break;
            }

            if (isLvalue(right_val_ptr))
            {
                ts__irBuildStore(
                    ir_mod,
                    right_val_ptr,
                    ts__irBuildBinary(ir_mod, op_kind, ir_type, right_val, one_val));
            }

            expr->value = right_val;
            break;
        }

        case UNOP_BITNOT: {
            astBuildExpr(ast_mod, ir_mod, expr->unary.right);
            IRInst *right_val = loadVal(ir_mod, expr->unary.right->value);

            IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, expr->type);
            expr->value = ts__irBuildUnary(ir_mod, SpvOpNot, ir_type, right_val);
            break;
        }
        }
        break;
    }

    case EXPR_BINARY: {
        astBuildExpr(ast_mod, ir_mod, expr->binary.left);
        IRInst *left_val = loadVal(ir_mod, expr->binary.left->value);
        astBuildExpr(ast_mod, ir_mod, expr->binary.right);
        IRInst *right_val = loadVal(ir_mod, expr->binary.right->value);

        AstType *elem_type = ts__getElemType(expr->binary.left->type);
        assert(elem_type);
        SpvOp op = {0};

        IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, expr->type);

        switch (expr->binary.op)
        {
        case BINOP_ADD:
        case BINOP_SUB:
        case BINOP_MUL:
        case BINOP_DIV:
        case BINOP_MOD: {
            if (left_val->type->kind == IR_TYPE_VECTOR &&
                right_val->type->kind != IR_TYPE_VECTOR)
            {
                uint32_t field_count = left_val->type->vector.size;

                IRInst **fields = NEW_ARRAY(compiler, IRInst *, field_count);
                for (uint32_t i = 0; i < field_count; ++i)
                {
                    fields[i] = right_val;
                }
                right_val = ts__irBuildCompositeConstruct(ir_mod, ir_type, fields, field_count);
            }
            else if (
                right_val->type->kind == IR_TYPE_VECTOR &&
                left_val->type->kind != IR_TYPE_VECTOR)
            {
                uint32_t field_count = right_val->type->vector.size;

                IRInst **fields = NEW_ARRAY(compiler, IRInst *, field_count);
                for (uint32_t i = 0; i < field_count; ++i)
                {
                    fields[i] = left_val;
                }
                left_val = ts__irBuildCompositeConstruct(ir_mod, ir_type, fields, field_count);
            }

            break;
        }

        default: break;
        }

        switch (expr->binary.op)
        {
        case BINOP_ADD: {
            switch (elem_type->kind)
            {
            case TYPE_FLOAT: op = SpvOpFAdd; break;
            case TYPE_INT: op = SpvOpIAdd; break;
            default: assert(0); break;
            }
            break;
        }
        case BINOP_SUB: {
            switch (elem_type->kind)
            {
            case TYPE_FLOAT: op = SpvOpFSub; break;
            case TYPE_INT: op = SpvOpISub; break;
            default: assert(0); break;
            }
            break;
        }
        case BINOP_MUL: {
            switch (elem_type->kind)
            {
            case TYPE_FLOAT: op = SpvOpFMul; break;
            case TYPE_INT: op = SpvOpIMul; break;
            default: assert(0); break;
            }
            break;
        }
        case BINOP_DIV: {
            switch (elem_type->kind)
            {
            case TYPE_FLOAT: op = SpvOpFDiv; break;
            case TYPE_INT:
                if (elem_type->int_.is_signed)
                    op = SpvOpSDiv;
                else
                    op = SpvOpUDiv;
                break;
            default: assert(0); break;
            }
            break;
        }
        case BINOP_MOD: {
            switch (elem_type->kind)
            {
            case TYPE_FLOAT: op = SpvOpFRem; break;
            case TYPE_INT:
                if (elem_type->int_.is_signed)
                    op = SpvOpSMod;
                else
                    op = SpvOpUMod;
                break;
            default: assert(0); break;
            }
            break;
        }

        case BINOP_EQ: {
            switch (elem_type->kind)
            {
            case TYPE_FLOAT: op = SpvOpFOrdEqual; break;
            case TYPE_INT: op = SpvOpIEqual; break;
            default: assert(0); break;
            }
            break;
        }

        case BINOP_NOTEQ: {
            switch (elem_type->kind)
            {
            case TYPE_FLOAT: op = SpvOpFOrdNotEqual; break;
            case TYPE_INT: op = SpvOpINotEqual; break;
            default: assert(0); break;
            }
            break;
        }

        case BINOP_LESS: {
            switch (elem_type->kind)
            {
            case TYPE_FLOAT: op = SpvOpFOrdLessThan; break;
            case TYPE_INT:
                if (elem_type->int_.is_signed)
                    op = SpvOpSLessThan;
                else
                    op = SpvOpULessThan;
                break;
            default: assert(0); break;
            }
            break;
        }

        case BINOP_LESSEQ: {
            switch (elem_type->kind)
            {
            case TYPE_FLOAT: op = SpvOpFOrdLessThanEqual; break;
            case TYPE_INT:
                if (elem_type->int_.is_signed)
                    op = SpvOpSLessThanEqual;
                else
                    op = SpvOpULessThanEqual;
                break;
            default: assert(0); break;
            }
            break;
        }

        case BINOP_GREATER: {
            switch (elem_type->kind)
            {
            case TYPE_FLOAT: op = SpvOpFOrdGreaterThan; break;
            case TYPE_INT:
                if (elem_type->int_.is_signed)
                    op = SpvOpSGreaterThan;
                else
                    op = SpvOpUGreaterThan;
                break;
            default: assert(0); break;
            }
            break;
        }

        case BINOP_GREATEREQ: {
            switch (elem_type->kind)
            {
            case TYPE_FLOAT: op = SpvOpFOrdGreaterThanEqual; break;
            case TYPE_INT:
                if (elem_type->int_.is_signed)
                    op = SpvOpSGreaterThanEqual;
                else
                    op = SpvOpUGreaterThanEqual;
                break;
            default: assert(0); break;
            }
            break;
        }
        case BINOP_LSHIFT: {
            op = SpvOpShiftLeftLogical;
            break;
        }
        case BINOP_RSHIFT: {
            op = SpvOpShiftRightLogical;
            break;
        }
        case BINOP_BITXOR: {
            op = SpvOpBitwiseXor;
            break;
        }
        case BINOP_BITOR: {
            op = SpvOpBitwiseOr;
            break;
        }
        case BINOP_BITAND: {
            op = SpvOpBitwiseAnd;
            break;
        }
        case BINOP_LOGICAL_AND: {
            op = SpvOpLogicalAnd;
            break;
        }
        case BINOP_LOGICAL_OR: {
            op = SpvOpLogicalOr;
            break;
        }
        }

        expr->value = ts__irBuildBinary(ir_mod, op, ir_type, left_val, right_val);

        break;
    }

    case EXPR_TERNARY: {
        IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, expr->type);

        astBuildExpr(ast_mod, ir_mod, expr->ternary.cond);
        IRInst *cond = loadVal(ir_mod, expr->ternary.cond->value);

        astBuildExpr(ast_mod, ir_mod, expr->ternary.true_expr);
        IRInst *true_value = loadVal(ir_mod, expr->ternary.true_expr->value);

        astBuildExpr(ast_mod, ir_mod, expr->ternary.false_expr);
        IRInst *false_value = loadVal(ir_mod, expr->ternary.false_expr->value);

        if (ir_type->kind == IR_TYPE_VECTOR)
        {
            IRInst **fields = NEW_ARRAY(ir_mod->compiler, IRInst *, ir_type->vector.size);
            for (uint32_t i = 0; i < ir_type->vector.size; ++i)
            {
                fields[i] = cond;
            }

            IRType *cond_vec_type = ts__irNewVectorType(ir_mod, cond->type, ir_type->vector.size);
            cond =
                ts__irBuildCompositeConstruct(ir_mod, cond_vec_type, fields, ir_type->vector.size);
        }

        expr->value = ts__irBuildSelect(ir_mod, ir_type, cond, true_value, false_value);
        break;
    }

    case EXPR_AUTO_CAST:
    {
        if (expr->type->kind == TYPE_VECTOR &&
            ts__getScalarTypeNoVec(expr->auto_cast.sub->type))
        {
            astBuildExpr(ast_mod, ir_mod, expr->auto_cast.sub);
            IRInst *value = loadVal(ir_mod, expr->auto_cast.sub->value);
            if (expr->auto_cast.sub->type != expr->type->vector.elem_type)
            {
                IRType *dst_type =
                    convertTypeToIR(ast_mod, ir_mod, expr->type->vector.elem_type);
                value = ts__irBuildCast(ir_mod, dst_type, value);
            }

            // Construct vector from element
            uint32_t field_count = expr->type->vector.size;
            IRInst **fields = NEW_ARRAY(compiler, IRInst*, field_count);
            for (uint32_t i = 0; i < field_count; ++i)
            {
                fields[i] = value;
            }

            IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, expr->type);
            expr->value = ts__irBuildCompositeConstruct(
                ir_mod,
                ir_type,
                fields,
                field_count);
        }
        else if (ts__getScalarTypeNoVec(expr->type) &&
                 ts__getScalarTypeNoVec(expr->auto_cast.sub->type))
        {
            // Cast scalar
            astBuildExpr(ast_mod, ir_mod, expr->auto_cast.sub);
            IRInst *value = loadVal(ir_mod, expr->auto_cast.sub->value);

            IRType *dst_type = convertTypeToIR(ast_mod, ir_mod, expr->type);
            expr->value = ts__irBuildCast(ir_mod, dst_type, value);
        }
        else if (expr->type->kind == TYPE_VECTOR &&
                 expr->auto_cast.sub->type->kind == TYPE_VECTOR &&
                 expr->auto_cast.sub->type->vector.size == expr->type->vector.size)
        {
            // Cast vector to vector
            astBuildExpr(ast_mod, ir_mod, expr->auto_cast.sub);
            IRInst *value = loadVal(ir_mod, expr->auto_cast.sub->value);

            IRType *dst_type = convertTypeToIR(ast_mod, ir_mod, expr->type);
            expr->value = ts__irBuildCast(ir_mod, dst_type, value);
        }
        else
        {
            assert(0);
        }
        break;
    }

    case EXPR_RUNTIME_ARRAY_TYPE:
    case EXPR_ARRAY_TYPE:
    case EXPR_CONSTANT_BUFFER_TYPE:
    case EXPR_STRUCTURED_BUFFER_TYPE:
    case EXPR_RW_STRUCTURED_BUFFER_TYPE:
    case EXPR_SAMPLER_TYPE:
    case EXPR_TEXTURE_TYPE: {
        break;
    }
    }
}

static void astBuildStmt(Module *ast_mod, IRModule *ir_mod, AstStmt *stmt)
{
    TsCompiler *compiler = ast_mod->compiler;

    switch (stmt->kind)
    {
    case STMT_DECL: {
        astBuildDecl(ast_mod, ir_mod, stmt->decl);
        break;
    }

    case STMT_EXPR: {
        astBuildExpr(ast_mod, ir_mod, stmt->expr);
        break;
    }

    case STMT_RETURN: {
        if (stmt->return_.value)
        {
            astBuildExpr(ast_mod, ir_mod, stmt->return_.value);
            assert(stmt->return_.value->value);
            ts__irBuildReturn(ir_mod, loadVal(ir_mod, stmt->return_.value->value));
        }
        else
        {
            ts__irBuildReturn(ir_mod, NULL);
        }
        break;
    }

    case STMT_DISCARD: {
        ts__irBuildDiscard(ir_mod);
        break;
    }

    case STMT_CONTINUE: {
        assert(ast_mod->continue_stack.len > 0);
        IRInst *block = ast_mod->continue_stack.ptr[ast_mod->continue_stack.len - 1];
        ts__irBuildBr(ir_mod, block, NULL, NULL);
        break;
    }

    case STMT_BREAK: {
        assert(ast_mod->break_stack.len > 0);
        IRInst *block = ast_mod->break_stack.ptr[ast_mod->break_stack.len - 1];
        ts__irBuildBr(ir_mod, block, NULL, NULL);
        break;
    }

    case STMT_BLOCK: {
        for (uint32_t i = 0; i < stmt->block.stmts.len; ++i)
        {
            AstStmt *sub_stmt = stmt->block.stmts.ptr[i];
            astBuildStmt(ast_mod, ir_mod, sub_stmt);

            if (ts__irBlockHasTerminator(ts__irGetCurrentBlock(ir_mod)))
            {
                break;
            }
        }
        break;
    }

    case STMT_IF: {
        astBuildExpr(ast_mod, ir_mod, stmt->if_.cond);
        IRInst *cond = stmt->if_.cond->value;
        assert(cond);
        cond = loadVal(ir_mod, cond);
        cond = boolVal(ir_mod, cond);

        IRInst *current_block = ts__irGetCurrentBlock(ir_mod);
        IRInst *func = current_block->block.func;

        IRInst *then_block = ts__irCreateBlock(ir_mod, func);
        IRInst *else_block = NULL;
        if (stmt->if_.else_stmt)
        {
            else_block = ts__irCreateBlock(ir_mod, func);
        }
        IRInst *merge_block = ts__irCreateBlock(ir_mod, func);
        if (!else_block) else_block = merge_block;

        ts__irBuildCondBr(ir_mod, cond, then_block, else_block, merge_block, NULL);

        // Then
        {
            ts__irPositionAtEnd(ir_mod, then_block);
            ts__irAddBlock(ir_mod, then_block);

            astBuildStmt(ast_mod, ir_mod, stmt->if_.if_stmt);

            if (!ts__irBlockHasTerminator(ts__irGetCurrentBlock(ir_mod)))
            {
                ts__irBuildBr(ir_mod, merge_block, NULL, NULL);
            }
        }

        // Else
        if (stmt->if_.else_stmt)
        {
            ts__irPositionAtEnd(ir_mod, else_block);
            ts__irAddBlock(ir_mod, else_block);

            astBuildStmt(ast_mod, ir_mod, stmt->if_.else_stmt);

            if (!ts__irBlockHasTerminator(ts__irGetCurrentBlock(ir_mod)))
            {
                ts__irBuildBr(ir_mod, merge_block, NULL, NULL);
            }
        }

        ts__irPositionAtEnd(ir_mod, merge_block);
        ts__irAddBlock(ir_mod, merge_block);

        break;
    }

    case STMT_WHILE: {
        IRInst *current_block = ts__irGetCurrentBlock(ir_mod);
        IRInst *func = current_block->block.func;

        IRInst *check_block = ts__irCreateBlock(ir_mod, func);
        IRInst *body_block = ts__irCreateBlock(ir_mod, func);
        IRInst *continue_block = ts__irCreateBlock(ir_mod, func);
        IRInst *merge_block = ts__irCreateBlock(ir_mod, func);

        ts__irBuildBr(ir_mod, check_block, NULL, NULL);

        {
            ts__irPositionAtEnd(ir_mod, check_block);
            ts__irAddBlock(ir_mod, check_block);

            astBuildExpr(ast_mod, ir_mod, stmt->while_.cond);
            IRInst *cond = stmt->while_.cond->value;
            assert(cond);
            cond = loadVal(ir_mod, cond);
            cond = boolVal(ir_mod, cond);

            if (!ts__irBlockHasTerminator(ts__irGetCurrentBlock(ir_mod)))
            {
                ts__irBuildCondBr(
                    ir_mod, cond, body_block, merge_block, merge_block, continue_block);
            }
        }

        {
            ts__irPositionAtEnd(ir_mod, body_block);
            ts__irAddBlock(ir_mod, body_block);

            arrPush(compiler, &ast_mod->continue_stack, continue_block);
            arrPush(compiler, &ast_mod->break_stack, merge_block);
            astBuildStmt(ast_mod, ir_mod, stmt->while_.stmt);
            arrPop(&ast_mod->continue_stack);
            arrPop(&ast_mod->break_stack);

            if (!ts__irBlockHasTerminator(ts__irGetCurrentBlock(ir_mod)))
            {
                ts__irBuildBr(ir_mod, continue_block, NULL, NULL);
            }
        }

        {
            ts__irPositionAtEnd(ir_mod, continue_block);
            ts__irAddBlock(ir_mod, continue_block);

            ts__irBuildBr(ir_mod, check_block, NULL, NULL);
        }

        ts__irPositionAtEnd(ir_mod, merge_block);
        ts__irAddBlock(ir_mod, merge_block);

        break;
    }

    case STMT_DO_WHILE: {
        IRInst *current_block = ts__irGetCurrentBlock(ir_mod);
        IRInst *func = current_block->block.func;

        IRInst *header_block = ts__irCreateBlock(ir_mod, func);
        IRInst *body_block = ts__irCreateBlock(ir_mod, func);
        IRInst *continue_block = ts__irCreateBlock(ir_mod, func);
        IRInst *merge_block = ts__irCreateBlock(ir_mod, func);

        ts__irBuildBr(ir_mod, header_block, NULL, NULL);

        {
            ts__irPositionAtEnd(ir_mod, header_block);
            ts__irAddBlock(ir_mod, header_block);

            ts__irBuildBr(ir_mod, body_block, merge_block, continue_block);
        }

        {
            ts__irPositionAtEnd(ir_mod, body_block);
            ts__irAddBlock(ir_mod, body_block);

            arrPush(compiler, &ast_mod->continue_stack, continue_block);
            arrPush(compiler, &ast_mod->break_stack, merge_block);
            astBuildStmt(ast_mod, ir_mod, stmt->do_while.stmt);
            arrPop(&ast_mod->continue_stack);
            arrPop(&ast_mod->break_stack);

            if (!ts__irBlockHasTerminator(ts__irGetCurrentBlock(ir_mod)))
            {
                ts__irBuildBr(ir_mod, continue_block, NULL, NULL);
            }
        }

        {
            ts__irPositionAtEnd(ir_mod, continue_block);
            ts__irAddBlock(ir_mod, continue_block);

            astBuildExpr(ast_mod, ir_mod, stmt->do_while.cond);
            IRInst *cond = stmt->do_while.cond->value;
            assert(cond);
            cond = loadVal(ir_mod, cond);
            cond = boolVal(ir_mod, cond);

            if (!ts__irBlockHasTerminator(ts__irGetCurrentBlock(ir_mod)))
            {
                ts__irBuildCondBr(ir_mod, cond, header_block, merge_block, NULL, NULL);
            }
        }

        ts__irPositionAtEnd(ir_mod, merge_block);
        ts__irAddBlock(ir_mod, merge_block);

        break;
    }

    case STMT_FOR: {
        IRInst *current_block = ts__irGetCurrentBlock(ir_mod);
        IRInst *func = current_block->block.func;

        if (stmt->for_.init)
        {
            astBuildStmt(ast_mod, ir_mod, stmt->for_.init);
        }

        IRInst *check_block = ts__irCreateBlock(ir_mod, func);
        IRInst *body_block = ts__irCreateBlock(ir_mod, func);
        IRInst *continue_block = ts__irCreateBlock(ir_mod, func);
        IRInst *merge_block = ts__irCreateBlock(ir_mod, func);

        ts__irBuildBr(ir_mod, check_block, NULL, NULL);

        {
            ts__irPositionAtEnd(ir_mod, check_block);
            ts__irAddBlock(ir_mod, check_block);

            IRInst *cond = NULL;

            if (stmt->for_.cond)
            {
                astBuildExpr(ast_mod, ir_mod, stmt->for_.cond);
                cond = stmt->for_.cond->value;
                assert(cond);
                cond = loadVal(ir_mod, cond);
                cond = boolVal(ir_mod, cond);
            }
            else
            {
                cond = ts__irBuildConstBool(ir_mod, true);
            }

            if (!ts__irBlockHasTerminator(ts__irGetCurrentBlock(ir_mod)))
            {
                ts__irBuildCondBr(
                    ir_mod, cond, body_block, merge_block, merge_block, continue_block);
            }
        }

        {
            ts__irPositionAtEnd(ir_mod, body_block);
            ts__irAddBlock(ir_mod, body_block);

            arrPush(compiler, &ast_mod->continue_stack, continue_block);
            arrPush(compiler, &ast_mod->break_stack, merge_block);
            astBuildStmt(ast_mod, ir_mod, stmt->for_.stmt);
            arrPop(&ast_mod->continue_stack);
            arrPop(&ast_mod->break_stack);

            if (!ts__irBlockHasTerminator(ts__irGetCurrentBlock(ir_mod)))
            {
                ts__irBuildBr(ir_mod, continue_block, NULL, NULL);
            }
        }

        {
            ts__irPositionAtEnd(ir_mod, continue_block);
            ts__irAddBlock(ir_mod, continue_block);

            if (stmt->for_.inc)
            {
                astBuildExpr(ast_mod, ir_mod, stmt->for_.inc);
            }

            ts__irBuildBr(ir_mod, check_block, NULL, NULL);
        }

        ts__irPositionAtEnd(ir_mod, merge_block);
        ts__irAddBlock(ir_mod, merge_block);

        break;
    }
    }
}

static void astBuildDecl(Module *ast_mod, IRModule *ir_mod, AstDecl *decl)
{
    TsCompiler *compiler = ast_mod->compiler;

    switch (decl->kind)
    {
    case DECL_FUNC: {
        if (!decl->func.called) break;

        IRInst *entry_block = ts__irCreateBlock(ir_mod, decl->value);
        ts__irPositionAtEnd(ir_mod, entry_block);
        ts__irAddBlock(ir_mod, entry_block);

        IRInst **ptr_func_params =
            NEW_ARRAY(compiler, IRInst *, decl->func.params.len);

        // We make allocas for input parameters
        for (uint32_t i = 0; i < arrLength(decl->func.params); ++i)
        {
            AstDecl *param_decl = decl->func.params.ptr[i];
            ptr_func_params[i] = param_decl->value;

            switch (param_decl->var.parameter_kind)
            {
            case PARAM_IN: {
                IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, param_decl->type);
                ptr_func_params[i] = ts__irBuildAlloca(ir_mod, ir_type);
                break;
            }

            default: break;
            }
        }

        // Make allocas for local variables
        for (uint32_t i = 0; i < arrLength(decl->func.var_decls); ++i)
        {
            AstDecl *var_decl = decl->func.var_decls.ptr[i];
            IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, var_decl->type);
            var_decl->value = ts__irBuildAlloca(ir_mod, ir_type);
        }

        // Then we copy inputs into the allocas:
        for (uint32_t i = 0; i < arrLength(decl->func.params); ++i)
        {
            AstDecl *param_decl = decl->func.params.ptr[i];
            switch (param_decl->var.parameter_kind)
            {
            case PARAM_IN: {
                ts__irBuildStore(ir_mod, ptr_func_params[i], loadVal(ir_mod, param_decl->value));
                param_decl->value = ptr_func_params[i];
                break;
            }

            default: break;
            }
        }

        // And then generate the statements:
        for (uint32_t i = 0; i < arrLength(decl->func.stmts); ++i)
        {
            AstStmt *stmt = decl->func.stmts.ptr[i];
            astBuildStmt(ast_mod, ir_mod, stmt);
            if (ts__irBlockHasTerminator(ts__irGetCurrentBlock(ir_mod)))
            {
                break;
            }
        }

        // TODO: copy stuff back to output parameters

        if (!ts__irBlockHasTerminator(ts__irGetCurrentBlock(ir_mod)))
        {
            ts__irBuildReturn(ir_mod, NULL);
        }

        break;
    }

    case DECL_VAR: {
        IRInst *initializer = NULL;

        assert(decl->value);

        if (decl->var.value_expr)
        {
            astBuildExpr(ast_mod, ir_mod, decl->var.value_expr);
            initializer = decl->var.value_expr->value;
            assert(initializer);
            initializer = loadVal(ir_mod, initializer);
            ts__irBuildStore(ir_mod, decl->value, initializer);
        }

        if (decl->name)
        {
            ts__irSetInstName(ir_mod, decl->value, decl->name);
        }

        break;
    }

    case DECL_STRUCT:
    {
        assert(decl->as_type);
        if (decl->name)
        {
            AstType *type = decl->as_type;
            IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, type);
            ts__irSetTypeName(ir_mod, ir_type, decl->name);

            for (uint32_t i = 0; i < type->struct_.field_count; ++i)
            {
                AstDecl *field_decl = type->struct_.field_decls[i];
                ts__irSetMemberName(ir_mod, ir_type, i, field_decl->name);
            }
        }
        break;
    }

    case DECL_ALIAS: break;
    case DECL_STRUCT_FIELD: break;
    }
}

static bool astSemanticToDecoration(
    Module *ast_mod,
    char* semantic,
    IRDecoration *dec)
{
    if (ts__strcasecmp(semantic, "SV_Position") == 0 &&
        ast_mod->stage == TS_SHADER_STAGE_FRAGMENT)
    {
        dec->kind = SpvDecorationBuiltIn;
        dec->value = SpvBuiltInFragCoord;
    }
    else if (ts__strcasecmp(semantic, "SV_Position") == 0 &&
        ast_mod->stage == TS_SHADER_STAGE_VERTEX)
    {
        dec->kind = SpvDecorationBuiltIn;
        dec->value = SpvBuiltInPosition;
    }
    else if (
        ts__strcasecmp(semantic, "SV_InstanceID") == 0 &&
        ast_mod->stage == TS_SHADER_STAGE_VERTEX)
    {
        dec->kind = SpvDecorationBuiltIn;
        dec->value = SpvBuiltInInstanceIndex;
    }
    else if (
        ts__strcasecmp(semantic, "SV_VertexID") == 0 &&
        ast_mod->stage == TS_SHADER_STAGE_VERTEX)
    {
        dec->kind = SpvDecorationBuiltIn;
        dec->value = SpvBuiltInVertexIndex;
    }
    else if (
        ts__strcasecmp(semantic, "SV_DispatchThreadID") == 0 &&
        ast_mod->stage == TS_SHADER_STAGE_COMPUTE)
    {
        dec->kind = SpvDecorationBuiltIn;
        dec->value = SpvBuiltInGlobalInvocationId;
    }
    else if (
        ts__strcasecmp(semantic, "SV_GroupID") == 0 &&
        ast_mod->stage == TS_SHADER_STAGE_COMPUTE)
    {
        dec->kind = SpvDecorationBuiltIn;
        dec->value = SpvBuiltInWorkgroupId;
    }
    else if (
        ts__strcasecmp(semantic, "SV_GroupIndex") == 0 &&
        ast_mod->stage == TS_SHADER_STAGE_COMPUTE)
    {
        dec->kind = SpvDecorationBuiltIn;
        dec->value = SpvBuiltInLocalInvocationIndex;
    }
    else if (
        ts__strcasecmp(semantic, "SV_GroupThreadID") == 0 &&
        ast_mod->stage == TS_SHADER_STAGE_COMPUTE)
    {
        dec->kind = SpvDecorationBuiltIn;
        dec->value = SpvBuiltInLocalInvocationId;
    }
    else
    {
        return false;
    }

    return true;
}

static void astRecursivelyAddOutputs(
    Module *ast_mod,
    IRModule *ir_mod,
    AstDecl *decl,
    uint32_t *current_location,
    ArrayOfIRInstPtr *values) {

    TsCompiler *compiler = ast_mod->compiler;

    switch (decl->kind)
    {
    case DECL_FUNC:
    {
        AstType *return_type = decl->type->func.return_type;
        assert(return_type);

        if (return_type->kind == TYPE_STRUCT)
        {
            for (uint32_t i = 0; i < return_type->struct_.field_count; ++i)
            {
                AstDecl *struct_field_decl = return_type->struct_.field_decls[i];
                astRecursivelyAddOutputs(
                    ast_mod,
                    ir_mod,
                    struct_field_decl,
                    current_location,
                    values);
            }
        }
        else if (return_type->kind != TYPE_VOID)
        {
            assert(decl->semantic);

            IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, return_type);
            IRInst *value = ts__irAddOutput(ir_mod, ir_type);

            IRDecoration dec = {0};
            if (!astSemanticToDecoration(
                ast_mod,
                decl->semantic,
                &dec))
            {
                dec.kind = SpvDecorationLocation;
                dec.value = (*current_location)++;
            }

            ts__irDecorateInst(ir_mod, value, &dec);

            arrPush(compiler, values, value);
        }

        for (uint32_t i = 0; i < decl->func.params.len; ++i)
        {
            AstDecl *param_decl = decl->func.params.ptr[i];
            if (param_decl->var.parameter_kind == PARAM_OUT)
            {
                astRecursivelyAddOutputs(
                    ast_mod,
                    ir_mod,
                    param_decl,
                    current_location,
                    values);
            }
        }
        break;
    }

    case DECL_STRUCT_FIELD:
    case DECL_VAR:
    {
        if (decl->type->kind == TYPE_STRUCT)
        {
            for (uint32_t i = 0; i < decl->type->struct_.field_count; ++i)
            {
                AstDecl *struct_field_decl = decl->type->struct_.field_decls[i];
                astRecursivelyAddOutputs(
                    ast_mod,
                    ir_mod,
                    struct_field_decl,
                    current_location,
                    values);
            }
        }
        else
        {
            IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, decl->type);
            IRInst *value = ts__irAddOutput(ir_mod, ir_type);

            assert(decl->semantic);

            IRDecoration dec = {0};
            if (!astSemanticToDecoration(
                ast_mod,
                decl->semantic,
                &dec))
            {
                dec.kind = SpvDecorationLocation;
                dec.value = (*current_location)++;
            }

            ts__irDecorateInst(ir_mod, value, &dec);

            arrPush(compiler, values, value);
        }
        break;
    }

    default: break;
    }
}

static void astRecursivelyAddInputs(
    Module *ast_mod,
    IRModule *ir_mod,
    AstDecl *decl,
    uint32_t *current_location,
    ArrayOfIRInstPtr *values) {

    TsCompiler *compiler = ast_mod->compiler;

    switch (decl->kind)
    {
    case DECL_FUNC:
    {
        for (uint32_t i = 0; i < decl->func.params.len; ++i)
        {
            AstDecl *param_decl = decl->func.params.ptr[i];
            if (param_decl->var.parameter_kind == PARAM_IN)
            {
                astRecursivelyAddInputs(
                    ast_mod,
                    ir_mod,
                    param_decl,
                    current_location,
                    values);
            }
        }
        break;
    }

    case DECL_STRUCT_FIELD:
    case DECL_VAR:
    {
        if (decl->type->kind == TYPE_STRUCT)
        {
            for (uint32_t i = 0; i < decl->type->struct_.field_count; ++i)
            {
                AstDecl *struct_field_decl = decl->type->struct_.field_decls[i];
                astRecursivelyAddInputs(
                    ast_mod,
                    ir_mod,
                    struct_field_decl,
                    current_location,
                    values);
            }
        }
        else
        {
            IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, decl->type);
            IRInst *value = ts__irAddInput(ir_mod, ir_type);

            assert(decl->semantic);

            IRDecoration dec = {0};
            if (!astSemanticToDecoration(
                ast_mod,
                decl->semantic,
                &dec))
            {
                dec.kind = SpvDecorationLocation;
                dec.value = (*current_location)++;
            }

            ts__irDecorateInst(ir_mod, value, &dec);

            arrPush(compiler, values, value);
        }
        break;
    }

    default: break;
    }
}

void ts__astModuleBuild(Module *ast_mod, IRModule *ir_mod)
{
    TsCompiler *compiler = ast_mod->compiler;

    // Add functions / globals
    for (uint32_t i = 0; i < ast_mod->decl_count; ++i)
    {
        AstDecl *decl = ast_mod->decls[i];
        switch (decl->kind)
        {
        case DECL_FUNC: {
            assert(decl->type);

            if (!decl->func.called) break;

            IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, decl->type);
            decl->value = ts__irAddFunction(ir_mod, ir_type, SpvFunctionControlInlineMask);

            for (uint32_t k = 0; k < arrLength(decl->func.params); ++k)
            {
                AstDecl *param = decl->func.params.ptr[k];
                IRType *ir_param_type = convertTypeToIR(ast_mod, ir_mod, param->type);
                bool by_reference = false;
                if (param->var.parameter_kind == PARAM_OUT)
                {
                    ir_param_type =
                        ts__irNewPointerType(ir_mod, SpvStorageClassFunction, ir_param_type);
                    by_reference = true;
                }
                param->value =
                    ts__irAddFuncParam(ir_mod, decl->value, ir_param_type, by_reference);
            }

            if (decl->name)
            {
                ts__irSetInstName(ir_mod, decl->value, decl->name);
            }

            break;
        }

        case DECL_VAR: {
            IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, decl->type);
            SpvStorageClass storage_class;

            switch (decl->var.storage_class)
            {
            case VAR_STORAGE_CLASS_UNIFORM:
            {
                switch (ir_type->kind)
                {
                case IR_TYPE_SAMPLER:
                case IR_TYPE_IMAGE:
                case IR_TYPE_SAMPLED_IMAGE:
                    storage_class = SpvStorageClassUniformConstant;
                    break;
                default: storage_class = SpvStorageClassUniform; break;
                }
                break;
            }
            case VAR_STORAGE_CLASS_GROUPSHARED:
            {
                storage_class = SpvStorageClassWorkgroup;
                break;
            }
            default: break;
            }

            decl->value = ts__irAddUniformGlobal(
                ir_mod,
                ir_type,
                storage_class);

            IRDecoration set_dec = {0};
            set_dec.kind = SpvDecorationDescriptorSet;
            set_dec.value = decl->var.set;
            ts__irDecorateInst(ir_mod, decl->value, &set_dec);

            IRDecoration binding_dec = {0};
            binding_dec.kind = SpvDecorationBinding;
            binding_dec.value = decl->var.binding;
            ts__irDecorateInst(ir_mod, decl->value, &binding_dec);

            if (decl->name)
            {
                ts__irSetInstName(ir_mod, decl->value, decl->name);
            }

            break;
        }

        default: break;
        }
    }

    for (uint32_t i = 0; i < ast_mod->decl_count; ++i)
    {
        AstDecl *decl = ast_mod->decls[i];
        astBuildDecl(ast_mod, ir_mod, decl);
    }

    if (ast_mod->entry_point_func)
    {
        // Add entry point wrapper function
        IRType *func_wrapper_type = ts__irNewFuncType(
            ir_mod,
            ts__irNewBasicType(ir_mod, IR_TYPE_VOID),
            NULL,
            0);

        IRInst *entry_func_wrapper = ts__irAddFunction(
            ir_mod,
            func_wrapper_type,
            SpvFunctionControlMaskNone);

        AstDecl *func_decl = ast_mod->entry_point_func;

        IRInst *entry_block = ts__irCreateBlock(ir_mod, entry_func_wrapper);
        ts__irPositionAtEnd(ir_mod, entry_block);
        ts__irAddBlock(ir_mod, entry_block);

        ArrayOfIRInstPtr inputs = {0};
        ArrayOfIRInstPtr outputs = {0};

        assert(func_decl->type->kind == TYPE_FUNC);

        // Create the stage inputs/outputs
        uint32_t current_input_loc = 0;
        astRecursivelyAddInputs(
            ast_mod,
            ir_mod,
            func_decl,
            &current_input_loc,
            &inputs);

        uint32_t current_output_loc = 0;
        astRecursivelyAddOutputs(
            ast_mod,
            ir_mod,
            func_decl,
            &current_output_loc,
            &outputs);

        // Build out param allocas
        ArrayOfIRInstPtr out_param_allocas = {0};
        for (uint32_t i = 0; i < func_decl->func.params.len; ++i)
        {
            AstDecl *param_decl = func_decl->func.params.ptr[i];
            if (param_decl->var.parameter_kind == PARAM_OUT)
            {
                IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, param_decl->type);
                IRInst *alloca = ts__irBuildAlloca(ir_mod, ir_type);
                arrPush(compiler, &out_param_allocas, alloca);
            }
        }

        // Construct input parameters
        current_input_loc = 0;
        ArrayOfIRInstPtr in_params = {0};
        for (uint32_t i = 0; i < func_decl->func.params.len; ++i)
        {
            AstDecl *param_decl = func_decl->func.params.ptr[i];
            if (param_decl->var.parameter_kind == PARAM_IN)
            {
                if (param_decl->type->kind == TYPE_STRUCT)
                {
                    // Parameter is a struct, so we have to build it
                    ArrayOfIRInstPtr fields = {0};

                    for (uint32_t j = 0; j < param_decl->type->struct_.field_count; ++j)
                    {
                        IRInst *loaded_input = ts__irBuildLoad(ir_mod, inputs.ptr[current_input_loc]);
                        arrPush(compiler, &fields, loaded_input);
                        current_input_loc++;
                    }

                    IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, param_decl->type);
                    IRInst *composite = ts__irBuildCompositeConstruct(
                        ir_mod, ir_type, fields.ptr, fields.len);

                    arrPush(compiler, &in_params, composite);
                }
                else
                {
                    // Parameter is a simple value, just push it
                    IRInst *loaded_input = ts__irBuildLoad(ir_mod, inputs.ptr[current_input_loc]);
                    arrPush(compiler, &in_params, loaded_input);
                    current_input_loc++;
                }
            }
        }

        // Join the in/out params in one array
        ArrayOfIRInstPtr func_params = {0};
        current_input_loc = 0;
        current_output_loc = 0;
        for (uint32_t i = 0; i < func_decl->func.params.len; ++i)
        {
            AstDecl *param_decl = func_decl->func.params.ptr[i];
            if (param_decl->var.parameter_kind == PARAM_IN)
            {
                arrPush(compiler, &func_params, in_params.ptr[current_input_loc]);
                current_input_loc++;
            }
            else if (param_decl->var.parameter_kind == PARAM_OUT)
            {
                arrPush(compiler, &func_params, out_param_allocas.ptr[current_output_loc]);
                current_output_loc++;
            }
        }

        IRInst *return_value = ts__irBuildFuncCall(
            ir_mod,
            func_decl->value,
            func_params.ptr,
            func_params.len);

        IRType *uint_type = ts__irNewIntType(ir_mod, 32, false);

        current_output_loc = 0;

        // Copy returned value to output variable
        AstType *return_type = func_decl->type->func.return_type;
        if (return_type->kind != TYPE_VOID)
        {
            if (return_type->kind == TYPE_STRUCT)
            {
                // Parameter is a struct, so we have to build it
                for (uint32_t j = 0; j < return_type->struct_.field_count; ++j)
                {
                    IRInst *field_value = ts__irBuildCompositeExtract(
                        ir_mod,
                        return_value,
                        &j,
                        1);
                    ts__irBuildStore(ir_mod, outputs.ptr[current_output_loc], field_value);
                    current_output_loc++;
                }
            }
            else
            {
                ts__irBuildStore(ir_mod, outputs.ptr[current_output_loc], return_value);
                current_output_loc++;
            }
        }

        // Copy values from the output allocas back into the real output variables
        uint32_t out_param_index = 0;
        for (uint32_t i = 0; i < func_decl->func.params.len; ++i)
        {
            AstDecl *param_decl = func_decl->func.params.ptr[i];
            if (param_decl->var.parameter_kind == PARAM_OUT)
            {
                if (param_decl->type->kind == TYPE_STRUCT)
                {
                    // Parameter is a struct, so we have to build it
                    for (uint32_t j = 0; j < param_decl->type->struct_.field_count; ++j)
                    {
                        AstDecl *struct_field_decl = param_decl->type->struct_.field_decls[j];
                        IRType *ir_type = convertTypeToIR(ast_mod, ir_mod, struct_field_decl->type);

                        IRInst *index = ts__irBuildConstInt(ir_mod, uint_type, j);
                        IRInst *field_ptr = ts__irBuildAccessChain(
                            ir_mod,
                            ir_type,
                            out_param_allocas.ptr[out_param_index],
                            &index,
                            1);
                        IRInst *loaded_output = ts__irBuildLoad(ir_mod, field_ptr);
                        ts__irBuildStore(ir_mod, outputs.ptr[current_output_loc], loaded_output);
                        current_output_loc++;
                    }
                }
                else
                {
                    IRInst *loaded_output = ts__irBuildLoad(
                        ir_mod, out_param_allocas.ptr[out_param_index]);
                    ts__irBuildStore(ir_mod, outputs.ptr[current_output_loc], loaded_output);
                    current_output_loc++;
                }

                out_param_index++;
            }
        }


        ts__irBuildReturn(ir_mod, NULL);

        SpvExecutionModel execution_model;
        switch (ast_mod->stage)
        {
        case TS_SHADER_STAGE_COMPUTE: execution_model = SpvExecutionModelGLCompute; break;
        case TS_SHADER_STAGE_FRAGMENT: execution_model = SpvExecutionModelFragment; break;
        case TS_SHADER_STAGE_VERTEX: execution_model = SpvExecutionModelVertex; break;
        }

        ArrayOfIRInstPtr entry_point_globals = {0};

        for (uint32_t i = 0; i < inputs.len; ++i)
        {
            arrPush(compiler, &entry_point_globals, inputs.ptr[i]);
        }

        for (uint32_t i = 0; i < outputs.len; ++i)
        {
            arrPush(compiler, &entry_point_globals, outputs.ptr[i]);
        }

        IRInst *entry_point = ts__irAddEntryPoint(
            ir_mod,
            func_decl->name,
            entry_func_wrapper,
            execution_model,
            entry_point_globals.ptr,
            entry_point_globals.len);

        if (ast_mod->stage == TS_SHADER_STAGE_COMPUTE)
        {
            assert(ast_mod->compute_dims[0] >= 1);
            assert(ast_mod->compute_dims[1] >= 1);
            assert(ast_mod->compute_dims[2] >= 1);

            ts__irEntryPointSetComputeDims(
                entry_point,
                ast_mod->compute_dims[0],
                ast_mod->compute_dims[1],
                ast_mod->compute_dims[2]);
        }
    }
}
