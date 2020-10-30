/**
 * This file is part of the tinyshader library.
 * See tinyshader.h for license details.
 */
#include "tinyshader_internal.h"

static char *irTypeToString(TsCompiler *compiler, IRType *type)
{
    if (type->string) return type->string;

    char *prefix = NULL;
    char *storage_class = NULL;
    char *sub = NULL;
    char *postfix = NULL;

    switch (type->kind)
    {
    case IR_TYPE_VOID: prefix = "void"; break;

    case IR_TYPE_BOOL: prefix = "bool"; break;

    case IR_TYPE_FLOAT:
        ts__sbReset(&compiler->sb);
        ts__sbSprintf(&compiler->sb, "float%u", type->float_.bits);
        prefix = ts__sbBuild(&compiler->sb, &compiler->alloc);
        break;

    case IR_TYPE_INT:
        ts__sbReset(&compiler->sb);

        if (type->int_.is_signed)
            ts__sbAppend(&compiler->sb, "int");
        else
            ts__sbAppend(&compiler->sb, "uint");

        ts__sbSprintf(&compiler->sb, "%u", type->int_.bits);
        prefix = ts__sbBuild(&compiler->sb, &compiler->alloc);
        break;

    case IR_TYPE_VECTOR:
        ts__sbReset(&compiler->sb);
        ts__sbSprintf(&compiler->sb, "vec%u", type->vector.size);
        prefix = ts__sbBuild(&compiler->sb, &compiler->alloc);

        sub = irTypeToString(compiler, type->vector.elem_type);
        break;
    case IR_TYPE_MATRIX:
        ts__sbReset(&compiler->sb);
        ts__sbSprintf(&compiler->sb, "mat%u", type->matrix.col_count);
        prefix = ts__sbBuild(&compiler->sb, &compiler->alloc);

        sub = irTypeToString(compiler, type->matrix.col_type);
        break;

    case IR_TYPE_POINTER: {
        prefix = "ptr";

        switch (type->ptr.storage_class)
        {
        case SpvStorageClassInput: storage_class = "input"; break;
        case SpvStorageClassOutput: storage_class = "output"; break;
        case SpvStorageClassUniformConstant: storage_class = "constant"; break;
        case SpvStorageClassUniform: storage_class = "uniform"; break;
        case SpvStorageClassStorageBuffer: storage_class = "storage"; break;
        case SpvStorageClassFunction: storage_class = "function"; break;
        case SpvStorageClassWorkgroup: storage_class = "groupshared"; break;

        default: assert(0); break;
        }

        assert(type->ptr.sub);
        sub = irTypeToString(compiler, type->ptr.sub);
        break;
    }

    case IR_TYPE_RUNTIME_ARRAY: {
        prefix = "r_array";

        assert(type->array.sub);
        sub = irTypeToString(compiler, type->array.sub);
        break;
    }

    case IR_TYPE_FUNC: {
        prefix = "func";

        char *return_type = irTypeToString(compiler, type->func.return_type);
        char **params = NEW_ARRAY(compiler, char *, type->func.param_count);
        for (uint32_t i = 0; i < type->func.param_count; ++i)
        {
            params[i] = irTypeToString(compiler, type->func.params[i]);
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

    case IR_TYPE_STRUCT: {
        ts__sbReset(&compiler->sb);
        ts__sbSprintf(
            &compiler->sb, "struct%u%s", strlen(type->struct_.name), type->struct_.name);
        prefix = ts__sbBuild(&compiler->sb, &compiler->alloc);
        break;
    }

    case IR_TYPE_SAMPLER: {
        prefix = "sampler";
        break;
    }

    case IR_TYPE_IMAGE: {
        prefix = "image";

        sub = irTypeToString(compiler, type->image.sampled_type);

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

    case IR_TYPE_SAMPLED_IMAGE: {
        prefix = "sampledImage";
        sub = irTypeToString(compiler, type->sampled_image.image_type);
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

static bool irIsTypeCastable(IRType *src_type, IRType *dst_type, SpvOp *op)
{
    if (src_type == dst_type)
    {
        *op = SpvOpNop;
        return true;
    }
    else if (src_type->kind == IR_TYPE_INT)
    {
        if (src_type->int_.is_signed)
        {
            if (dst_type->kind == IR_TYPE_INT)
            {
                if (dst_type->int_.is_signed)
                {
                    *op = SpvOpSConvert;
                    return true;
                }
                else
                {
                    return false;
                }
            }
            else if (dst_type->kind == IR_TYPE_FLOAT)
            {
                *op = SpvOpConvertSToF;
                return true;
            }
            else
            {
                return false;
            }
        }
        else
        {
            if (dst_type->kind == IR_TYPE_INT)
            {
                if (dst_type->int_.is_signed)
                {
                    return false;
                }
                else
                {
                    *op = SpvOpUConvert;
                    return true;
                }
            }
            else if (dst_type->kind == IR_TYPE_FLOAT)
            {
                *op = SpvOpConvertUToF;
                return true;
            }
            else
            {
                return false;
            }
        }
    }
    else if (src_type->kind == IR_TYPE_FLOAT)
    {
        if (dst_type->kind == IR_TYPE_INT)
        {
            if (dst_type->int_.is_signed)
            {
                *op = SpvOpConvertFToS;
                return true;
            }
            else
            {
                *op = SpvOpConvertFToU;
                return true;
            }
        }
        else if (dst_type->kind == IR_TYPE_FLOAT)
        {
            *op = SpvOpFConvert;
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

void ts__irTypeSetDecorations(
    IRModule *m, IRType *type, IRDecoration *decorations, uint32_t decoration_count)
{
    type->decoration_count = decoration_count;

    type->decorations = NEW_ARRAY(m->compiler, IRDecoration, decoration_count);
    memcpy(type->decorations, decorations, sizeof(IRDecoration) * decoration_count);
}

static IRType *irGetCachedType(IRModule *m, IRType *type)
{
    char *type_string = irTypeToString(m->compiler, type);
    assert(type_string);
    assert(strlen(type_string) > 0);

    IRType *found_type = NULL;
    if (ts__hashGet(&m->type_cache, type_string, (void **)&found_type))
    {
        assert(found_type);
        return found_type;
    }

    ts__hashSet(&m->type_cache, type_string, type);

    return type;
}

IRType *ts__irNewBasicType(IRModule *m, IRTypeKind kind)
{
    IRType *ty = NEW(m->compiler, IRType);
    ty->kind = kind;
    return irGetCachedType(m, ty);
}

IRType *ts__irNewPointerType(IRModule *m, SpvStorageClass storage_class, IRType *sub)
{
    IRType *ty = NEW(m->compiler, IRType);
    ty->kind = IR_TYPE_POINTER;
    ty->ptr.storage_class = storage_class;
    ty->ptr.sub = sub;
    return irGetCachedType(m, ty);
}

IRType *ts__irNewVectorType(IRModule *m, IRType *elem_type, uint32_t size)
{
    IRType *ty = NEW(m->compiler, IRType);
    ty->kind = IR_TYPE_VECTOR;
    ty->vector.elem_type = elem_type;
    ty->vector.size = size;
    return irGetCachedType(m, ty);
}

IRType *ts__irNewMatrixType(IRModule *m, IRType *col_type, uint32_t col_count)
{
    IRType *ty = NEW(m->compiler, IRType);
    ty->kind = IR_TYPE_MATRIX;
    ty->matrix.col_type = col_type;
    ty->matrix.col_count = col_count;
    return irGetCachedType(m, ty);
}

IRType *ts__irNewFloatType(IRModule *m, uint32_t bits)
{
    IRType *ty = NEW(m->compiler, IRType);
    ty->kind = IR_TYPE_FLOAT;
    ty->float_.bits = bits;
    return irGetCachedType(m, ty);
}

IRType *ts__irNewIntType(IRModule *m, uint32_t bits, bool is_signed)
{
    IRType *ty = NEW(m->compiler, IRType);
    ty->kind = IR_TYPE_INT;
    ty->int_.bits = bits;
    ty->int_.is_signed = is_signed;
    return irGetCachedType(m, ty);
}

IRType *ts__irNewRuntimeArrayType(IRModule *m, IRType *sub)
{
    IRType *ty = NEW(m->compiler, IRType);
    ty->kind = IR_TYPE_RUNTIME_ARRAY;
    ty->array.sub = sub;
    return irGetCachedType(m, ty);
}

IRType *
ts__irNewFuncType(IRModule *m, IRType *return_type, IRType **params, uint32_t param_count)
{
    IRType *ty = NEW(m->compiler, IRType);
    ty->kind = IR_TYPE_FUNC;
    ty->func.return_type = return_type;
    if (param_count > 0)
    {
        ty->func.param_count = param_count;
        ty->func.params = NEW_ARRAY(m->compiler, IRType *, param_count);
        memcpy(ty->func.params, params, sizeof(IRType *) * param_count);
    }
    return irGetCachedType(m, ty);
}

IRType *ts__irNewStructType(
    IRModule *m,
    char *name,
    IRType **fields,
    uint32_t field_count,
    IRMemberDecoration *field_decorations,
    uint32_t field_decoration_count)
{
    IRType *ty = NEW(m->compiler, IRType);
    ty->kind = IR_TYPE_STRUCT;
    ty->struct_.name = name;

    if (field_count > 0)
    {
        ty->struct_.field_count = field_count;

        ty->struct_.fields = NEW_ARRAY(m->compiler, IRType *, field_count);
        memcpy(ty->struct_.fields, fields, sizeof(IRType *) * field_count);

        ty->struct_.field_decoration_count = field_decoration_count;

        ty->struct_.field_decorations =
            NEW_ARRAY(m->compiler, IRMemberDecoration, field_decoration_count);
        memcpy(
            ty->struct_.field_decorations,
            field_decorations,
            sizeof(IRMemberDecoration) * field_decoration_count);
    }

    return irGetCachedType(m, ty);
}

IRType *ts__irNewImageType(IRModule *m, IRType *sampled_type, SpvDim dim)
{
    IRType *ty = NEW(m->compiler, IRType);
    ty->kind = IR_TYPE_IMAGE;
    ty->image.sampled_type = sampled_type;
    ty->image.dim = dim;
    ty->image.depth = 0;
    ty->image.arrayed = 0;
    ty->image.multisampled = 0;
    ty->image.sampled = 1;
    ty->image.format = SpvImageFormatUnknown;
    return irGetCachedType(m, ty);
}

IRType *ts__irNewSampledImageType(IRModule *m, IRType *image_type)
{
    IRType *ty = NEW(m->compiler, IRType);
    ty->kind = IR_TYPE_SAMPLED_IMAGE;
    ty->sampled_image.image_type = image_type;
    return irGetCachedType(m, ty);
}

static uint32_t irModuleReserveId(IRModule *m)
{
    return m->id_bound++;
}

IRInst *ts__irAddEntryPoint(
    IRModule *m,
    char *name,
    IRInst *func,
    SpvExecutionModel execution_model,
    IRInst **globals,
    uint32_t global_count)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_ENTRY_POINT;
    inst->entry_point.name = name;
    inst->entry_point.func = func;
    inst->entry_point.execution_model = execution_model;
    inst->entry_point.globals = globals;
    inst->entry_point.global_count = global_count;

    arrPush(m->compiler, &m->entry_points, inst);
    return inst;
}

void
ts__irEntryPointSetComputeDims(IRInst *entry_point, uint32_t x, uint32_t y, uint32_t z)
{
    entry_point->entry_point.compute_dims.x = x;
    entry_point->entry_point.compute_dims.y = y;
    entry_point->entry_point.compute_dims.z = z;
}

IRInst *ts__irAddFunction(IRModule *m, IRType *func_type)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->id = irModuleReserveId(m);
    inst->kind = IR_INST_FUNCTION;
    inst->type = func_type;

    arrPush(m->compiler, &m->functions, inst);

    return inst;
}

IRInst *
ts__irAddFuncParam(IRModule *m, IRInst *func, IRType *type, bool is_by_reference)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->id = irModuleReserveId(m);
    inst->kind = IR_INST_FUNC_PARAM;
    inst->type = type;
    inst->func_param.is_by_reference = is_by_reference;

    arrPush(m->compiler, &func->func.params, inst);

    return inst;
}

// Does not add the block to the function
IRInst *ts__irCreateBlock(IRModule *m, IRInst *func)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_BLOCK;

    inst->id = irModuleReserveId(m);

    inst->block.func = func;

    return inst;
}

// Finally adds the block to the function
void ts__irAddBlock(IRModule *m, IRInst *block)
{
    assert(block->block.func->kind == IR_INST_FUNCTION);
    arrPush(m->compiler, &block->block.func->func.blocks, block);
}

IRInst *ts__irAddGlobal(IRModule *m, IRType *type, SpvStorageClass storage_class)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->id = irModuleReserveId(m);
    inst->kind = IR_INST_VARIABLE;
    inst->var.storage_class = storage_class;
    inst->type = ts__irNewPointerType(m, inst->var.storage_class, type);

    arrPush(m->compiler, &m->globals, inst);
    arrPush(m->compiler, &m->all_globals, inst);

    return inst;
}

IRInst *ts__irAddInput(IRModule *m, IRInst *func, IRType *type)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->id = irModuleReserveId(m);
    inst->kind = IR_INST_VARIABLE;
    inst->var.storage_class = SpvStorageClassInput;
    inst->type = ts__irNewPointerType(m, inst->var.storage_class, type);

    arrPush(m->compiler, &func->func.inputs, inst);
    arrPush(m->compiler, &m->all_globals, inst);

    return inst;
}

IRInst *ts__irAddOutput(IRModule *m, IRInst *func, IRType *type)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->id = irModuleReserveId(m);
    inst->kind = IR_INST_VARIABLE;
    inst->var.storage_class = SpvStorageClassOutput;
    inst->type = ts__irNewPointerType(m, inst->var.storage_class, type);

    arrPush(m->compiler, &func->func.outputs, inst);
    arrPush(m->compiler, &m->all_globals, inst);

    return inst;
}

IRInst *ts__irGetCurrentBlock(IRModule *m)
{
    return m->current_block;
}

void ts__irPositionAtEnd(IRModule *m, IRInst *block)
{
    m->current_block = block;
}

bool ts__irBlockHasTerminator(IRInst *block)
{
    if (arrLength(block->block.insts) == 0) return false;

    IRInst *last_inst = block->block.insts.ptr[arrLength(block->block.insts) - 1];
    switch (last_inst->kind)
    {
    case IR_INST_COND_BRANCH:
    case IR_INST_BRANCH:
    case IR_INST_DISCARD:
    case IR_INST_RETURN: return true;
    default: return false;
    }

    return false;
}

static char *irConstToString(TsCompiler *compiler, IRInst *inst)
{
    ts__sbReset(&compiler->sb);

    switch (inst->kind)
    {
    case IR_INST_CONSTANT: {
        if (inst->type->kind == IR_TYPE_FLOAT)
        {
            switch (inst->type->float_.bits)
            {
            case 32:
                ts__sbSprintf(&compiler->sb, "float%f", *((float *)inst->constant.value));
                break;
            case 64:
                ts__sbSprintf(
                    &compiler->sb, "double%lf", *((double *)inst->constant.value));
                break;
            default: assert(0); break;
            }
        }
        else if (inst->type->kind == IR_TYPE_INT)
        {
            if (inst->type->int_.is_signed)
            {
                switch (inst->type->int_.bits)
                {
                case 8:
                    ts__sbSprintf(
                        &compiler->sb, "char%c", *((char *)inst->constant.value));
                    break;
                case 16:
                    ts__sbSprintf(
                        &compiler->sb, "short%h", *((short *)inst->constant.value));
                    break;
                case 32:
                    ts__sbSprintf(&compiler->sb, "int%d", *((int *)inst->constant.value));
                    break;
                case 64:
                    ts__sbSprintf(
                        &compiler->sb, "long%ld", *((long *)inst->constant.value));
                    break;
                default: assert(0); break;
                }
            }
            else
            {
                switch (inst->type->int_.bits)
                {
                case 8:
                    ts__sbSprintf(
                        &compiler->sb,
                        "uchar%uc",
                        *((unsigned char *)inst->constant.value));
                    break;
                case 16:
                    ts__sbSprintf(
                        &compiler->sb,
                        "ushort%hu",
                        *((unsigned short *)inst->constant.value));
                    break;
                case 32:
                    ts__sbSprintf(
                        &compiler->sb, "uint%u", *((unsigned int *)inst->constant.value));
                    break;
                case 64:
                    ts__sbSprintf(
                        &compiler->sb,
                        "ulong%lu",
                        *((unsigned long *)inst->constant.value));
                    break;
                default: assert(0); break;
                }
            }
        }
        else
        {
            assert(0);
        }
        break;
    }

    case IR_INST_CONSTANT_BOOL: {
        if (inst->constant_bool.value)
        {
            ts__sbAppendChar(&compiler->sb, 't');
        }
        else
        {
            ts__sbAppendChar(&compiler->sb, 'f');
        }
        break;
    }

    default: assert(0); break;
    }

    char *const_name = ts__sbBuild(&compiler->sb, &compiler->alloc);
    return const_name;
}

static IRInst *irGetCachedConst(IRModule *m, IRInst *inst)
{
    char *const_string = irConstToString(m->compiler, inst);
    assert(const_string);
    assert(strlen(const_string) > 0);

    IRInst *found_inst = NULL;
    if (ts__hashGet(&m->const_cache, const_string, (void **)&found_inst))
    {
        assert(found_inst);
        return found_inst;
    }

    ts__hashSet(&m->const_cache, const_string, inst);
    arrPush(m->compiler, &m->constants, inst);

    return inst;
}

////////////////////////////////
//
// IR instruction builders
//
////////////////////////////////

IRInst *ts__irBuildConstFloat(IRModule *m, IRType *type, double value)
{
    assert(type->kind == IR_TYPE_FLOAT);

    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_CONSTANT;

    inst->type = type;

    if (type->float_.bits == 32)
    {
        float *new_value = NEW(m->compiler, float);
        *new_value = (float)value;
        inst->constant.value = new_value;
        inst->constant.value_size_bytes = sizeof(float);
    }
    else if (type->float_.bits == 64)
    {
        double *new_value = NEW(m->compiler, double);
        *new_value = value;
        inst->constant.value = new_value;
        inst->constant.value_size_bytes = sizeof(double);
    }
    else
    {
        assert(0);
    }

    inst = irGetCachedConst(m, inst);

    return inst;
}

IRInst *ts__irBuildConstInt(IRModule *m, IRType *type, uint64_t value)
{
    assert(type->kind == IR_TYPE_INT);

    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_CONSTANT;

    inst->type = type;

    inst->constant.value_size_bytes = type->int_.bits / 8;

    switch (type->int_.bits)
    {
    case 8: {
        uint8_t *new_value = NEW(m->compiler, uint8_t);
        *new_value = (uint8_t)value;
        inst->constant.value = new_value;
        break;
    }

    case 16: {
        uint16_t *new_value = NEW(m->compiler, uint16_t);
        *new_value = (uint16_t)value;
        inst->constant.value = new_value;
        break;
    }

    case 32: {
        uint32_t *new_value = NEW(m->compiler, uint32_t);
        *new_value = (uint32_t)value;
        inst->constant.value = new_value;
        break;
    }

    case 64: {
        uint64_t *new_value = NEW(m->compiler, uint64_t);
        *new_value = (uint64_t)value;
        inst->constant.value = new_value;
        break;
    }

    default: assert(0);
    }

    inst = irGetCachedConst(m, inst);

    return inst;
}

IRInst *ts__irBuildConstBool(IRModule *m, bool value)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_CONSTANT_BOOL;

    inst->type = ts__irNewBasicType(m, IR_TYPE_BOOL);
    inst->constant_bool.value = value;

    inst = irGetCachedConst(m, inst);

    return inst;
}

IRInst *ts__irBuildAlloca(IRModule *m, IRType *type)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_VARIABLE;
    inst->var.storage_class = SpvStorageClassFunction;
    inst->type = ts__irNewPointerType(m, inst->var.storage_class, type);

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);

    return inst;
}

void ts__irBuildStore(IRModule *m, IRInst *pointer, IRInst *value)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_STORE;
    inst->store.pointer = pointer;
    inst->store.value = value;

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);
}

IRInst *ts__irBuildLoad(IRModule *m, IRInst *pointer)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_LOAD;

    assert(pointer->type);
    assert(pointer->type->kind == IR_TYPE_POINTER);

    inst->type = pointer->type->ptr.sub;
    inst->load.pointer = pointer;

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);

    return inst;
}

IRInst *ts__irBuildAccessChain(
    IRModule *m, IRType *type, IRInst *base, IRInst **indices, uint32_t index_count)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_ACCESS_CHAIN;

    inst->type = ts__irNewPointerType(m, base->type->ptr.storage_class, type);

    assert(base->type->kind == IR_TYPE_POINTER);

    inst->access_chain.base = base;
    inst->access_chain.indices = indices;
    inst->access_chain.index_count = index_count;

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);

    return inst;
}

IRInst *ts__irBuildVectorShuffle(
    IRModule *m,
    IRInst *vector_a,
    IRInst *vector_b,
    uint32_t *indices,
    uint32_t index_count)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_VECTOR_SHUFFLE;

    assert(vector_a->type->kind == IR_TYPE_VECTOR);
    assert(vector_b->type->kind == IR_TYPE_VECTOR);
    assert(vector_a->type->vector.elem_type == vector_b->type->vector.elem_type);

    inst->type = ts__irNewVectorType(m, vector_a->type->vector.elem_type, index_count);

    inst->vector_shuffle.vector_a = vector_a;
    inst->vector_shuffle.vector_b = vector_b;
    inst->vector_shuffle.indices = indices;
    inst->vector_shuffle.index_count = index_count;

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);

    return inst;
}

IRInst *ts__irBuildCompositeExtract(
    IRModule *m, IRInst *value, uint32_t *indices, uint32_t index_count)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_COMPOSITE_EXTRACT;

    if (value->type->kind == IR_TYPE_VECTOR)
    {
        if (index_count == 1)
        {
            inst->type = value->type->vector.elem_type;
        }
        else
        {
            inst->type = ts__irNewVectorType(m, value->type->vector.elem_type, index_count);
        }
    }
    else
    {
        assert(0);
    }

    inst->composite_extract.value = value;

    inst->composite_extract.indices = NEW_ARRAY(m->compiler, uint32_t, index_count);
    memcpy(
        inst->composite_extract.indices,
        indices,
        sizeof(uint32_t) * index_count);

    inst->composite_extract.index_count = index_count;

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);

    return inst;
}

IRInst *ts__irBuildCompositeConstruct(
    IRModule *m, IRType *type, IRInst **fields, uint32_t field_count)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_COMPOSITE_CONSTRUCT;

    inst->type = type;

    inst->composite_construct.fields = fields;
    inst->composite_construct.field_count = field_count;

    IRInst *block = ts__irGetCurrentBlock(m);
    assert(block);
    arrPush(m->compiler, &block->block.insts, inst);

    return inst;
}

IRInst *
ts__irBuildFuncCall(IRModule *m, IRInst *function, IRInst **params, uint32_t param_count)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_FUNC_CALL;

    inst->type = function->type->func.return_type;
    assert(inst->type);

    assert(param_count == function->type->func.param_count);

    inst->func_call.func = function;
    inst->func_call.params = params;
    inst->func_call.param_count = param_count;

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);

    return inst;
}

IRInst *ts__irBuildBuiltinCall(
    IRModule *m,
    IRBuiltinInstKind kind,
    IRType *result_type,
    IRInst **params,
    uint32_t param_count)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_BUILTIN_CALL;

    inst->type = result_type;
    assert(inst->type);

    inst->builtin_call.kind = kind;
    inst->builtin_call.params = params;
    inst->builtin_call.param_count = param_count;

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);

    return inst;
}

IRInst *ts__irBuildBarrier(
    IRModule *m,
    bool with_group_sync,
    uint32_t execution_scope,
    uint32_t memory_scope,
    uint32_t semantics)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_BARRIER;

    inst->type = ts__irNewBasicType(m, IR_TYPE_VOID);
    assert(inst->type);

    IRType *uint_type = ts__irNewIntType(m, 32, false);

    inst->barrier.with_group_sync = with_group_sync;
    inst->barrier.execution_scope = ts__irBuildConstInt(m, uint_type, execution_scope);
    inst->barrier.memory_scope = ts__irBuildConstInt(m, uint_type, memory_scope);
    inst->barrier.semantics = ts__irBuildConstInt(m, uint_type, semantics);

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);

    return inst;
}

IRInst *
ts__irBuildSampleImplicitLod(IRModule *m, IRType *type, IRInst *image_sampler, IRInst *coords)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_SAMPLE_IMPLICIT_LOD;
    inst->type = type;
    assert(inst->type);

    inst->sample.image_sampler = image_sampler;
    inst->sample.coords = coords;

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);

    return inst;
}

IRInst *ts__irBuildSampleExplicitLod(
    IRModule *m, IRType *type, IRInst *image_sampler, IRInst *coords, IRInst *lod)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_SAMPLE_EXPLICIT_LOD;
    inst->type = type;
    assert(inst->type);

    inst->sample.image_sampler = image_sampler;
    inst->sample.coords = coords;
    inst->sample.lod = lod;

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);

    return inst;
}

IRInst *ts__irBuildQuerySizeLod(IRModule *m, IRInst *image, IRInst *lod)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_QUERY_SIZE_LOD;

    m->uses_image_query = true;

    uint32_t dim = 0;
    switch (image->type->image.dim)
    {
    case SpvDim1D: dim = 1; break;
    case SpvDim2D: dim = 2; break;
    case SpvDim3D: dim = 3; break;
    case SpvDimCube: dim = 2; break;

    default: assert(0); break;
    }

    assert(dim > 0);

    inst->type = ts__irNewVectorType(m, ts__irNewIntType(m, 32, false), dim);
    assert(inst->type);

    inst->query_size_lod.image = image;
    inst->query_size_lod.lod = lod;

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);

    return inst;
}

IRInst *ts__irBuildQueryLevels(IRModule *m, IRInst *image)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_QUERY_LEVELS;

    m->uses_image_query = true;

    inst->type = ts__irNewIntType(m, 32, false);
    assert(inst->type);

    inst->query_levels.image = image;

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);

    return inst;
}

IRInst *ts__irBuildCast(IRModule *m, IRType *dst_type, IRInst *value)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_CAST;
    inst->type = dst_type;

    IRType *src_type = value->type;
    assert(src_type);

    bool castable = irIsTypeCastable(src_type, dst_type, &inst->cast.op);
    assert(castable);

    if (inst->cast.op == SpvOpNop)
    {
        inst->cast.redundant = true;
    }

    inst->cast.dst_type = dst_type;
    inst->cast.value = value;

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);

    return inst;
}

IRInst *ts__irBuildUnary(IRModule *m, SpvOp op, IRType *type, IRInst *right)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_UNARY;
    inst->type = type;
    assert(inst->type);

    inst->unary.op = op;
    inst->unary.right = right;

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);

    return inst;
}

IRInst *
ts__irBuildBinary(IRModule *m, SpvOp op, IRType *type, IRInst *left, IRInst *right)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_BINARY;
    inst->type = type;
    assert(inst->type);

    inst->binary.op = op;
    inst->binary.left = left;
    inst->binary.right = right;

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);

    return inst;
}

IRInst *ts__irBuildSelect(
    IRModule *m, IRType *type, IRInst *cond, IRInst *true_value, IRInst *false_value)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_SELECT;
    inst->type = type;
    assert(inst->type);

    inst->select.cond = cond;
    inst->select.true_value = true_value;
    inst->select.false_value = false_value;

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);

    return inst;
}

void ts__irBuildReturn(IRModule *m, IRInst *value)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_RETURN;
    inst->return_.value = value;

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);
}

void ts__irBuildDiscard(IRModule *m)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_DISCARD;

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);
}

void
ts__irBuildBr(IRModule *m, IRInst *target, IRInst *merge_block, IRInst *continue_block)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_BRANCH;
    inst->branch.target = target;
    inst->branch.merge_block = merge_block;
    inst->branch.continue_block = continue_block;

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);
}

void ts__irBuildCondBr(
    IRModule *m,
    IRInst *cond,
    IRInst *true_block,
    IRInst *false_block,
    IRInst *merge_block,
    IRInst *continue_block)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_COND_BRANCH;
    inst->cond_branch.cond = cond;
    inst->cond_branch.true_block = true_block;
    inst->cond_branch.false_block = false_block;
    inst->cond_branch.merge_block = merge_block;
    inst->cond_branch.continue_block = continue_block;

    IRInst *block = ts__irGetCurrentBlock(m);
    arrPush(m->compiler, &block->block.insts, inst);
}

////////////////////////////////
//
// IR instruction encoding
//
////////////////////////////////

static void
irModuleEncodeInst(IRModule *m, SpvOp opcode, uint32_t *params, size_t params_count)
{
    uint32_t opcode_word = opcode;
    opcode_word |= ((uint16_t)(params_count + 1)) << 16;

    arrPush(m->compiler, &m->stream, opcode_word);
    for (uint32_t i = 0; i < params_count; ++i)
    {
        arrPush(m->compiler, &m->stream, params[i]);
    }
}

static void irModuleEncodeExtInst(
    IRModule *m, IRInst *inst, enum GLSLstd450 op, uint32_t *params, size_t params_count)
{
    uint32_t *inst_params = NEW_ARRAY(m->compiler, uint32_t, 4 + params_count);
    inst_params[0] = inst->type->id;
    inst_params[1] = inst->id;
    inst_params[2] = m->glsl_ext_inst;
    inst_params[3] = op;

    memcpy(&inst_params[4], params, params_count * sizeof(uint32_t));

    irModuleEncodeInst(m, SpvOpExtInst, inst_params, 4 + params_count);
}

static void irModuleReserveTypeIds(IRModule *m)
{
    for (uint32_t i = 0; i < arrLength(m->type_cache.values); ++i)
    {
        IRType *type = (IRType *)m->type_cache.values.ptr[i];
        type->id = irModuleReserveId(m);
    }
}

static void irModuleEncodeDecorations(IRModule *m)
{
    for (uint32_t i = 0; i < arrLength(m->all_globals); ++i)
    {
        IRInst *inst = m->all_globals.ptr[i];
        assert(inst->kind == IR_INST_VARIABLE);
        assert(inst->id);

        for (uint32_t j = 0; j < arrLength(inst->decorations); ++j)
        {
            IRDecoration *dec = &inst->decorations.ptr[j];
            uint32_t param_count = 3;
            uint32_t params[3] = {inst->id, dec->kind, dec->value};
            switch (dec->kind)
            {
            case SpvDecorationBlock: assert(0); break;
            default: break;
            }
            irModuleEncodeInst(m, SpvOpDecorate, params, param_count);
        }
    }

    for (uint32_t i = 0; i < arrLength(m->type_cache.values); ++i)
    {
        IRType *type = (IRType *)m->type_cache.values.ptr[i];
        assert(type->id > 0);

        if (type->kind == IR_TYPE_STRUCT)
        {
            for (uint32_t j = 0; j < type->struct_.field_decoration_count; ++j)
            {
                IRMemberDecoration *member_dec = &type->struct_.field_decorations[j];
                uint32_t params[4];
                params[0] = type->id;
                params[1] = member_dec->member_index;

                params[2] = member_dec->kind;
                params[3] = member_dec->value;

                uint32_t param_count = 4;
                switch (member_dec->kind)
                {
                case SpvDecorationNonWritable:
                case SpvDecorationRowMajor:
                case SpvDecorationColMajor: param_count = 3; break;
                default: break;
                }

                irModuleEncodeInst(m, SpvOpMemberDecorate, params, param_count);
            }
        }

        for (uint32_t j = 0; j < type->decoration_count; ++j)
        {
            IRDecoration *dec = &type->decorations[j];
            uint32_t param_count = 3;
            uint32_t params[3] = {type->id, dec->kind, dec->value};

            switch (dec->kind)
            {
            case SpvDecorationBlock: param_count = 2; break;
            case SpvDecorationBufferBlock: param_count = 2; break;
            default: break;
            }
            irModuleEncodeInst(m, SpvOpDecorate, params, param_count);
        }
    }
}

static void irModuleEncodeTypes(IRModule *m)
{
    for (uint32_t i = 0; i < arrLength(m->type_cache.values); ++i)
    {
        IRType *type = (IRType *)m->type_cache.values.ptr[i];

        switch (type->kind)
        {
        case IR_TYPE_VOID: {
            irModuleEncodeInst(m, SpvOpTypeVoid, &type->id, 1);
            break;
        }

        case IR_TYPE_BOOL: {
            irModuleEncodeInst(m, SpvOpTypeBool, &type->id, 1);
            break;
        }

        case IR_TYPE_FLOAT: {
            uint32_t params[2] = {type->id, type->float_.bits};
            irModuleEncodeInst(m, SpvOpTypeFloat, params, 2);
            break;
        }

        case IR_TYPE_INT: {
            uint32_t params[3] = {
                type->id, type->int_.bits, (uint32_t)type->int_.is_signed};
            irModuleEncodeInst(m, SpvOpTypeInt, params, 3);
            break;
        }

        case IR_TYPE_POINTER: {
            uint32_t params[3] = {type->id, type->ptr.storage_class, type->ptr.sub->id};
            irModuleEncodeInst(m, SpvOpTypePointer, params, 3);
            break;
        }

        case IR_TYPE_RUNTIME_ARRAY: {
            uint32_t params[2] = {type->id, type->array.sub->id};
            irModuleEncodeInst(m, SpvOpTypeRuntimeArray, params, 2);
            break;
        }

        case IR_TYPE_FUNC: {
            uint32_t param_count = 2 + type->func.param_count;
            uint32_t *params = NEW_ARRAY(m->compiler, uint32_t, param_count);

            params[0] = type->id;
            params[1] = type->func.return_type->id;

            for (uint32_t j = 0; j < type->func.param_count; ++j)
            {
                IRType *param_type = type->func.params[j];
                params[2 + j] = param_type->id;
            }

            irModuleEncodeInst(m, SpvOpTypeFunction, params, param_count);
            break;
        }

        case IR_TYPE_VECTOR: {
            uint32_t params[3] = {
                type->id, type->vector.elem_type->id, type->vector.size};
            irModuleEncodeInst(m, SpvOpTypeVector, params, 3);
            break;
        }

        case IR_TYPE_MATRIX: {
            uint32_t params[3] = {
                type->id, type->matrix.col_type->id, type->matrix.col_count};
            irModuleEncodeInst(m, SpvOpTypeMatrix, params, 3);
            break;
        }

        case IR_TYPE_STRUCT: {
            uint32_t word_count = 1 + type->struct_.field_count;
            uint32_t *params = NEW_ARRAY(m->compiler, uint32_t, word_count);
            params[0] = type->id;

            for (uint32_t i = 0; i < type->struct_.field_count; ++i)
            {
                params[1 + i] = type->struct_.fields[i]->id;
            }

            irModuleEncodeInst(m, SpvOpTypeStruct, params, word_count);
            break;
        }

        case IR_TYPE_SAMPLER: {
            uint32_t params[1] = {
                type->id,
            };
            irModuleEncodeInst(m, SpvOpTypeSampler, params, 1);
            break;
        }

        case IR_TYPE_IMAGE: {
            uint32_t params[8] = {
                type->id,
                type->image.sampled_type->id,
                type->image.dim,
                type->image.depth,
                type->image.arrayed,
                type->image.multisampled,
                type->image.sampled,
                type->image.format,
            };
            irModuleEncodeInst(m, SpvOpTypeImage, params, 8);
            break;
        }

        case IR_TYPE_SAMPLED_IMAGE: {
            uint32_t params[2] = {
                type->id,
                type->sampled_image.image_type->id,
            };
            irModuleEncodeInst(m, SpvOpTypeSampledImage, params, 2);
            break;
        }
        }
    }
}

static void irModuleEncodeEntryPoints(IRModule *m)
{
    TsCompiler *compiler = m->compiler;

    for (uint32_t i = 0; i < arrLength(m->entry_points); ++i)
    {
        IRInst *inst = m->entry_points.ptr[i];
        assert(inst->kind == IR_INST_ENTRY_POINT);
        assert(inst->entry_point.func);
        assert(inst->entry_point.func->id);

        {
            size_t name_len = strlen(inst->entry_point.name);
            uint32_t name_words = ROUND_TO_4(name_len + 1) / 4;

            uint32_t param_count = 2 + name_words + inst->entry_point.global_count;
            uint32_t *params = NEW_ARRAY(compiler, uint32_t, param_count);

            params[0] = inst->entry_point.execution_model;
            params[1] = inst->entry_point.func->id;

            memcpy(&params[2], inst->entry_point.name, name_len);

            uint32_t globals_start = 2 + name_words;
            for (uint32_t j = 0; j < inst->entry_point.global_count; ++j)
            {
                params[globals_start + j] = inst->entry_point.globals[j]->id;
            }

            irModuleEncodeInst(m, SpvOpEntryPoint, params, param_count);
        }

        if (inst->entry_point.execution_model == SpvExecutionModelFragment)
        {
            uint32_t params[2] = {
                inst->entry_point.func->id, SpvExecutionModeOriginUpperLeft};
            irModuleEncodeInst(m, SpvOpExecutionMode, params, 2);
        }

        if (inst->entry_point.execution_model == SpvExecutionModelGLCompute)
        {
            uint32_t params[5] = {
                inst->entry_point.func->id,
                SpvExecutionModeLocalSize,
                inst->entry_point.compute_dims.x,
                inst->entry_point.compute_dims.y,
                inst->entry_point.compute_dims.z,
            };
            irModuleEncodeInst(m, SpvOpExecutionMode, params, 5);
        }
    }
}

static void irModuleEncodeBlock(IRModule *m, IRInst *block)
{
    assert(ts__irBlockHasTerminator(block));

    {
        uint32_t params[1] = {block->id};
        irModuleEncodeInst(m, SpvOpLabel, params, 1);
    }

    for (uint32_t i = 0; i < arrLength(block->block.insts); ++i)
    {
        IRInst *inst = block->block.insts.ptr[i];
        switch (inst->kind)
        {
        case IR_INST_VARIABLE: {
            inst->id = irModuleReserveId(m);

            IRType *ptr_type = inst->type;
            SpvStorageClass storage_class = inst->var.storage_class;
            IRInst *initializer = inst->var.initializer;

            if (initializer)
            {
                uint32_t initializer_id = initializer->id;
                assert(initializer_id);
                uint32_t params[4] = {
                    ptr_type->id, inst->id, storage_class, initializer_id};
                irModuleEncodeInst(m, SpvOpVariable, params, 4);
            }
            else
            {
                uint32_t params[3] = {ptr_type->id, inst->id, storage_class};
                irModuleEncodeInst(m, SpvOpVariable, params, 3);
            }
            break;
        }

        case IR_INST_RETURN: {
            if (inst->return_.value)
            {
                uint32_t params[1] = {inst->return_.value->id};
                irModuleEncodeInst(m, SpvOpReturnValue, params, 1);
            }
            else
            {
                irModuleEncodeInst(m, SpvOpReturn, NULL, 0);
            }
            break;
        }

        case IR_INST_DISCARD: {
            irModuleEncodeInst(m, SpvOpKill, NULL, 0);
            break;
        }

        case IR_INST_BRANCH: {
            if (inst->branch.continue_block && inst->branch.merge_block)
            {
                assert(inst->branch.merge_block);

                uint32_t params[3] = {
                    inst->branch.merge_block->id,
                    inst->branch.continue_block->id,
                    SpvLoopControlMaskNone,
                };
                irModuleEncodeInst(m, SpvOpLoopMerge, params, 3);
            }

            uint32_t params[1] = {inst->branch.target->id};
            irModuleEncodeInst(m, SpvOpBranch, params, 1);
            break;
        }

        case IR_INST_COND_BRANCH: {
            if (!inst->cond_branch.continue_block && inst->cond_branch.merge_block)
            {
                uint32_t params[2] = {
                    inst->cond_branch.merge_block->id,
                    SpvSelectionControlMaskNone,
                };
                irModuleEncodeInst(m, SpvOpSelectionMerge, params, 2);
            }
            else if (inst->cond_branch.continue_block && inst->cond_branch.merge_block)
            {
                uint32_t params[3] = {
                    inst->cond_branch.merge_block->id,
                    inst->cond_branch.continue_block->id,
                    SpvLoopControlMaskNone,
                };
                irModuleEncodeInst(m, SpvOpLoopMerge, params, 3);
            }

            {
                uint32_t params[3] = {
                    inst->cond_branch.cond->id,
                    inst->cond_branch.true_block->id,
                    inst->cond_branch.false_block->id,
                };
                irModuleEncodeInst(m, SpvOpBranchConditional, params, 3);
            }
            break;
        }

        case IR_INST_LOAD: {
            inst->id = irModuleReserveId(m);

            IRType *loaded_type = inst->load.pointer->type;
            assert(loaded_type->kind == IR_TYPE_POINTER);
            loaded_type = loaded_type->ptr.sub;

            uint32_t params[3] = {loaded_type->id, inst->id, inst->load.pointer->id};
            irModuleEncodeInst(m, SpvOpLoad, params, 3);
            break;
        }

        case IR_INST_STORE: {
            uint32_t params[2] = {inst->store.pointer->id, inst->store.value->id};
            irModuleEncodeInst(m, SpvOpStore, params, 2);
            break;
        }

        case IR_INST_ACCESS_CHAIN: {
            inst->id = irModuleReserveId(m);

            uint32_t param_count = 3 + inst->access_chain.index_count;
            uint32_t *params = NEW_ARRAY(m->compiler, uint32_t, param_count);

            assert(inst->type->id > 0);
            assert(inst->access_chain.base->id > 0);

            params[0] = inst->type->id;
            params[1] = inst->id;
            params[2] = inst->access_chain.base->id;

            for (uint32_t i = 0; i < inst->access_chain.index_count; ++i)
            {
                assert(inst->access_chain.indices[i]->id);
                params[3 + i] = inst->access_chain.indices[i]->id;
            }

            irModuleEncodeInst(m, SpvOpAccessChain, params, param_count);
            break;
        }

        case IR_INST_SELECT: {
            inst->id = irModuleReserveId(m);

            uint32_t params[5] = {
                inst->type->id,
                inst->id,
                inst->select.cond->id,
                inst->select.true_value->id,
                inst->select.false_value->id,
            };
            irModuleEncodeInst(m, SpvOpSelect, params, 5);
            break;
        }

        case IR_INST_COMPOSITE_CONSTRUCT: {
            inst->id = irModuleReserveId(m);

            uint32_t param_count = 2 + inst->composite_construct.field_count;
            uint32_t *params = NEW_ARRAY(m->compiler, uint32_t, param_count);

            assert(inst->type->id > 0);

            params[0] = inst->type->id;
            params[1] = inst->id;

            for (uint32_t i = 0; i < inst->composite_construct.field_count; ++i)
            {
                assert(inst->composite_construct.fields[i]->id);
                params[2 + i] = inst->composite_construct.fields[i]->id;
            }

            irModuleEncodeInst(m, SpvOpCompositeConstruct, params, param_count);
            break;
        }

        case IR_INST_FUNC_CALL: {
            inst->id = irModuleReserveId(m);

            uint32_t param_count = 3 + inst->func_call.param_count;
            uint32_t *params = NEW_ARRAY(m->compiler, uint32_t, param_count);

            params[0] = inst->type->id;
            params[1] = inst->id;
            params[2] = inst->func_call.func->id;
            for (uint32_t i = 0; i < inst->func_call.param_count; ++i)
            {
                assert(inst->func_call.params[i]->id);
                params[3 + i] = inst->func_call.params[i]->id;
            }

            irModuleEncodeInst(m, SpvOpFunctionCall, params, param_count);
            break;
        }

        case IR_INST_BARRIER: {
            SpvOp op;
            uint32_t param_count;
            uint32_t *params;

            if (inst->barrier.with_group_sync)
            {
                op = SpvOpControlBarrier;
                param_count = 3;
                params = NEW_ARRAY(m->compiler, uint32_t, param_count);

                assert(inst->barrier.execution_scope->id);
                assert(inst->barrier.memory_scope->id);
                assert(inst->barrier.semantics->id);

                params[0] = inst->barrier.execution_scope->id;
                params[1] = inst->barrier.memory_scope->id;
                params[2] = inst->barrier.semantics->id;
            }
            else
            {
                op = SpvOpMemoryBarrier;
                param_count = 2;
                params = NEW_ARRAY(m->compiler, uint32_t, param_count);

                assert(inst->barrier.memory_scope->id);
                assert(inst->barrier.semantics->id);

                params[0] = inst->barrier.memory_scope->id;
                params[1] = inst->barrier.semantics->id;
            }

            irModuleEncodeInst(m, op, params, param_count);
            break;
        }

        case IR_INST_BUILTIN_CALL: {
            inst->id = irModuleReserveId(m);

            uint32_t param_value_count = inst->builtin_call.param_count;
            IRInst **param_values = inst->builtin_call.params;

            for (uint32_t i = 0; i < param_value_count; ++i)
            {
                assert(param_values[i]);
                assert(param_values[i]->id);
            }

            switch (inst->builtin_call.kind)
            {
            case IR_BUILTIN_DOT: {
                IRInst *a = param_values[0];
                IRInst *b = param_values[1];
                uint32_t params[4] = {inst->type->id, inst->id, a->id, b->id};
                irModuleEncodeInst(m, SpvOpDot, params, 4);
                break;
            }

            case IR_BUILTIN_CROSS: {
                uint32_t params[2] = {param_values[0]->id, param_values[1]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Cross, params, 2);
                break;
            }

            case IR_BUILTIN_DISTANCE: {
                uint32_t params[2] = {param_values[0]->id, param_values[1]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Distance, params, 2);
                break;
            }

            case IR_BUILTIN_LENGTH: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Length, params, 1);
                break;
            }

            case IR_BUILTIN_NORMALIZE: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Normalize, params, 1);
                break;
            }

            case IR_BUILTIN_MUL: {
                IRInst *a = param_values[0];
                IRInst *b = param_values[1];

                if (a->type->kind == IR_TYPE_VECTOR && b->type->kind == IR_TYPE_MATRIX)
                {
                    // Matrix times vector, yes, it's backwards
                    uint32_t params[4] = {inst->type->id, inst->id, b->id, a->id};
                    irModuleEncodeInst(m, SpvOpMatrixTimesVector, params, 4);
                }
                else if (
                    a->type->kind == IR_TYPE_MATRIX && b->type->kind == IR_TYPE_VECTOR)
                {
                    // Vector times matrix, yes, it's backwards
                    uint32_t params[4] = {inst->type->id, inst->id, b->id, a->id};
                    irModuleEncodeInst(m, SpvOpVectorTimesMatrix, params, 4);
                }
                else if (
                    a->type->kind == IR_TYPE_VECTOR && b->type->kind == IR_TYPE_VECTOR)
                {
                    // Vector dot product
                    uint32_t params[4] = {inst->type->id, inst->id, a->id, b->id};
                    irModuleEncodeInst(m, SpvOpDot, params, 4);
                }
                else if (
                    a->type->kind == IR_TYPE_MATRIX && b->type->kind == IR_TYPE_MATRIX)
                {
                    // Matrix times matrix
                    uint32_t params[4] = {inst->type->id, inst->id, b->id, a->id};
                    irModuleEncodeInst(m, SpvOpMatrixTimesMatrix, params, 4);
                }
                else
                {
                    assert(0);
                }

                break;
            }

            case IR_BUILTIN_CREATE_SAMPLED_IMAGE: {
                IRInst *image = param_values[0];
                IRInst *sampler = param_values[1];
                uint32_t params[4] = {inst->type->id, inst->id, image->id, sampler->id};
                irModuleEncodeInst(m, SpvOpSampledImage, params, 4);
                break;
            }

            case IR_BUILTIN_RADIANS: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Radians, params, 1);
                break;
            }

            case IR_BUILTIN_DEGREES: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Degrees, params, 1);
                break;
            }

            case IR_BUILTIN_SIN: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Sin, params, 1);
                break;
            }

            case IR_BUILTIN_COS: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Cos, params, 1);
                break;
            }

            case IR_BUILTIN_TAN: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Tan, params, 1);
                break;
            }

            case IR_BUILTIN_ASIN: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Asin, params, 1);
                break;
            }

            case IR_BUILTIN_ACOS: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Acos, params, 1);
                break;
            }

            case IR_BUILTIN_ATAN: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Atan, params, 1);
                break;
            }

            case IR_BUILTIN_SINH: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Sinh, params, 1);
                break;
            }

            case IR_BUILTIN_COSH: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Cosh, params, 1);
                break;
            }

            case IR_BUILTIN_TANH: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Tanh, params, 1);
                break;
            }

            case IR_BUILTIN_ATAN2: {
                uint32_t params[2] = {param_values[0]->id, param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Atan2, params, 2);
                break;
            }

            case IR_BUILTIN_SQRT: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Sqrt, params, 1);
                break;
            }

            case IR_BUILTIN_RSQRT: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450InverseSqrt, params, 1);
                break;
            }

            case IR_BUILTIN_REFLECT: {
                uint32_t params[2] = {param_values[0]->id, param_values[1]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Reflect, params, 2);
                break;
            }

            case IR_BUILTIN_REFRACT: {
                uint32_t params[3] = {
                    param_values[0]->id, param_values[1]->id, param_values[2]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Refract, params, 3);
                break;
            }

            case IR_BUILTIN_POW: {
                uint32_t params[2] = {param_values[0]->id, param_values[1]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Pow, params, 2);
                break;
            }

            case IR_BUILTIN_EXP: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Exp, params, 1);
                break;
            }

            case IR_BUILTIN_EXP2: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Exp2, params, 1);
                break;
            }

            case IR_BUILTIN_LOG: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Log, params, 1);
                break;
            }

            case IR_BUILTIN_LOG2: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Log2, params, 1);
                break;
            }

            case IR_BUILTIN_ABS: {
                IRType *scalar_type = inst->type;
                if (scalar_type->kind == IR_TYPE_VECTOR)
                {
                    scalar_type = scalar_type->vector.elem_type;
                }

                uint32_t params[1] = {param_values[0]->id};

                if (scalar_type->kind == IR_TYPE_FLOAT)
                {
                    irModuleEncodeExtInst(m, inst, GLSLstd450FAbs, params, 1);
                }
                else if (scalar_type->kind == IR_TYPE_INT)
                {
                    irModuleEncodeExtInst(m, inst, GLSLstd450SAbs, params, 1);
                }
                else
                {
                    assert(0);
                }
                break;
            }

            case IR_BUILTIN_MIN: {
                IRType *scalar_type = inst->type;
                if (scalar_type->kind == IR_TYPE_VECTOR)
                {
                    scalar_type = scalar_type->vector.elem_type;
                }

                uint32_t params[2] = {param_values[0]->id, param_values[1]->id};

                if (scalar_type->kind == IR_TYPE_FLOAT)
                {
                    irModuleEncodeExtInst(m, inst, GLSLstd450FMin, params, 2);
                }
                else if (scalar_type->kind == IR_TYPE_INT)
                {
                    if (scalar_type->int_.is_signed)
                    {
                        irModuleEncodeExtInst(m, inst, GLSLstd450SMin, params, 2);
                    }
                    else
                    {
                        irModuleEncodeExtInst(m, inst, GLSLstd450UMin, params, 2);
                    }
                }

                break;
            }

            case IR_BUILTIN_MAX: {
                IRType *scalar_type = inst->type;
                if (scalar_type->kind == IR_TYPE_VECTOR)
                {
                    scalar_type = scalar_type->vector.elem_type;
                }

                uint32_t params[2] = {param_values[0]->id, param_values[1]->id};

                if (scalar_type->kind == IR_TYPE_FLOAT)
                {
                    irModuleEncodeExtInst(m, inst, GLSLstd450FMax, params, 2);
                }
                else if (scalar_type->kind == IR_TYPE_INT)
                {
                    if (scalar_type->int_.is_signed)
                    {
                        irModuleEncodeExtInst(m, inst, GLSLstd450SMax, params, 2);
                    }
                    else
                    {
                        irModuleEncodeExtInst(m, inst, GLSLstd450UMax, params, 2);
                    }
                }
                break;
            }

            case IR_BUILTIN_FRAC: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Fract, params, 1);
                break;
            }

            case IR_BUILTIN_TRUNC: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Trunc, params, 1);
                break;
            }

            case IR_BUILTIN_CEIL: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Ceil, params, 1);
                break;
            }

            case IR_BUILTIN_FLOOR: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Floor, params, 1);
                break;
            }

            case IR_BUILTIN_LERP: {
                uint32_t params[3] = {
                    param_values[0]->id, param_values[1]->id, param_values[2]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450FMix, params, 3);
                break;
            }

            case IR_BUILTIN_CLAMP: {
                IRType *scalar_type = inst->type;
                if (scalar_type->kind == IR_TYPE_VECTOR)
                {
                    scalar_type = scalar_type->vector.elem_type;
                }

                uint32_t params[3] = {
                    param_values[0]->id, param_values[1]->id, param_values[2]->id};

                if (scalar_type->kind == IR_TYPE_FLOAT)
                {
                    irModuleEncodeExtInst(m, inst, GLSLstd450FClamp, params, 3);
                }
                else if (scalar_type->kind == IR_TYPE_INT)
                {
                    if (scalar_type->int_.is_signed)
                    {
                        irModuleEncodeExtInst(m, inst, GLSLstd450SClamp, params, 3);
                    }
                    else
                    {
                        irModuleEncodeExtInst(m, inst, GLSLstd450UClamp, params, 3);
                    }
                }
                break;
            }

            case IR_BUILTIN_STEP: {
                uint32_t params[2] = {param_values[0]->id, param_values[1]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Step, params, 2);
                break;
            }

            case IR_BUILTIN_SMOOTHSTEP: {
                uint32_t params[3] = {
                    param_values[0]->id, param_values[1]->id, param_values[2]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450SmoothStep, params, 3);
                break;
            }

            case IR_BUILTIN_FMOD: {
                uint32_t param_count = 4;
                uint32_t params[4] = {
                    inst->type->id,
                    inst->id,
                    param_values[0]->id,
                    param_values[1]->id,
                };

                irModuleEncodeInst(m, SpvOpFRem, params, param_count);
                break;
            }

            case IR_BUILTIN_TRANSPOSE: {
                uint32_t params[3] = {inst->type->id, inst->id, param_values[0]->id};
                irModuleEncodeInst(m, SpvOpTranspose, params, 3);
                break;
            }

            case IR_BUILTIN_DETERMINANT: {
                uint32_t params[1] = {param_values[0]->id};
                irModuleEncodeExtInst(m, inst, GLSLstd450Determinant, params, 1);
                break;
            }

            case IR_BUILTIN_DDX: {
                uint32_t params[3] = {inst->type->id, inst->id, param_values[0]->id};
                irModuleEncodeInst(m, SpvOpDPdx, params, 3);
                break;
            }

            case IR_BUILTIN_DDY: {
                uint32_t params[3] = {inst->type->id, inst->id, param_values[0]->id};
                irModuleEncodeInst(m, SpvOpDPdy, params, 3);
                break;
            }

            case IR_BUILTIN_ASFLOAT:
            case IR_BUILTIN_ASINT:
            case IR_BUILTIN_ASUINT: {
                uint32_t params[3] = {inst->type->id, inst->id, param_values[0]->id};
                irModuleEncodeInst(m, SpvOpBitcast, params, 3);
                break;
            }

            case IR_BUILTIN_INTERLOCKED_ADD: {
                assert(param_values[0]->type->kind == IR_TYPE_POINTER);

                uint32_t params[6] = {
                    param_values[0]->type->ptr.sub->id,
                    inst->id,
                    param_values[0]->id,
                    param_values[1]->id,
                    param_values[2]->id,
                    param_values[3]->id,
                };
                irModuleEncodeInst(m, SpvOpAtomicIAdd, params, 6);
                break;
            }

            case IR_BUILTIN_INTERLOCKED_AND: {
                assert(param_values[0]->type->kind == IR_TYPE_POINTER);

                uint32_t params[6] = {
                    param_values[0]->type->ptr.sub->id,
                    inst->id,
                    param_values[0]->id,
                    param_values[1]->id,
                    param_values[2]->id,
                    param_values[3]->id,
                };
                irModuleEncodeInst(m, SpvOpAtomicAnd, params, 6);
                break;
            }

            case IR_BUILTIN_INTERLOCKED_MIN: {
                assert(param_values[0]->type->kind == IR_TYPE_POINTER);

                IRType *int_type = param_values[0]->type->ptr.sub;
                assert(int_type->kind == IR_TYPE_INT);

                uint32_t params[6] = {
                    int_type->id,
                    inst->id,
                    param_values[0]->id,
                    param_values[1]->id,
                    param_values[2]->id,
                    param_values[3]->id,
                };

                if (int_type->int_.is_signed)
                {
                    irModuleEncodeInst(m, SpvOpAtomicSMin, params, 6);
                }
                else
                {
                    irModuleEncodeInst(m, SpvOpAtomicUMin, params, 6);
                }
                break;
            }

            case IR_BUILTIN_INTERLOCKED_MAX: {
                assert(param_values[0]->type->kind == IR_TYPE_POINTER);

                IRType *int_type = param_values[0]->type->ptr.sub;
                assert(int_type->kind == IR_TYPE_INT);

                uint32_t params[6] = {
                    int_type->id,
                    inst->id,
                    param_values[0]->id,
                    param_values[1]->id,
                    param_values[2]->id,
                    param_values[3]->id,
                };

                if (int_type->int_.is_signed)
                {
                    irModuleEncodeInst(m, SpvOpAtomicSMax, params, 6);
                }
                else
                {
                    irModuleEncodeInst(m, SpvOpAtomicUMax, params, 6);
                }
                break;
            }

            case IR_BUILTIN_INTERLOCKED_OR: {
                assert(param_values[0]->type->kind == IR_TYPE_POINTER);

                IRType *int_type = param_values[0]->type->ptr.sub;
                assert(int_type->kind == IR_TYPE_INT);

                uint32_t params[6] = {
                    int_type->id,
                    inst->id,
                    param_values[0]->id,
                    param_values[1]->id,
                    param_values[2]->id,
                    param_values[3]->id,
                };

                irModuleEncodeInst(m, SpvOpAtomicOr, params, 6);
                break;
            }

            case IR_BUILTIN_INTERLOCKED_XOR: {
                assert(param_values[0]->type->kind == IR_TYPE_POINTER);

                IRType *int_type = param_values[0]->type->ptr.sub;
                assert(int_type->kind == IR_TYPE_INT);

                uint32_t params[6] = {
                    int_type->id,
                    inst->id,
                    param_values[0]->id,
                    param_values[1]->id,
                    param_values[2]->id,
                    param_values[3]->id,
                };

                irModuleEncodeInst(m, SpvOpAtomicXor, params, 6);
                break;
            }

            case IR_BUILTIN_INTERLOCKED_EXCHANGE: {
                assert(param_values[0]->type->kind == IR_TYPE_POINTER);

                IRType *int_type = param_values[0]->type->ptr.sub;
                assert(int_type->kind == IR_TYPE_INT);

                uint32_t params[6] = {
                    int_type->id,
                    inst->id,
                    param_values[0]->id,
                    param_values[1]->id,
                    param_values[2]->id,
                    param_values[3]->id,
                };

                irModuleEncodeInst(m, SpvOpAtomicExchange, params, 6);

                params[0] = param_values[4]->id;
                params[1] = inst->id;
                irModuleEncodeInst(m, SpvOpStore, params, 2);
                break;
            }

            case IR_BUILTIN_INTERLOCKED_COMPARE_EXCHANGE: {
                assert(param_values[0]->type->kind == IR_TYPE_POINTER);

                IRType *int_type = param_values[0]->type->ptr.sub;
                assert(int_type->kind == IR_TYPE_INT);

                uint32_t params[8] = {
                    int_type->id,
                    inst->id,
                    param_values[0]->id, // pointer
                    param_values[1]->id, // scope
                    param_values[2]->id, // memory semantic
                    param_values[3]->id, // memory semantic
                    param_values[5]->id, // value
                    param_values[4]->id, // compare value
                };

                irModuleEncodeInst(m, SpvOpAtomicCompareExchange, params, 8);

                params[0] = param_values[6]->id; // original value
                params[1] = inst->id;
                irModuleEncodeInst(m, SpvOpStore, params, 2);
                break;
            }

            case IR_BUILTIN_INTERLOCKED_COMPARE_STORE: {
                assert(param_values[0]->type->kind == IR_TYPE_POINTER);

                IRType *int_type = param_values[0]->type->ptr.sub;
                assert(int_type->kind == IR_TYPE_INT);

                uint32_t params[8] = {
                    int_type->id,
                    inst->id,
                    param_values[0]->id, // pointer
                    param_values[1]->id, // scope
                    param_values[2]->id, // memory semantic
                    param_values[3]->id, // memory semantic
                    param_values[5]->id, // value
                    param_values[4]->id, // compare value
                };

                irModuleEncodeInst(m, SpvOpAtomicCompareExchange, params, 8);
                break;
            }
            }

            break;
        }

        case IR_INST_SAMPLE_IMPLICIT_LOD: {
            inst->id = irModuleReserveId(m);

            IRInst *sampled_image = inst->sample.image_sampler;
            IRInst *coordinate = inst->sample.coords;
            uint32_t params[4] = {
                inst->type->id, inst->id, sampled_image->id, coordinate->id};
            irModuleEncodeInst(m, SpvOpImageSampleImplicitLod, params, 4);
            break;
        }

        case IR_INST_SAMPLE_EXPLICIT_LOD: {
            inst->id = irModuleReserveId(m);

            IRInst *sampled_image = inst->sample.image_sampler;
            IRInst *coordinate = inst->sample.coords;
            IRInst *lod = inst->sample.lod;

            uint32_t operand_mask = SpvImageOperandsLodMask;

            uint32_t params[6] = {
                inst->type->id,
                inst->id,
                sampled_image->id,
                coordinate->id,
                operand_mask,
                lod->id,
            };
            irModuleEncodeInst(m, SpvOpImageSampleExplicitLod, params, 6);
            break;
        }

        case IR_INST_QUERY_SIZE_LOD: {
            inst->id = irModuleReserveId(m);

            IRInst *image = inst->query_size_lod.image;
            IRInst *lod = inst->query_size_lod.lod;

            uint32_t params[4] = {inst->type->id, inst->id, image->id, lod->id};
            irModuleEncodeInst(m, SpvOpImageQuerySizeLod, params, 4);
            break;
        }

        case IR_INST_QUERY_LEVELS: {
            inst->id = irModuleReserveId(m);

            IRInst *image = inst->query_size_lod.image;

            uint32_t params[3] = {inst->type->id, inst->id, image->id};
            irModuleEncodeInst(m, SpvOpImageQueryLevels, params, 3);
            break;
        }

        case IR_INST_CAST: {
            if (inst->cast.redundant)
            {
                inst->id = inst->cast.value->id;
                break;
            }

            inst->id = irModuleReserveId(m);
            assert(inst->cast.op != SpvOpNop);

            uint32_t params[3] = {inst->type->id, inst->id, inst->cast.value->id};
            irModuleEncodeInst(m, inst->cast.op, params, 3);

            break;
        }

        case IR_INST_COMPOSITE_EXTRACT: {
            inst->id = irModuleReserveId(m);

            uint32_t param_count = 3 + inst->composite_extract.index_count;
            uint32_t *params = NEW_ARRAY(m->compiler, uint32_t, param_count);

            assert(inst->type->id > 0);

            params[0] = inst->type->id;
            params[1] = inst->id;
            params[2] = inst->composite_extract.value->id;

            for (uint32_t i = 0; i < inst->composite_extract.index_count; ++i)
            {
                params[3 + i] = inst->composite_extract.indices[i];
            }

            irModuleEncodeInst(m, SpvOpCompositeExtract, params, param_count);
            break;
        }

        case IR_INST_VECTOR_SHUFFLE: {
            inst->id = irModuleReserveId(m);

            uint32_t param_count = 4 + inst->vector_shuffle.index_count;
            uint32_t *params = NEW_ARRAY(m->compiler, uint32_t, param_count);

            assert(inst->type->id > 0);

            params[0] = inst->type->id;
            params[1] = inst->id;
            params[2] = inst->vector_shuffle.vector_a->id;
            params[3] = inst->vector_shuffle.vector_b->id;

            for (uint32_t i = 0; i < inst->vector_shuffle.index_count; ++i)
            {
                params[4 + i] = inst->vector_shuffle.indices[i];
            }

            irModuleEncodeInst(m, SpvOpVectorShuffle, params, param_count);
            break;
        }

        case IR_INST_UNARY: {
            inst->id = irModuleReserveId(m);
            assert(inst->type->id > 0);

            uint32_t param_count = 3;
            uint32_t params[3] = {inst->type->id, inst->id, inst->unary.right->id};

            irModuleEncodeInst(m, inst->unary.op, params, param_count);
            break;
        }

        case IR_INST_BINARY: {
            inst->id = irModuleReserveId(m);
            assert(inst->type->id > 0);

            uint32_t param_count = 4;
            uint32_t params[4] = {
                inst->type->id, inst->id, inst->binary.left->id, inst->binary.right->id};

            irModuleEncodeInst(m, inst->binary.op, params, param_count);
            break;
        }

        case IR_INST_FUNC_PARAM:
        case IR_INST_CONSTANT:
        case IR_INST_CONSTANT_BOOL:
        case IR_INST_FUNCTION:
        case IR_INST_ENTRY_POINT:
        case IR_INST_BLOCK: assert(0); break;
        }
    }
}

static void irModuleEncodeConstants(IRModule *m)
{
    for (uint32_t i = 0; i < arrLength(m->constants); ++i)
    {
        IRInst *inst = m->constants.ptr[i];
        switch (inst->kind)
        {
        case IR_INST_CONSTANT: {
            inst->id = irModuleReserveId(m);

            uint32_t value_words = ROUND_TO_4(inst->constant.value_size_bytes) / 4;
            assert((value_words * 4) >= inst->constant.value_size_bytes);
            assert(value_words > 0);

            uint32_t *params = NEW_ARRAY(m->compiler, uint32_t, 2 + value_words);
            params[0] = inst->type->id;
            params[1] = inst->id;
            assert(params[0] != params[1]);
            memcpy(&params[2], inst->constant.value, inst->constant.value_size_bytes);

            irModuleEncodeInst(m, SpvOpConstant, params, 2 + value_words);
            break;
        }

        case IR_INST_CONSTANT_BOOL: {
            inst->id = irModuleReserveId(m);
            uint32_t params[2] = {inst->type->id, inst->id};
            if (inst->constant_bool.value)
            {
                irModuleEncodeInst(m, SpvOpConstantTrue, params, 2);
            }
            else
            {
                irModuleEncodeInst(m, SpvOpConstantFalse, params, 2);
            }
            break;
        }

        default: assert(0); break;
        }
    }
}

static void irModuleEncodeGlobals(IRModule *m)
{
    for (uint32_t i = 0; i < arrLength(m->all_globals); ++i)
    {
        IRInst *inst = m->all_globals.ptr[i];
        assert(inst->kind == IR_INST_VARIABLE);

        uint32_t params[3] = {inst->type->id, inst->id, inst->var.storage_class};
        irModuleEncodeInst(m, SpvOpVariable, params, 3);
    }
}

static void irModuleEncodeFunctions(IRModule *m)
{
    for (uint32_t i = 0; i < arrLength(m->functions); ++i)
    {
        IRInst *inst = m->functions.ptr[i];
        assert(inst->kind == IR_INST_FUNCTION);
        assert(inst->id);

        {
            uint32_t params[4] = {
                inst->type->func.return_type->id,
                inst->id,
                SpvFunctionControlInlineMask,
                inst->type->id,
            };
            irModuleEncodeInst(m, SpvOpFunction, params, 4);
        }

        for (uint32_t j = 0; j < arrLength(inst->func.params); ++j)
        {
            IRInst *func_param = inst->func.params.ptr[j];
            uint32_t params[2] = {
                func_param->type->id,
                func_param->id,
            };
            irModuleEncodeInst(m, SpvOpFunctionParameter, params, 2);
        }

        for (uint32_t j = 0; j < arrLength(inst->func.blocks); ++j)
        {
            IRInst *block = inst->func.blocks.ptr[j];
            irModuleEncodeBlock(m, block);
        }

        irModuleEncodeInst(m, SpvOpFunctionEnd, NULL, 0);
    }
}

static void irModuleEncodeModule(IRModule *m)
{
    assert(m->stream.ptr == NULL);

    static const uint8_t MAGIC_NUMBER[4] = {'T', 'I', 'N', 'Y'};
    uint32_t uint_magic_number;
    memcpy(&uint_magic_number, MAGIC_NUMBER, sizeof(uint32_t));

    arrPush(m->compiler, &m->stream, SpvMagicNumber);
    arrPush(m->compiler, &m->stream, SpvVersion);
    arrPush(m->compiler, &m->stream, uint_magic_number);
    arrPush(m->compiler, &m->stream, 0); // ID Bound (fill out later)
    arrPush(m->compiler, &m->stream, 0);

    {
        uint32_t params[1] = {SpvCapabilityShader};
        irModuleEncodeInst(m, SpvOpCapability, params, 1);
    }

    if (m->uses_image_query)
    {
        uint32_t params[1] = {SpvCapabilityImageQuery};
        irModuleEncodeInst(m, SpvOpCapability, params, 1);
    }

    {
        uint32_t params[5];
        memset(params, 0, sizeof(params));

        params[0] = m->glsl_ext_inst;

        char *str = "GLSL.std.450";
        memcpy(&params[1], str, 12);

        irModuleEncodeInst(m, SpvOpExtInstImport, params, 5);
    }

    {
        uint32_t params[2] = {SpvAddressingModelLogical, SpvMemoryModelGLSL450};
        irModuleEncodeInst(m, SpvOpMemoryModel, params, 2);
    }

    irModuleEncodeEntryPoints(m);

    uint32_t params[2] = {SpvSourceLanguageHLSL, 660};
    irModuleEncodeInst(m, SpvOpSource, params, 2);

    irModuleReserveTypeIds(m);

    irModuleEncodeDecorations(m);

    irModuleEncodeTypes(m);

    irModuleEncodeConstants(m);

    irModuleEncodeGlobals(m);

    irModuleEncodeFunctions(m);

    // Fill out ID bound
    m->stream.ptr[3] = m->id_bound;
}

IRModule *ts__irModuleCreate(TsCompiler *compiler)
{
    IRModule *m = NEW(compiler, IRModule);
    memset(m, 0, sizeof(*m));
    m->compiler = compiler;

    ts__hashInit(compiler, &m->type_cache, 0);
    ts__hashInit(compiler, &m->const_cache, 0);

    irModuleReserveId(m); // 0th ID
    m->glsl_ext_inst = irModuleReserveId(m);

    return m;
}

void ts__irModuleDestroy(IRModule *m)
{
    ts__hashDestroy(&m->type_cache);
    ts__hashDestroy(&m->const_cache);
    arrFree(m->compiler, &m->stream);
}

uint32_t *ts__irModuleCodegen(IRModule *ir_mod, size_t *word_count)
{
    irModuleEncodeModule(ir_mod);

    *word_count = ir_mod->stream.len;
    uint32_t *result = malloc((*word_count) * 4);
    memcpy(result, ir_mod->stream.ptr, (*word_count) * 4);

    return result;
}
