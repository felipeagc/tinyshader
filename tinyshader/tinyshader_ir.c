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

        default: assert(0); break;
        }

        assert(type->ptr.sub);
        sub = irTypeToString(compiler, type->ptr.sub);
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

static IRType *irNewBasicType(IRModule *m, IRTypeKind kind)
{
    IRType *ty = NEW(m->compiler, IRType);
    ty->kind = kind;
    return irGetCachedType(m, ty);
}

static IRType *irNewPointerType(IRModule *m, SpvStorageClass storage_class, IRType *sub)
{
    IRType *ty = NEW(m->compiler, IRType);
    ty->kind = IR_TYPE_POINTER;
    ty->ptr.storage_class = storage_class;
    ty->ptr.sub = sub;
    return irGetCachedType(m, ty);
}

static IRType *irNewVectorType(IRModule *m, IRType *elem_type, uint32_t size)
{
    IRType *ty = NEW(m->compiler, IRType);
    ty->kind = IR_TYPE_VECTOR;
    ty->vector.elem_type = elem_type;
    ty->vector.size = size;
    return irGetCachedType(m, ty);
}

static IRType *irNewMatrixType(IRModule *m, IRType *col_type, uint32_t col_count)
{
    IRType *ty = NEW(m->compiler, IRType);
    ty->kind = IR_TYPE_MATRIX;
    ty->matrix.col_type = col_type;
    ty->matrix.col_count = col_count;
    return irGetCachedType(m, ty);
}

static IRType *irNewFloatType(IRModule *m, uint32_t bits)
{
    IRType *ty = NEW(m->compiler, IRType);
    ty->kind = IR_TYPE_FLOAT;
    ty->float_.bits = bits;
    return irGetCachedType(m, ty);
}

static IRType *irNewIntType(IRModule *m, uint32_t bits, bool is_signed)
{
    IRType *ty = NEW(m->compiler, IRType);
    ty->kind = IR_TYPE_INT;
    ty->int_.bits = bits;
    ty->int_.is_signed = is_signed;
    return irGetCachedType(m, ty);
}

static IRType *
irNewFuncType(IRModule *m, IRType *return_type, IRType **params, uint32_t param_count)
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

static IRType *irNewStructType(
    IRModule *m,
    char *name,
    IRType **fields,
    uint32_t field_count,
    IRDecoration *decorations,
    uint32_t decoration_count,
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

        ty->struct_.decoration_count = decoration_count;

        ty->struct_.decorations = NEW_ARRAY(m->compiler, IRDecoration, decoration_count);
        memcpy(
            ty->struct_.decorations,
            decorations,
            sizeof(IRDecoration) * decoration_count);
    }

    return irGetCachedType(m, ty);
}

static IRType *irNewImageType(IRModule *m, IRType *sampled_type, SpvDim dim)
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

static IRType *irNewSampledImageType(IRModule *m, IRType *image_type)
{
    IRType *ty = NEW(m->compiler, IRType);
    ty->kind = IR_TYPE_SAMPLED_IMAGE;
    ty->sampled_image.image_type = image_type;
    return irGetCachedType(m, ty);
}

static IRType *convertTypeToIR(Module *module, IRModule *ir_module, AstType *type)
{
    switch (type->kind)
    {
    case TYPE_TYPE: assert(0); break;

    case TYPE_VOID: {
        return irNewBasicType(ir_module, IR_TYPE_VOID);
    }

    case TYPE_BOOL: {
        return irNewBasicType(ir_module, IR_TYPE_BOOL);
    }

    case TYPE_FLOAT: {
        return irNewFloatType(ir_module, type->float_.bits);
    }

    case TYPE_INT: {
        return irNewIntType(ir_module, type->int_.bits, type->int_.is_signed);
    }

    case TYPE_STRUCT: {
        IRType **field_types =
            NEW_ARRAY(module->compiler, IRType *, type->struct_.field_count);
        for (uint32_t i = 0; i < type->struct_.field_count; ++i)
        {
            field_types[i] = convertTypeToIR(module, ir_module, type->struct_.fields[i]);
        }
        return irNewStructType(
            ir_module,
            type->struct_.name,
            field_types,
            type->struct_.field_count,
            type->struct_.decorations,
            arrLength(type->struct_.decorations),
            type->struct_.field_decorations,
            arrLength(type->struct_.field_decorations));
    }

    case TYPE_VECTOR: {
        IRType *elem_type = convertTypeToIR(module, ir_module, type->vector.elem_type);
        return irNewVectorType(ir_module, elem_type, type->vector.size);
    }

    case TYPE_IMAGE: {
        IRType *sampled_type = convertTypeToIR(
            module, ir_module, ts__getScalarType(type->image.sampled_type));
        return irNewImageType(ir_module, sampled_type, type->image.dim);
    }

    case TYPE_SAMPLER: {
        return irNewBasicType(ir_module, IR_TYPE_SAMPLER);
    }

    case TYPE_FUNC: {
        IRType *return_type = convertTypeToIR(module, ir_module, type->func.return_type);
        IRType **param_types =
            NEW_ARRAY(module->compiler, IRType *, type->func.param_count);
        for (uint32_t i = 0; i < type->func.param_count; ++i)
        {
            param_types[i] = convertTypeToIR(module, ir_module, type->func.params[i]);
        }
        return irNewFuncType(ir_module, return_type, param_types, type->func.param_count);
    }

    case TYPE_MATRIX: {
        IRType *col_type = convertTypeToIR(module, ir_module, type->matrix.col_type);
        return irNewMatrixType(ir_module, col_type, type->matrix.col_count);
    }

    case TYPE_POINTER: {
        IRType *elem_type = convertTypeToIR(module, ir_module, type->ptr.sub);
        return irNewPointerType(ir_module, type->ptr.storage_class, elem_type);
    }

    case TYPE_SAMPLED_IMAGE: {
        IRType *image_type =
            convertTypeToIR(module, ir_module, type->sampled_image.image_type);
        return irNewSampledImageType(ir_module, image_type);
    }
    }

    return NULL;
}

static uint32_t irModuleReserveId(IRModule *m)
{
    return m->id_bound++;
}

static void irAddEntryPoint(
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

    arrPush(m->entry_points, inst);
}

static IRInst *irAddFunction(IRModule *m, IRType *func_type)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->id = irModuleReserveId(m);
    inst->kind = IR_INST_FUNCTION;
    inst->type = func_type;

    arrPush(m->functions, inst);

    return inst;
}

static IRInst *
irAddFuncParam(IRModule *m, IRInst *func, IRType *type, bool is_by_reference)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->id = irModuleReserveId(m);
    inst->kind = IR_INST_FUNC_PARAM;
    inst->type = type;
    inst->func_param.is_by_reference = is_by_reference;

    arrPush(func->func.params, inst);

    return inst;
}

static IRInst *irAddBlock(IRModule *m, IRInst *func)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_BLOCK;

    assert(func->kind == IR_INST_FUNCTION);
    arrPush(func->func.blocks, inst);

    return inst;
}

static IRInst *irAddGlobal(IRModule *m, IRType *type, SpvStorageClass storage_class)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->id = irModuleReserveId(m);
    inst->kind = IR_INST_VARIABLE;
    inst->var.storage_class = storage_class;
    inst->type = irNewPointerType(m, inst->var.storage_class, type);

    arrPush(m->globals, inst);
    arrPush(m->all_globals, inst);

    return inst;
}

static IRInst *irAddInput(IRModule *m, IRInst *func, IRType *type)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->id = irModuleReserveId(m);
    inst->kind = IR_INST_VARIABLE;
    inst->var.storage_class = SpvStorageClassInput;
    inst->type = irNewPointerType(m, inst->var.storage_class, type);

    arrPush(func->func.inputs, inst);
    arrPush(m->all_globals, inst);

    return inst;
}

static IRInst *irAddOutput(IRModule *m, IRInst *func, IRType *type)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->id = irModuleReserveId(m);
    inst->kind = IR_INST_VARIABLE;
    inst->var.storage_class = SpvStorageClassOutput;
    inst->type = irNewPointerType(m, inst->var.storage_class, type);

    arrPush(func->func.outputs, inst);
    arrPush(m->all_globals, inst);

    return inst;
}

static IRInst *irGetCurrentBlock(IRModule *m)
{
    return m->current_block;
}

static void irPositionAtEnd(IRModule *m, IRInst *block)
{
    m->current_block = block;
}

static bool irBlockHasTerminator(IRInst *block)
{
    if (arrLength(block->block.insts) == 0) return false;

    IRInst *last_inst = block->block.insts[arrLength(block->block.insts) - 1];
    switch (last_inst->kind)
    {
    case IR_INST_RETURN: return true;
    default: return false;
    }

    return false;
}

static IRInst *irBuildConstFloat(IRModule *m, IRType *type, double value)
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

    arrPush(m->constants, inst);

    return inst;
}

static IRInst *irBuildConstInt(IRModule *m, IRType *type, uint64_t value)
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

    arrPush(m->constants, inst);

    return inst;
}

static IRInst *irBuildAlloca(IRModule *m, IRType *type)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_VARIABLE;
    inst->var.storage_class = SpvStorageClassFunction;
    inst->type = irNewPointerType(m, inst->var.storage_class, type);

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static void irBuildStore(IRModule *m, IRInst *pointer, IRInst *value)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_STORE;
    inst->store.pointer = pointer;
    inst->store.value = value;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);
}

static IRInst *irBuildLoad(IRModule *m, IRInst *pointer)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_LOAD;

    assert(pointer->type);
    assert(pointer->type->kind == IR_TYPE_POINTER);

    inst->type = pointer->type->ptr.sub;
    inst->load.pointer = pointer;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static IRInst *irBuildAccessChain(
    IRModule *m, IRType *type, IRInst *base, IRInst **indices, uint32_t index_count)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_ACCESS_CHAIN;

    inst->type = irNewPointerType(m, base->type->ptr.storage_class, type);

    assert(base->type->kind == IR_TYPE_POINTER);

    inst->access_chain.base = base;
    inst->access_chain.indices = indices;
    inst->access_chain.index_count = index_count;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static IRInst *irBuildVectorShuffle(
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

    inst->type = irNewVectorType(m, vector_a->type->vector.elem_type, index_count);

    inst->vector_shuffle.vector_a = vector_a;
    inst->vector_shuffle.vector_b = vector_b;
    inst->vector_shuffle.indices = indices;
    inst->vector_shuffle.index_count = index_count;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static IRInst *irBuildCompositeExtract(
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
            inst->type = irNewVectorType(m, value->type->vector.elem_type, index_count);
        }
    }
    else
    {
        assert(0);
    }

    inst->composite_extract.value = value;
    inst->composite_extract.indices = indices;
    inst->composite_extract.index_count = index_count;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static IRInst *irBuildCompositeConstruct(
    IRModule *m, IRType *type, IRInst **fields, uint32_t field_count)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_COMPOSITE_CONSTRUCT;

    inst->type = type;

    inst->composite_construct.fields = fields;
    inst->composite_construct.field_count = field_count;

    IRInst *block = irGetCurrentBlock(m);
    assert(block);
    arrPush(block->block.insts, inst);

    return inst;
}

static IRInst *
irBuildFuncCall(IRModule *m, IRInst *function, IRInst **params, uint32_t param_count)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_FUNC_CALL;

    inst->type = function->type->func.return_type;
    assert(inst->type);

    inst->func_call.func = function;
    inst->func_call.params = params;
    inst->func_call.param_count = param_count;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static IRInst *irBuildBuiltinCall(
    IRModule *m, IRBuiltinInstKind kind, IRInst **params, uint32_t param_count)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_BUILTIN_CALL;

    switch (kind)
    {
    case IR_BUILTIN_DOT: {
        assert(param_count == 2);
        IRInst *a = params[0];
        IRInst *b = params[1];

        assert(a->type->kind == IR_TYPE_VECTOR);
        assert(b->type == a->type);

        inst->type = a->type->vector.elem_type;
        assert(inst->type);

        break;
    }

    case IR_BUILTIN_MUL: {
        assert(param_count == 2);
        IRInst *a = params[0];
        IRInst *b = params[1];

        if (a->type->kind == IR_TYPE_VECTOR && b->type->kind == IR_TYPE_MATRIX)
        {
            // Matrix times vector, yes, it's backwards
            inst->type = a->type;
        }
        else if (a->type->kind == IR_TYPE_MATRIX && b->type->kind == IR_TYPE_VECTOR)
        {
            // Vector times matrix, yes, it's backwards
            inst->type = b->type;
        }
        else if (a->type->kind == IR_TYPE_VECTOR && b->type->kind == IR_TYPE_VECTOR)
        {
            // Vector dot product
            inst->type = a->type->vector.elem_type;
        }
        else if (a->type->kind == IR_TYPE_MATRIX && b->type->kind == IR_TYPE_MATRIX)
        {
            // Matrix times matrix
            inst->type = a->type;
        }
        else
        {
            assert(0);
        }

        break;
    }

    case IR_BUILTIN_CREATE_SAMPLED_IMAGE: {
        assert(param_count == 2);
        IRInst *img_param = params[0];
        IRType *img_type = img_param->type;
        assert(img_type->kind == IR_TYPE_IMAGE);
        inst->type = irNewSampledImageType(m, img_type);
        break;
    }
    }

    assert(inst->type);

    inst->builtin_call.kind = kind;
    inst->builtin_call.params = params;
    inst->builtin_call.param_count = param_count;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static IRInst *
irBuildSampleImplicitLod(IRModule *m, IRType *type, IRInst *image_sampler, IRInst *coords)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_SAMPLE_IMPLICIT_LOD;
    inst->type = type;
    assert(inst->type);

    inst->sample.image_sampler = image_sampler;
    inst->sample.coords = coords;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static IRInst *irBuildCast(IRModule *m, IRType *dst_type, IRInst *value)
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

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static IRInst *irBuildUnary(IRModule *m, SpvOp op, IRType *type, IRInst *right)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_UNARY;
    inst->type = type;
    assert(inst->type);

    inst->unary.op = op;
    inst->unary.right = right;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static IRInst *
irBuildBinary(IRModule *m, SpvOp op, IRType *type, IRInst *left, IRInst *right)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_BINARY;
    inst->type = type;
    assert(inst->type);

    inst->binary.op = op;
    inst->binary.left = left;
    inst->binary.right = right;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);

    return inst;
}

static void irBuildReturn(IRModule *m, IRInst *value)
{
    IRInst *inst = NEW(m->compiler, IRInst);
    inst->kind = IR_INST_RETURN;
    inst->return_.value = value;

    IRInst *block = irGetCurrentBlock(m);
    arrPush(block->block.insts, inst);
}

static void
irModuleEncodeInst(IRModule *m, SpvOp opcode, uint32_t *params, size_t params_count)
{
    uint32_t opcode_word = opcode;
    opcode_word |= ((uint16_t)(params_count + 1)) << 16;

    arrPush(m->stream, opcode_word);
    for (uint32_t i = 0; i < params_count; ++i)
    {
        arrPush(m->stream, params[i]);
    }
}

static void irModuleReserveTypeIds(IRModule *m)
{
    for (uint32_t i = 0; i < arrLength(m->type_cache.values); ++i)
    {
        IRType *type = (IRType *)m->type_cache.values[i];
        type->id = irModuleReserveId(m);
    }
}

static void irModuleEncodeDecorations(IRModule *m)
{
    for (uint32_t i = 0; i < arrLength(m->all_globals); ++i)
    {
        IRInst *inst = m->all_globals[i];
        assert(inst->kind == IR_INST_VARIABLE);
        assert(inst->id);

        for (uint32_t j = 0; j < arrLength(inst->decorations); ++j)
        {
            IRDecoration *dec = &inst->decorations[j];
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
        IRType *type = (IRType *)m->type_cache.values[i];
        if (type->kind == IR_TYPE_STRUCT)
        {
            assert(type->id > 0);

            for (uint32_t j = 0; j < type->struct_.decoration_count; ++j)
            {
                IRDecoration *dec = &type->struct_.decorations[j];
                uint32_t param_count = 3;
                uint32_t params[3] = {type->id, dec->kind, dec->value};

                switch (dec->kind)
                {
                case SpvDecorationBlock: param_count = 2; break;
                default: break;
                }
                irModuleEncodeInst(m, SpvOpDecorate, params, param_count);
            }

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
                case SpvDecorationRowMajor:
                case SpvDecorationColMajor: param_count = 3; break;
                default: break;
                }

                irModuleEncodeInst(m, SpvOpMemberDecorate, params, param_count);
            }
        }
    }
}

static void irModuleEncodeTypes(IRModule *m)
{
    for (uint32_t i = 0; i < arrLength(m->type_cache.values); ++i)
    {
        IRType *type = (IRType *)m->type_cache.values[i];

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
        IRInst *inst = m->entry_points[i];
        assert(inst->kind == IR_INST_ENTRY_POINT);
        assert(inst->entry_point.func);
        assert(inst->entry_point.func->id);

        {
            uint32_t name_len = strlen(inst->entry_point.name);
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
    }
}

static void irModuleEncodeBlock(IRModule *m, IRInst *block)
{
    assert(irBlockHasTerminator(block));

    block->id = irModuleReserveId(m);

    {
        uint32_t params[1] = {block->id};
        irModuleEncodeInst(m, SpvOpLabel, params, 1);
    }

    for (uint32_t i = 0; i < arrLength(block->block.insts); ++i)
    {
        IRInst *inst = block->block.insts[i];
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
                params[4 + i] = inst->composite_extract.indices[i];
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
        IRInst *inst = m->constants[i];
        assert(inst->kind == IR_INST_CONSTANT);
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
    }
}

static void irModuleEncodeGlobals(IRModule *m)
{
    for (uint32_t i = 0; i < arrLength(m->all_globals); ++i)
    {
        IRInst *inst = m->all_globals[i];
        assert(inst->kind == IR_INST_VARIABLE);

        uint32_t params[3] = {inst->type->id, inst->id, inst->var.storage_class};
        irModuleEncodeInst(m, SpvOpVariable, params, 3);
    }
}

static void irModuleEncodeFunctions(IRModule *m)
{
    for (uint32_t i = 0; i < arrLength(m->functions); ++i)
    {
        IRInst *inst = m->functions[i];
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
            IRInst *func_param = inst->func.params[j];
            uint32_t params[2] = {
                func_param->type->id,
                func_param->id,
            };
            irModuleEncodeInst(m, SpvOpFunctionParameter, params, 2);
        }

        for (uint32_t j = 0; j < arrLength(inst->func.blocks); ++j)
        {
            IRInst *block = inst->func.blocks[j];
            irModuleEncodeBlock(m, block);
        }

        irModuleEncodeInst(m, SpvOpFunctionEnd, NULL, 0);
    }
}

static void irModuleEncodeModule(IRModule *m)
{
    assert(m->stream == NULL);

    static const uint8_t MAGIC_NUMBER[4] = {'T', 'I', 'N', 'Y'};
    uint32_t uint_magic_number;
    memcpy(&uint_magic_number, MAGIC_NUMBER, sizeof(uint32_t));

    arrPush(m->stream, SpvMagicNumber);
    arrPush(m->stream, SpvVersion);
    arrPush(m->stream, uint_magic_number);
    arrPush(m->stream, 0); // ID Bound (fill out later)
    arrPush(m->stream, 0);

    {
        uint32_t params[1] = {SpvCapabilityShader};
        irModuleEncodeInst(m, SpvOpCapability, params, 1);
    }

    {
        uint32_t params[5];
        memset(params, 0, sizeof(params));

        params[0] = irModuleReserveId(m);

        char *str = "GLSL.std.450";
        memcpy(&params[1], str, 12);

        irModuleEncodeInst(m, SpvOpExtInstImport, params, 5);
    }

    {
        uint32_t params[2] = {SpvAddressingModelLogical, SpvMemoryModelGLSL450};
        irModuleEncodeInst(m, SpvOpMemoryModel, params, 2);
    }

    irModuleEncodeEntryPoints(m);

    uint32_t params[2] = {SpvSourceLanguageHLSL, 500};
    irModuleEncodeInst(m, SpvOpSource, params, 2);

    irModuleReserveTypeIds(m);

    irModuleEncodeDecorations(m);

    irModuleEncodeTypes(m);

    irModuleEncodeConstants(m);

    irModuleEncodeGlobals(m);

    irModuleEncodeFunctions(m);

    // Fill out ID bound
    m->stream[3] = m->id_bound;
}

////////////////////////////////
//
// IR generation from AST
//
////////////////////////////////

static void irModuleBuildDecl(IRModule *m, AstDecl *decl);
static void irModuleBuildStmt(IRModule *m, AstStmt *stmt);
static void irModuleBuildExpr(IRModule *m, AstExpr *expr);

static bool isLvalue(IRInst *value)
{
    return (
        value->kind == IR_INST_VARIABLE || value->kind == IR_INST_ACCESS_CHAIN ||
        (value->kind == IR_INST_FUNC_PARAM && value->func_param.is_by_reference));
}

static IRInst *irLoadVal(IRModule *m, IRInst *value)
{
    assert(value);
    if (isLvalue(value))
    {
        return irBuildLoad(m, value);
    }

    return value;
}

static void irModuleBuildExpr(IRModule *m, AstExpr *expr)
{
    TsCompiler *compiler = m->compiler;

    assert(expr->type);

    switch (expr->kind)
    {
    case EXPR_PRIMARY: {
        IRType *ir_type = convertTypeToIR(m->mod, m, expr->type);

        switch (expr->primary.token->kind)
        {
        case TOKEN_FLOAT_LIT: {
            expr->value = irBuildConstFloat(m, ir_type, expr->primary.token->double_);
            break;
        }

        case TOKEN_INT_LIT: {
            switch (expr->type->kind)
            {
            case TYPE_FLOAT: {
                expr->value =
                    irBuildConstFloat(m, ir_type, (double)expr->primary.token->int_);
                break;
            }

            case TYPE_INT: {
                expr->value =
                    irBuildConstInt(m, ir_type, (uint64_t)expr->primary.token->int_);
                break;
            }

            default: assert(0);
            }
            break;
        }

        default: assert(0);
        }
        break;
    }

    case EXPR_IDENT: {
        AstDecl *decl = expr->ident.decl;
        assert(decl);
        expr->value = decl->value;
        break;
    }

    case EXPR_ACCESS: {
        IRType *index_type = irNewIntType(m, 32, false);

        AstExpr *base = expr->access.base;
        assert(base->type);

        irModuleBuildExpr(m, base);
        IRInst *value = base->value;
        assert(value);

        uint32_t index_count = 0;
        IRInst **indices = NEW_ARRAY(compiler, IRInst *, arrLength(expr->access.chain));

        if (base->type->kind == TYPE_STRUCT)
        {
            for (uint32_t i = 0; i < arrLength(expr->access.chain); ++i)
            {
                AstExpr *field_ident = expr->access.chain[i];
                assert(field_ident->kind == EXPR_IDENT);

                if (!field_ident->ident.decl) break;

                index_count++;

                AstDecl *field_decl = field_ident->ident.decl;
                assert(field_decl->kind == DECL_STRUCT_FIELD);

                indices[i] =
                    irBuildConstInt(m, index_type, field_decl->struct_field.index);
            }

            AstType *last_type = expr->access.chain[index_count - 1]->type;
            IRType *ir_last_type = convertTypeToIR(m->mod, m, last_type);
            value =
                irBuildAccessChain(m, ir_last_type, base->value, indices, index_count);
        }

        for (uint32_t i = index_count; i < arrLength(expr->access.chain); ++i)
        {
            // Must be a vector swizzle

            AstExpr *field_ident = expr->access.chain[i];
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
                    IRInst *vec_value = irLoadVal(m, value);
                    value = irBuildCompositeExtract(
                        m, vec_value, shuffle_indices, shuffle_index_count);
                }
                else
                {
                    // It's a variable
                    IRInst **ir_indices = NEW_ARRAY(compiler, IRInst *, 1);
                    ir_indices[0] = irBuildConstInt(m, index_type, shuffle_indices[0]);
                    IRType *accessed_type = convertTypeToIR(m->mod, m, field_ident->type);
                    value = irBuildAccessChain(m, accessed_type, value, ir_indices, 1);
                }
            }
            else
            {
                IRInst *vec_value = irLoadVal(m, value);

                value = irBuildVectorShuffle(
                    m, vec_value, vec_value, shuffle_indices, shuffle_index_count);
            }
        }

        expr->value = value;

        break;
    }

    case EXPR_FUNC_CALL: {
        if (expr->func_call.self_param)
        {
            // Method call
            AstExpr *method_name_expr = expr->func_call.func_expr;
            assert(method_name_expr->kind == EXPR_IDENT);
            char *method_name = method_name_expr->ident.name;

            AstType *self_type = expr->func_call.self_param->type;

            irModuleBuildExpr(m, expr->func_call.self_param);
            assert(expr->func_call.self_param->value);

            uint32_t param_count = arrLength(expr->func_call.params);
            IRInst **param_values = NEW_ARRAY(compiler, IRInst *, param_count);

            IRInst *self_value = irLoadVal(m, expr->func_call.self_param->value);

            for (uint32_t i = 0; i < arrLength(expr->func_call.params); ++i)
            {
                AstExpr *param = expr->func_call.params[i];
                irModuleBuildExpr(m, param);
                assert(param->value);
                param_values[i] = irLoadVal(m, param->value);
            }

            if (self_type->kind == TYPE_IMAGE && strcmp(method_name, "Sample") == 0)
            {
                IRInst **sampled_image_params = NEW_ARRAY(compiler, IRInst *, 2);
                sampled_image_params[0] = self_value;
                sampled_image_params[1] = param_values[0];
                IRInst *sampled_image = irBuildBuiltinCall(
                    m, IR_BUILTIN_CREATE_SAMPLED_IMAGE, sampled_image_params, 2);

                IRType *result_type = convertTypeToIR(m->mod, m, expr->type);
                IRInst *coords = param_values[1];
                expr->value =
                    irBuildSampleImplicitLod(m, result_type, sampled_image, coords);
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
            IRType *ir_constructed_type = convertTypeToIR(m->mod, m, constructed_type);

            uint32_t param_count = arrLength(expr->func_call.params);
            AstExpr **params = expr->func_call.params;

            if (constructed_type->kind == TYPE_VECTOR)
            {
                if (param_count == constructed_type->vector.size)
                {
                    IRInst **fields =
                        NEW_ARRAY(compiler, IRInst *, constructed_type->vector.size);

                    for (uint32_t i = 0; i < constructed_type->vector.size; ++i)
                    {
                        irModuleBuildExpr(m, params[i]);
                        assert(params[i]->value);
                        fields[i] = irLoadVal(m, params[i]->value);
                    }

                    expr->value = irBuildCompositeConstruct(
                        m, ir_constructed_type, fields, constructed_type->vector.size);
                }
                else if (param_count == 1)
                {
                    IRInst **fields =
                        NEW_ARRAY(compiler, IRInst *, constructed_type->vector.size);

                    irModuleBuildExpr(m, params[0]);
                    assert(params[0]->value);
                    IRInst *field_val = irLoadVal(m, params[0]->value);

                    for (uint32_t i = 0; i < constructed_type->vector.size; ++i)
                    {
                        fields[i] = field_val;
                    }

                    expr->value = irBuildCompositeConstruct(
                        m, ir_constructed_type, fields, constructed_type->vector.size);
                }
                else
                {
                    assert(0);
                }
            }
            else if (param_count == 1)
            {
                irModuleBuildExpr(m, params[0]);
                assert(params[0]->value);
                expr->value =
                    irBuildCast(m, ir_constructed_type, irLoadVal(m, params[0]->value));
            }
            else
            {
                assert(0);
            }
        }
        else
        {
            // Actual function call
            assert(func_type->kind == TYPE_FUNC);

            irModuleBuildExpr(m, expr->func_call.func_expr);
            IRInst *func_val = expr->func_call.func_expr->value;
            assert(func_val);

            uint32_t param_count = arrLength(expr->func_call.params);
            IRInst **param_values = NEW_ARRAY(compiler, IRInst *, param_count);

            for (uint32_t i = 0; i < param_count; ++i)
            {
                AstExpr *param = expr->func_call.params[i];
                irModuleBuildExpr(m, param);
                assert(param->value);
                param_values[i] = param->value;
                if (func_type->func.params[i]->kind != TYPE_POINTER)
                {
                    param_values[i] = irLoadVal(m, param_values[i]);
                }
            }

            expr->value = irBuildFuncCall(m, func_val, param_values, param_count);
        }

        break;
    }

    case EXPR_BUILTIN_CALL: {
        uint32_t param_count = arrLength(expr->builtin_call.params);
        IRInst **param_values = NEW_ARRAY(compiler, IRInst *, param_count);

        for (uint32_t i = 0; i < param_count; ++i)
        {
            AstExpr *param = expr->builtin_call.params[i];
            irModuleBuildExpr(m, param);
            assert(param->value);
            param_values[i] = irLoadVal(m, param->value);
        }

        expr->value =
            irBuildBuiltinCall(m, expr->builtin_call.kind, param_values, param_count);

        break;
    }

    case EXPR_UNARY: {
        switch (expr->unary.op)
        {
        case UNOP_NEG: {
            irModuleBuildExpr(m, expr->unary.right);
            IRInst *right_val = irLoadVal(m, expr->unary.right->value);

            AstType *scalar_type = ts__getScalarType(expr->type);
            SpvOp op = {0};

            switch (scalar_type->kind)
            {
            case TYPE_FLOAT: op = SpvOpFNegate; break;
            case TYPE_INT: op = SpvOpSNegate; break;
            default: assert(0); break;
            }

            IRType *ir_type = convertTypeToIR(m->mod, m, expr->type);
            expr->value = irBuildUnary(m, op, ir_type, right_val);
            break;
        }

        case UNOP_NOT: {
            irModuleBuildExpr(m, expr->unary.right);
            IRInst *right_val = irLoadVal(m, expr->unary.right->value);

            IRType *ir_type = convertTypeToIR(m->mod, m, expr->type);
            expr->value = irBuildUnary(m, SpvOpNot, ir_type, right_val);
            break;
        }
        }
        break;
    }

    case EXPR_BINARY: {
        irModuleBuildExpr(m, expr->binary.left);
        IRInst *left_val = irLoadVal(m, expr->binary.left->value);
        irModuleBuildExpr(m, expr->binary.right);
        IRInst *right_val = irLoadVal(m, expr->binary.right->value);

        AstType *elem_type = ts__getElemType(expr->binary.left->type);
        assert(elem_type);
        SpvOp op = {0};

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
            case TYPE_FLOAT: op = SpvOpFMod; break;
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
        }

        IRType *ir_type = convertTypeToIR(m->mod, m, expr->type);
        expr->value = irBuildBinary(m, op, ir_type, left_val, right_val);

        break;
    }

    case EXPR_SAMPLER_TYPE:
    case EXPR_TEXTURE_TYPE: {
        break;
    }
    }
}

static void irModuleBuildStmt(IRModule *m, AstStmt *stmt)
{
    switch (stmt->kind)
    {
    case STMT_DECL: {
        irModuleBuildDecl(m, stmt->decl);
        break;
    }

    case STMT_EXPR: {
        irModuleBuildExpr(m, stmt->expr);
        break;
    }

    case STMT_RETURN: {
        if (stmt->return_.value)
        {
            irModuleBuildExpr(m, stmt->return_.value);
            assert(stmt->return_.value->value);
            irBuildReturn(m, irLoadVal(m, stmt->return_.value->value));
        }
        else
        {
            irBuildReturn(m, NULL);
        }
        break;
    }

    case STMT_VAR_ASSIGN: {
        irModuleBuildExpr(m, stmt->var_assign.value_expr);
        IRInst *to_store = stmt->var_assign.value_expr->value;
        assert(to_store);
        to_store = irLoadVal(m, to_store);

        irModuleBuildExpr(m, stmt->var_assign.assigned_expr);
        IRInst *assigned_value = stmt->var_assign.assigned_expr->value;

        irBuildStore(m, assigned_value, to_store);

        break;
    }
    
    case STMT_BLOCK: {
        for (uint32_t i = 0; i < arrLength(stmt->block.stmts); ++i)
        {
            AstStmt *sub_stmt = stmt->block.stmts[i];
            irModuleBuildStmt(m, sub_stmt);
        }
        break;
    }
    }
}

static void irModuleBuildDecl(IRModule *m, AstDecl *decl)
{
    switch (decl->kind)
    {
    case DECL_FUNC: {
        if (!decl->func.called) break;

        if (decl->func.execution_model)
        {
            IRInst **globals = NULL;

            for (uint32_t i = 0; i < arrLength(m->globals); ++i)
            {
                arrPush(globals, m->globals[i]);
            }

            for (uint32_t i = 0; i < arrLength(decl->value->func.inputs); ++i)
            {
                arrPush(globals, decl->value->func.inputs[i]);
            }

            for (uint32_t i = 0; i < arrLength(decl->value->func.outputs); ++i)
            {
                arrPush(globals, decl->value->func.outputs[i]);
            }

            irAddEntryPoint(
                m,
                decl->name,
                decl->value,
                *decl->func.execution_model,
                globals,
                arrLength(globals));
        }

        IRInst *entry_block = irAddBlock(m, decl->value);
        irPositionAtEnd(m, entry_block);

        IRInst **old_inputs =
            NEW_ARRAY(m->compiler, IRInst *, arrLength(decl->func.inputs));

        for (uint32_t i = 0; i < arrLength(decl->func.inputs); ++i)
        {
            AstDecl *var = decl->func.inputs[i];
            old_inputs[i] = var->value;

            IRType *ir_type = convertTypeToIR(m->mod, m, var->type);
            var->value = irBuildAlloca(m, ir_type);
        }

        for (uint32_t i = 0; i < arrLength(decl->func.var_decls); ++i)
        {
            AstDecl *var = decl->func.var_decls[i];
            IRType *ir_type = convertTypeToIR(m->mod, m, var->type);
            var->value = irBuildAlloca(m, ir_type);
        }

        for (uint32_t i = 0; i < arrLength(decl->func.inputs); ++i)
        {
            AstDecl *var = decl->func.inputs[i];
            IRInst *loaded = irBuildLoad(m, old_inputs[i]);
            irBuildStore(m, var->value, loaded);
        }

        for (uint32_t i = 0; i < arrLength(decl->func.stmts); ++i)
        {
            AstStmt *stmt = decl->func.stmts[i];
            irModuleBuildStmt(m, stmt);
        }

        if (!irBlockHasTerminator(irGetCurrentBlock(m)))
        {
            irBuildReturn(m, NULL);
        }

        break;
    }

    case DECL_VAR: {
        IRInst *initializer = NULL;

        if (decl->var.value_expr)
        {
            irModuleBuildExpr(m, decl->var.value_expr);
            initializer = decl->var.value_expr->value;
            assert(initializer);
            initializer = irLoadVal(m, initializer);
            irBuildStore(m, decl->value, initializer);
        }

        break;
    }

    case DECL_CONST: {
        irModuleBuildExpr(m, decl->constant.value_expr);
        decl->value = decl->constant.value_expr->value;
        break;
    }

    case DECL_STRUCT_FIELD: break;
    case DECL_STRUCT: break;
    }
}

static void irModuleInit(IRModule *m, TsCompiler *compiler)
{
    memset(m, 0, sizeof(*m));
    m->compiler = compiler;

    ts__hashInit(&m->type_cache, 0);
}

void irModuleDestroy(IRModule *m)
{
    ts__hashDestroy(&m->type_cache);
    arrFree(m->stream);
}

uint32_t *ts__irModuleCodegen(Module *mod, size_t *word_count)
{
    TsCompiler *compiler = mod->compiler;

    IRModule *m = NEW(compiler, IRModule);
    irModuleInit(m, compiler);

    m->mod = mod;

    irModuleReserveId(m); // 0th ID

    // Add functions / globals
    for (uint32_t i = 0; i < arrLength(m->mod->files); ++i)
    {
        File *file = m->mod->files[i];

        for (uint32_t j = 0; j < arrLength(file->decls); ++j)
        {
            AstDecl *decl = file->decls[j];
            switch (decl->kind)
            {
            case DECL_FUNC: {
                assert(decl->type);

                if (!decl->func.called) break;

                IRType *ir_type = convertTypeToIR(m->mod, m, decl->type);
                decl->value = irAddFunction(m, ir_type);

                for (uint32_t k = 0; k < arrLength(decl->func.func_params); ++k)
                {
                    AstDecl *param = decl->func.func_params[k];
                    IRType *ir_param_type = convertTypeToIR(m->mod, m, param->type);
                    bool by_reference = false;
                    if (param->var.kind == VAR_IN_PARAM ||
                        param->var.kind == VAR_OUT_PARAM ||
                        param->var.kind == VAR_INOUT_PARAM)
                    {
                        ir_param_type =
                            irNewPointerType(m, SpvStorageClassFunction, ir_param_type);
                        by_reference = true;
                    }
                    param->value =
                        irAddFuncParam(m, decl->value, ir_param_type, by_reference);
                }

                for (uint32_t k = 0; k < arrLength(decl->func.inputs); ++k)
                {
                    AstDecl *input = decl->func.inputs[k];
                    IRType *ir_param_type = convertTypeToIR(m->mod, m, input->type);
                    input->value = irAddInput(m, decl->value, ir_param_type);
                    input->value->decorations = input->decorations;
                }

                for (uint32_t k = 0; k < arrLength(decl->func.outputs); ++k)
                {
                    AstDecl *output = decl->func.outputs[k];
                    IRType *ir_param_type = convertTypeToIR(m->mod, m, output->type);
                    output->value = irAddOutput(m, decl->value, ir_param_type);
                    output->value->decorations = output->decorations;
                }

                break;
            }

            case DECL_VAR: {
                IRType *ir_type = convertTypeToIR(m->mod, m, decl->type);
                SpvStorageClass storage_class;
                if (decl->var.kind == VAR_UNIFORM)
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
                }
                else
                {
                    assert(0);
                }
                decl->value = irAddGlobal(m, ir_type, storage_class);
                decl->value->decorations = decl->decorations;
                break;
            }

            default: break;
            }
        }
    }

    for (uint32_t i = 0; i < arrLength(m->mod->files); ++i)
    {
        File *file = m->mod->files[i];

        for (uint32_t j = 0; j < arrLength(file->decls); ++j)
        {
            AstDecl *decl = file->decls[j];
            irModuleBuildDecl(m, decl);
        }
    }

    irModuleEncodeModule(m);

    *word_count = arrLength(m->stream);
    uint32_t *result = malloc((*word_count) * 4);
    memcpy(result, m->stream, (*word_count) * 4);

    irModuleDestroy(m);

    return result;
}
