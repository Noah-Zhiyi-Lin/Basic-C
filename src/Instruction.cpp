#include "Instruction.h"
#include "BasicBlock.h"
#include <iostream>
#include "Function.h"
#include "Type.h"
extern FILE* yyout;

Instruction::Instruction(unsigned instType, BasicBlock *insert_bb)
{
    prev = next = this;
    opcode = -1;
    this->instType = instType;
    if (insert_bb != nullptr)
    {
        insert_bb->insertBack(this);
        parent = insert_bb;
    }
}

Instruction::~Instruction()
{
    parent->remove(this);
}

BasicBlock *Instruction::getParent()
{
    return parent;
}

void Instruction::setParent(BasicBlock *bb)
{
    parent = bb;
}

void Instruction::setNext(Instruction *inst)
{
    next = inst;
}

void Instruction::setPrev(Instruction *inst)
{
    prev = inst;
}

Instruction *Instruction::getNext()
{
    return next;
}

Instruction *Instruction::getPrev()
{
    return prev;
}

BinaryInstruction::BinaryInstruction(unsigned opcode, Operand *dst, Operand *src1, Operand *src2, BasicBlock *insert_bb) : Instruction(BINARY, insert_bb)
{
    this->opcode = opcode;
    operands.push_back(dst);
    operands.push_back(src1);
    operands.push_back(src2);
    dst->setDef(this);
    src1->addUse(this);
    src2->addUse(this);
}

BinaryInstruction::~BinaryInstruction()
{
    operands[0]->setDef(nullptr);
    if(operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
    operands[2]->removeUse(this);
}

void BinaryInstruction::output() const
{
    std::string s1, s2, s3, op, type;
    s1 = operands[0]->toStr();
    s2 = operands[1]->toStr();
    s3 = operands[2]->toStr();
    type = operands[0]->getType()->toStr();
    switch (opcode)
    {
    case ADD:
        op = "add";
        break;
    case SUB:
        op = "sub";
        break;
    default:
        break;
    }
    fprintf(yyout, "  %s = %s %s %s, %s\n", s1.c_str(), op.c_str(), type.c_str(), s2.c_str(), s3.c_str());
}

CmpInstruction::CmpInstruction(unsigned opcode, Operand *dst, Operand *src1, Operand *src2, BasicBlock *insert_bb): Instruction(CMP, insert_bb){
    this->opcode = opcode;
    operands.push_back(dst);
    operands.push_back(src1);
    operands.push_back(src2);
    dst->setDef(this);
    src1->addUse(this);
    src2->addUse(this);
}

CmpInstruction::~CmpInstruction()
{
    operands[0]->setDef(nullptr);
    if(operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
    operands[2]->removeUse(this);
}

void CmpInstruction::output() const
{
    std::string s1, s2, s3, op, type;
    s1 = operands[0]->toStr();
    s2 = operands[1]->toStr();
    s3 = operands[2]->toStr();
    type = operands[1]->getType()->toStr();
    switch (opcode)
    {
    case E:
        op = "eq";
        break;
    case NE:
        op = "ne";
        break;
    case L:
        op = "slt";
        break;
    case LE:
        op = "sle";
        break;
    case G:
        op = "sgt";
        break;
    case GE:
        op = "sge";
        break;
    default:
        op = "";
        break;
    }

    fprintf(yyout, "  %s = icmp %s %s %s, %s\n", s1.c_str(), op.c_str(), type.c_str(), s2.c_str(), s3.c_str());
}

UncondBrInstruction::UncondBrInstruction(BasicBlock *to, BasicBlock *insert_bb) : Instruction(UNCOND, insert_bb)
{
    branch = to;
}

void UncondBrInstruction::output() const
{
    fprintf(yyout, "  br label %%B%d\n", branch->getNo());
}

void UncondBrInstruction::setBranch(BasicBlock *bb)
{
    branch = bb;
}

BasicBlock *UncondBrInstruction::getBranch()
{
    return branch;
}

CondBrInstruction::CondBrInstruction(BasicBlock*true_branch, BasicBlock*false_branch, Operand *cond, BasicBlock *insert_bb) : Instruction(COND, insert_bb){
    this->true_branch = true_branch;
    this->false_branch = false_branch;
    cond->addUse(this);
    operands.push_back(cond);
}

CondBrInstruction::~CondBrInstruction()
{
    operands[0]->removeUse(this);
}

void CondBrInstruction::output() const
{
    std::string cond, type;
    cond = operands[0]->toStr();
    type = operands[0]->getType()->toStr();
    int true_label = true_branch->getNo();
    int false_label = false_branch->getNo();
    fprintf(yyout, "  br %s %s, label %%B%d, label %%B%d\n", type.c_str(), cond.c_str(), true_label, false_label);
}

void CondBrInstruction::setFalseBranch(BasicBlock *bb)
{
    false_branch = bb;
}

BasicBlock *CondBrInstruction::getFalseBranch()
{
    return false_branch;
}

void CondBrInstruction::setTrueBranch(BasicBlock *bb)
{
    true_branch = bb;
}

BasicBlock *CondBrInstruction::getTrueBranch()
{
    return true_branch;
}

RetInstruction::RetInstruction(Operand *src, BasicBlock *insert_bb) : Instruction(RET, insert_bb)
{
    if(src != nullptr)
    {
        operands.push_back(src);
        src->addUse(this);
    }
}

RetInstruction::~RetInstruction()
{
    if(!operands.empty())
        operands[0]->removeUse(this);
}

void RetInstruction::output() const
{
    if(operands.empty())
    {
        fprintf(yyout, "  ret void\n");
    }
    else
    {
        std::string ret, type;
        ret = operands[0]->toStr();
        type = operands[0]->getType()->toStr();
        fprintf(yyout, "  ret %s %s\n", type.c_str(), ret.c_str());
    }
}

AllocaInstruction::AllocaInstruction(Operand *dst, SymbolEntry *se, BasicBlock *insert_bb) : Instruction(ALLOCA, insert_bb)
{
    operands.push_back(dst);
    dst->setDef(this);
    this->se = se;
}

AllocaInstruction::~AllocaInstruction()
{
    operands[0]->setDef(nullptr);
    if(operands[0]->usersNum() == 0)
        delete operands[0];
}

void AllocaInstruction::output() const
{
    std::string dst, type;
    dst = operands[0]->toStr();
    type = se->getType()->toStr();
    fprintf(yyout, "  %s = alloca %s, align 4\n", dst.c_str(), type.c_str());
}

LoadInstruction::LoadInstruction(Operand *dst, Operand *src_addr, BasicBlock *insert_bb) : Instruction(LOAD, insert_bb)
{
    operands.push_back(dst);
    operands.push_back(src_addr);
    dst->setDef(this);
    src_addr->addUse(this);
}

LoadInstruction::~LoadInstruction()
{
    operands[0]->setDef(nullptr);
    if(operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
}

void LoadInstruction::output() const
{
    std::string dst = operands[0]->toStr();
    std::string src = operands[1]->toStr();
    std::string src_type;
    std::string dst_type;
    dst_type = operands[0]->getType()->toStr();
    src_type = operands[1]->getType()->toStr();
    fprintf(yyout, "  %s = load %s, %s %s, align 4\n", dst.c_str(), dst_type.c_str(), src_type.c_str(), src.c_str());
}

StoreInstruction::StoreInstruction(Operand *dst_addr, Operand *src, BasicBlock *insert_bb) : Instruction(STORE, insert_bb)
{
    operands.push_back(dst_addr);
    operands.push_back(src);
    dst_addr->addUse(this);
    src->addUse(this);
}

StoreInstruction::~StoreInstruction()
{
    operands[0]->removeUse(this);
    operands[1]->removeUse(this);
}

void StoreInstruction::output() const
{
    std::string dst = operands[0]->toStr();
    std::string src = operands[1]->toStr();
    std::string dst_type = operands[0]->getType()->toStr();
    std::string src_type = operands[1]->getType()->toStr();

    fprintf(yyout, "  store %s %s, %s %s, align 4\n", src_type.c_str(), src.c_str(), dst_type.c_str(), dst.c_str());
}

MachineOperand* Instruction::genMachineOperand(Operand* ope)
{
    auto se = ope->getEntry();
    MachineOperand* mope = nullptr;
    if(se->isConstant())
        mope = new MachineOperand(MachineOperand::IMM, dynamic_cast<ConstantSymbolEntry*>(se)->getValue());
    else if(se->isTemporary())
        mope = new MachineOperand(MachineOperand::VREG, dynamic_cast<TemporarySymbolEntry*>(se)->getLabel());
    else if(se->isVariable())
    {
        auto id_se = dynamic_cast<IdentifierSymbolEntry*>(se);
        if(id_se->isGlobal())
            mope = new MachineOperand(id_se->toStr().c_str());
        else
            exit(0);
    }
    return mope;
}

MachineOperand* Instruction::genMachineReg(int reg) 
{
    return new MachineOperand(MachineOperand::REG, reg);
}

MachineOperand* Instruction::genMachineVReg() 
{
    return new MachineOperand(MachineOperand::VREG, SymbolTable::getLabel());
}

MachineOperand* Instruction::genMachineImm(int val) 
{
    return new MachineOperand(MachineOperand::IMM, val);
}

MachineOperand* Instruction::genMachineLabel(int block_no)
{
    std::ostringstream buf;
    buf << ".L" << block_no;
    std::string label = buf.str();
    return new MachineOperand(label);
}

void AllocaInstruction::genMachineCode(AsmBuilder* builder)
{
    /* 提示:
    * 为局部变量分配栈空间
    * 存储帧偏移量 */
    auto cur_func = builder->getFunction();
    int offset = cur_func->AllocSpace(4);
    dynamic_cast<TemporarySymbolEntry*>(operands[0]->getEntry())->setOffset(-offset);
}

void LoadInstruction::genMachineCode(AsmBuilder* builder)
{
    auto cur_block = builder->getBlock();
    MachineInstruction* cur_inst = nullptr;
    // Load global operand
    if(operands[1]->getEntry()->isVariable()
    && dynamic_cast<IdentifierSymbolEntry*>(operands[1]->getEntry())->isGlobal())
    {
        auto dst = genMachineOperand(operands[0]);
        auto internal_reg1 = genMachineVReg();
        auto internal_reg2 = new MachineOperand(*internal_reg1);
        auto src = genMachineOperand(operands[1]);
        // example: load r0, addr_a
        cur_inst = new LoadMInstruction(cur_block, internal_reg1, src);
        cur_block->InsertInst(cur_inst);
        // example: load r1, [r0]
        cur_inst = new LoadMInstruction(cur_block, dst, internal_reg2);
        cur_block->InsertInst(cur_inst);
    }
    // Load local operand
    else if(operands[1]->getEntry()->isTemporary()
    && operands[1]->getDef()
    && operands[1]->getDef()->isAlloc())
    {
        // example: load r1, [r0, #4]
        auto dst = genMachineOperand(operands[0]);
        auto src1 = genMachineReg(11);
        auto src2 = genMachineImm(dynamic_cast<TemporarySymbolEntry*>(operands[1]->getEntry())->getOffset());
        cur_inst = new LoadMInstruction(cur_block, dst, src1, src2);
        cur_block->InsertInst(cur_inst);
    }
    // Load operand from temporary variable
    else
    {
        // example: load r1, [r0]
        auto dst = genMachineOperand(operands[0]);
        auto src = genMachineOperand(operands[1]);
        cur_inst = new LoadMInstruction(cur_block, dst, src);
        cur_block->InsertInst(cur_inst);
    }
}

void StoreInstruction::genMachineCode(AsmBuilder* builder)
{
    auto cur_block = builder->getBlock();
    MachineInstruction* cur_inst = nullptr;

    // 获取目标操作数（存储地址）
    auto dst = genMachineOperand(operands[0]);
    // 获取源操作数（数据）
    auto src = genMachineOperand(operands[1]);

    if (operands[0]->getEntry()->isTemporary() && operands[0]->getDef() && operands[0]->getDef()->isAlloc())
    {
        // 如果目标操作数是局部变量（临时变量），将其存储到堆栈中
        auto src_reg = genMachineVReg(); // 生成一个临时寄存器
        cur_inst = new LoadMInstruction(cur_block, src_reg, src);  // 从内存加载源操作数
        cur_block->InsertInst(cur_inst);
        
        auto dst_reg = genMachineReg(11);  // 假设我们使用寄存器 r11 作为存储目标
        cur_inst = new StoreMInstruction(cur_block, dst_reg, src_reg);  // 存储到内存中
        cur_block->InsertInst(cur_inst);
    }
    else
    {
        // 存储其他类型的操作数（如全局变量等）
        cur_inst = new StoreMInstruction(cur_block, dst, src);
        cur_block->InsertInst(cur_inst);
    }
}

void BinaryInstruction::genMachineCode(AsmBuilder* builder)
{
    // TODO:
    // 实现其他指令
    auto cur_block = builder->getBlock();
    auto dst = genMachineOperand(operands[0]);
    auto src1 = genMachineOperand(operands[1]);
    auto src2 = genMachineOperand(operands[2]);
    /* 提示:
    * 在中间代码中，ADD指令的源操作数都可以是直接的数值
    * 但汇编代码中不可以这样
    * 所以需要插入LOAD/MOV指令来加载当前数值到寄存器中
    * 对于其他指令，如MUL、CMP，也需要这样处理.*/
    MachineInstruction* cur_inst = nullptr;
    if(src1->isImm())
    {
        auto internal_reg = genMachineVReg();
        cur_inst = new LoadMInstruction(cur_block, internal_reg, src1);
        cur_block->InsertInst(cur_inst);
        src1 = new MachineOperand(*internal_reg);
    }
    switch (opcode)
    {
    case ADD:
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, dst, src1, src2);
        break;
    case SUB:
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::SUB, dst, src1, src2);
        break;
    case AND:
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::AND, dst, src1, src2);
        break;
    case OR:
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::OR, dst, src1, src2);
        break;
    default:
        break;
    }
    cur_block->InsertInst(cur_inst);
}

void CmpInstruction::genMachineCode(AsmBuilder* builder)
{
    // TODO: 实现比较指令的机器码生成逻辑
    auto cur_block = builder->getBlock();
    auto dst = genMachineOperand(operands[0]);  // 结果操作数
    auto src1 = genMachineOperand(operands[1]); // 第一个源操作数
    auto src2 = genMachineOperand(operands[2]); // 第二个源操作数

    MachineInstruction* cur_inst = nullptr;

    // 生成条件比较指令
    switch (opcode) 
    {
    case E:
        // eq: 相等比较
        cur_inst = new CmpMInstruction(cur_block, CmpMInstruction::EQ, src1, src2);
        break;
    case NE:
        // ne: 不相等比较
        cur_inst = new CmpMInstruction(cur_block, CmpMInstruction::NE, src1, src2);
        break;
    case L:
        // lt: 小于比较
        cur_inst = new CmpMInstruction(cur_block, CmpMInstruction::LT, src1, src2);
        break;
    case LE:
        // le: 小于等于比较
        cur_inst = new CmpMInstruction(cur_block, CmpMInstruction::LE, src1, src2);
        break;
    case G:
        // gt: 大于比较
        cur_inst = new CmpMInstruction(cur_block, CmpMInstruction::GT, src1, src2);
        break;
    case GE:
        // ge: 大于等于比较
        cur_inst = new CmpMInstruction(cur_block, CmpMInstruction::GE, src1, src2);
        break;
    default:
        std::cerr << "Unknown comparison opcode" << std::endl;
        break;
    }

    // 插入到当前基本块中
    if (cur_inst) {
        cur_block->InsertInst(cur_inst);
    }
}

void UncondBrInstruction::genMachineCode(AsmBuilder* builder)
{
    // TODO: 实现无条件跳转指令的机器码生成逻辑
    auto cur_block = builder->getBlock();
    MachineOperand* target_label = genMachineLabel(branch->getNo());  // 获取跳转目标的标签
    MachineInstruction* cur_inst = new UncondBrMInstruction(cur_block, target_label);
    cur_block->InsertInst(cur_inst);
}

void CondBrInstruction::genMachineCode(AsmBuilder* builder)
{
    // TODO: 实现条件跳转指令的机器码生成逻辑
    auto cur_block = builder->getBlock();
    auto cond = genMachineOperand(operands[0]);  // 获取条件操作数

    std::string cond_op;
    switch (opcode)
    {
    case E:
        cond_op = "eq";  // 相等
        break;
    case NE:
        cond_op = "ne";  // 不相等
        break;
    case L:
        cond_op = "lt";  // 小于
        break;
    case LE:
        cond_op = "le";  // 小于等于
        break;
    case G:
        cond_op = "gt";  // 大于
        break;
    case GE:
        cond_op = "ge";  // 大于等于
        break;
    default:
        cond_op = "eq";  // 默认相等
        break;
    }

    // 生成条件跳转指令
    auto true_label = genMachineLabel(true_branch->getNo());
    auto false_label = genMachineLabel(false_branch->getNo());
    MachineInstruction* cur_inst = new CondBranchMInstruction(cur_block, cond_op, cond, true_label, false_label);
    cur_block->InsertInst(cur_inst);
}

void RetInstruction::genMachineCode(AsmBuilder* builder)
{
    // TODO: 实现返回指令的机器码生成逻辑
    /* 提示:
    * 1. 生成mov指令，将返回值保存到r0
    * 2. 恢复调用者函数使用的寄存器和sp、fp
    * 3. 生成bx指令 */
    auto cur_block = builder->getBlock();
    auto cur_func = builder->getFunction();

    // 如果有返回值，处理返回值
    if (!operands.empty())
    {
        auto ret_value = genMachineOperand(operands[0]);
        auto reg_r0 = genMachineReg(0);  // 假设 r0 是返回值寄存器
        MachineInstruction* cur_inst = new MoveMInstruction(cur_block, reg_r0, ret_value);
        cur_block->InsertInst(cur_inst);
    }

    // 恢复调用者保存的寄存器（假设使用 x86/ARM 调用约定）
    // 例如恢复 sp 和 fp
    // ... (这里根据实际的调用约定做调整)

    // 生成返回指令 (通常为 `bx lr` 或 `ret`)
    auto lr = genMachineReg(14);  // 假设 lr 是返回寄存器
    MachineInstruction* cur_inst = new BranchMInstruction(cur_block, lr);  // 跳转到 lr
    cur_block->InsertInst(cur_inst);
}
