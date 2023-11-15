#ifndef __AST_HPP__
#define __AST_HPP__

#include <deque>
#include <iostream>
#include <map>
#include <vector>

#include <string>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Utils.h>

#include <llvm/Passes/OptimizationLevel.h>
#include <llvm/Passes/PassBuilder.h>


using namespace llvm;

void yyerror(const char *s);
void yyerror(const char *s, int line);
extern int yylineno;

enum DataType {
  TYPE_UNDEFINED_ERROR,
  TYPE_int,
  TYPE_char,
  TYPE_nothing,
  TYPE_charList, // hidden
  TYPE_bool,     // hidden
};

struct Sem {
  DataType type;
  std::vector<int> array_size;
};

// needs to be able to see DataType
#include "symbol.cpp"
struct FunctionDetails {
  std::string functionName;
  Type *returnLLVMType;
  std::vector<Type *> argTypes;

  DataType returnType;
  std::vector<bool> argsPointer;
  Function::LinkageTypes linkage =
      Function::ExternalLinkage; // Set default linkage here
  Function *func = nullptr;      // Pointer to the created function
};


inline char escSeqToChar(const std::string &v) {
  if (v[0] != '\\' || v.size() < 2) {
    return -1; // Indicates no valid escape sequence found
  }
  switch (v[1]) {
  case 'n': return '\n';
  case 't': return '\t';
  case 'r': return '\r';
  case '0': return '\0';
  case '\\': return '\\';
  case '\'': return '\'';
  case '\"': return '\"';
  case 'x':
    if (v.size() >= 4) {
      return std::stoi(v.substr(2, 2), nullptr, 16);
    }
    return -1; // Incomplete hex escape sequence
  default: return -1; // Unrecognized escape sequence
  }
}
class AST {
public:
  AST() : line(yylineno) {}
  virtual ~AST() = default;
  virtual void printAST(std::ostream &out) const = 0;
  virtual Value *compile() const { logError("Called ast const compile"); }
  virtual Value *compile() { logError("Called ast compile"); }
  virtual Node *compileArray() { logError("Called ast compileArray"); }
  virtual void sem() { logError("Called default sem"); }

  std::string getName() {}

  Function *createFunction(const FunctionDetails &details, LLVMContext &context,
                           const std::unique_ptr<Module> &module) {
    FunctionType *funcType =
        FunctionType::get(details.returnLLVMType, details.argTypes, false);
    return Function::Create(funcType, details.linkage, details.functionName,
                            module.get());
  }

  Type *getLlvmType(DataType dtype, std::vector<int> array_size = {}) {

    Type *itype;

    if (dtype == TYPE_int)
      itype = Type::getInt64Ty(TheContext);
    else if (dtype == TYPE_char)
      itype = Type::getInt8Ty(TheContext);
    else if (dtype == TYPE_nothing)
      itype = Type::getVoidTy(TheContext);
    else if (dtype == TYPE_charList)
      itype = i8_ptr;

    else {
      logError("Error, couldnt find type!");
    }
    if (array_size.size() > 0) {
      std::reverse(array_size.begin(), array_size.end());
      ArrayType *ArrayTy;

      for (auto size : array_size) {
        ArrayTy = ArrayType::get(itype, size);
        itype = ArrayTy;
      }
      std::clog << "Hereeee!!!!!!!" << std::endl;
    }
    return itype;
  }

  void llvm_compile_and_dump(bool optimize = false) {

    std::clog << "Optimizations: " << optimize << std::endl;

    // Initialize
    TheModule = std::make_unique<Module>("Grace", TheContext);

//    TheFPM = std::make_unique<legacy::FunctionPassManager>(TheModule.get());
//    if (optimize) {
//      TheFPM->add(createPromoteMemoryToRegisterPass());
//      TheFPM->add(createInstructionCombiningPass());
//      TheFPM->add(createReassociatePass());
//      TheFPM->add(createGVNPass());
//      TheFPM->add(createCFGSimplificationPass());
//    }
//    TheFPM->doInitialization();
    // Initialize types
    i8 = IntegerType::get(TheContext, 8);
    i32 = IntegerType::get(TheContext, 32);
    i64 = IntegerType::get(TheContext, 64);

    void_type = Type::getVoidTy(TheContext);

    i8_ptr = PointerType::get(i8, 0);

    externalFuncMap = {
        {"writeInteger",
         {"writeInteger", void_type, {i64}, TYPE_nothing, {false}}},
        {"writeChar", {"writeChar", void_type, {i8}, TYPE_nothing, {false}}},
        {"writeString",
         {"writeString", void_type, {i8_ptr}, TYPE_nothing, {true}}},
        {"readInteger", {"readInteger", i64, {}, TYPE_int, {}}},
        {"readChar", {"readChar", i8, {}, TYPE_char, {}}},
        {"readString",
         {"readString", void_type, {i64, i8_ptr}, TYPE_nothing, {false, true}}},
        {"ascii", {"ascii", i64, {i8}, TYPE_int, {false}}},
        {"chr", {"chr", i8, {i64}, TYPE_char, {false}}},
        {"strlen", {"strlen", i64, {i8_ptr}, TYPE_int, {true}}},
        {"strcmp",
         {"strcmp", void_type, {i8_ptr, i8_ptr}, TYPE_nothing, {true, true}}},
        {"strcpy",
         {"strcpy", void_type, {i8_ptr, i8_ptr}, TYPE_nothing, {true, true}}},
        {"strcat",
         {"strcat", void_type, {i8_ptr, i8_ptr}, TYPE_nothing, {true, true}}},

    };

    // Create functions and store them in the map
    for (auto &entry : externalFuncMap) {
      // second is the value
      entry.second.func = createFunction(entry.second, TheContext, TheModule);
    }

    std::clog << "externalFuncMap size:" << externalFuncMap.size() << std::endl;
    std::clog << "About to start sem analysis" << std::endl;

    // do sem analysis
    sem();


    //We also want to check that all functions that were declared were defined

    std::vector<Node *> fNodes = st.getFunctionNodes();

    for (Node *fNode : fNodes){
      if(!fNode->isSet)
        logError("The function definition is missing for function "+fNode->name,false);
    }

    std::clog << "Sem analysis is now complete!" << std::endl;

    // Define and start the main function.
    FunctionType *main_type = FunctionType::get(i64, {}, false);
    Function *main = Function::Create(main_type, Function::ExternalLinkage,
                                      "main", TheModule.get());
    BasicBlock *BB = BasicBlock::Create(TheContext, "entry", main);
    Builder.SetInsertPoint(BB);

    // Emit the program code.
    compile();

    std::clog << "Compiled!" << std::endl;

    // We should remove any empty bb's, unsafe to remove while iterating
    std::vector<BasicBlock *> EmptyBBs;

    for (Function &Func : *TheModule) {
      for (BasicBlock &BB : Func) {
        if (BB.empty()) {
          EmptyBBs.push_back(&BB);
        }
      }
    }

    for (BasicBlock *BB : EmptyBBs) {
      Builder.SetInsertPoint(BB);

      Function *parentFunc = BB->getParent();
      Type *returnType = parentFunc->getReturnType();

      // Check if the function's return type is void
      if (returnType->isVoidTy()) {
        Builder.CreateRetVoid();
      } else {
        // For non-void functions, provide a default return value
        Value *defaultReturnValue;

        if (returnType->isIntegerTy()) {
          defaultReturnValue =
              ConstantInt::get(returnType, 0); // Default to 0 for integer types
        } else {
          // Handle other types as needed
          // For now, we'll just leave a null pointer for pointer types
          defaultReturnValue = Constant::getNullValue(returnType);
        }

        Builder.CreateRet(defaultReturnValue);
      }
    }
    std::clog << "Removed empty BBs!" << std::endl;

    Builder.SetInsertPoint(BB);
    Builder.CreateRet(c64(0));

    std::clog << "Created return!" << std::endl;
    // Verify the IR.
    bool bad = verifyModule(*TheModule, &errs());
    std::clog << "Verified!1" << std::endl;
    if (bad) {
      std::clog << "The IR is bad!" << std::endl;
      //      TheModule->print(errs(), nullptr);
      //      std::exit(1);
    }
    std::clog << "Verified!" << std::endl;

    if (optimize) {
      LoopAnalysisManager LAM;
      FunctionAnalysisManager FAM;
      CGSCCAnalysisManager CGAM;
      ModuleAnalysisManager MAM;

      // Create the new pass manager builder.
      // Take a look at the PassBuilder constructor parameters for more
      // customization, e.g. specifying a TargetMachine or various debugging
      // options.
      PassBuilder PB;

      // Register all the basic analyses with the managers.
      PB.registerModuleAnalyses(MAM);
      PB.registerCGSCCAnalyses(CGAM);
      PB.registerFunctionAnalyses(FAM);
      PB.registerLoopAnalyses(LAM);
      PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

      // Create the pass manager.
      // This one corresponds to a typical -O2 optimization pipeline.
      ModulePassManager MPM =
          PB.buildPerModuleDefaultPipeline(OptimizationLevel::O2);

      // Optimize the IR!
      MPM.run(*TheModule, MAM);
    }

    TheModule->print(outs(), nullptr);
  }

public:
  static SymbolTable st; // maybe some special class to do this?

  static std::map<std::string, FunctionDetails> externalFuncMap;

protected:
  int line;

  static LLVMContext TheContext;
  static IRBuilder<> Builder;
  static std::unique_ptr<Module> TheModule;
  static std::unique_ptr<legacy::FunctionPassManager> TheFPM;

  static Type *i8;
  static Type *i32;
  static Type *i64;

  static Type *i8_ptr;
  static Type *void_type;

  static ConstantInt *c8(char c) {
    return ConstantInt::get(TheContext, APInt(8, c, true));
  }
  static ConstantInt *c32(int n) {
    return ConstantInt::get(TheContext, APInt(32, n, true));
  }
  static ConstantInt *c64(int n) {
    return ConstantInt::get(TheContext, APInt(64, n, true));
  }
  void logError(const std::string msg,bool showLine=true) const {
    if(showLine)
      yyerror(msg.c_str(), line);
    else
      yyerror(msg.c_str());
  }
};

inline std::ostream &operator<<(std::ostream &out, DataType t) {
  switch (t) {
  case TYPE_UNDEFINED_ERROR:
    out << "TYPE_UNDEFINED_ERROR";
    break;
  case TYPE_int:
    out << "TYPE_int";
    break;
  case TYPE_char:
    out << "TYPE_char";
    break;
  case TYPE_nothing:
    out << "TYPE_nothing";
    break;
  case TYPE_bool:
    out << "TYPE_bool";
    break;
  }
  return out;
}

inline std::ostream &operator<<(std::ostream &out, const AST &ast) {
  ast.printAST(out);
  return out;
}

class Expr : virtual public AST {
public:
  Sem sem_struct;
  virtual Value *compileAssign() {
    std::clog << "Called Lvalue compileAssign!" << std::endl;
    return nullptr;
  }
  virtual std::vector<int> getArraySize() {
    std::clog << "Called default Lvalue getArraySize!" << std::endl;
    exit(2);
  }

  virtual bool isEmpty() const {
    // we check if its Expr base class. If s then its empty
    return typeid(*this) == typeid(Expr);
  }
  virtual bool isArray(){
    logError("Called default Expr isArray");
  }

  void sem() {
    std::clog << "Empty sem" << std::endl;
    sem_struct.type = TYPE_nothing;
  }

  void check_type(DataType t, bool isArray = true) {
    // TODO: needs to also check array/array size
    sem();
    if (sem_struct.type == TYPE_UNDEFINED_ERROR)
      logError("Should never return TYPE_UNDEFINED_ERROR");

    std::clog << "Check_TYPE " << sem_struct.type << " " << t << std::endl;
    //      printAST(std::clog);
    //      std::clog << std::endl;

    if (!isArray && sem_struct.array_size.size() > 0)
      logError("Wasnt expecting array.");
    if (sem_struct.type != t)
      logError("Type mismatch");
  }

  void printAST(std::ostream &out) const override { out << "Expr(empty)"; }

  Value *compile() override {
    std::clog << "Called base EXPR compile!" << std::endl;
    return nullptr;
    // Implement compile for Expr if necessary, or keep it pure virtual
  }
};

// only used for definitions! VARDEC?
class Id : public Expr {
public:
  Id(std::string *c) : var(*c) {}
  void printAST(std::ostream &out) const override {
    out << "Id(" << var << ")";
  }

  std::string getName() const { return var; }

private:
  std::string var;
};

class IdList : public AST {
public:
  IdList() : id_list() {}
  ~IdList() {
    for (Id *d : id_list)
      delete d;
  }
  void add_front(Id *d) { id_list.push_front(d); }
  void add(Id *d) { id_list.push_back(d); }

  void sem() override {
    for (Id *d : id_list)
      d->sem();
  }
  void printAST(std::ostream &out) const override {
    out << "IdList(";
    bool first = true;
    for (const auto &d : id_list) {
      if (!first)
        out << ", ";
      first = false;
      out << *d;
    }

    out << ")";
  }

  // TODO: needs to return the type of each!
  std::vector<Id *> getIds() const {

    return std::vector<Id *>(id_list.begin(), id_list.end());
  }

  std::vector<Value *> compileVector() {
    std::vector<Value *> args;

    std::clog << "id_list compile: " << std::endl;

    // std::clog << "type: " <<

    for (const auto &d : id_list) {
      args.push_back(d->compile());
    }

    return args;
  }

private:
  std::deque<Id *> id_list;
};

class Stmt : virtual public AST {
public:
  void name() { std::clog << "Stmt name" << std::endl; }

  void printAST(std::ostream &out) const override { out << "Stmt(empty)"; }

  virtual Value *compile() override {
    std::clog << "Called STMT compile" << std::endl;
    exit(2);
    return nullptr;
  }

  virtual bool isReturn() { return false; }
};

class ArraySize : public Stmt {
public:
  ArraySize() : array_list() {}

  void add(int s) { array_list.push_back(s); }
  void add_front(int s) { array_list.push_front(s); }

  void sem() override {
    for (int s : array_list) {
      if (s <= 0) {
        logError("Array size needs to be >0!");
      }
    }
    // TODO: figure out what we need
  }

  void printAST(std::ostream &out) const override {
    out << "ArraySize(";
    bool first = true;
    for (const auto &s : array_list) {
      if (!first)
        out << ", ";
      first = false;
      out << s;
    }
    out << ")";
  }

  std::vector<int> getSizes() const {
    return std::vector<int>(array_list.begin(), array_list.end());
    // return array_list;
  }

private:
  std::deque<int> array_list;
};

class TypeDef : public Stmt {
public:
  TypeDef(DataType t, ArraySize *s) : type(t), array_size(s) {}

  void sem() override {

    if (type == TYPE_UNDEFINED_ERROR) {
      logError("Unexpected type error");
    }

    array_size->sem();
  }

  void printAST(std::ostream &out) const override {
    out << "TypeDef(" << type << ", " << *array_size << ")";
  }

  DataType getType() const { return type; }

  std::vector<int> getSizes() const { return array_size->getSizes(); }

private:
  DataType type;
  ArraySize *array_size;
};

class VarDec : public Stmt {
public:
  VarDec(IdList *i, TypeDef *t) : id_list(i), type(t) {}

  void printAST(std::ostream &out) const override {
    out << "VarDec(" << *id_list << ": " << *type << ")";
  }

  void sem() override {
    // create the st entry
    for (auto id : id_list->getIds()) {

      std::clog << "VarDec" << std::endl;
      // we only want to lookup local scope?
      if (st.lookupNodeLocal(id->getName(), DECL_var))
        logError("Variable is already declared");

      // check that there isnt another node with same name,type
      Node *idNode = new Node();
      idNode->name = id->getName();
      idNode->type = type->getType();
      idNode->decl_type = DECL_var;
      idNode->array_size = type->getSizes();

      std::clog << "ARRAY SIZE: " << idNode->array_size.size() << std::endl;

      //      idNode->var = gVar;
      //
      //            idNode->llvm_type =
      //            getLlvmType(idNode->type,idNode->array_size.size() != 0);
      idNode->assigned = false;
      idNode->isPointer = true;
      idNode->isFirstArrayDimUnbounded = false;

      st.insertNode(idNode);
    }

    type->sem();
  }

  std::vector<Id *> getIds() const { return id_list->getIds(); }

  Value *compile() override {

    Type *itype;

    std::vector<int> arraysizes = type->getSizes();

    itype = getLlvmType(type->getType(), arraysizes);

    Constant *Initializer = ConstantAggregateZero::get(itype);
    std::clog << "Array size: " << arraysizes.size() << std::endl;

    for (auto id : id_list->getIds()) {
      std::clog << "Compiling VarDec name: " << id->getName()
                << " Current scope: " << st.currentScope()->name << std::endl;

      Node *node = st.lookupNode(id->getName(), DECL_var);

      AllocaInst *gVar =
          Builder.CreateAlloca(itype, nullptr, id->getName() + "_var");

      gVar->setAlignment(Align(8));

      std::clog << "Alloc " << id->getName() << std::endl;

      node->var = gVar;
      node->llvm_type = itype;
    }

    return nullptr;
  }

  Value *compileFuncParams() {}

private:
  IdList *id_list;
  TypeDef *type;
};

class CharConst : public Expr {
public:
  CharConst(char s) : var(s) {}
  void printAST(std::ostream &out) const override {
    out << "CharConst(" << var << ")";
  }
  void sem() override { sem_struct.type = TYPE_char; }

  Value *compile() override {
    std::clog << "Called CharConst compile!" << std::endl;
    return c8(var);
    //    return ConstantInt::get(i8, var);
  }

private:
  char var;
};

class CharConstSpecial : public CharConst {
public:
  CharConstSpecial(std::string *s) : CharConst('a'), var(*s) {}
  void printAST(std::ostream &out) const override {
    out << "CharConstSpecial(" << escSeqToChar(var) << ")";
  }
  void sem() override { sem_struct.type = TYPE_char; }

  Value *compile() override {
    std::clog << "Called CharConstSpecial compile!" << std::endl;

    return c8(escSeqToChar(var));
  }

private:
  std::string var;
};

class Lvalue : public Expr {
public:
  Lvalue() {}
  virtual std::string getName() {
    std::clog << "Called Lvalue getName!" << std::endl;
    return "";
  }
  virtual Value *compileAssign() {
    std::clog << "Called Lvalue compileAssign!" << std::endl;
    return nullptr;
  }
  virtual void updatelookup() {}
  virtual std::vector<Value *> getIndexes() {
    std::clog << "Called Lvalue getIndexes!" << std::endl;
    exit(2);
  }
  virtual int depth() { return 0; }
  virtual Lvalue *getLHS() { logError("Lvalue getLHS should never be called"); }
  virtual void sem_arrayelem() {}
};

class IdLval : public Lvalue {
public:
  IdLval(std::string *c) : var(*c), semAnalysis(false) {}
  void printAST(std::ostream &out) const override {
    out << "IdLval(" << var << ")";
  }

  void sem() override {

    // TODO: check offset too?
    std::clog << "Scope Name!: " << st.getName() << std::endl;

    std::clog << "Looking for id!: " << var << std::endl;
    // we dont want this to run in vardec

    Node *idNode = st.lookupNode(var, DECL_var, true);
    std::clog << "tt" << std::endl;

    if (idNode == nullptr)
      logError("Variable is not declared.");

    // TODO: we should do the below test only on access, check where getName is
    // called?
    //     else if(!idNode->assigned)
    //         //TODO: make this work for ref too?
    //         logError("Error, tried to access a variable that isnt assigned");
    else {
      sem_struct.type = idNode->type;
      sem_struct.array_size = idNode->array_size;
      std::clog << "CHECK array size: " << sem_struct.array_size.size()
                << std::endl;
    }

    std::clog << sem_struct.type << std::endl;
    std::clog << "sem" << std::endl;
    node = idNode;

    semAnalysis = true;
  }

  bool isArray(){
    if(node == nullptr)
      logError("Need to first run sem");

    return node->array_size.size() >0 || node->isFirstArrayDimUnbounded;
  }

  std::string getName() override { return var; }

  void updatelookup() {
    std::clog << "Update lookup" << std::endl;
    node->assigned = true;
  }

  // double check if we can do this better
  Value *compileAssign() override {

    std::clog << "COmpileAssign" << std::endl;
    std::clog << "Sem analysis done : " << semAnalysis << std::endl;

    if (node == nullptr)
      logError("Variable: " + var + " not declared");

      return node->var;
    }

    Value *compile() {
      std::clog << "Compiling IdLval" << std::endl;

      Value *gvar = compileAssign();

      std::clog << "Here" << std::endl;
      std::clog << node->name << std::endl;
      if (!node->isPointer) {
        std::clog << "Here2" << std::endl;

        return gvar;
      } else {
        std::clog << "Here3 " << var << std::endl;

        LoadInst *load =
            Builder.CreateLoad(node->llvm_type, gvar, var + "_load");
        std::clog << "Here4" << std::endl;
        load->setAlignment(Align(8));
        return load;
      }
    }

    std::vector<Value *> getIndexes() override {
      std::vector<Value *> v =
          {}; // This is needed , its the base pointer for an array! , I think i
              // added this smwhere else
      return v;
    }

    // get array maybe is better name?
    Node *compileArray() override {

      Node *idNode = st.lookupNode(var);
      if (idNode == nullptr) {
        std::cerr << "Error: variable " << var << " not declared" << std::endl;
        exit(1);
      }

      return idNode;
    }

    std::vector<int> getArraySize() override {
      Node *idNode = st.lookupNode(var);
      if (idNode == nullptr) {
        std::cerr << "Error: variable " << var << " not declared" << std::endl;
        exit(1);
      }

      return idNode->array_size;
    }

  private:
    Node *node;

    std::string var;

    bool semAnalysis;
  };

  class ArrayElem : public Lvalue {
  public:
    ArrayElem(Lvalue *lhs, Expr *rhs) : var(lhs), expr(rhs) {}
    ~ArrayElem() {
      delete var;
      delete expr;
    }
    void printAST(std::ostream &out) const override {
      out << "ArrayElem(" << *var << ", " << *expr << ")";
    }

    Lvalue *getLHS() override {
      if (typeid(*var) != typeid(ArrayElem)) {
        return var;
      } else {
        return var->getLHS();
      }
    }
    int depth() override { return var->depth() + 1; }

    void sem_arrayelem() override {
      expr->sem();

      std::clog << "Type: " << expr->sem_struct.type << std::endl;
      if (expr->sem_struct.type != TYPE_int)
        logError("Index needs to be an int");

      var->sem_arrayelem();
    }
    void sem() override {

      std::clog << "Array elem " << std::endl;

      // this checks on every ArrayElem the index type. We need this since we
      // run sem only for first ArrayElem
      sem_arrayelem();

      std::clog << "Depth: " << depth() << std::endl;

      Lvalue *lhs = getLHS();

      lhs->sem();

      std::clog << "Name: " << lhs->getName() << std::endl;

      std::clog << "Arr size: " << lhs->sem_struct.array_size.size()
                << std::endl;

//      std::clog << "Type " << lhs->sem_struct.type << " " << lhs->sem_struct.array_size.size() << " " << lhs->compileArray()->isFirstArrayDimUnbounded << std::endl;

      int array_size = lhs->sem_struct.array_size.size() +
                       (int)lhs->compileArray()->isFirstArrayDimUnbounded;

      if (array_size == 0)
        logError("LHS needs to be an array");

      // if isFirstArrayDimUnbounded then we have 1 less on array_size, so add
      // it.
      if (array_size != depth())
        logError("Array indices need to match");

      sem_struct.type = lhs->sem_struct.type;
    }

    std::string getName() override {
      std::clog << "Called ArrayElem getName!" << std::endl;
      return var->getName();
    }

    Value *compileAssign() {
      std::clog << "Compiling compileAssign array element: " << *var
                << std::endl;

      Node *array = var->compileArray();
      std::vector<Value *> arrayIndex = getIndexes();

      node = array;

      std::clog << "before gep!!!!!!!!!!!!!!!! " << var->getName() << std::endl;

      std::clog << "Array size: " << array->array_size.size() << std::endl;

      // ArrayType* ArrayTy = ArrayType::get(i8, 0);

      std::clog << arrayIndex.size() << std::endl;
      std::clog << "here" << std::endl;
      std::clog << line << std::endl;

      std::string typeStr;
      llvm::raw_string_ostream rso(typeStr);
      array->llvm_type->print(rso);

      std::clog << typeStr << std::endl;
      //      logError(" ");

      // its weird but essentially we want to add a 0 in front only if its an
      // array, not if its an array pointer
      if (isa<ArrayType>(array->llvm_type) &&
          array->array_size.size() == arrayIndex.size()) {
        arrayIndex.insert(arrayIndex.begin(), c64(0));
      }

      std::clog << "Making gep, line:  " << line << std::endl;
      return Builder.CreateInBoundsGEP(array->llvm_type, array->var, arrayIndex,
                                       var->getName() + "_arrayElem_arg");
    }

    Node *compileArray() override { return var->compileArray(); }

    std::vector<Value *> getIndexes() override {
      std::vector<Value *> indexes = var->getIndexes();
      indexes.push_back(expr->compile());
      return indexes;
    }

    Value *compile() override {

      std::clog << "Compiling array element: " << *var << std::endl;

      Value *elementPtr = compileAssign();
      std::clog << "HEREER" << std::endl;

      Type *elementType = getLlvmType(node->type); //?

      LoadInst *elementValue =
          Builder.CreateLoad(elementType, elementPtr, "elementValue");

      elementValue->setAlignment(Align(8));
      return elementValue;
    }

  private:
    Lvalue *var;
    Expr *expr;

    DataType datatype;
    Node *node;

    int offset;
  };

  class Assign : public Stmt {
  public:
    Assign(Lvalue *lhs, Expr *rhs) : var(lhs), expr(rhs) {}
    ~Assign() {
      delete var;
      delete expr;
    }
    void printAST(std::ostream &out) const override {
      out << "Assign(" << *var << ", " << *expr << ")";
    }

    void sem() override {
      var->sem();
      //      std::clog << var->sem_type << std::endl;
      expr->sem();
      std::clog << var->sem_struct.type << " and " << expr->sem_struct.type
                << std::endl;

      if (var->sem_struct.type == TYPE_UNDEFINED_ERROR or
          expr->sem_struct.type == TYPE_UNDEFINED_ERROR)
        logError("Should never return TYPE_UNDEFINED_ERROR");

      if (var->sem_struct.array_size.size() > 0 ||
          expr->sem_struct.array_size.size() > 0)
        logError("Cant assign from/to array.");

      if (var->sem_struct.type != expr->sem_struct.type)
        logError("Type mismatch...");

      std::clog << "Assign check sem complete" << std::endl;

      var->updatelookup(); // in order to detect later if it has been assigned
                           // before use, a better name would be updateAssigned

      std::clog << "Assign sem complete" << std::endl;
    }

    Value *compile() override {

      std::clog << "Assign compile: Current Scope: " << st.currentScope()->name
                << std::endl;

      std::clog << *var << std::endl;

      std::clog << "Variable name: " << var->getName() << std::endl;

      // order matters, we first do lhs
      Value *lhs = var->compileAssign();

      Value *rhs = expr->compile();

      std::clog << "Compiled rhs" << std::endl;

      // Store the constant value into the alloca.
      StoreInst *store = Builder.CreateStore(rhs, lhs);
      store->setAlignment(Align(8));
      std::clog << "Created store!" << std::endl;

      return nullptr;
    }

  private:
    Lvalue *var;
    Expr *expr;
  };

  class Return : public Stmt {
  public:
    Return(Expr *e) : expr(e) {}
    ~Return() { delete expr; }
    void printAST(std::ostream &out) const override {
      out << "Return(" << *expr << ")";
    }

    void sem() {
      std::clog << "Return sem" << std::endl;
      expr->sem();

      if (st.currentScope()->functionOwner->type != expr->sem_struct.type)
        logError("Invalid return type ");

      // TODO: check that expr type matches function!
    }

    bool isReturn() override { return true; }
    Value *compile() override {
      std::clog << "Compiling return! " << std::endl;
      std::clog << *expr << std::endl;

      if (expr->isEmpty()) {
        return Builder.CreateRetVoid();
      } else {

        Value *compiledExpr = expr->compile();
        return Builder.CreateRet(compiledExpr);
      }
    }

  private:
    Expr *expr;
  };

  class StmtList : public Stmt {
  public:
    StmtList() : stmt_list() {}
    ~StmtList() {
      for (Stmt *s : stmt_list)
        delete s;
    }
    void add(Stmt *s) { stmt_list.push_back(s); }

    void name() { std::clog << "StmtList name" << std::endl; }
    // TODO: We need to check that the order of statements is correct! SUPER
    // IMPORTANT!

    void sem() override {
      std::clog << "STMT Scope Name!: " << st.getName() << std::endl;

      for (Stmt *s : stmt_list)
        s->sem();
    }

    bool isReturn() override {
      for (Stmt *s : stmt_list) {
        if (s->isReturn()) {
          return true;
        }
      }
      return false;
    }

    Value *compile() override {
      std::clog << "StmtList compile: " << std::endl;

      for (Stmt *s : stmt_list) {
        if (s->isReturn()) {
          std::clog << "Found return in stmtlist" << std::endl;
          s->compile();
          return nullptr;
        } else {
          s->compile();
        }
      };

      std::clog << "StmtList compile done" << std::endl;

      return nullptr;
    }

    void printAST(std::ostream &out) const override {
      out << "StmtList(";
      bool first = true;
      for (const auto &s : stmt_list) {
        if (!first)
          out << ", ";
        first = false;
        out << *s;
      }
      out << ")";
    }

  private:
    std::vector<Stmt *> stmt_list;
  };

  class Cond : public Expr {
    //  public:
    //   virtual void execute() const = 0;

  public:
    virtual Value *compile() { logError("Called compile from Cond"); }
    virtual Value *compile(BasicBlock *thenBB, BasicBlock *afterBB) {
      return compile(); // if not implemented => if not binopcond (used for
                        // short circuit)
    }
  };

  class If : public Stmt {
  public:
    If(Cond *c, Stmt *s1, Stmt *s2 = nullptr) : cond(c), stmt1(s1), stmt2(s2) {}
    ~If() {
      delete cond;
      delete stmt1;
      delete stmt2;
    }

    bool isReturn() override {

      if (stmt2 == nullptr) {
        return false;
      } else {
        return stmt1->isReturn() && stmt2->isReturn();
      }
    }

    void printAST(std::ostream &out) const override {
      out << "If(" << *cond << ", " << *stmt1;
      if (stmt2 != nullptr)
        out << ", " << *stmt2;
      out << ")";
    }

    void sem() override {
      std::clog << "Checking if type: " << std::endl;
      cond->check_type(TYPE_bool);

      stmt1->sem();
      if (stmt2 != nullptr)
        stmt2->sem();
    }

    Value *compile() override {

      std::clog << "Called If compile!" << std::endl;

      std::clog << "Called cond compile" << std::endl;
      // Value *cond = Builder.CreateICmpNE(v, c32(0), "if_cond");
      Function *TheFunction = Builder.GetInsertBlock()->getParent();
      BasicBlock *ThenBB = BasicBlock::Create(TheContext, "then", TheFunction);

      BasicBlock *AfterBB =
          BasicBlock::Create(TheContext, "endif", TheFunction);

      std::clog << "Before cond" << std::endl;

      std::clog << "After cond" << std::endl;

      // if there is no else statement
      if (stmt2 == nullptr) {
        Value *condition = cond->compile(ThenBB, AfterBB);

        Builder.CreateCondBr(condition, ThenBB, AfterBB);
        Builder.SetInsertPoint(ThenBB);

        std::clog << "About to compile stmt" << std::endl;

        stmt1->compile();

        // we do this cause llvm doesnt like it if we have a command (br) after
        // return
        if (!stmt1->isReturn()) {
          Builder.CreateBr(AfterBB);
          Builder.SetInsertPoint(AfterBB);
        }

      } else {
        BasicBlock *ElseBB =
            BasicBlock::Create(TheContext, "else", TheFunction);

        Value *condition = cond->compile(ThenBB, ElseBB);

        Builder.CreateCondBr(condition, ThenBB, ElseBB);
        Builder.SetInsertPoint(ThenBB);

        std::clog << "About to compile stmt" << std::endl;

        stmt1->compile();
        if (!stmt1->isReturn()) {
          Builder.CreateBr(AfterBB);
        }
        Builder.SetInsertPoint(ElseBB);

        std::clog << "About to compile else stmts" << std::endl;
        stmt2->compile();

        std::clog << "is return: " << stmt2->isReturn() << std::endl;
        if (!stmt2->isReturn()) {
          Builder.CreateBr(AfterBB);
          // Builder.SetInsertPoint(AfterBB);
        }
      }

      Builder.SetInsertPoint(AfterBB);




      return nullptr;
    }

  private:
    Cond *cond;
    Stmt *stmt1;
    Stmt *stmt2;
  };

  class While : public Stmt {
  public:
    While(Cond *e, Stmt *s) : cond(e), stmt(s) {}
    ~While() {
      delete cond;
      delete stmt;
    }
    void printAST(std::ostream &out) const override {
      out << "While(" << *cond << ", " << *stmt << ")";
    }

    void sem() override {
      cond->check_type(TYPE_bool);

      stmt->sem();
    }

    bool isReturn() override { return stmt->isReturn(); }

    Value *compile() override {
      std::clog << "while compile" << std::endl;

      BasicBlock *originalBB = Builder.GetInsertBlock();

      Function *TheFunction = originalBB->getParent();

      BasicBlock *ConditionBB =
          BasicBlock::Create(TheContext, "condition", TheFunction);

      BasicBlock *DoBB = BasicBlock::Create(TheContext, "do", TheFunction);

      BasicBlock *AfterBB =
          BasicBlock::Create(TheContext, "after", TheFunction);

      // start from condition
      Builder.CreateBr(ConditionBB);

      Builder.SetInsertPoint(ConditionBB);

      Value *condition = cond->compile(DoBB, AfterBB);

      Builder.CreateCondBr(condition, DoBB, AfterBB);

      // make the do part

      Builder.SetInsertPoint(DoBB);
      std::clog << "About top stmt compile: " << std::endl;
      std::clog << line << std::endl;

      stmt->compile();

      Builder.CreateBr(ConditionBB);

      Builder.SetInsertPoint(AfterBB);

      return nullptr;
    }

  private:
    Cond *cond;
    Stmt *stmt;
  };

  class BinOp : public Expr {

  public:
    BinOp(Expr *e1, std::string *s, Expr *e2) : expr1(e1), op(*s), expr2(e2) {}
    ~BinOp() {
      delete expr1;
      delete expr2;
    }
    void printAST(std::ostream &out) const override {
      out << "BinOp(" << *expr1 << ", " << op << ", " << *expr2 << ")";
    }

    void sem() override {
      // we need both to be int
      expr1->check_type(TYPE_int, false);
      expr2->check_type(TYPE_int, false);

      sem_struct.type = TYPE_int;
    }

    Value *compile() override {
      std::clog << "BinOp compile: " << std::endl;
      std::clog << "Left expr: " << std::endl;
      std::clog << typeid(*expr1).name() << std::endl;
      Value *l = expr1->compile();

      std::clog << "Right expr: " << std::endl;
      Value *r = expr2->compile();

      if (op == "+") {
        return Builder.CreateAdd(l, r, "addtmp");
      } else if (op == "-") {
        return Builder.CreateSub(l, r, "subtmp");
      } else if (op == "*") {
        return Builder.CreateMul(l, r, "multmp");
      } else if (op == "div") {
        return Builder.CreateSDiv(l, r, "divtmp");
      } else if (op == "mod") {
        return Builder.CreateSRem(l, r, "modtmp");
      }
    }

  private:
    Expr *expr1;
    std::string op;
    Expr *expr2;
  };

  class UnaryOp : public Expr {
  public:
    UnaryOp(Expr *e1, std::string *s) : expr1(e1), var(*s) {}
    ~UnaryOp() { delete expr1; }
    void printAST(std::ostream &out) const override {
      out << "UnaryOp(" << var << ", " << *expr1 << ")";
    }

    void sem() override {
      expr1->check_type(TYPE_int);
      sem_struct.type = TYPE_int;
    }

    Value *compile() override {

      if (var == "+") {
        return expr1->compile();
      } else if (var == "-") {
        // we do 0-value(expr1) to inverse
        Value *v = expr1->compile();
        return Builder.CreateSub(Builder.getInt64(0), v, "invertedVal");
      }
    }

  private:
    Expr *expr1;
    std::string var;
  };

  class Ref : public Stmt {
  public:
    Ref(bool r) : refExists(r) {}

    void printAST(std::ostream &out) const override {
      out << "Ref(" << refExists << ")";
    }

    bool getRef() { return refExists; }

  private:
    bool refExists;
  };

  class FparArray : public Stmt {
  public:
    FparArray(bool e, ArraySize *a) : firstArraySizeEmpty(e), array_size(a) {}
    ~FparArray() { delete array_size; }

    void printAST(std::ostream &out) const override {
      out << "FparArray(" << firstArraySizeEmpty << ", " << *array_size << ")";
    }

    // maybe throw in somwhere there the firstArraySizeEmpty?
    std::vector<int> getSizes() const {
      return array_size->getSizes();
      // return array_list;
    }

    bool farraySizeEmpty() { return firstArraySizeEmpty; }

    bool isArray(){
      return getSizes().size() > 0 || farraySizeEmpty();
    }
  private:
    bool firstArraySizeEmpty;

    ArraySize *array_size;
  };

  class FparType : public Stmt {
  public:
    FparType(DataType t, FparArray *a) : type(t), fpar_array(a) {}
    ~FparType() { delete array_size; }

    void printAST(std::ostream &out) const override {
      out << "FparType(" << type << ", " << *fpar_array << ")";
    }

    DataType getType() { return type; }

    bool isArray() {
      //    return fpar_array->getSizes().size() != 0 ||
      //    fpar_array->farraySizeEmpty();

      //Working but inaccurate
      //      return !fpar_array
//                  ->farraySizeEmpty(); // We want to know if we want to handle
//                                       // it as llvm pointer or array
      return fpar_array->isArray();

    }
    bool farraySizeEmpty(){
      return fpar_array->farraySizeEmpty();
    }

    std::vector<int> getArraySizes() { return fpar_array->getSizes(); }

  private:
    DataType type;
    bool arrySizeEmpty;
    FparArray *fpar_array;
    ArraySize *array_size;
  };

  class FparDef : public Stmt {
  public:
    FparDef(Ref *r, VarDec *i, FparType *ft)
        : ref(r), id_list(i), fpar_type(ft) {}
    ~FparDef() {
      delete ref;
      delete id_list;
      delete fpar_type;
    }

    void sem() override { id_list->sem(); }
    void printAST(std::ostream &out) const override {
      out << "FparDef(" << *ref << ", " << *id_list << ", " << *fpar_type
          << ")";
    }

    Value *compile() override {
      std::clog << "FparDef compile: " << std::endl;

      return nullptr;
    }

    std::vector<FuncArg *> getArgs() {
      std::vector<Id *> ids = id_list->getIds();

      //    std::reverse(ids.begin(), ids.end());

      std::vector<FuncArg *> args;

      for (Id *id : ids) {

        FuncArg *arg = new FuncArg();
        arg->type = fpar_type->getType();
        arg->isArray = fpar_type->isArray();
        arg->isFirstArrayDimUnbounded = fpar_type->farraySizeEmpty();
        arg->array_size = fpar_type->getArraySizes();
        arg->name = id->getName();

        arg->ref = ref->getRef();

        args.push_back(arg);
      }

      // Value * idList = id_list->compile();
      std::clog << "FparDef compiled! " << std::endl;
      // return id_list;
      return args;
    }

  private:
    Ref *ref;

    VarDec *id_list;
    FparType *fpar_type;
  };

  class FparDefList : public Stmt {
  public:
    FparDefList() : fpardef_list() {}
    ~FparDefList() {
      for (FparDef *d : fpardef_list)
        delete d;
    }
    void add(FparDef *d) { fpardef_list.push_back(d); }
    void add_front(FparDef *d) { fpardef_list.push_front(d); }

    void sem() override {
      for (FparDef *d : fpardef_list)
        d->sem();
    }
    void printAST(std::ostream &out) const override {
      out << "FparDefList(";
      bool first = true;
      for (const auto &d : fpardef_list) {
        if (!first)
          out << ", ";
        first = false;
        out << *d;
      }

      out << ")";
    }

    std::vector<Value *> compileVector() {
      std::vector<Value *> args;

      std::clog << "FparDefList compile: " << std::endl;

      for (const auto &d : fpardef_list) {

        args.push_back(d->compile());
      }

      std::clog << "Fpar list compiled! " << std::endl;

      return args;
    }

    std::vector<FuncArg *> getArgs() {
      std::vector<FuncArg *> args = {};

      for (const auto &d : fpardef_list) {

        std::vector<FuncArg *> toMergeArgs = d->getArgs();
        // b.insert(b.end(), a.begin(), a.end());
        // merge arrays

        args.insert(args.end(), toMergeArgs.begin(), toMergeArgs.end());
      }

      return args;
    }

  private:
    std::deque<FparDef *> fpardef_list;
  };

  class FunctionHeader : public Stmt {
  public:
    FunctionHeader(std::string *s, FparDefList *f, DataType t)
        : Tid(*s), fpardef_list(f), type(t), argTypes({}) {}
    ~FunctionHeader() { delete fpardef_list; }

    void printAST(std::ostream &out) const override {
      out << "FunctionHeader(" << Tid << ", " << *fpardef_list << ", " << type
          << ")";
    }

    DataType getReturnType() { return type; }

    void sem() override {

      std::vector<FuncArg *> args = getArgs();

      //      argTypes = {};

      for (FuncArg *arg : args) {
        // TODO: handle ref, pass as pointer

        std::clog << arg->name << " Is array " << arg->isArray << " ref " << arg->ref <<std::endl;
        if(arg->isArray && !arg->ref)
          logError("Arrays need ref on definition.");

        Node *node = new Node();
        node->name = arg->name;
        node->decl_type = DECL_var;
        node->type = arg->type;
        std::clog << "New node! :" << arg->name << " is Array: " << arg->isArray
                  << std::endl;
        node->array_size = arg->array_size;
        node->assigned =
            true; // we do this since its arguments and the args are assigned
        node->isPointer = arg->ref;

//        if(node->array_size.size() >0 )

        //isArray only checks first part
        node->isFirstArrayDimUnbounded = arg->isFirstArrayDimUnbounded; //
        node->isSet = false;

        node->argTypes = argTypes;

        argnodes.push_back(node);
        std::clog << "Arg node: " << node->name << "is ref: " << arg->ref
                  << "is array: " << arg->isArray << std::endl;


      }

      Node *functionNode = new Node();
      functionNode->name = Tid;
      functionNode->decl_type = DECL_func;
      functionNode->isCompiled = false;
      functionNode->type = getReturnType();
      functionNode->funcargs = getArgs();

      std::clog << "Function Node set" << std::endl;
      // functionNode->function = F;
      functionNode->argnodes = &argnodes; // we do this on compile
      // functionNode->block = BB;

      fnode = functionNode;

      //we do this so we can check at the end if all were used
      st.addFunctionNode(functionNode);

      st.insertNode(functionNode, DECL_func);

      st.createScope(Tid, fnode); // add the name in scope

      fpardef_list->sem();

      // adding arguments in the st
      for (Node *node : argnodes) {
        st.insertNode(node, DECL_var);
      }

      st.exitScope();
    }

    Value *compile() override {
      std::clog << "FunctionHeader compile: " << std::endl;

      DataType dtype = getReturnType();

      Type *type = getLlvmType(dtype); // function cant return array?

      std::clog << "Adding arg nodes! " << std::endl;
      std::clog << argnodes.size() << std::endl;
      for (Node *node : argnodes) {
        node->llvm_type = getLlvmType(node->type, node->array_size);

        if (node->isPointer) {
          argTypes.push_back(PointerType::get(node->llvm_type, 0));
        } else {
          argTypes.push_back(node->llvm_type);
        }
      }

      FunctionType *FT = FunctionType::get(type, argTypes, false);

      std::clog << "here" << std::endl;

      Function *F =
          Function::Create(FT, Function::ExternalLinkage, Tid, TheModule.get());
      std::clog << "here2" << std::endl;

      func = F;

      // BasicBlock *previousBB = Builder.GetInsertBlock();

      BasicBlock *previousBB = Builder.GetInsertBlock();

      BasicBlock *BB = BasicBlock::Create(TheContext, Tid, F);

      fnode->function = F;
      fnode->block = BB;
      fnode->isCompiled = true;

      //    fnode = functionNode;

      Builder.SetInsertPoint(BB);

      // get arguments
      // TODO: double check the following!
      unsigned Idx = 0;
      for (auto &Arg : F->args()) {
        Arg.setName(argnodes[Idx]->name + "_funcarg");

        if (!argnodes[Idx]->isSet) {

          if (argnodes[Idx]->isPointer) {
            argnodes[Idx]->var = &Arg;
          } else {
            // null ptr needs to be arraySize if exists!
            AllocaInst *Alloca =
                Builder.CreateAlloca(argnodes[Idx]->llvm_type, nullptr,
                                     argnodes[Idx]->name + "_funcarg");
            // AllocaInst *Alloca = CreateEntryBlockAlloca(F,
            // argnodes[Idx]->name, argnodes[Idx]->llvm_type);
            StoreInst *store = Builder.CreateStore(&Arg, Alloca);

            store->setAlignment(Align(8));
            argnodes[Idx]->isSet = true;
            argnodes[Idx]->var = Alloca;
            argnodes[Idx]->isPointer = true;
          }
        }
        Idx++;
      }

      Builder.SetInsertPoint(previousBB);

      std::clog << "Gereee" << std::endl;

      return nullptr;
    }

    Function *getFunction() { return func; }

    std::vector<FuncArg *> getArgs() { return fpardef_list->getArgs(); }

    std::string getTid() { return Tid; }

  public:
    std::string Tid;
    FparDefList *fpardef_list;
    DataType type;

    std::vector<Type *> argTypes;
    std::vector<Node *> argnodes;

    Node *fnode;

    Function *func;
  };

  class LocalDefList : public Stmt {
  public:
    LocalDefList() : localdef_list() {}
    ~LocalDefList() {
      for (Stmt *d : localdef_list)
        delete d;
    }
    void add(Stmt *d) { localdef_list.push_back(d); }

    void sem() override {
      // variables need to be declared before functions
      std::vector<Stmt *> vardec_list = {};
      std::vector<Stmt *> stmt_list = {};

      for (Stmt *d : localdef_list) {
        if (typeid(*d) == typeid(VarDec)) {
          vardec_list.push_back(d);
        } else {
          stmt_list.push_back(d);
        }
      }

      for (Stmt *d : vardec_list) {
        d->sem();
      }

      for (Stmt *d : stmt_list) {
        d->sem();
      }
    }
    void printAST(std::ostream &out) const override {
      out << "LocalDefList(";
      bool first = true;
      for (const auto &d : localdef_list) {
        if (!first)
          out << ", ";
        first = false;
        out << *d;
      }

      out << ")";
    }
    Value *compile() const override {

      std::clog << "LocalDefList compile: " << std::endl;

      for (const auto &d : localdef_list) {
        d->compile();
      }

      return nullptr;
    }

  private:
    std::vector<Stmt *> localdef_list;
  };

  class FunctionDef : public Stmt {
  public:
    bool firstFunction; // we set this to true on lexer for the main function

    FunctionDef(FunctionHeader *h, LocalDefList *l, StmtList *s)
        : header(h), localdef_list(l), stmt_list(s), firstFunction(false) {}
    ~FunctionDef() {
      delete localdef_list;
      delete stmt_list;
      delete header;
    }

    void sem() override {

      DataType funRetType = header->getReturnType();

      if (firstFunction && funRetType != TYPE_nothing)
        logError("Main needs to have return type: nothing");

      if(firstFunction && header->getArgs().size() != 0)
        logError("Main function cant have arguments.");

      if (funRetType != TYPE_nothing && !stmt_list->isReturn())
        logError(
            "Function doesnt return"); // TODO: line shows the end of the
                                       // function, check if thats what i want

      // TODO: make this work
      //      if(!stmt_list->isReturn(funRetType))
      //          logError("Wrong return type");

      std::string funcName = header->getTid(); // funcname is Tid ?

      // its possible that only header has been declared before
      functionNode = st.lookupNodeLocal(
          funcName, DECL_func); // TODO: maybe do polymorphism?
      if (functionNode == nullptr) {
        header->sem();
        functionNode = header->fnode;
      }

      if (functionNode->isSet)
        logError("This function is already defined!");

      functionNode->isSet = true;

      std::clog << "Scope Name!: " << st.getName() << std::endl;

      st.enterScope(funcName);
      std::clog << "Scope Name!: " << st.getName() << std::endl;

      localdef_list->sem();
      std::clog << "STMTLIST makeScope Name!: " << st.getName() << std::endl;

      stmt_list->sem();

      st.exitScope(); // remove the name from scope
    }

    Value *compile() override {

      BasicBlock *previousBB = Builder.GetInsertBlock();
      BasicBlock *BB;
      DataType dtype = header->getReturnType();

      std::string funcName = header->getTid(); // funcname is Tid ?

      Function *F;

      std::clog << "FunctionDef funcname: " << funcName << std::endl;
      // Node *node = st.lookupNode(funcName,DECL_func);

      std::clog << "Function def lookup complete" << std::endl;

      if (functionNode == nullptr) {
        logError("This shouldnt occur!");
      } else if (!functionNode->isCompiled) {
        // if not declared
        std::clog << "About to compile header" << std::endl;
        header->compile();
        std::clog << "Function: " << funcName << "header compiled" << std::endl;
        F = header->getFunction();
        BB = header->fnode->block;
      } else {
        std::clog << "Function: " << funcName << " already declared!"
                  << std::endl;
        // if declared
        F = functionNode->function;
        BB = functionNode->block;
      }

      std::clog << "First Function: " << firstFunction << std::endl;
      if (firstFunction) {
        Builder.CreateCall(F);
        firstFunction = false;
      }

      std::clog << "Entering scope: " << funcName << std::endl;
      st.enterScope(funcName);

      // BasicBlock *BB = BasicBlock::Create(TheContext, funcName, F);
      Builder.SetInsertPoint(BB);

      localdef_list->compile();

      stmt_list->compile();

      if (dtype == TYPE_nothing && !stmt_list->isReturn())
        Builder.CreateRetVoid();

      Builder.SetInsertPoint(previousBB);

      std::clog << "Exiting scope: " << st.currentScope()->name << std::endl;

      st.exitScope(); // remove the name from scope

      return nullptr;
    }

    void printAST(std::ostream &out) const override {
      out << "FunctionDef(" << *header << ", " << *localdef_list << ", "
          << *stmt_list << ")";
    }

  private:
    FunctionHeader *header;
    LocalDefList *localdef_list;
    StmtList *stmt_list;

    Node *functionNode;
  };

  class BinOpCond : public Cond {
    // also take care of conds
  public:
    BinOpCond(Cond *e1, std::string *s, Cond *e2)
        : expr1(e1), op(*s), expr2(e2) {} // binOpCond belong to parent
    ~BinOpCond() {
      delete expr1;
      delete expr2;
    }
    void printAST(std::ostream &out) const override {
      out << "BinOpCond(" << op << ", " << *expr1 << ", " << *expr2 << ")";
    }

    void sem() override {
      // TODO: check maybe if its cond? or change type?
      std::clog << "Checking BinOpCond" << std::endl;
      expr1->check_type(TYPE_bool);
      expr2->check_type(TYPE_bool);

      sem_struct.type = TYPE_bool;
    }

    // after or else!
    // then aka do
    Value *compile(BasicBlock *thenBB, BasicBlock *afterBB) override {

      std::clog << "Compiling binopcond " << op << std::endl;

      Function *TheFunction = Builder.GetInsertBlock()->getParent();

      std::clog << "Compiling left" << std::endl;
      std::clog << "Compiled left" << std::endl;
      BasicBlock *evalRightBB =
          BasicBlock::Create(TheContext, "evalRight", TheFunction);

      // TODO: write test cases to make sure its correct
      if (op == "and") {
        Value *leftValue = expr1->compile(evalRightBB, afterBB);

        // if true and false -> evaluate left an
        Builder.CreateCondBr(leftValue, evalRightBB, afterBB);
      } else if (op == "or") {
        Value *leftValue = expr1->compile(thenBB, evalRightBB);

        Builder.CreateCondBr(leftValue, thenBB, evalRightBB);
      }
      Builder.SetInsertPoint(evalRightBB);

      return expr2->compile(thenBB, afterBB); // evaluate right expression
    }

  private:
    Cond *expr1;
    std::string op;
    Cond *expr2;
  };

  class CompareOp : public Cond {
    // also take care of conds
  public:
    CompareOp(Expr *e1, std::string *s, Expr *e2)
        : expr1(e1), op(*s), expr2(e2) {}
    ~CompareOp() {
      delete expr1;
      delete expr2;
    }
    void printAST(std::ostream &out) const override {
      out << "CompareOp(" << op << ", " << *expr1 << ", " << *expr2 << ")";
    }

    void sem() override {
      // TODO: also check if array?

      expr1->sem();
      expr2->sem();
      std::clog << "Sem CompareOp" << std::endl;
      // should have .type
      //      std::clog << "Type 1:" << expr1->sem_type << " Type 2: " <<
      //      expr2->sem_type << std::endl;
      if (expr1->sem_struct.type != expr2->sem_struct.type)
        logError("Compare types dont match");

      sem_struct.type = TYPE_bool;
    }

    Value *compile() override {

      Value *val1 = expr1->compile();
      Value *val2 = expr2->compile();

      // TODO: support arrays ??
      if (op == "<") {
        return Builder.CreateICmpSLT(val1, val2, "cmplt");
      } else if (op == "<=") {
        return Builder.CreateICmpSLE(val1, val2, "cmpgt");
      } else if (op == ">") {
        return Builder.CreateICmpSGT(val1, val2, "cmpgt");
      } else if (op == ">=") {
        return Builder.CreateICmpSGE(val1, val2, "cmpgt");
      } else if (op == "=") {
        return Builder.CreateICmpEQ(val1, val2, "cmpeq");
      } else if (op == "#") {
        return Builder.CreateICmpNE(val1, val2, "cmpne");
      }

      else {
        std::cerr << "invalid binary operator" << std::endl;
        exit(1);
      }
    }

  private:
    Expr *expr1;
    std::string op;
    Expr *expr2;
  };

  class UnaryOpCond : public Cond {
    // also take care of conds
  public:
    UnaryOpCond(Expr *e1, std::string *s) : expr1(e1), op(*s) {}
    ~UnaryOpCond() { delete expr1; }
    void printAST(std::ostream &out) const override {
      out << "UnaryOpCond(" << op << ", " << *expr1 << ")";
    }

    void sem() override {
      expr1->check_type(TYPE_bool);
      sem_struct.type = TYPE_bool;
    }
    Value *compile() override {

      Value *val1 = expr1->compile();

      // not other unary ops
      if (op == "not") {
        return Builder.CreateNot(val1, "not");
      }

      else {
        std::cerr << "invalid unary operator" << std::endl;
        exit(1);
      }
    }

  private:
    Expr *expr1;
    std::string op;
  };

  class ExprList : public Expr {
  public:
    ExprList() : expr_list() {}
    ~ExprList() {
      for (Expr *d : expr_list)
        delete d;
    }
    void add(Expr *d) { expr_list.push_back(d); }
    void add_front(Expr *d) { expr_list.push_front(d); }

    void sem() override {
      std::clog << "Calling ExprList sem" << std::endl;
      for (const auto &d : expr_list)
        d->sem();
    }

    void printAST(std::ostream &out) const override {
      out << "ExprList(";
      bool first = true;
      for (const auto &d : expr_list) {
        if (!first)
          out << ", ";
        first = false;
        out << *d;
      }

      out << ")";
    }

    std::deque<Expr *> getExprList() const { return expr_list; }

    std::vector<Value *> compileVector() {
      std::vector<Value *> args;

      for (const auto &d : expr_list) {
        args.push_back(d->compile());
      }

      return args;
    }

    std::vector<Value *> compileAssignVector() {
      std::vector<Value *> args;

      for (const auto &d : expr_list) {
        args.push_back(d->compileAssign());
      }

      return args;
    }

  private:
    std::deque<Expr *> expr_list;
  };
  class StringConst : public Lvalue {
  public:
    StringConst(std::string *s) : var(*s) {}
    void printAST(std::ostream &out) const override {
      out << "StringConst(" << var << ")";
    }
    void sem() override {
      //TODO: charlist doesnt work, maybe get rid of it?
      sem_struct.type = TYPE_char;
    }

    // TODO: make it work for the rest of the special chars
    std::string processString(const std::string &input) {
      std::string output;
      for (size_t i = 0; i < input.size(); ++i) {
        if (input[i] == '\\' && i + 1 < input.size()) {
          // Handle potential escape sequences
          int escapedChar = escSeqToChar(input.substr(i, (input[i + 1] == 'x' && i + 3 < input.size()) ? 4 : 2));
          if (escapedChar != -1) {
            output.push_back(static_cast<char>(escapedChar));
            i += (input[i + 1] == 'x' && i + 3 < input.size()) ? 3 : 1; // Skip processed characters
          } else {
            output.push_back(input[i]);
          }
        } else {
          output.push_back(input[i]);
        }
      }

      return output;
    }

    Value *compile() override {
      std::clog << "Called StringConst compile!" << std::endl;

      strConstant =
          ConstantDataArray::getString(TheContext, processString(var));

      GlobalVariable *gv = new GlobalVariable(
          *TheModule, strConstant->getType(),
          false, // we dont trully want this as constant..
          GlobalValue::ExternalLinkage, strConstant, "str_const");

      gv->setAlignment(Align(8));

      return gv;
    }
    Value *compileAssign() override { return compile(); }
    Node *compileArray() override {

      Value *val = compile();

      Node *array = new Node();

      int size = static_cast<int>(var.size());

      array->array_size = {size};
      array->llvm_type = ArrayType::get(i8, size);
      array->type = TYPE_charList;
      array->var = val;
      return array;
    }
    std::string getName() const { return "const_string"; }

    std::vector<Value *> getIndexes() override {
      std::vector<Value *> v =
          {}; // This is needed , its the base pointer for an array! , I think i
              // added this smwhere else
      return v;
    }
    std::vector<int> getArraySize() override {
      std::vector<int> v;
      v.push_back(var.size());
      return v;
    }

  private:
    std::string var;

    Constant *strConstant;
  };

  class FuncCall : public Stmt, public Expr {
  public:
    FuncCall(std::string *s, ExprList *e) : id(*s), expr_list(e) {}
    ~FuncCall() { delete expr_list; }
    void printAST(std::ostream &out) const override {
      out << "FuncCall(" << id << ", " << *expr_list << ")";
    }

    void sem() override {
      // TODO: implement this, we need to check that the arguments of the
      // function have the correct types also add return type to type

      expr_list->sem();

      functionNode = st.lookupNode(id, DECL_func);

      std::clog << "Current Scope: " << st.currentScope()->name << std::endl;

      std::clog << "Func sem analysis" << std::endl;
      if (functionNode ==
          nullptr) // if the function is not in Symbol Table or LIBRARY
      {
        std::clog << "Didnt find" << std::endl;

        if (externalFuncMap.find(id) == externalFuncMap.end()) {
          logError("Function named: " + id + " hasnt been declared");
        } else {
          auto funcDetails = externalFuncMap.find(id);
          FunctionDetails function = funcDetails->second;
          sem_struct.type = function.returnType;
        }
      } else {

        //TODO: this doesnt work. We need another way to check
        //Maybe keep all function nodes somewhere and check at the end of sem
//        if (!functionNode->isSet)
//          logError("The function definition is missing");

        std::clog << "func type" << std::endl;
        std::clog << functionNode->type << std::endl;
        sem_struct.type = functionNode->type;

        std::clog << "Func sem analysis2" << std::endl;

        std::vector<FuncArg *> funcargs = functionNode->funcargs;

        std::deque<Expr *> exprlist = expr_list->getExprList();

        std::clog << funcargs.size() << std::endl;
        std::clog << exprlist.size() << std::endl;

        if (exprlist.size() != funcargs.size())
          logError("Wrong number of arguments");

        int i = 0;
        for (auto &expr : exprlist) {



          if (funcargs[i]->ref) {
            // TODO: figure out whats up with the warning below
            std::clog << "here" << std::endl;
            if (typeid(*expr) != typeid(IdLval) and typeid(*expr) != typeid(ArrayElem) and typeid(*expr) != typeid(StringConst))
              logError("Argument needs to be IdLval or ArrayElem cause its a pointer");


            //std::clog << "Is array arg: " << funcargs[i]->isArray << " Ar: "<< expr->isArray() << std::endl;

            if(funcargs[i]->isArray &&  typeid(*expr) == typeid(IdLval) && !expr->isArray())
              logError("Argument needs to be an array");
          }
          std::clog << expr->sem_struct.type << " " << funcargs[i]->type << std::endl;
          if (expr->sem_struct.type != funcargs[i]->type)
            logError("Argument type mismatch");

          i++;
        }
      }
    }
    Value *compile() override {
      std::clog << "Compiling function call: " << id << std::endl;

      // std::vector<Value*> args = expr_list->compileVector();
      std::vector<Value *> args;

      // we do this now in sem
      // Node* functionNode = st.lookupNode(id, DECL_func);

      std::deque<Expr *> exprlist = expr_list->getExprList();
      if (functionNode == nullptr) {
        // either doesnt exist or lib

      } else {
        // argslist

        std::clog << "Getting args" << std::endl;
        std::vector<FuncArg *> funcargs = functionNode->funcargs;

        int i = 0;
        for (auto &expr : exprlist) {
          // if pointer compileAssign
          std::clog << "here" << std::endl;
          if (funcargs[i]->ref) {
            // we need the array size
            std::clog << "here2" << std::endl;
            std::clog << "Argument ref, name: " << funcargs[i]->name
                      << std::endl;
            std::clog << i << " " << exprlist.size() << " " << funcargs.size()
                      << std::endl;
            args.push_back(expr->compileAssign());

          } else {
            args.push_back(exprlist[i]->compile());
          }
          i++;
        }

        // Here we pass extra args related to nesting
        for (auto &n : functionNode->extraArgNodes) {
          // TODO: maybe improve this!
          args.push_back(st.lookupNode(n->name, DECL_var, true)->var);
          // args.push_back(n->realNode->var);
        }
      }

      Function *func;

      if (functionNode == nullptr) {

        auto fun = externalFuncMap.find(id);

        if (fun == externalFuncMap.end()) {
          logError("Cant find externalFunc");
        }
        FunctionDetails function = fun->second;

        //        std::vector<Bool> argsPointer = function.argsPointer;

        int i = 0;
        for (bool pointer : function.argsPointer) {
          // if pointer compileAssign
          if (pointer) {
            // we need the array size
            args.push_back(exprlist[i]->compileAssign());

          } else {
            args.push_back(exprlist[i]->compile());
          }
          i++;
        }

        func = function.func;

      } else {
        func = functionNode->function;
      }

      std::clog << "Function: "
                << " found" << std::endl;

      Value *res = Builder.CreateCall(func, args);
      std::clog << "Function: "
                << " found2" << std::endl;

      if (id == "readInteger" || id == "strlen" || id == "ascii") {

        return Builder.CreateSExt(res, i64, "cast");

      } else {
        return res;
      }
    }

  private:
    std::string id;
    ExprList *expr_list;

    Node *functionNode;

    int offset;
  };

  class IntConst : public Expr {
  public:
    IntConst(int n) : num(n) {}
    void printAST(std::ostream &out) const override {
      out << "IntConst(" << num << ")";
    }

    void sem() override { sem_struct.type = TYPE_int; }

    Value *compile() override {

      std::clog << "Compiling expression: " << num << std::endl;
      return ConstantInt::get(i64, num);
    }

  private:
    int num;
  };


#endif