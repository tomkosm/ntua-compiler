#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <vector>
#include <map>
#include <deque>



#include <string>




#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Utils.h>

using namespace llvm;




extern std::vector<int> rt_stack;


enum DataType 
{   TYPE_int, 
    TYPE_char, 
    TYPE_nothing
};

//needs to be able to see DataType
#include "symbol.cpp"




class AST {
 public:
  virtual ~AST() = default;
  virtual void printAST(std::ostream &out) const = 0;
  virtual Value* compile() const {std::clog << "Called ast const compile" <<std::endl; return nullptr; }
  virtual Value* compile()  { std::clog << "Called ast compile" <<std::endl; return nullptr; }
  virtual Node *compileArray() {}
  std::string getName(){}

  Type* getLlvmType(DataType dtype){
    if(dtype == TYPE_int)
        return Type::getInt32Ty(TheContext);
    else if(dtype == TYPE_char)
        return Type::getInt8Ty(TheContext);
    else if(dtype == TYPE_nothing)
        return Type::getVoidTy(TheContext);
    else{
        std::clog << "Error, couldnt find type!" << std::endl;
        exit(2);
    }
      
  }


  void llvm_compile_and_dump(bool optimize=false) {
    // Initialize
    TheModule = std::make_unique<Module>("Grace", TheContext);
    
    TheFPM = std::make_unique<legacy::FunctionPassManager>(TheModule.get());
    if (optimize) {
      TheFPM->add(createPromoteMemoryToRegisterPass());
      TheFPM->add(createInstructionCombiningPass());
      TheFPM->add(createReassociatePass());
      TheFPM->add(createGVNPass());
      TheFPM->add(createCFGSimplificationPass());
    }
    TheFPM->doInitialization();
    // Initialize types
    i8 = IntegerType::get(TheContext, 8);
    i32 = IntegerType::get(TheContext, 32);
    i64 = IntegerType::get(TheContext, 64);
 


    // Initialize library functions
    FunctionType *writeInteger_type =
      FunctionType::get(Type::getVoidTy(TheContext), {i64}, false);
    TheWriteInteger =
      Function::Create(writeInteger_type, Function::ExternalLinkage,
                       "writeInteger", TheModule.get());


    FunctionType *writeChar_type =
      FunctionType::get(Type::getVoidTy(TheContext), {i8}, false);
    TheWriteChar =
      Function::Create(writeChar_type, Function::ExternalLinkage,
                       "writeChar", TheModule.get());




    FunctionType *writeString_type =
      FunctionType::get(Type::getVoidTy(TheContext),
                        {PointerType::get(i8, 0)}, false);
    TheWriteString =
      Function::Create(writeString_type, Function::ExternalLinkage,
                       "writeString", TheModule.get());



    FunctionType *readInteger_type =
      FunctionType::get(i64, {}, false);

    TheReadInteger =
      Function::Create(readInteger_type, Function::ExternalLinkage,
                       "readInteger", TheModule.get());








    // Define and start the main function.
    FunctionType *main_type = FunctionType::get(i32, {}, false);
    Function *main =
      Function::Create(main_type, Function::ExternalLinkage,
                       "main", TheModule.get());
    BasicBlock *BB = BasicBlock::Create(TheContext, "entry", main);
    Builder.SetInsertPoint(BB);

    // Emit the program code.
    compile();

    std::clog << "Compiled!" << std::endl;

    Builder.SetInsertPoint(BB);
    Builder.CreateRet(c32(0));

    // Verify the IR.
    bool bad = verifyModule(*TheModule, &errs());
    if (bad) {
      std::cerr << "The IR is bad!" << std::endl;
      TheModule->print(errs(), nullptr);
      std::exit(1);
    }

    // Optimize!
    TheFPM->run(*main);

    // Print out the IR.
    TheModule->print(outs(), nullptr);
  }

 public:
  static SymbolTable st; //maybe some special class to do this?



 protected:
  static LLVMContext TheContext;
  static IRBuilder<> Builder;
  static std::unique_ptr<Module> TheModule;
  static std::unique_ptr<legacy::FunctionPassManager> TheFPM;

  static GlobalVariable *TheVars;
  static GlobalVariable *TheNL;
  static Function *TheWriteInteger;

  static Function *TheWriteChar;


  static Function *TheWriteString;


    static Function *TheReadInteger;


  static Type *i8;
  static Type *i32;
  static Type *i64;

  static ConstantInt* c8(char c) {
    return ConstantInt::get(TheContext, APInt(8, c, true));
  }
  static ConstantInt* c32(int n) {
    return ConstantInt::get(TheContext, APInt(32, n, true));
  }




};


inline std::ostream &operator<<(std::ostream &out, DataType t) {
  switch (t) {
    case TYPE_int:  out << "int";  break;
    case TYPE_char: out << "char"; break;
    case TYPE_nothing: out << "nothing"; break;

  }
  return out;
}

inline std::ostream &operator<<(std::ostream &out, const AST &ast) {
  ast.printAST(out);
  return out;
}


class Expr : public AST {
 public:
  virtual Value* compileAssign() { std::clog << "Called Lvalue compileAssign!" << std::endl; return nullptr; }

 void printAST(std::ostream &out) const override {
    out << "Expr(empty)";
  }

    Value* compile() override {
      std::clog << "Called base EXPR compile!" << std::endl;
        // Implement compile for Expr if necessary, or keep it pure virtual
    }
};



class Id : public Expr {
 public:
  Id(std::string *c): var(*c) {}
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
    for (Id *d : id_list) delete d;
  }
  void add(Id *d) { id_list.push_back(d); }

  void printAST(std::ostream &out) const override {
    out << "IdList(";
    bool first = true;
    for (const auto &d : id_list) {
      if (!first) out << ", ";
      first = false;
      out << *d;
    }

    out << ")";
  }

  //TODO: needs to return the type of each!
  std::vector<Id *> getIds() const { return id_list; }


    std::vector<Value*> compileVector() {
    std::vector<Value*> args;

    std::clog << "id_list compile: " << std::endl;

    // std::clog << "type: " << 


    for(const auto &d : id_list) {
        args.push_back(d->compile());
    }


    return args;
  }


  private:
    std::vector<Id *> id_list;
};





class Stmt : public AST {
 public:
   void name(){std::clog << "Stmt name" << std::endl;}

   void printAST(std::ostream &out) const override {
    out << "Stmt(empty)";
  }

//   virtual void execute() const = 0;
};




class ArraySize : public Stmt {
 public:
  ArraySize() : array_list() {}


  void add(int s) { array_list.push_back(s); }

  void printAST(std::ostream &out) const override {
    out << "ArraySize(";
    bool first = true;
    for (const auto &s : array_list) {
      if (!first) out << ", ";
      first = false;
      out << s;
    }
    out << ")";
  }

  std::vector<int> getSizes() const { return array_list;}

  

 private:
  std::vector<int> array_list;
};




class TypeDef : public Stmt {
 public:
  TypeDef(DataType t, ArraySize *s): type(t), array_size(s) {}

  void printAST(std::ostream &out) const override {
    out << "TypeDef(" << type << ", " << *array_size << ")";
  }
  //TODO: handle arrays
  DataType getType() const { return type; }
 
  std::vector<int> getSizes() const { return array_size->getSizes();}



 private:
  DataType type;
  ArraySize *array_size;
};






class VarDec : public Stmt {
 public:
  VarDec(IdList *i, TypeDef *t): id_list(i), type(t) {}

  void printAST(std::ostream &out) const override {
    out << "VarDec(" << *id_list << ": " << *type << ")";
  }

  std::vector<Id *> getIds() const { return id_list->getIds(); }



  Value* compile() override {

    Type * itype;
    int initializerSize;

    DataType dtype = type->getType();



    if(dtype == TYPE_int){
      itype = i32;
      initializerSize = 32;
    }
    else if(dtype == TYPE_char){
      itype = i8;
      initializerSize = 8;
    }


    std::vector<int> arraysizes = type->getSizes();
    Constant* Initializer;
    ArrayType* ArrayTy;

    if(arraysizes.size() == 0){
      Initializer = ConstantInt::get(TheContext, llvm::APInt(initializerSize, 0));

    }else if(arraysizes.size() == 1){
      //wrong need to handle multi dim
      ArrayTy = ArrayType::get(itype, arraysizes[0]);
      std::vector<Constant*> constArray(arraysizes[0], ConstantInt::get(TheContext, APInt(32, 0)));
      Initializer = ConstantArray::get(ArrayTy, constArray);

      itype = ArrayTy;
    }


    //issues if multiple same name etc 
    for(auto id : id_list->getIds()) {
      std::clog << "Compiling VarDec name: " << id->getName() << " Current scope: " << st.currentScope()->name << std::endl; 

      // AllocaInst* allocaInst = Builder.CreateAlloca(i32, 0, id->getName());


      GlobalVariable *gVar = new llvm::GlobalVariable(
        *TheModule,
        itype,
        false, // isConstant
        GlobalValue::PrivateLinkage,
        Initializer, // Initializer
        "gVar"
      );



      std::clog << "Alloc " << id->getName() << std::endl;


      Node *idNode = new Node();
      idNode->name = id->getName();
      idNode->type = type->getType();
      idNode->decl_type = DECL_var;
      idNode->var = gVar;
      idNode->llvm_type = itype;
      idNode->assigned = false;
      idNode->isPointer = true;

      std::clog << "Created node" << std::endl;
      st.insertNode(idNode);

      std::clog << "Inserted node" << std::endl;

    }

    return nullptr;
  }

  Value *compileFuncParams(){

  }

 private:
  IdList *id_list;
  TypeDef *type;
};



class CharConst : public Expr {
 public:
  CharConst(char s): var(s) {}
  void printAST(std::ostream &out) const override {
    out << "CharConst(" << var << ")";
  }

  Value *compile() override {
    std::clog << "Called CharConst compile!" << std::endl;
    return ConstantInt::get(i8, var);
  }


 private:
  char var;
  };


class CharConstSpecial : public CharConst {
 public:
  CharConstSpecial(std::string *s): CharConst('a'), var(*s)  {}
  void printAST(std::ostream &out) const override {
    out << "CharConstSpecial(" << escSeqToChar(var) << ")";
  }

  int escSeqToChar(std::string v) const
{
  
    int res;
    if (v[0] == '\\')
    {


        switch (v[1])
        {
        case 'n':
            res = '\n'; break;
        case 't':
            res = '\t'; break;
        case 'r':
            res = '\r'; break;
        case '0':
            res = '\0'; break;
        case '\\':
            res = '\\'; break;
        case '\'':
            res = '\''; break;
        case '\"':
            res = '\"'; break;
        case 'x':
            res = std::stoi(v.substr(2), nullptr, 16); break;
        default:
            break;
        }
    

    return res;
}
}
  Value *compile() override {
    std::clog << "Called CharConstSpecial compile!" << std::endl;
    return ConstantInt::get(i8, escSeqToChar(var));
  }



 private:
  std::string var;
  };




class Lvalue : public Expr {
    public:
    Lvalue() {}
    virtual std::string getName() { std::clog << "Called Lvalue getName!" << std::endl; return ""; }
    virtual Value* compileAssign() { std::clog << "Called Lvalue compileAssign!" << std::endl; return nullptr; }
    virtual void updatelookup() {}
//  public:
//   virtual void execute() const = 0;
};

class IdLval : public Lvalue {
 public:
  IdLval(std::string *c) : var(*c) {}
  void printAST(std::ostream &out) const override {
    out << "IdLval(" << var << ")";
  }

  std::string getName() override {
    std::clog << "IDLVAL var: "  << std::endl;
    return var;
  }

  void updatelookup(){
      node->assigned = true;
  }

  Value* compileAssign() override{
    
    Node* idNode = st.lookupNode(var);
    if(idNode == nullptr){
      std::cerr << "Error: variable " << var << " not declared" << std::endl;
      exit(1);
    }

    Value * gvar = idNode->var;

    node = idNode;

    return gvar;
    
    // TODO: we need to check the type!!
    //

  }

  Value *compile() {
    std::clog << "Compiling IdLval!" << std::endl;


    Value *gvar = compileAssign();


    if(!node->assigned){
      std::clog << "Error, tried to access a variable that isnt assigned" << std::endl;
      exit(2);
    }

    if(!node->isPointer){
      return gvar;
    }else{
      return Builder.CreateLoad(node->llvm_type,gvar, var + "_load");
    }


  }

  Node *compileArray() override{

    Node* idNode = st.lookupNode(var);
    if(idNode == nullptr){
      std::cerr << "Error: variable " << var << " not declared" << std::endl;
      exit(1);
    }

    
    return idNode;



  }

 private:

  Node *node;

  std::string var;
  int offset;
};



class ArrayElem : public Lvalue {
 public: /*need to support arrays too!*/
  ArrayElem(Lvalue *lhs, Expr *rhs): var(lhs), expr(rhs) {}
  ~ArrayElem() { delete var; delete expr; }
  void printAST(std::ostream &out) const override {
    out << "ArrayElem(" << *var << ", " << *expr << ")";
  }

  std::string getName() override {
    std::clog << "Called ArrayElem getName!" << std::endl;
    return var->getName();
  }

  Value *compileAssign(){
    std::clog << "Compiling compileAssign array element: " << *var << std::endl;

    Node *array = var->compileArray();

    Value *index = expr->compile();

    std::vector<Value*> arrayIndex = {c32(0), index};

    Value *elementPtr = Builder.CreateGEP(array->llvm_type,array->var, arrayIndex, "arrayElem");
    


    //used for compile()
    this->datatype = array->type;
    
    return elementPtr;

    //
    //return elementValue;

  }

    Value *compile() override{

    std::clog << "Compiling array element: " << *var << std::endl;
    
    Value *elementPtr = compileAssign();
    

    Type* elementType;
    if(this->datatype == TYPE_int){
      elementType = i32;
    }
    else if(this->datatype == TYPE_char){
      elementType = i8;
    }
    else{
      std::cerr << "Error: invalid type" << std::endl;
      exit(1);
    }


    Value* elementValue = Builder.CreateLoad({i32},elementPtr, "elementValue");

    return elementValue;

    
  }


 private:
  Lvalue *var;
  Expr *expr;

  DataType datatype;

  int offset;
};




class Assign : public Stmt {
 public: /*need to support arrays too!*/
  Assign(Lvalue *lhs, Expr *rhs): var(lhs), expr(rhs) {}
  ~Assign() { delete var; delete expr; }
  void printAST(std::ostream &out) const override {
    out << "Assign(" << *var << ", " << *expr << ")";
  }
  
  Value* compile() override {


    std::clog << "Assign compile: Current Scope: " << st.currentScope()->name  << std::endl;

    std::clog << *var << std::endl;

    std::clog << "Variable name: "<< var->getName() << std::endl;


    Value *rhs = expr->compile();



    Value *lhs = var->compileAssign();

    var->updatelookup();


    std::clog << "Compiled rhs" << std::endl;

    // Store the constant value into the alloca.
    Builder.CreateStore(rhs, lhs);

    std::clog << "Created store!" << std::endl;




    return nullptr;
  }

 private:
  Lvalue *var;
  Expr *expr;

};



class Return : public Stmt {
 public:
  Return(Expr *e): expr(e) {}
  ~Return() { delete expr;  }
  void printAST(std::ostream &out) const override {
    out << "Return(" << *expr << ")";
  }

  Value *compile() override{

    return Builder.CreateRet(expr->compile());
  }

 private:
  Expr *expr;
};

class StmtList : public Stmt {
 public:
  StmtList() : stmt_list() {}
  ~StmtList() {
    for (Stmt *s : stmt_list) delete s;
  }
  void add(Stmt *s) { stmt_list.push_back(s); }

  void name(){std::clog << "StmtList name" << std::endl;}
  //TODO: We need to check that the order of statements is correct! SUPER IMPORTANT!


  Value* compile() const override {
    std::clog << "StmtList compile: " << std::endl;

    for (Stmt *s : stmt_list) s->compile();

    std::clog << "StmtList compile done" << std::endl;

    return nullptr;
  }


  void printAST(std::ostream &out) const override {
    out << "StmtList(";
    bool first = true;
    for (const auto &s : stmt_list) {
      if (!first) out << ", ";
      first = false;
      out << *s;
    }
    out << ")";
  }

 private:
  std::vector<Stmt *> stmt_list;
};


class If : public Stmt {
 public:
  If(Expr *c, Stmt *s1, Stmt *s2 = nullptr) : cond(c), stmt1(s1), stmt2(s2) {}
  ~If() { delete cond; delete stmt1; delete stmt2; }
  void printAST(std::ostream &out) const override {
    out << "If(" << *cond << ", " << *stmt1;
    if (stmt2 != nullptr) out << ", " << *stmt2;
    out << ")";
  }

  Value* compile()  override {
  
    std::clog << "Called If compile!" << std::endl;

    //TODO: figure out if there is some better way other than doing this...
    StmtList* stmtList1 = dynamic_cast<StmtList*>(stmt1);
    StmtList* stmtList2 = dynamic_cast<StmtList*>(stmt2);



    Value *condition = cond->compile();
    // Value *cond = Builder.CreateICmpNE(v, c32(0), "if_cond");
    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock *ThenBB =
      BasicBlock::Create(TheContext, "then", TheFunction);
    BasicBlock *ElseBB =
      BasicBlock::Create(TheContext, "else", TheFunction);
    BasicBlock *AfterBB =
      BasicBlock::Create(TheContext, "endif", TheFunction);
    Builder.CreateCondBr(condition, ThenBB, ElseBB);
    Builder.SetInsertPoint(ThenBB);
    std::clog << "About to compile stmt" << std::endl;
    stmtList1->compile();
    Builder.CreateBr(AfterBB);
    Builder.SetInsertPoint(ElseBB);
    if (stmtList2 != nullptr) stmtList2->compile();
    Builder.CreateBr(AfterBB);
    Builder.SetInsertPoint(AfterBB);
    return nullptr;


  
  }

 private:
  Expr *cond;
  Stmt *stmt1;
  Stmt *stmt2;
};


class While : public Stmt {
 public:
  While(Expr *e, Stmt *s): cond(e), stmt(s) {}
  ~While() { delete cond; delete stmt; }
  void printAST(std::ostream &out) const override {
    out << "While(" << *cond << ", " << *stmt << ")";
  }

  Value *compile() override{
    std::clog <<"while compile" << std::endl;


    BasicBlock *originalBB = Builder.GetInsertBlock();

    Function *TheFunction = originalBB->getParent();
    
    BasicBlock *ConditionBB =
        BasicBlock::Create(TheContext, "condition", TheFunction);

    BasicBlock *DoBB =
        BasicBlock::Create(TheContext, "do", TheFunction);

    BasicBlock *AfterBB =
      BasicBlock::Create(TheContext, "after", TheFunction);


    //start from condition
    Builder.CreateBr(ConditionBB);

    Builder.SetInsertPoint(ConditionBB);

    Value *condition = cond->compile();

    Builder.CreateCondBr(condition, DoBB, AfterBB);


    //make the do part

    Builder.SetInsertPoint(DoBB);
    std::clog << "About top stmt compile: " << std::endl;

    //TODO: figure out if there is some better way other than doing this...
    StmtList* stmtList = dynamic_cast<StmtList*>(stmt);

    stmtList->compile();

    Builder.CreateBr(ConditionBB);



    Builder.SetInsertPoint(AfterBB);




    return nullptr;
  }

 private:
  Expr *cond;
  Stmt *stmt;
};

class BinOp : public Expr {
    //also take care of conds 
 public:
  BinOp(Expr *e1, std::string *s, Expr *e2) : expr1(e1), op(*s), expr2(e2) {}
  ~BinOp() { delete expr1; delete expr2; }
  void printAST(std::ostream &out) const override {
    out << "BinOp(" << *expr1 << ", " << op << ", " << *expr2 << ")";
  }

  Value* compile() override{
    std::clog << "BinOp compile: " << std::endl;
    std::clog << "Left expr: " << std::endl;
    std::clog << typeid(*expr1).name() << std::endl;
    Value *l = expr1->compile();



    std::clog << "Right expr: " << std::endl;
    Value *r = expr2->compile();

    

    if(op == "+") {
      return Builder.CreateAdd(l, r, "addtmp");
    } else if (op == "-") {
      return Builder.CreateSub(l, r, "subtmp");
    } else if (op == "*"){
      return Builder.CreateMul(l, r, "multmp");
    } else if (op == "div"){
      return Builder.CreateSDiv(l, r, "divtmp");
    } else if (op == "mod"){
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
  UnaryOp(Expr *e1, std::string *s) : expr1(e1), var(*s){}
  ~UnaryOp() { delete expr1; }
  void printAST(std::ostream &out) const override {
    out << "UnaryOp("<< var<< ", " << *expr1 << ")";

  }
  Value *compile() override{

    if(var == "+"){
      //i dont think we need to do anything here
      return expr1->compile();
    }
    else if(var == "-"){
      // we do 0-value(expr1) to inverse
      Value *v = expr1->compile();
      return Builder.CreateSub(Builder.getInt32(0), v, "invertedVal");

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

  bool getRef(){
    return refExists;
  }
  private:
    bool refExists;
};







class FparType : public Stmt {
 public:
  FparType(DataType t, bool e, ArraySize *a) : type(t),arrySizeEmpty(e),array_size(a) {}
  ~FparType() {
    delete array_size;
  }

  void printAST(std::ostream &out) const override {
    out << "FparType("<< type<< ", " << arrySizeEmpty << ", " << *array_size << ")";

  }

  DataType getType(){
    return type;
  }

  bool isArray(){
    return !arrySizeEmpty;
  }

  ArraySize *getArraySize(){
    return array_size;
  }


  private:

    DataType type;
    bool arrySizeEmpty;

    ArraySize *array_size;

};


class FparDef : public Stmt {
 public:
  FparDef(Ref *r,VarDec *i,FparType *ft) : ref(r),id_list(i),fpar_type(ft) {}
  ~FparDef() {
    delete ref; delete id_list; delete fpar_type;
  }


  void printAST(std::ostream &out) const override {
    out << "FparDef("<< *ref <<  ", " << *id_list << ", " << *fpar_type << ")";
  }

  Value* compile() override{
    std::clog << "FparDef compile: " << std::endl;

    return nullptr;
  }

  std::vector<FuncArg *> getArgs(){
    std::vector<Id *> ids = id_list->getIds();

    std::vector<FuncArg *> args;

    for(Id *id : ids){
      FuncArg *arg = new FuncArg();
      arg->type = fpar_type->getType();
      arg->isArray = fpar_type->isArray();
      arg->name = id->getName();
      arg->isArray = fpar_type->isArray();
      arg->ref = ref->getRef();

      args.push_back(arg);


    }

    //Value * idList = id_list->compile();
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
    for (FparDef *d : fpardef_list) delete d;
  }
  void add(FparDef *d) { fpardef_list.push_back(d); }

  void printAST(std::ostream &out) const override {
    out << "FparDefList(";
    bool first = true;
    for (const auto &d : fpardef_list) {
      if (!first) out << ", ";
      first = false;
      out << *d;
    }

    out << ")";
  }

  std::vector<Value*> compileVector() {
    std::vector<Value*> args ;

    std::clog << "FparDefList compile: " << std::endl;

    for(const auto &d : fpardef_list) {

        args.push_back(d->compile());
    }

    std::clog << "Fpar list compiled! " << std::endl;

    return args;
  }

  std::vector<FuncArg *> getArgs(){
    std::vector<FuncArg *> args ={};

    for(const auto &d : fpardef_list) {

      std::vector<FuncArg *> toMergeArgs = d->getArgs();
      //b.insert(b.end(), a.begin(), a.end());
      //merge arrays

      args.insert(args.end(),toMergeArgs.begin(),toMergeArgs.end());
    }



    return args;
  }

  private:
    std::vector<FparDef *> fpardef_list;


};





class FunctionHeader : public Stmt {
 public:
  FunctionHeader(std::string *s,FparDefList *f,DataType t) : Tid(*s),fpardef_list(f),type(t) {}
  ~FunctionHeader() {
    delete fpardef_list;
  }


  void printAST(std::ostream &out) const override {
    out << "FunctionHeader("<< Tid<< ", " << *fpardef_list << ", " << type << ")";
  }

  DataType getReturnType(){
    return type;
  }

  Value* compile() override{
      std::clog << "FunctionHeader compile: " << std::endl;
      fpardef_list->compileVector();

      return nullptr;
  }

  std::vector<FuncArg *> getArgs(){
    return fpardef_list->getArgs();
  }

  public:
    std::string Tid;
    FparDefList *fpardef_list;
    DataType type;

};


class LocalDefList : public Stmt {
 public:
  LocalDefList() : localdef_list() {}
  ~LocalDefList() {
    for (Stmt *d : localdef_list) delete d;
  }
  void add(Stmt *d) { localdef_list.push_back(d); }

  void printAST(std::ostream &out) const override {
    out << "LocalDefList(";
    bool first = true;
    for (const auto &d : localdef_list) {
      if (!first) out << ", ";
      first = false;
      out << *d;
    }

    out << ")";
  }
    Value* compile() const override {
        
        std::clog << "LocalDefList compile: " << std::endl;

        for(const auto &d : localdef_list) {
            d->compile();
        }

        return nullptr;
    }

  private:
    std::vector<Stmt *> localdef_list;
};




class FunctionDef : public Stmt {
 public:
  bool firstFunction;

  FunctionDef(FunctionHeader *h,LocalDefList *l,StmtList *s) : header(h),localdef_list(l),stmt_list(s),firstFunction(false) {}
  ~FunctionDef() {
    delete localdef_list; delete stmt_list; delete header;
  }

  Value* compile() override {


    // Make the function type:  double(double,double) etc.

    DataType dtype = header->getReturnType();


    Type *type = getLlvmType(dtype);



    //args

    std::vector<Node*> argnodes;

    std::vector<FuncArg *> args = header->getArgs();
    std::vector<Type*> argTypes = {};
    for(FuncArg *arg : args){
      //TODO: handle ref, pass as pointer
 

      Node *node = new Node();
      node->name = arg->name;
      node->decl_type = DECL_var;
      node->type = arg->type;
      node->llvm_type = getLlvmType(arg->type);
      node->assigned = true;//we do this since its arguments and the args are assigned
      node->isPointer = arg->ref;


      argnodes.push_back(node);
      if(arg->ref){
        argTypes.push_back(PointerType::get(node->llvm_type, 0));
      }else{
        argTypes.push_back(node->llvm_type);
      }


    }


    std::clog << "Func arg size: " << args.size() << std::endl;




    FunctionType *FT = FunctionType::get(type,argTypes,false);



    Function *F = Function::Create(FT, Function::ExternalLinkage, header->Tid,TheModule.get());


    //get arguments
    unsigned Idx = 0;
    for (auto &Arg : F->args()) {
      Arg.setName(argnodes[Idx]->name);
      argnodes[Idx]->var = &Arg;
      Idx++;
    }


    std::clog << "First Function: " << firstFunction << std::endl;
    if(firstFunction){
      Builder.CreateCall(F);
      firstFunction = false;
    }


    BasicBlock *previousBB = Builder.GetInsertBlock();


    BasicBlock *BB = BasicBlock::Create(TheContext, header->Tid, F);
    Builder.SetInsertPoint(BB);

    std::clog << "Gereee" << std::endl;



    Node *functionNode = new Node();
    functionNode->name = header->Tid;
    functionNode->decl_type = DECL_func;
    functionNode->function = F;
    functionNode->funcargs = header->getArgs();




    st.insertNode(functionNode,DECL_func);


    st.createScope(header->Tid); //add the name in scope 
    
    //adding arguments in the st
    for(Node *node : argnodes){
      st.insertNode(node,DECL_var);
    }


    std::clog << "Current Scope: " <<  st.currentScope()->name << std::endl;

    

    localdef_list->compile();

    stmt_list->compile();

    //TODO: we should only do this if no return in stmt_list!
    //maybe it works that way too cause worst case we have doulbe ret
    if(dtype == TYPE_nothing)
      Builder.CreateRetVoid();

    Builder.SetInsertPoint(previousBB);
    // 


    std::clog << "Exiting scope: " << st.currentScope()->name << std::endl;

    st.exitScope(); //remove the name from scope


 



    return nullptr;



  }


  void printAST(std::ostream &out) const override {
    out << "FunctionDef("<< *header<< ", " << *localdef_list << ", " << *stmt_list << ")";
  }



  private:
    FunctionHeader *header;
    LocalDefList *localdef_list;
    StmtList *stmt_list;
    

};







class Cond : public Expr {
//  public:
//   virtual void execute() const = 0;
};


class BinOpCond : public Cond {
    //also take care of conds 
 public:
  BinOpCond(Expr *e1, std::string *s, Expr *e2) : expr1(e1), op(*s), expr2(e2) {}
  ~BinOpCond() { delete expr1; delete expr2; }
  void printAST(std::ostream &out) const override {
    out << "BinOpCond("<< op<< ", " << *expr1 << ", " << *expr2 << ")";

  }
  Value *compile() override{

    Value *val1 = expr1->compile();
    Value *val2 = expr2->compile();
    
    if(op == "and"){
      return Builder.CreateAnd(val1, val2, "andtmp");
    }
    else if(op == "or"){
      return Builder.CreateOr(val1, val2, "ortmp");
    }
  }
 private:
  Expr *expr1;
  std::string op;
  Expr *expr2;
};


class CompareOp : public Cond {
    //also take care of conds 
 public:
  CompareOp(Expr *e1, std::string *s, Expr *e2) : expr1(e1), op(*s), expr2(e2) {}
  ~CompareOp() { delete expr1; delete expr2; }
  void printAST(std::ostream &out) const override {
    out << "CompareOp("<< op<< ", " << *expr1 << ", " << *expr2 << ")";
  }

  Value *compile() override{

    Value *val1 = expr1->compile();
    Value *val2 = expr2->compile();

    //TODO: support arrays
    if(op == "<"){
      return Builder.CreateICmpSLT(val1, val2, "cmplt");
    }
    else if(op == "<="){
      return Builder.CreateICmpSLE(val1, val2, "cmpgt");
    }
    else if(op == ">"){
      return Builder.CreateICmpSGT(val1, val2, "cmpgt");
    }
    else if(op == ">="){
      return Builder.CreateICmpSGE(val1, val2, "cmpgt");
    }
    else if(op == "="){
      return Builder.CreateICmpEQ(val1, val2, "cmpeq");
    }
    else if(op == "#"){
      return Builder.CreateICmpNE(val1, val2, "cmpne");
    }

    else{
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
    //also take care of conds 
 public:
  UnaryOpCond(Expr *e1, std::string *s) : expr1(e1), op(*s) {}
  ~UnaryOpCond() { delete expr1; }
  void printAST(std::ostream &out) const override {
    out << "UnaryOpCond("<< op<< ", " << *expr1 << ")";
  }

  Value *compile() override{

    Value *val1 = expr1->compile();

    //not other unary ops
    if(op == "not"){
      return Builder.CreateNot(val1, "not");
    }

    else{
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
    for (Expr *d : expr_list) delete d;
  }
  void add(Expr *d) { expr_list.push_back(d); }
  void add_front(Expr *d) { expr_list.push_front(d); }

  void printAST(std::ostream &out) const override {
    out << "ExprList(";
    bool first = true;
    for (const auto &d : expr_list) {
      if (!first) out << ", ";
      first = false;
      out << *d;
    }

    out << ")";
  }

  std::deque<Expr *> getExprList() const {
    return expr_list;
  }

  std::vector<Value*> compileVector() {
    std::vector<Value*> args;

    for(const auto &d : expr_list) {
        args.push_back(d->compile());
    }

    return args;
  }

  private:
    std::deque<Expr *> expr_list;
};

class FuncCall : public Stmt, public Expr {
 public:
  FuncCall(std::string *s, ExprList *e): id(*s),expr_list(e) {}
  ~FuncCall() {
        delete expr_list;
  }
  void printAST(std::ostream &out) const override {
    out << "FuncCall(" << id << ", " << *expr_list << ")";
  }
  Value* compile() override{
    std::clog << "Compiling function call: " << id << std::endl;

    // std::vector<Value*> args = expr_list->compileVector();
    std::vector<Value*> args;

    Node* functionNode = st.lookupNode(id, DECL_func);

    if(functionNode == nullptr){

      args = expr_list->compileVector();

    }else{
      //argslist
      std::deque<Expr *> exprlist = expr_list->getExprList();


      std::clog << "Getting args" << std::endl;
      std::vector<FuncArg *> funcargs = functionNode->funcargs;

      std::clog << "Got args" << std::endl;


      int i = 0;
      for(auto &a : funcargs) {
        //if pointer compileAssign

        if(a->ref){
          args.push_back(exprlist[i]->compileAssign());
        }else{
          args.push_back(exprlist[i]->compile());
        }

        i++;

      }
    }





    //
    // std::clog << "Funccall!! : " << functionNode->funcargs.size() << std::endl;


    Function *func;

    if (functionNode == nullptr) {
        if(id == "writeInteger"){
          
          //cast vector Value to 64 bit
          std::vector<Value*> args64;
          for(auto &a : args) {
              args64.push_back(Builder.CreateSExt(a, i64, "cast"));
          }
          args = args64;

          func = TheWriteInteger;
        }
        else if(id == "writeChar"){

          func = TheWriteChar;

        }
        else if(id == "writeString"){
                 
          
          std::vector<Value*> argPointer = {Builder.CreateGEP(args[0]->getType(),args[0], {c32(0), c32(0)}, "nl")};
          

          args = argPointer;

          func = TheWriteString;

        }
        else if(id == "readInteger"){
          
          func = TheReadInteger;
        }
          else{
          std::cerr << "Function " << id << " not declared" << std::endl;
          exit(1);
        }
    }else{
        func = functionNode->function;
    }


    std::clog << "Function: " << " found" << std::endl;

    
    Value *res = Builder.CreateCall(func, args);


    if(id == "readInteger"){
      return Builder.CreateTrunc(res, i32, "cast");
    }
    else{
      return res;
    }

  }


 private:
  std::string id;
  ExprList *expr_list;
  
  int offset;
};

class IntConst : public Expr {
 public:
  IntConst(int n): num(n) {}
  void printAST(std::ostream &out) const override {
    out << "IntConst(" << num << ")";
  }

  Value *compile() override {

    std::clog << "Compiling expression: " << num << std::endl;
    //return ConstantInt::get(TheContext, APInt(32, num));
    //return ConstantInt::get(i32, num);
    return ConstantInt::get(i32, num);

  }

 private:
  int num;
};






class StringConst : public Lvalue {
 public:
  StringConst(std::string *s) : var(*s) {}
  void printAST(std::ostream &out) const override {
    out << "StringConst(" << var << ")";
  }


  //TODO: make it work for the rest of the special chars  
  std::string processString(const std::string& input) {
    std::string output;
    for (size_t i = 0; i < input.size(); ++i) {
      if (input[i] == '\\') {
        if (i + 1 < input.size()) {
          switch (input[i + 1]) {
            case 'n':
              output.push_back('\n');
              ++i;  // Skip the next character
              break;
            // Add other escape sequences as needed...
            default:
              output.push_back(input[i]);
              break;
          }
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

    Constant *strConstant = ConstantDataArray::getString(TheContext, processString(var));


    GlobalVariable *gv = new GlobalVariable(*TheModule,
                                                      strConstant->getType(),
                                                      true,
                                                      GlobalValue::PrivateLinkage,
                                                      strConstant);

    return gv;

  }


 private:
  std::string var;
};




#endif