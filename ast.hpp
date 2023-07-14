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
 
    // Initialize global variables
    ArrayType *vars_type = ArrayType::get(i32, 26);
    TheVars = new GlobalVariable(
      *TheModule, vars_type, false, GlobalValue::PrivateLinkage,
      ConstantAggregateZero::get(vars_type), "vars");
    TheVars->setAlignment(MaybeAlign(16));
    ArrayType *nl_type = ArrayType::get(i8, 2);
    TheNL = new GlobalVariable(
      *TheModule, nl_type, true, GlobalValue::PrivateLinkage,
      ConstantArray::get(nl_type, {c8('\n'), c8('\0')}), "nl");
    TheNL->setAlignment(MaybeAlign(1));
 
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


  std::vector<Id *> getIds() const { return id_list; }


    std::vector<Value*> compileVector() {
    std::vector<Value*> args;

    std::clog << "id_list compile: " << std::endl;

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

 private:
  std::vector<int> array_list;
};




class TypeDef : public Stmt {
 public:
  TypeDef(DataType t, ArraySize *s): type(t), array_size(s) {}

  void printAST(std::ostream &out) const override {
    out << "TypeDef(" << type << ", " << *array_size << ")";
  }

  DataType getType() const { return type; }
//   void allocate() const {
//     rt_stack.push_back(0);
//   }
//   void deallocate() const {
//     rt_stack.pop_back();
//   }
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

  Value* compile() override {

    //issues if multiple same name etc 
    //TODO: correct type handling!, this works just for i32
    for(auto id : id_list->getIds()) {
      std::clog << "Compiling VarDec name: " << id->getName() << " Current scope: " << st.currentScope()->name << std::endl; 

      // AllocaInst* allocaInst = Builder.CreateAlloca(i32, 0, id->getName());


      GlobalVariable *gVar = new llvm::GlobalVariable(
        *TheModule,
        i32,
        false, // isConstant
        GlobalValue::PrivateLinkage,
        llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0)), // Initializer
        "gVar"
      );



      std::clog << "Alloc " << id->getName() << std::endl;


      Node *idNode = new Node();
      idNode->name = id->getName();
      idNode->type = type->getType();
      idNode->decl_type = DECL_var;
      idNode->var = gVar;


      std::clog << "Created node" << std::endl;
      st.insertNode(idNode);

      std::clog << "Inserted node" << std::endl;



    }







    return nullptr;
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
    std::clog << "specia! " << std::endl;
    
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



class If : public Stmt {
 public:
  If(Expr *c, Stmt *s1, Stmt *s2 = nullptr) : cond(c), stmt1(s1), stmt2(s2) {}
  ~If() { delete cond; delete stmt1; delete stmt2; }
  void printAST(std::ostream &out) const override {
    out << "If(" << *cond << ", " << *stmt1;
    if (stmt2 != nullptr) out << ", " << *stmt2;
    out << ")";
  }
//   void sem() override {
//     cond->check_type(TYPE_bool);
//     stmt1->sem();
//     if (stmt2 != nullptr) stmt2->sem();
//   }
//   void execute() const override {
//     if (cond->eval())
//       stmt1->execute();
//     else if (stmt2 != nullptr)
//       stmt2->execute();
//   }
 private:
  Expr *cond;
  Stmt *stmt1;
  Stmt *stmt2;
};


class Lvalue : public Expr {
    public:
    Lvalue() {}
    virtual std::string getName() const { }

//  public:
//   virtual void execute() const = 0;
};



class Assign : public Stmt {
 public: /*need to support arrays too!*/
  Assign(Lvalue *lhs, Expr *rhs): var(lhs), expr(rhs) {}
  ~Assign() { delete var; delete expr; }
  void printAST(std::ostream &out) const override {
    out << "Assign(" << *var << ", " << *expr << ")";
  }
  
  Value* compile() override {

    // char name[] = { var, '_', 'p', 't', 'r', '\0' };
    // Value *lhs = Builder.CreateGEP(TheVars, {c32(0), c32(var - 'a')}, name);
    // 
    // Builder.CreateStore(rhs, lhs);


    std::clog << "Assign compile: Current Scope: " << st.currentScope()->name  << std::endl;

    std::clog << "Variable name: "<< var->getName() << std::endl;

    Node *idNode = st.lookupNode(var->getName());

    std::clog << "Found node: " << idNode->name << std::endl;



    //Value *rhs = expr->compile();


    Value *rhs = expr->compile();


    std::clog << "Compiled rhs" << std::endl;

    // Store the constant value into the alloca.
    Builder.CreateStore(rhs, idNode->var);




    return nullptr;
  }

 private:
  Lvalue *var;
  Expr *expr;

};

class While : public Stmt {
 public:
  While(Expr *e, Stmt *s): expr(e), stmt(s) {}
  ~While() { delete expr; delete stmt; }
  void printAST(std::ostream &out) const override {
    out << "While(" << *expr << ", " << *stmt << ")";
  }
//   void sem() override {
//     expr->check_type(TYPE_int);
//     stmt->sem();
//   }
//   void execute() const override {
//     for (int times = expr->eval(), i = 0; i < times; ++i)
//       stmt->execute();
//   }
 private:
  Expr *expr;
  Stmt *stmt;
};

class Return : public Stmt {
 public:
  Return(Expr *e): expr(e) {}
  ~Return() { delete expr;  }
  void printAST(std::ostream &out) const override {
    out << "Return(" << *expr << ")";
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
//   void sem() override {
//     st.push_scope();
//     for (Decl *d : decl_list) d->sem();
//     for (Stmt *s : stmt_list) s->sem();
//     st.pop_scope();
//   }
//   void execute() const override {
//     for (Decl *d : decl_list) d->allocate();
//     for (Stmt *s : stmt_list) s->execute();
//     for (Decl *d : decl_list) d->deallocate();
//   }
 private:
  std::vector<Stmt *> stmt_list;
};


// class Block : public Stmt {
//  public:
//   Block() : decl_list(), stmt_list() {}
//   ~Block() {
//     for (Decl *d : decl_list) delete d;
//     for (Stmt *s : stmt_list) delete s;
//   }
//   void append_decl(Decl *d) { decl_list.push_back(d); }
//   void append_stmt(Stmt *s) { stmt_list.push_back(s); }
//   void merge(Block *b) {
//     stmt_list = b->stmt_list;
//     b->stmt_list.clear();
//     delete b;
//   }
//   void printAST(std::ostream &out) const override {
//     out << "Block(";
//     bool first = true;
//     for (const auto &d : decl_list) {
//       if (!first) out << ", ";
//       first = false;
//       out << *d;
//     }
//     for (const auto &s : stmt_list) {
//       if (!first) out << ", ";
//       first = false;
//       out << *s;
//     }
//     out << ")";
//   }
//   void sem() override {
//     st.push_scope();
//     for (Decl *d : decl_list) d->sem();
//     for (Stmt *s : stmt_list) s->sem();
//     st.pop_scope();
//   }
//   void execute() const override {
//     for (Decl *d : decl_list) d->allocate();
//     for (Stmt *s : stmt_list) s->execute();
//     for (Decl *d : decl_list) d->deallocate();
//   }
//  private:
//   std::vector<Decl *> decl_list;
//   std::vector<Stmt *> stmt_list;
// };

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
    } else if(op == "-") {
        return Builder.CreateSub(l, r, "subtmp");
    } else if (op == "*"){
        return Builder.CreateMul(l, r, "multmp");
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
//   void sem() override {
//     expr1->check_type(TYPE_int);
//     switch(op) {
//       case '+': case '-': case '*': case '/': case '%':
// 	type = TYPE_int;
//         break;
//       case '<': case '=': case '>':
// 	type = TYPE_bool;
//         break;
//     }
//   }
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

    Value * idList = id_list->compile();
    std::clog << "FparDef compiled! " << std::endl;
    return idList;
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

  Value* compile() override{
      std::clog << "FunctionHeader compile: " << std::endl;
      fpardef_list->compileVector();

      return nullptr;
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


    Type *voidType = Type::getVoidTy(TheContext);

    FunctionType *FT = FunctionType::get(voidType,{},false);



    Function *F = Function::Create(FT, Function::ExternalLinkage, header->Tid,TheModule.get());


    //TODO: only if first function

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



    st.insertNode(functionNode,DECL_func);


    st.createScope(header->Tid); //add the name in scope 
    
    std::clog << "Current Scope: " <<  st.currentScope()->name << std::endl;

    //TODO: figure out how we make arguments in llvm!
    // header->compile();

    localdef_list->compile();

    stmt_list->compile();

    //TODO: we should only do this if no return in stmt_list!
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

    std::vector<Value*> args = expr_list->compileVector();


    Node* functionNode = st.lookupNode(id, DECL_func);

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

                    //cast vector Value to 64 bit
          std::vector<Value*> args64;
          for(auto &a : args) {
              args64.push_back(Builder.CreateSExt(a, i64, "cast"));
          }
          //args = args64;

          func = TheWriteChar;
        

        }
          else{
          std::cerr << "Function " << id << " not declared" << std::endl;
          exit(1);
        }
    }else{
        func = functionNode->function;
    }

    //Value* callResult = Builder.CreateCall(TheWriteInteger, args64, "calltmp");
    //false to last parm?
    Builder.CreateCall(func, args);

  
    return nullptr;
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
//   void sem() override {
//     type = TYPE_int;
//   }
//   int eval() const override {
//     return num;
//   }
 private:
  int num;
};





class IdLval : public Lvalue {
 public:
  IdLval(std::string *c) : var(*c) {}
  void printAST(std::ostream &out) const override {
    out << "IdLval(" << var << ")";
  }

  std::string getName() const {
    return var;
  }

  Value* compile() override{
    
    Node* idNode = st.lookupNode(var);
    if(idNode == nullptr){
      std::cerr << "Error: variable " << var << " not declared" << std::endl;
      exit(1);
    }

    GlobalVariable* gvar = idNode->var;



    return Builder.CreateLoad(i32,gvar, var + "_load");



  }

 private:
  std::string var;
  int offset;
};



class StringConst : public Lvalue {
 public:
  StringConst(std::string *s) : var(*s) {}
  void printAST(std::ostream &out) const override {
    out << "StringConst(" << var << ")";
  }

 private:
  std::string var;
};


class ArrayElem : public Lvalue {
 public: /*need to support arrays too!*/
  ArrayElem(AST *lhs, Expr *rhs): var(lhs), expr(rhs) {}
  ~ArrayElem() { delete var; delete expr; }
  void printAST(std::ostream &out) const override {
    out << "ArrayElem(" << *var << ", " << *expr << ")";
  }
//   void sem() override {
//     STEntry *e = st.lookup(var);
//     expr->check_type(e->type);
//     offset = e->offset;
//   }
//   void execute() const override {
//     rt_stack[offset] = expr->eval();
//   }
 private:
  AST *var;
  Expr *expr;
  int offset;
};



#endif