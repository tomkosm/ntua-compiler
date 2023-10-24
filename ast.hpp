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




//extern std::vector<int> rt_stack;

void yyerror(const char *s,int line);
extern int yylineno;

enum DataType 
{   TYPE_UNDEFINED_ERROR,
    TYPE_int,
    TYPE_char, 
    TYPE_nothing,
    TYPE_charList, //hidden
    TYPE_bool, //hidden
};




//needs to be able to see DataType
#include "symbol.cpp"




class AST {
 public:

  AST() : line(yylineno){}
  virtual ~AST() = default;
  virtual void printAST(std::ostream &out) const = 0;
  virtual Value* compile() const {std::clog << "Called ast const compile" <<std::endl; exit(2); return nullptr; }
  virtual Value* compile()  { std::clog << "Called ast compile" <<std::endl; exit(2); return nullptr; }
  virtual Node *compileArray() {std::clog << "Called ast compilearray" <<std::endl; exit(2);}
  virtual void sem(){ logError("Called default sem");}

  std::string getName(){}

  Type* getLlvmType(DataType dtype,bool isArray=false){

    Type *itype;

    if(dtype == TYPE_int)
        itype =  Type::getInt64Ty(TheContext);
    else if(dtype == TYPE_char)
        itype = Type::getInt8Ty(TheContext);
    else if(dtype == TYPE_nothing)
        itype = Type::getVoidTy(TheContext);
    else{
        std::clog << "Error, couldnt find type!" << std::endl;
        exit(2);
    }
    if(isArray){
      //TODO: handle multi dim?
        std::clog << "Hereeee!!!!!!!" << std::endl;
        ArrayType* ArrayTy = ArrayType::get(itype, 0);
        itype = ArrayTy;
    }

    return itype;
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



    FunctionType *strlen_type =
      FunctionType::get(i64, {PointerType::get(i8, 0)}, false);

    Thestrlen =
      Function::Create(strlen_type, Function::ExternalLinkage,
                       "strlen", TheModule.get());


    FunctionType *strcpy_type =
      FunctionType::get(Type::getVoidTy(TheContext), {PointerType::get(i8, 0),PointerType::get(i8, 0)}, false);

    Thestrcpy =
      Function::Create(strcpy_type, Function::ExternalLinkage,
                       "strcpy", TheModule.get());

    FunctionType *ascii_type =
          FunctionType::get(i64, {i8}, false);


    Theascii =
      Function::Create(ascii_type, Function::ExternalLinkage,
                       "ascii", TheModule.get());


    FunctionType *chr_type =
          FunctionType::get(i8, {i64}, false);


    Thechr =
          Function::Create(chr_type, Function::ExternalLinkage,
                           "chr", TheModule.get());


      //do sem analysis
    sem();

    std::clog << "Sem analysis is now complete!" << std::endl;



    // Define and start the main function.
    FunctionType *main_type = FunctionType::get(i64, {}, false);
    Function *main =
      Function::Create(main_type, Function::ExternalLinkage,
                       "main", TheModule.get());
    BasicBlock *BB = BasicBlock::Create(TheContext, "entry", main);
    Builder.SetInsertPoint(BB);

    // Emit the program code.
    compile();

    std::clog << "Compiled!" << std::endl;


    // We should remove any empty bb's, unsafe to remove while iterating
    std::vector<BasicBlock*> EmptyBBs;

    for (Function& Func : *TheModule) {
        for (BasicBlock& BB : Func) {
            if (BB.empty()) {
                EmptyBBs.push_back(&BB);
            }
        }
    }
//TODO: we probably need this!, figure out if better way? we cant just delete cause of dependencies, see
    for (BasicBlock* BB : EmptyBBs) {
//        BB->eraseFromParent();
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
                defaultReturnValue = ConstantInt::get(returnType, 0);  // Default to 0 for integer types
            } else {
                // Handle other types as needed
                // For now, we'll just leave a null pointer for pointer types
                defaultReturnValue = Constant::getNullValue(returnType);
            }

            Builder.CreateRet(defaultReturnValue);

        }
    }
    std::clog << "Removed empty BBs!" << std::endl;


    // Print the names of all basic blocks
    // for (BasicBlock* BB : BasicBlocks) {
    //     std::cout << "Basic Block Name: " << BB->getName().str() << std::endl;
    // }




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

    // Optimize!
    TheFPM->run(*main);

    // Print out the IR.
    TheModule->print(outs(), nullptr);
  }

 public:
  static SymbolTable st; //maybe some special class to do this?



 protected:
  int line;

  static LLVMContext TheContext;
  static IRBuilder<> Builder;
  static std::unique_ptr<Module> TheModule;
  static std::unique_ptr<legacy::FunctionPassManager> TheFPM;

  static Function *TheWriteInteger;

  static Function *TheWriteChar;


  static Function *TheWriteString;


  static Function *TheReadInteger;

  static Function *Thestrlen;

  static Function *Thestrcpy;

    static Function *Theascii;

    static Function *Thechr;


    static Type *i8;
  static Type *i32;
  static Type *i64;

  static ConstantInt* c8(char c) {
    return ConstantInt::get(TheContext, APInt(8, c, true));
  }
  static ConstantInt* c32(int n) {
    return ConstantInt::get(TheContext, APInt(32, n, true));
  }
  static ConstantInt* c64(int n) {
    return ConstantInt::get(TheContext, APInt(64, n, true));
  }
  void logError(std::string msg){
      yyerror(msg.c_str(),line);
  }

  //not sure if we can put that somewhere else?
    std::map<std::string, int> LIBRARY_FUNCTIONS = {
            {"writeInteger", 1},
            {"writeString", 1},
            {"strlen",1},
            {"readInteger",1},
            {"writeChar",1},
            {"ascii",1},
            {"strcpy",1},
            {"chr",1}
    };


};


inline std::ostream &operator<<(std::ostream &out, DataType t) {
  switch (t) {
      case TYPE_UNDEFINED_ERROR: out << "TYPE_UNDEFINED_ERROR" ; break;
    case TYPE_int:  out << "TYPE_int";  break;
    case TYPE_char: out << "TYPE_char"; break;
    case TYPE_nothing: out << "TYPE_nothing"; break;
    case TYPE_bool : out << "TYPE_bool"; break;

  }
  return out;
}

inline std::ostream &operator<<(std::ostream &out, const AST &ast) {
  ast.printAST(out);
  return out;
}


class Expr : virtual public AST {
 public:

    DataType sem_type;
    virtual Value* compileAssign() { std::clog << "Called Lvalue compileAssign!" << std::endl; return nullptr; }
    virtual std::vector<int> getArraySize() { std::clog << "Called default Lvalue getArraySize!" << std::endl; exit(2);}
    void check_type(DataType t){
        sem();
        if(sem_type == TYPE_UNDEFINED_ERROR)
            logError("Should never return TYPE_UNDEFINED_ERROR");

        std::clog << "Check_TYPE " << sem_type << " " << t << std::endl;
        //      printAST(std::clog);
        //      std::clog << std::endl;
        if(sem_type != t) logError("Type mismatch");
    }


    void printAST(std::ostream &out) const override {
    out << "Expr(empty)";
    }

    Value* compile() override {
      std::clog << "Called base EXPR compile!" << std::endl;
      return nullptr;
        // Implement compile for Expr if necessary, or keep it pure virtual
    }

};


//only used for definitions! VARDEC?
class Id : public Expr {
 public:
  Id(std::string *c): var(*c) {}
  void printAST(std::ostream &out) const override {
    out << "Id(" << var << ")";
  }
  //this class just gives name to VarDec
//  void sem() override{
      //This is implemented higher up, this is never called!
      //TODO: to implement this we need to add in st!

//      Node *node = new Node;
//
//
//      st.insertNode();
//  }
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
  void add_front(Id *d) { id_list.push_front(d); }
  void add(Id *d) { id_list.push_back(d); }

  void sem() override{
      for (Id *d : id_list) d->sem();
  }
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
  std::vector<Id *> getIds() const { 

    return std::vector<Id*>(id_list.begin(), id_list.end()); 
  }


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
    std::deque<Id *> id_list;

};





class Stmt : virtual public AST {
 public:
  void name(){std::clog << "Stmt name" << std::endl;}


  void printAST(std::ostream &out) const override {
    out << "Stmt(empty)";
  }

  virtual Value* compile() override  { std::clog << "Called STMT compile" <<std::endl; exit(2); return nullptr; }

  virtual bool isReturn(){return false;}


//   virtual void execute() const = 0;
};




class ArraySize : public Stmt {
 public:
  ArraySize() : array_list() {}


  void add(int s) { array_list.push_back(s); }

  void sem() override {
    //TODO: figure out what testing we need
  }

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

  void sem() override{
      //TODO:fill
  }

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

  void sem() override {
      //create the st entry

      for (auto id: id_list->getIds()) {


          if (st.lookupNode(id->getName(), DECL_var) != nullptr) {
              logError("Variable is already declared");
          }

          //check that there isnt another node with same name,type
          Node *idNode = new Node();
          idNode->name = id->getName();
          idNode->type = type->getType();
          idNode->decl_type = DECL_var;
          idNode->array_size = type->getSizes();

    //      idNode->var = gVar;
    //
//            idNode->llvm_type = getLlvmType(idNode->type,idNode->array_size.size() != 0);
          idNode->assigned = false;
          idNode->isPointer = true;

          st.insertNode(idNode);
      }
  }

  std::vector<Id *> getIds() const { return id_list->getIds(); }



  Value* compile() override {

    int initializerSize;

    Type * itype;
    itype = getLlvmType(type->getType(),false);
    std::vector<int> arraysizes = type->getSizes();

    Constant* Initializer;
    std::clog << "Array size: " << arraysizes.size() << std::endl;
    if(arraysizes.size() == 0){
      //Initializer = 0;
//      Initializer = ConstantInt::get(TheContext, llvm::APInt(initializerSize, 0));
        Initializer = ConstantInt::get(getLlvmType(type->getType(),false), 0);

    }else if(arraysizes.size() >= 1){
        //do this in getLLvmType TODO:

      //we reverse it so we get last first

      std::reverse(arraysizes.begin(), arraysizes.end());



      std::clog << "Array size: " << arraysizes[0] << " " << arraysizes[1] << std::endl;
      //wrong need to handle multi dim
      
      //Type *ltype = itype;
      Initializer = c64(0);

      ArrayType* ArrayTy;

      std::vector<llvm::Constant*> arrayElems;
      for(auto size : arraysizes) {

        std::clog << "Itype size: " << size << std::endl;
        ArrayTy = ArrayType::get(itype, size);
        std::clog << "hereee" << std::endl;

        itype = ArrayTy;

        arrayElems = {static_cast<unsigned long>(size),Initializer};

        Initializer = ConstantArray::get(ArrayTy,arrayElems);


      }

      
      // std::vector<Constant*> constArray(arraysizes[0], ConstantInt::get(TheContext, APInt(32, 0)));
      // Initializer = ConstantArray::get(ArrayTy, constArray);

      // itype = ArrayTy;
    }

    //start here

    //TODO: issues if multiple same name etc 
    for(auto id : id_list->getIds()) {
      std::clog << "Compiling VarDec name: " << id->getName() << " Current scope: " << st.currentScope()->name << std::endl; 

      // AllocaInst* allocaInst = Builder.CreateAlloca(i32, 0, id->getName());

      Node* node = st.lookupNode(id->getName(),DECL_var);

      GlobalVariable *gVar = new llvm::GlobalVariable(
        *TheModule,
        itype,
        false, // isConstant
        GlobalValue::ExternalLinkage,
        Initializer, // Initializer
        id->getName()+"_var"
      );



      std::clog << "Alloc " << id->getName() << std::endl;


      node->var = gVar;
      node->llvm_type = itype;
//      Node *idNode = new Node();
//      idNode->name = id->getName();
//      idNode->type = type->getType();
//      idNode->decl_type = DECL_var;
//      idNode->var = gVar;
//      idNode->llvm_type = itype;
//      idNode->assigned = false;
//      idNode->isPointer = true;
//
//      std::clog << "Created node" << std::endl;
//      st.insertNode(idNode);
//
//      std::clog << "Inserted node" << std::endl;

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
  void sem() override{
    sem_type = TYPE_char;
  }

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
  CharConstSpecial(std::string *s): CharConst('a'), var(*s)  {}
  void printAST(std::ostream &out) const override {
    out << "CharConstSpecial(" << escSeqToChar(var) << ")";
  }
  void sem() override{
    sem_type = TYPE_char;
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

    return c8(escSeqToChar(var));
    //    return ConstantInt::get(i8, escSeqToChar(var));
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
    virtual std::vector<Value *> getIndexes() {std::clog << "Called Lvalue getIndexes!" << std::endl; exit(2);}

//    DataType sem_type;

//  public:
//   virtual void execute() const = 0;
};

// IdLval for array is ArrayElem
class IdLval : public Lvalue {
 public:
  IdLval(std::string *c) : var(*c) {}
  void printAST(std::ostream &out) const override {
    out << "IdLval(" << var << ")";
  }

  void sem() override{
      //TODO: check offset too?

      // we dont want this to run in vardec
      Node* idNode = st.lookupNode(var);
      if(idNode == nullptr)
          logError("Cant find id in sem");
      //TODO: we should do the below test only on access, check where getName is called?
//     else if(!idNode->assigned)
//         //TODO: make this work for ref too?
//         logError("Error, tried to access a variable that isnt assigned");
     else
        sem_type = idNode->type;

     std::clog << sem_type << std::endl;
     node = idNode;
        //TODO: does this work for pointer? do we need new types?
          //

//          std::clog << st.currentScope()->name << std::endl;
//          std::cerr << "Error: variable " << var << " not declared" << std::endl;
//          exit(1);


  }

  std::string getName() override {
    return var;
  }

  void updatelookup(){
      std::clog << "Update lookup" << std::endl;
      node->assigned = true;
  }

  //double check if we can do this better
  Value* compileAssign() override{
    
    Node* idNode = st.lookupNode(var);
    if(idNode == nullptr){
      //
      std::clog << st.currentScope()->name << std::endl;
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
    std::clog << "Compiling IdLval" << std::endl;


    Value *gvar = compileAssign();


    //TODO: figure out if we need this and also make it work for refs too
//    if(!node->assigned){
//      std::clog << "Error, tried to access a variable that isnt assigned" << std::endl;
//      //exit(2);
//    }

    if(!node->isPointer){
      return gvar;
    }else{
      return Builder.CreateLoad(node->llvm_type,gvar, var + "_load");
    }


  }

  std::vector<Value *> getIndexes() override{
    std::vector<Value *> v = {c64(0)};
    return v;
  }

  //get array maybe is better name?
  Node *compileArray() override{

    Node* idNode = st.lookupNode(var);
    if(idNode == nullptr){
      std::cerr << "Error: variable " << var << " not declared" << std::endl;
      exit(1);
    }

    return idNode;
  }

  std::vector<int> getArraySize() override{
    Node* idNode = st.lookupNode(var);
    if(idNode == nullptr){
      std::cerr << "Error: variable " << var << " not declared" << std::endl;
      exit(1);
    }

    return idNode->array_size;

  }

 private:

  Node *node;

  std::string var;
};



class ArrayElem : public Lvalue {
 public:
  ArrayElem(Lvalue *lhs, Expr *rhs): var(lhs), expr(rhs) {}
  ~ArrayElem() { delete var; delete expr; }
  void printAST(std::ostream &out) const override {
    out << "ArrayElem(" << *var << ", " << *expr << ")";
  }

  void sem() override{
      //TODO: implement???
      var->sem();

      std::clog << "Array elem "<< std::endl;

      //TODO?
      sem_type = var->sem_type;

      //TODO: check that array index is valid!
      //std::vector<Value*> arrayIndex = getIndexes();



  }

  std::string getName() override {
    std::clog << "Called ArrayElem getName!" << std::endl;
    return var->getName();
  }

  // std::vector<int> getArraySize(){
  //   return 
  // }

  Value *compileAssign(){
    std::clog << "Compiling compileAssign array element: " << *var << std::endl;

    Node *array = var->compileArray();



    std::vector<Value*> arrayIndex = getIndexes();






    node = array;

    std::clog << "before gep!!!!!!!!!!!!!!!! " << var->getName() << std::endl;
    //Value *v = Builder.CreateInBoundsGEP({i8},array->var,arrayIndex,var->getName()+"_arrayElem_arg");
    std::clog << "after gep" << std::endl;
    // std::clog << *array->var->getType() << std::endl;
    //TODO: would first work for both?
    // if(array->isPointer){


        //ArrayType* ArrayTy = ArrayType::get(i8, 0);


        std::clog <<"here" << std::endl;
        std::clog << line << std::endl;
       return Builder.CreateInBoundsGEP(array->llvm_type,array->var,arrayIndex,var->getName()+"_arrayElem_arg");
    // }else{
    //   Value *elementPtr = Builder.CreateGEP(array->llvm_type,array->var, arrayIndex, var->getName()+"_arrayElem");     
    
    //   return elementPtr;
    // }

  }

  Node *compileArray() override{
  
    return var->compileArray();
  }

  std::vector<Value *> getIndexes() override{
    std::vector<Value *> indexes = var->getIndexes();
    indexes.push_back(expr->compile());
    return indexes;
  }



    Value *compile() override{

    std::clog << "Compiling array element: " << *var << std::endl;
    
    Value *elementPtr = compileAssign();
        std::clog << "HEREER" << std::endl;


    Type* elementType = getLlvmType(node->type); //?
    

    Value* elementValue = Builder.CreateLoad(elementType,elementPtr, "elementValue");

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
  Assign(Lvalue *lhs, Expr *rhs): var(lhs), expr(rhs) {}
  ~Assign() { delete var; delete expr; }
  void printAST(std::ostream &out) const override {
    out << "Assign(" << *var << ", " << *expr << ")";
  }

  void sem() override{
      var->sem();
//      std::clog << var->sem_type << std::endl;
      expr->sem();
      std::clog << var->sem_type << " and " << expr->sem_type << std::endl;

      if(var->sem_type == TYPE_UNDEFINED_ERROR or expr->sem_type == TYPE_UNDEFINED_ERROR)
          logError("Should never return TYPE_UNDEFINED_ERROR");

      if(var->sem_type != expr->sem_type) logError("Type mismatch...");
      std::clog << "Assign check sem complete" << std::endl;

      var->updatelookup(); // in order to detect later if it has been assigned before use, a better name would be updateAssigned

      std::clog << "Assign sem complete" << std::endl;

  }
  
  Value* compile() override {


    std::clog << "Assign compile: Current Scope: " << st.currentScope()->name  << std::endl;

    std::clog << *var << std::endl;

    std::clog << "Variable name: "<< var->getName() << std::endl;


    Value *rhs = expr->compile();



    Value *lhs = var->compileAssign();


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

  void sem(){
    //TODO: check that expr type matches function!
  }


  bool isReturn() override{
    return true;
  }
  Value *compile() override{
    std::clog << "Compiling return! " << std::endl;
    Value * compiledExpr = expr->compile();

    if (compiledExpr == nullptr){
      return Builder.CreateRetVoid();
    }else{
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
    for (Stmt *s : stmt_list) delete s;
  }
  void add(Stmt *s) { stmt_list.push_back(s); }

  void name(){std::clog << "StmtList name" << std::endl;}
  //TODO: We need to check that the order of statements is correct! SUPER IMPORTANT!

  void sem() override{
      for (Stmt *s : stmt_list) s->sem();
  }

  bool isReturn() override{
    for (Stmt *s : stmt_list){
      if(s->isReturn()){
        return true;
      }
    }
    return false;

  }

  Value* compile() override {
    std::clog << "StmtList compile: " << std::endl;

    for (Stmt *s : stmt_list) {
      if(s->isReturn()){
        std::clog << "Found return in stmtlist" << std::endl;
        s->compile();
        return nullptr;
      }else{
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
      if (!first) out << ", ";
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

    virtual Value* compile(){ logError("Called compile from Cond");}
    virtual Value* compile(BasicBlock *thenBB,BasicBlock *afterBB) {
        return compile(); //if not implemented => if not binopcond (used for short circuit)
    }
};

class If : public Stmt {
 public:
  If(Cond *c, Stmt *s1, Stmt *s2 = nullptr) : cond(c), stmt1(s1), stmt2(s2) {}
  ~If() { delete cond; delete stmt1; delete stmt2; }

  bool isReturn() override{

    if(stmt2 == nullptr){
      return false;
    }
    else{
      return stmt1->isReturn() && stmt2->isReturn();
    }
  }


  void printAST(std::ostream &out) const override {
    out << "If(" << *cond << ", " << *stmt1;
    if (stmt2 != nullptr) out << ", " << *stmt2;
    out << ")";
  }

  void sem() override{
      std::clog << "Checking if type: " << std::endl;
      cond->check_type(TYPE_bool);

      stmt1->sem();
      if(stmt2 != nullptr) stmt2->sem();
  }

  Value* compile()  override {


    std::clog << "Called If compile!" << std::endl;

    //TODO: figure out if there is some better way other than doing this...
//    StmtList* stmtList1 = dynamic_cast<StmtList*>(stmt1);
//    StmtList* stmtList2 = dynamic_cast<StmtList*>(stmt2);



    std::clog << "Called cond compile" << std::endl;
    // Value *cond = Builder.CreateICmpNE(v, c32(0), "if_cond");
    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock *ThenBB =
      BasicBlock::Create(TheContext, "then", TheFunction);


    BasicBlock *AfterBB =
      BasicBlock::Create(TheContext, "endif", TheFunction);

      std::clog << "Before cond" << std::endl;

    std::clog << "After cond" << std::endl;

      //if there is no else statement
    if(stmt2 == nullptr){
      Value *condition = cond->compile(ThenBB,AfterBB);

      Builder.CreateCondBr(condition, ThenBB, AfterBB);
      Builder.SetInsertPoint(ThenBB);

      std::clog << "About to compile stmt" << std::endl;


      stmt1->compile();

       //we do this cause llvm doesnt like it if we have a command (br) after return
      if(!stmt1->isReturn()){
        Builder.CreateBr(AfterBB);
        Builder.SetInsertPoint(AfterBB);

      }

    }else{
      BasicBlock *ElseBB =
        BasicBlock::Create(TheContext, "else", TheFunction);

      Value *condition = cond->compile(ThenBB,ElseBB);

      Builder.CreateCondBr(condition, ThenBB, ElseBB);
      Builder.SetInsertPoint(ThenBB);
      
      std::clog << "About to compile stmt" << std::endl;

      stmt1->compile();
      if(!stmt1->isReturn()){
        Builder.CreateBr(AfterBB);
      }
      Builder.SetInsertPoint(ElseBB);

      std::clog << "About to compile else stmts" << std::endl;
      stmt2->compile();

      std::clog << "is return: "<< stmt2->isReturn() << std::endl;
      if(!stmt2->isReturn()){
        Builder.CreateBr(AfterBB);
        //Builder.SetInsertPoint(AfterBB);

      }

    }

      
      Builder.SetInsertPoint(AfterBB);


    // if(!stmt1->isReturn())
    //   Builder.CreateBr(AfterBB);

    
    // 
    //   Builder.CreateBr(AfterBB);
    // Builder.SetInsertPoint(AfterBB);

    return nullptr;


  
  }

 private:
  Cond *cond;
  Stmt *stmt1;
  Stmt *stmt2;
};


class While : public Stmt {
 public:
  While(Cond *e, Stmt *s): cond(e), stmt(s) {}
  ~While() { delete cond; delete stmt; }
  void printAST(std::ostream &out) const override {
    out << "While(" << *cond << ", " << *stmt << ")";
  }

  void sem() override{
    cond->check_type(TYPE_bool);

    stmt->sem();

  }

  bool isReturn() override{

    return stmt->isReturn();
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

    Value *condition = cond->compile(DoBB,AfterBB);

    Builder.CreateCondBr(condition, DoBB, AfterBB);


    //make the do part

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
  ~BinOp() { delete expr1; delete expr2; }
  void printAST(std::ostream &out) const override {
    out << "BinOp(" << *expr1 << ", " << op << ", " << *expr2 << ")";
  }

  void sem() override{
      //we need both to be int
      expr1->check_type(TYPE_int);
      expr2->check_type(TYPE_int);

      sem_type = TYPE_int;
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

  void sem() override{
      expr1->check_type(TYPE_int);
      sem_type = TYPE_int;
  }

  Value *compile() override{

    if(var == "+"){
      return expr1->compile();
    }
    else if(var == "-"){
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
    return array_size->getSizes().size() != 0 || arrySizeEmpty;
  }


  std::vector<int> getArraySizes(){
    return array_size->getSizes();
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

//    std::reverse(ids.begin(), ids.end());

    std::vector<FuncArg *> args;

    for(Id *id : ids){
      FuncArg *arg = new FuncArg();
      arg->type = fpar_type->getType();
      arg->isArray = fpar_type->isArray();
      // arg->array_size = fpar_type->getArraySizes();
      arg->name = id->getName();

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
  void add_front(FparDef *d) { fpardef_list.push_front(d); }

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
    std::deque<FparDef *> fpardef_list;


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

  void sem() override{


      std::vector<FuncArg *> args = getArgs();

      argTypes = {};

      for(FuncArg *arg : args){
          //TODO: handle ref, pass as pointer


          Node *node = new Node();
          node->name = arg->name;
          node->decl_type = DECL_var;
          node->type = arg->type;
          std::clog << "New node! :" << arg->name << " is Array: " << arg->isArray   << std::endl;
          node->llvm_type = getLlvmType(arg->type,arg->isArray);
          node->assigned = true;//we do this since its arguments and the args are assigned
          node->isPointer = arg->ref;



          argnodes.push_back(node);
          std::clog << "Arg node: " << node->name << "is ref: "<<arg->ref <<  "is array: " <<arg->isArray << std::endl;
          if(arg->ref){
              argTypes.push_back(PointerType::get(node->llvm_type, 0));
          }else{
              argTypes.push_back(node->llvm_type);
          }


      }


      Node *functionNode = new Node();
      functionNode->name = Tid;
      functionNode->decl_type = DECL_func;
      functionNode->isCompiled = false;
      std::clog << "Function Node set" << std::endl;
      //functionNode->function = F;
      //functionNode->funcargs = getArgs(); we do this on compile
      //functionNode->block = BB;

      fnode = functionNode;

      st.insertNode(functionNode,DECL_func);



      st.createScope(Tid); //add the name in scope



      //adding arguments in the st
      for(Node *node : argnodes){
          st.insertNode(node,DECL_var);
      }

      st.exitScope();


  }

  Value* compile() override{
      std::clog << "FunctionHeader compile: " << std::endl;
//      //fpardef_list->compileVector();
//
//
//    // Make the function type:  double(double,double) etc.
//
//    DataType dtype = getReturnType();
//
//
//    Type *type = getLlvmType(dtype);
//
//
//    //args
//
//    std::vector<Node*> argnodes;
//
//    std::vector<FuncArg *> args = getArgs();
//    std::vector<Type*> argTypes = {};
//    for(FuncArg *arg : args){
//      //TODO: handle ref, pass as pointer
//
//
//      Node *node = new Node();
//      node->name = arg->name;
//      node->decl_type = DECL_var;
//      node->type = arg->type;
//      std::clog << "New node! :" << arg->name << " is Array: " << arg->isArray   << std::endl;
//      node->llvm_type = getLlvmType(arg->type,arg->isArray);
//      node->assigned = true;//we do this since its arguments and the args are assigned
//      node->isPointer = arg->ref;
//
//
//
//      argnodes.push_back(node);
//      std::clog << "Arg node: " << node->name << "is ref: "<<arg->ref <<  "is array: " <<arg->isArray << std::endl;
//      if(arg->ref){
//        argTypes.push_back(PointerType::get(node->llvm_type, 0));
//      }else{
//        argTypes.push_back(node->llvm_type);
//      }
//
//
//    }

//    Node *funNode = st.lookupNode(Tid,DECL_func);

    DataType dtype = getReturnType();


    Type *type = getLlvmType(dtype);

    FunctionType *FT = FunctionType::get(type,argTypes,false);

      std::clog << "here" << std::endl;


    Function *F = Function::Create(FT, Function::ExternalLinkage, Tid,TheModule.get());
      std::clog << "here2" << std::endl;


    func = F;


    //BasicBlock *previousBB = Builder.GetInsertBlock();

    BasicBlock *previousBB = Builder.GetInsertBlock();

    BasicBlock *BB = BasicBlock::Create(TheContext, Tid, F);

    fnode->function = F;
    fnode->funcargs = getArgs();
    fnode->block = BB;
    fnode->isCompiled = true;

    //    fnode = functionNode;

    Builder.SetInsertPoint(BB);


    //get arguments
    //TODO: double check the following!
    unsigned Idx = 0;
    for (auto &Arg : F->args()) {
      Arg.setName(argnodes[Idx]->name+"_funcarg");

      if(argnodes[Idx]->isPointer){
        argnodes[Idx]->var = &Arg;
      }
      else{
        //null ptr needs to be arraySize if exists!
        llvm::AllocaInst* Alloca = Builder.CreateAlloca(argnodes[Idx]->llvm_type, nullptr, argnodes[Idx]->name+"_funcarg");
        //AllocaInst *Alloca = CreateEntryBlockAlloca(F, argnodes[Idx]->name, argnodes[Idx]->llvm_type);
        Builder.CreateStore(&Arg, Alloca);
        argnodes[Idx]->var = Alloca;
        argnodes[Idx]->isPointer = true;
      }

      Idx++;
    }

    Builder.SetInsertPoint(previousBB);


    std::clog << "Gereee" << std::endl;







    //We do all the below in sem
//    st.insertNode(functionNode,DECL_func);
//
//
//    st.createScope(Tid); //add the name in scope



//    //adding arguments in the st
//    for(Node *node : argnodes){
//      st.insertNode(node,DECL_var);
//    }
//
//    st.exitScope();

    // std::clog << "Current Scope: " <<  st.currentScope()->name << std::endl;





      return nullptr;
  }

  Function *getFunction(){
    return func;
  }

  std::vector<FuncArg *> getArgs(){
    return fpardef_list->getArgs();
  }

  std::string getTid(){
    return Tid;
  }

  public:
    std::string Tid;
    FparDefList *fpardef_list;
    DataType type;

    std::vector<Type*> argTypes;
    std::vector<Node*> argnodes;

    Node *fnode;

    Function *func;

};


class LocalDefList : public Stmt {
 public:
  LocalDefList() : localdef_list() {}
  ~LocalDefList() {
    for (Stmt *d : localdef_list) delete d;
  }
  void add(Stmt *d) { localdef_list.push_back(d); }

  void sem() override{
      for (Stmt *d : localdef_list) d->sem();
  }
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
  bool firstFunction;//we set this to true on lexer for the main function

  FunctionDef(FunctionHeader *h,LocalDefList *l,StmtList *s) : header(h),localdef_list(l),stmt_list(s),firstFunction(false) {}
  ~FunctionDef() {
    delete localdef_list; delete stmt_list; delete header;
  }

  void sem() override{


      std::string funcName = header->getTid(); //funcname is Tid ?

      //its possible that only header has been declared before
      Node *node = st.lookupNode(funcName,DECL_func);//TODO: maybe do polymorphism?
      if(node == nullptr){
          header->sem();
//          F = header->getFunction();
//          BB = header->fnode->block;
      }
//      else{
////          std::clog << "Function: " << funcName << " already declared!" << std::endl;
////          //if declared
////          F = node->function;
////          BB = node->block;
//      }

      st.enterScope(funcName);

      localdef_list->sem();
      stmt_list->sem();

      st.exitScope(); //remove the name from scope

  }

  Value* compile() override {

    BasicBlock *previousBB = Builder.GetInsertBlock();
    BasicBlock *BB;
    DataType dtype = header->getReturnType();

    std::string funcName = header->getTid(); //funcname is Tid ?


    Function *F;

    std::clog << "FunctionDef funcname: " << funcName << std::endl;
    Node *node = st.lookupNode(funcName,DECL_func);

    std::clog << "Function def lookup complete" << std::endl;

    if(node == nullptr) {
        logError("This shouldnt occur!");
    }
    else if(!node->isCompiled){
      //if not declared
      std::clog << "About to compile header" << std::endl;
      header->compile();
      std::clog << "Function: " << funcName << "header compiled" << std::endl;
      F = header->getFunction();
      BB = header->fnode->block;
    }
    else{
      std::clog << "Function: " << funcName << " already declared!" << std::endl;
      //if declared
      F = node->function;
      BB = node->block;
    }

    

    std::clog << "First Function: " << firstFunction << std::endl;
    if(firstFunction){
      Builder.CreateCall(F);
      firstFunction = false;
    }

    std::clog << "Entering scope: " << funcName << std::endl;
    st.enterScope(funcName);

    //BasicBlock *BB = BasicBlock::Create(TheContext, funcName, F);
    Builder.SetInsertPoint(BB);
    

    localdef_list->compile();

    stmt_list->compile();

    if(dtype == TYPE_nothing && !stmt_list->isReturn())
      Builder.CreateRetVoid();




    Builder.SetInsertPoint(previousBB);


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










class BinOpCond : public Cond {
    //also take care of conds 
 public:
  BinOpCond(Expr *e1, std::string *s, Expr *e2) : expr1(e1), op(*s), expr2(e2) {}//binOpCond belong to parent
  ~BinOpCond() { delete expr1; delete expr2; }
  void printAST(std::ostream &out) const override {
    out << "BinOpCond("<< op<< ", " << *expr1 << ", " << *expr2 << ")";
  }

  void sem() override{
      //TODO: check maybe if its cond? or change type?
      std::clog << "Checking BinOpCond" << std::endl;
      expr1->check_type(TYPE_bool);
      expr2->check_type(TYPE_bool);

      sem_type = TYPE_bool;
  }

  //after or else!
  //then aka do
  Value *compile(BasicBlock *thenBB, BasicBlock *afterBB) override{


    std::clog << "Compiling binopcond "<< std::endl;

    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock *BB = Builder.GetInsertBlock();

    Value *leftValue = expr1->compile();

    if(op == "and"){
        BasicBlock *evalRightBB = BasicBlock::Create(TheContext, "evalRight", TheFunction);

        //if true and false -> evaluate left an
        Builder.CreateCondBr(leftValue, evalRightBB, afterBB);


        Builder.SetInsertPoint(evalRightBB);

        return expr2->compile();  // evaluate right expression


    }
    else if(op == "or"){

        BasicBlock *evalRightBB = BasicBlock::Create(TheContext, "evalRight", TheFunction);

        Builder.CreateCondBr(leftValue,thenBB,evalRightBB);


        Builder.SetInsertPoint(evalRightBB);

        return expr2->compile();  // evaluate right expression


        //      return Builder.CreateOr(expr1->compile(), expr2->compile(), "ortmp");
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

  void sem() override{
      //TODO: also check if array?

      expr1->sem();
      expr2->sem();
      std::clog << "Sem CompareOp" << std::endl;
      //should have .type
      if(expr1->sem_type != expr2->sem_type)
          logError("Compare types dont match");

      sem_type = TYPE_bool;

  }

  Value *compile() override{

    Value *val1 = expr1->compile();
    Value *val2 = expr2->compile();

    //TODO: support arrays ??
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

  void sem() override{
      expr1->check_type(TYPE_bool);
      sem_type = TYPE_bool;


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

  void sem() override{
      for (const auto &d : expr_list )
          d->sem();
  }

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

    std::vector<Value*> compileAssignVector() {
    std::vector<Value*> args;

    for(const auto &d : expr_list) {
        args.push_back(d->compileAssign());
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

  void sem() override{
      //TODO: implement this, we need to check that the arguments of the function have the correct types
      //also add return type to type
      Node* functionNode = st.lookupNode(id, DECL_func);


      if(functionNode == nullptr && LIBRARY_FUNCTIONS.find(id) == LIBRARY_FUNCTIONS.end())// if the function is not in Symbol Table or LIBRARY
            logError("Function named: " + id + " hasnt been declared");

      //TODO: fix this
      if(id == "chr"){
          sem_type = TYPE_char;
      }
      else
        sem_type = TYPE_int; //TODO: fix this and return based on func


  }
  Value* compile() override{
    std::clog << "Compiling function call: " << id << std::endl;

    // std::vector<Value*> args = expr_list->compileVector();
    std::vector<Value*> args;

    Node* functionNode = st.lookupNode(id, DECL_func);

    if(functionNode == nullptr){



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
          //we need the array size
          args.push_back(exprlist[i]->compileAssign());
          
          std::clog << "Argument ref, name: "<< a->name << std::endl;

          // std::vector<int> arr = exprlist[i]->getArraySize();
          // std::clog << "Array size:!!!!!!!!  " << arr.size() << std::endl;
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
          args = expr_list->compileVector();

          //cast vector Value to 64 bit
          // std::vector<Value*> args64;
          // for(auto &a : args) {
          //     args64.push_back(Builder.CreateSExt(a, i64, "cast"));
          // }
          //args = args64;

          func = TheWriteInteger;
        }
        else if(id == "writeChar"){
          args = expr_list->compileVector();

          func = TheWriteChar;

        }
        else if(id == "writeString"){
          //args = {expr_list[0].compileAssign()};
          args = expr_list->compileAssignVector();


          std::clog << "Writing string" << std::endl;
          
          //std::vector<Value*> argPointer = {Builder.CreateGEP(args[0]->getType(),args[0], {c32(0), c32(0)}, "nl")};
                    std::clog << "Writing string2" << std::endl;


          //args = argPointer;

          func = TheWriteString;

        }
        else if(id == "readInteger"){
          args = expr_list->compileVector();

          func = TheReadInteger;
        }
        else if(id == "strlen"){
          args = expr_list->compileAssignVector();

          func = Thestrlen;

        }
        else if(id == "strcpy"){
          args = expr_list->compileAssignVector();

          func = Thestrcpy;

        }
        else if(id == "ascii"){

            args = expr_list->compileVector();

            func = Theascii;

        }
        else if(id == "chr"){

            args = expr_list->compileVector();

            func = Thechr;

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


    if(id == "readInteger" || id == "strlen" || id == "ascii"){
      
      return Builder.CreateSExt(res, i64, "cast");
      //return Builder.CreateTrunc(res, i32, "cast");
      // llvm::Value* truncatedValue = Builder.CreateTrunc(res, Builder.getInt32Ty());
      // return Builder.CreateZExt(truncatedValue, Builder.getInt64Ty());

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

  void sem() override{
      sem_type = TYPE_int;
  }

  Value *compile() override {

    std::clog << "Compiling expression: " << num << std::endl;
    //return ConstantInt::get(TheContext, APInt(32, num));
    //return ConstantInt::get(i32, num);
    return ConstantInt::get(i64, num);

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
  void sem() override{
      sem_type = TYPE_charList;
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

    strConstant = ConstantDataArray::getString(TheContext, processString(var));


    GlobalVariable *gv = new GlobalVariable(*TheModule,
                                                      strConstant->getType(),
                                                      false, //we dont trully want this as constant.. 
                                                      GlobalValue::PrivateLinkage,
                                                      strConstant,
                                                      "str_const");


    return gv;

  }
  Value* compileAssign() override{
    

    return compile();

    //Value *gvar = compile();

    //return Builder.CreateLoad(strConstant->getType(),gvar, "str_const_load");

  }

  std::vector<int> getArraySize() override{
    std::vector<int> v;
    v.push_back(var.size());
    return v;
  }


 private:
  std::string var;

  Constant *strConstant;

};




#endif