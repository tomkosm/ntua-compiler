#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <vector>
#include <map>
#include <deque>

#include "symbol.hpp"


#include <string>

using namespace std;

extern std::vector<int> rt_stack;


enum Type 
{   TYPE_int, 
    TYPE_char, 
    TYPE_nothing
};

class AST {
 public:
  virtual ~AST() = default;
  virtual void sem() {};
  virtual void printAST(std::ostream &out) const = 0;
};

inline std::ostream &operator<<(std::ostream &out, Type t) {
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


class Id : public AST {
 public:
  Id(string *c): var(*c) {}
  void printAST(std::ostream &out) const override {
    out << "Id(" << var << ")";
  }

 private:
  string var;
  int offset;
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

class Expr : public AST {
 public:

 void printAST(std::ostream &out) const override {
    out << "Expr(empty)";
  }

  void check_type(Type t) {
    sem();
    if (type != t) yyerror("Type mismatch");
  }
//   virtual int eval() const = 0;
 protected:
  Type type;
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
  TypeDef(Type t, ArraySize *s): type(t), array_size(s) {}

  void printAST(std::ostream &out) const override {
    out << "TypeDef(" << type << ", " << array_size << ")";
  }
//   void allocate() const {
//     rt_stack.push_back(0);
//   }
//   void deallocate() const {
//     rt_stack.pop_back();
//   }
 private:
  Type type;
  ArraySize *array_size;
};






class VarDec : public Stmt {
 public:
  VarDec(IdList *i, TypeDef *t): id_list(i), type(t) {}

  void printAST(std::ostream &out) const override {
    out << "VarDec(" << id_list << ": " << type << ")";
  }
//   void allocate() const {
//     rt_stack.push_back(0);
//   }
//   void deallocate() const {
//     rt_stack.pop_back();
//   }
 private:
  IdList *id_list;
  TypeDef *type;
};



class CharConst : public Expr {
 public:
  CharConst(char v): var(v) {}
  void printAST(std::ostream &out) const override {
    out << "CharConst(" << var << ")";
  }
 private:
  char var;
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
//     public:
//     Lvalue() {}
//     void printAST(std::ostream &out) const override {
//     out << "Lvalue()";
//   }
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
//   void sem() override {
//     STEntry *e = st.lookup(var);
//     expr->check_type(e->type);
//     offset = e->offset;
//   }
//   void execute() const override {
//     rt_stack[offset] = expr->eval();
//   }
 private:
  Lvalue *var;
  Expr *expr;
  int offset;
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
  BinOp(Expr *e1, string *s, Expr *e2) : expr1(e1), op(*s), expr2(e2) {}
  ~BinOp() { delete expr1; delete expr2; }
  void printAST(std::ostream &out) const override {
    out << "BinOp(" << *expr1 << ", " << op << ", " << *expr2 << ")";
  }
//   void sem() override {
//     expr1->check_type(TYPE_int);
//     expr2->check_type(TYPE_int);
//     switch(op) {
//       case '+': case '-': case '*': case '/': case '%':
// 	type = TYPE_int;
//         break;
//       case '<': case '=': case '>':
// 	type = TYPE_bool;
//         break;
//     }
//   }
//   int eval() const override {
//     switch (op) {
//       case '+': return expr1->eval() + expr2->eval();
//       case '-': return expr1->eval() - expr2->eval();
//       case '*': return expr1->eval() * expr2->eval();
//       case '/': return expr1->eval() / expr2->eval();
//       case '%': return expr1->eval() % expr2->eval();
//       case '<': return expr1->eval() < expr2->eval();
//       case '=': return expr1->eval() == expr2->eval();
//       case '>': return expr1->eval() > expr2->eval();
//     }
//     return 42;  // will never be reached...
//   }
 private:
  Expr *expr1;
  string op;
  Expr *expr2;
};


class UnaryOp : public Expr {
 public:
  UnaryOp(Expr *e1, string *s) : expr1(e1), var(*s){}
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
  string var;
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
  FparType(Type t, bool e, ArraySize *a) : type(t),arrySizeEmpty(e),array_size(a) {}
  ~FparType() {
    delete array_size;
  }

  void printAST(std::ostream &out) const override {
    out << "FparType("<< type<< ", " << arrySizeEmpty << ", " << *array_size << ")";

  }

  private:

    Type type;
    bool arrySizeEmpty;

    ArraySize *array_size;

};


class FparDef : public Stmt {
 public:
  FparDef(Ref *r,string *t,IdList *i,FparType *ft) : ref(r),Tid(*t),id_list(i),fpar_type(ft) {}
  ~FparDef() {
    delete ref; delete id_list; delete fpar_type;
  }


  void printAST(std::ostream &out) const override {
    out << "FparDef("<< *ref<< ", " << Tid << ", " << *id_list << ", " << *fpar_type << ")";
  }

  private:
    Ref *ref;
    string Tid;
    IdList *id_list;
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

  private:
    std::vector<FparDef *> fpardef_list;
};





class FunctionHeader : public Stmt {
 public:
  FunctionHeader(string *s,FparDefList *f,Type t) : Tid(*s),fpardef_list(f),type(t) {}
  ~FunctionHeader() {
    delete fpardef_list;
  }


  void printAST(std::ostream &out) const override {
    out << "FunctionHeader("<< Tid<< ", " << *fpardef_list << ", " << type << ")";
  }

  private:
    string Tid;
    FparDefList *fpardef_list;
    Type type;

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

  private:
    std::vector<Stmt *> localdef_list;
};




class FunctionDef : public Stmt {
 public:
  FunctionDef(FunctionHeader *h,LocalDefList *l,StmtList *s) : header(h),localdef_list(l),stmt_list(s) {}
  ~FunctionDef() {
    delete localdef_list; delete stmt_list; delete header;
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
  BinOpCond(Expr *e1, string *s, Expr *e2) : expr1(e1), op(*s), expr2(e2) {}
  ~BinOpCond() { delete expr1; delete expr2; }
  void printAST(std::ostream &out) const override {
    out << "BinOpCond("<< op<< ", " << *expr1 << ", " << *expr2 << ")";

  }

 private:
  Expr *expr1;
  string op;
  Expr *expr2;
};


class CompareOp : public Cond {
    //also take care of conds 
 public:
  CompareOp(Expr *e1, string *s, Expr *e2) : expr1(e1), op(*s), expr2(e2) {}
  ~CompareOp() { delete expr1; delete expr2; }
  void printAST(std::ostream &out) const override {
    out << "CompareOp("<< op<< ", " << *expr1 << ", " << *expr2 << ")";
  }

 private:
  Expr *expr1;
  string op;
  Expr *expr2;
};


class UnaryOpCond : public Cond {
    //also take care of conds 
 public:
  UnaryOpCond(Expr *e1, string *s) : expr1(e1), op(*s) {}
  ~UnaryOpCond() { delete expr1; }
  void printAST(std::ostream &out) const override {
    out << "UnaryOpCond("<< op<< ", " << *expr1 << ")";
  }

 private:
  Expr *expr1;
  string op;
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

  private:
    std::deque<Expr *> expr_list;
};

class FuncCall : public Stmt, public Expr {
 public:
  FuncCall(string *s, ExprList *e): id(*s),expr_list(e) {}
  ~FuncCall() {
        delete expr_list;
  }
  void printAST(std::ostream &out) const override {
    out << "FuncCall(" << id << ", " << *expr_list << ")";
  }

 private:
  string id;
  ExprList *expr_list;
  
  int offset;
};

class IntConst : public Expr {
 public:
  IntConst(int n): num(n) {}
  void printAST(std::ostream &out) const override {
    out << "IntConst(" << num << ")";
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
  IdLval(string *c) : var(*c) {}
  void printAST(std::ostream &out) const override {
    out << "Id(" << var << ")";
  }


 private:
  string var;
  int offset;
};



class StringConst : public Lvalue {
 public:
  StringConst(string *s) : var(*s) {}
  void printAST(std::ostream &out) const override {
    out << "StringConst(" << var << ")";
  }

 private:
  string var;
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