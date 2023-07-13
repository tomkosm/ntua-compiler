#include "ast.hpp"

LLVMContext AST::TheContext;
IRBuilder<> AST::Builder(TheContext);
std::unique_ptr<Module> AST::TheModule;
std::unique_ptr<legacy::FunctionPassManager> AST::TheFPM;

GlobalVariable *AST::TheVars;
GlobalVariable *AST::TheNL;
Function *AST::TheWriteInteger;
Function *AST::TheWriteString;

Type *AST::i8;
Type *AST::i32;
Type *AST::i64;



SymbolTable AST::st = SymbolTable();
