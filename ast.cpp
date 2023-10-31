#include "ast.hpp"

LLVMContext AST::TheContext;
IRBuilder<> AST::Builder(TheContext);
std::unique_ptr<Module> AST::TheModule;
std::unique_ptr<legacy::FunctionPassManager> AST::TheFPM;


Function *AST::TheWriteInteger;
Function *AST::TheWriteChar;
Function *AST::TheWriteString;
Function *AST::TheReadInteger;
Function *AST::Thestrlen;
Function *AST::Thestrcpy;
Function *AST::Theascii;
Function *AST::Thechr;


Type *AST::i8;
Type *AST::i8_ptr;
Type *AST::i32;
Type *AST::i64;

Type *AST::void_type;

std::map<std::string, FunctionDetails> AST::externalFuncMap;

SymbolTable AST::st = SymbolTable();

