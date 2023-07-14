#ifndef __SYMBOL_HPP__
#define __SYMBOL_HPP__

#include <map>
#include <vector>
#include <string>
#include <iostream>

#include "scope.cpp"



enum DeclType { DECL_var, DECL_func };

//shared
// enum DataType 
// {   TYPE_int, 
//     TYPE_char, 
//     TYPE_nothing
// };




struct Node{
    DataType type;
    DeclType decl_type;
    std::string name;

    GlobalVariable *var;
    Function *function;
};


struct ScopeEntry {
    std::string name;
    ScopeEntry *parent;

    std::map<std::tuple<std::string, DeclType>, Node*> nodes;


};


class SymbolTable{
    public:
    SymbolTable(){
        // create global scope
        ScopeEntry *global_scope = new ScopeEntry();
        global_scope->name = "global";
        global_scope->parent = nullptr;
        //add to map with key scopename
        scopes[global_scope->name] = global_scope;

        current_scope = global_scope;

    }

    ScopeEntry* getScope(std::string scope_name){
        return scopes[scope_name];
    }

    void createScope(std::string scope_name){//types too?
        ScopeEntry *new_scope = new ScopeEntry;
        new_scope->name = scope_name;
        new_scope->parent = current_scope;

        current_scope = new_scope;

        scopes[scope_name] = new_scope;
    }

    void exitScope(){
        if(current_scope->parent == nullptr){
            std::clog << "Cannot exit global scope" << std::endl;
            return;
        }
        current_scope = current_scope->parent;
    }

    void insertNode( Node *node, DeclType decl_type = DECL_var){

        current_scope->nodes[std::make_tuple(node->name,decl_type)] = node;
    }

    Node* lookupNode(std::string node_name, DeclType decl_type = DECL_var){
        ScopeEntry *temp_scope = current_scope;

        while(temp_scope != nullptr){
            if(temp_scope->nodes.find(std::make_tuple(node_name,decl_type)) != temp_scope->nodes.end()){
                std::clog << "Found node " << node_name << " in scope " << temp_scope->name << std::endl;
                return temp_scope->nodes[std::make_tuple(node_name,decl_type)];
            }
            temp_scope = temp_scope->parent;
        }
        //TODO: add error handling
        std::clog << "Variable " << node_name << " not found" << std::endl;
        // yyerror("Variable not found");
        return nullptr;
    }

    ScopeEntry* currentScope(){
        return current_scope;
    }


    private:
    
        ScopeEntry *current_scope;
        std::map<std::string, ScopeEntry*> scopes;

};


// SymbolTable* st = new SymbolTable();

// st->createScope("global2", nullptr);

// st->current_scope();

// int main(){

//     SymbolTable* stt = new SymbolTable(); // Declare and initialize SymbolTable object

//     stt->createScope("global2");

//     Node* node = new Node();
//     node->name = "x";
//     node->type = TYPE_int;
//     node->decl_type = DECL_var;


//     stt->insertNode(node);
//     stt->createScope("global3");

//     stt->currentScope();

//     stt->lookupNode("x");


// }

// enum Type { TYPE_int, TYPE_bool, TYPE_char };

// struct STEntry {
//   Type type;
//   int offset;
//   STEntry() {}
//   STEntry(Type t, int o) : type(t), offset(o) {}
// };

// class Scope {
//  public:
//   Scope(int o = -1) : offset(o) {}
//   STEntry *lookup(char c) {
//     if (locals.find(c) == locals.end()) return nullptr;
//     return &(locals[c]);
//   }
//   void insert(char c, Type t) {
//     if (locals.find(c) != locals.end())
//       yyerror("Duplicate variable declaration");
//     locals[c] = STEntry(t, offset++);
//   }
//   int get_offset() {
//     return offset;
//   }
//  private:
//   std::map<char, STEntry> locals;
//   int offset;
// };

// class SymbolTable {
//  public:
//   STEntry *lookup(char c) {
//     for (auto s = scopes.rbegin(); s != scopes.rend(); ++s) {
//       STEntry *e = s->lookup(c);
//       if (e != nullptr) return e;
//     }
//     yyerror("Variable not found");
//     return nullptr;
//   }
//   void insert(char c, Type t) {
//     scopes.back().insert(c, t);
//   }
//   void push_scope() {
//     int o = scopes.empty() ? 0 : scopes.back().get_offset();
//     scopes.push_back(Scope(o));
//   }
//   void pop_scope() {
//     scopes.pop_back();
//   }
//  private:
//   std::vector<Scope> scopes;
// };

// extern SymbolTable st;

#endif