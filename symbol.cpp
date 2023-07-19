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


//funcarg


struct FuncArg{
    bool ref;
    std::string name;

    DataType type;
    bool isArray;
    
    // ArraySize *array_size;

};




struct Node{
    DataType type;
    DeclType decl_type;
    std::string name;

    Value *var;
    Function *function;
    llvm::Type *llvm_type;

    //for func
    std::vector<FuncArg *> funcargs;
    std::vector<int> array_size;
    bool isPointer;

    bool isArgument;



    bool assigned;
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


#endif