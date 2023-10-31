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




//already have it in ast.hpp

struct FuncArg{
    bool ref;
    std::string name;

    DataType type;
    bool isArray;

    std::vector<int> array_size;

};




struct Node;

struct ScopeEntry {
    std::string name;
    ScopeEntry *parent;

    std::map<std::tuple<std::string, DeclType>, Node*> nodes;

    Node * functionOwner;
};


struct Node{
    DataType type;
    DeclType decl_type;
    std::string name;

    //for vardec
    std::vector<int> arraysizes;

    Value *var;
    Node *realNode; //nest func!
    Function *function;
    llvm::Type *llvm_type;

    //for func
    std::vector<Node *>* argnodes;
    std::vector<int> array_size;


    std::vector<FuncArg *> funcargs; //for call?


    std::vector<Node *> extraArgNodes; //needed for nesting
    std::vector<Type*> argTypes;


    //

    bool isCompiled; //atm used for function
    bool isPointer;
    bool isSet;

    bool isFirstArrayDimUnbounded;

    BasicBlock *block;


    ScopeEntry *scope;

    bool assigned;
};

class SymbolTable{
    public:
    SymbolTable(){
        // create global scope
        std::clog << "Making st entry " <<std::endl;
        ScopeEntry *global_scope = new ScopeEntry();
        global_scope->name = "global";
        global_scope->functionOwner = nullptr;
        global_scope->parent = nullptr;
        //add to map with key scopename
        scopes[global_scope->name] = global_scope;

        current_scope = global_scope;

    }

    ScopeEntry* getScope(std::string scope_name){
        return scopes[scope_name];
    }

    void createScope(std::string scope_name, Node* functionOwner){//types too?
        //create and enters
        ScopeEntry *new_scope = new ScopeEntry;
        new_scope->name = scope_name;
        new_scope->functionOwner = functionOwner;
        new_scope->parent = current_scope;

        current_scope = new_scope;

        scopes[scope_name] = new_scope;
    }

    std::string getName(){
        return current_scope->name;
    }

    void enterScope(std::string scope_name){
        // check if scope_name not in scopes
        if(scopes.find(scope_name) == scopes.end()){
            std::clog << "Scope " << scope_name << " not found" << std::endl;
            exit(2);         
            return;
        }
        current_scope = scopes[scope_name];

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
    void insertNodeToScope( Node *node,ScopeEntry *scope, DeclType decl_type = DECL_var){

        scope->nodes[std::make_tuple(node->name,decl_type)] = node;
    }

    Node* lookupNode(std::string node_name, DeclType decl_type = DECL_var,bool enableParentScope = false,llvm::Type * llvm_type=nullptr){
        ScopeEntry *temp_scope = current_scope;

        llvm::Type * tempType;

        Node * lastNode; //or first?

        int *type;

        std::vector<Node *> temp_argnodes;
        bool parentScopeFlag = false;

        while(temp_scope != nullptr){

            std::clog << "Looking in scope " << temp_scope->name << std::endl;
            if(temp_scope->nodes.find(std::make_tuple(node_name,decl_type)) != temp_scope->nodes.end()){
                std::clog << "Found node " << node_name << " in scope " << temp_scope->name << std::endl;

                Node* node = temp_scope->nodes[std::make_tuple(node_name,decl_type)];
                tempType = node->llvm_type;

                if(parentScopeFlag) {
                    std::clog << "ParentScopeFl" << std::endl;

                    std::clog << "size " << temp_argnodes.size() << std::endl;
                    for(Node *n : temp_argnodes){

                        n->llvm_type = node->llvm_type;

                        n->type = node->type;

                        std::clog << "ARGGGGG "<< n->type << std::endl;


                        FuncArg *arg = new FuncArg();

                        arg->name = n->name;
                        arg->type = n->type;
                        arg->ref = true;



                        n->realNode = node;

                        n->isFirstArrayDimUnbounded = node->isFirstArrayDimUnbounded;

                        n->scope->functionOwner->extraArgNodes.push_back(n);

                        n->scope->functionOwner->argnodes->push_back(n);
                        n->scope->functionOwner->funcargs.push_back(arg);

                        std::clog << "SymbolTable " << std::endl;
                        std::clog <<"SCOPE: " << n->scope->name << std::endl;
                        std::clog << n->name << std::endl;
                        std::clog << n->scope->functionOwner->name << std::endl;
                        insertNodeToScope(n,n->scope);
                        lastNode = n;

                    }
                    lastNode->type = node->type;

                    std::clog << "TEST "<< lastNode->type << " " << node->type <<  std::endl;
                    return lastNode;
                }
                else {
                    return node;
                }
            }
            std::clog << "Here"<< std::endl;
            std::clog << "node name: " << node_name << std::endl;

            std::clog << "EnableScope " << enableParentScope << std::endl;

            if(enableParentScope) {
                std::clog<<"adding nodes" << std::endl;
                parentScopeFlag = true;

                //adding an argument to the father function
                Node *arg = new Node();
//                arg->type = TYPE_int; //we should keep a pointer or smth?
//                arg->isArray = false;
                arg->array_size = {};
                arg->name = node_name;

                arg->isPointer = true;
//                arg->llvm_type = tempType;
                arg->isSet = false;
                arg->scope = temp_scope;

                //                arg->type = type;

//                insertNode(arg,DECL_var);

//                lastNode = arg;

                std::clog << "testt" << std::endl;
                std::clog << temp_scope->functionOwner->name << std::endl;

                //temp_scope->functionOwner->argnodes->push_back(arg);
                temp_argnodes.push_back(arg);
                std::clog << "Here22" << std::endl;
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