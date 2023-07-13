#include <string>
#include <sstream>
#include <vector>
#include <iterator>
#include <iostream>

class ScopeTracker{
    public:
    ScopeTracker(){
        scopes.push_back("global");
    }

    void addScope(std::string scope_name){
        scopes.push_back(scope_name);
    }

    void removeScope(){
        if(scopes.size() == 1){
            std::cout << "Cannot remove global scope" << std::endl;
            return;
        }
        scopes.pop_back();
    }

    std::string getCurrentScope(){

        std::stringstream ss;
        for(size_t i = 0; i < scopes.size(); ++i) {
            if(i != 0)
                ss << "::";
            ss << scopes[i];
        }

        return ss.str();
    }




    private:
     std::vector<std::string> scopes;
};

