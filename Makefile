.PHONY: clean distclean default
MAKEFLAGS += -j4
LLVMCONFIG=llvm-config

CXX=clang++
CXXFLAGS=-Wall -g `$(LLVMCONFIG) --cxxflags` -std=c++17
LDFLAGS=`$(LLVMCONFIG) --ldflags --system-libs --libs all`

default: gracec

lexer.cpp: lexer.l
	flex -s -o lexer.cpp lexer.l

lexer.o: lexer.cpp lexer.hpp parser.hpp ast.hpp
	$(CXX) $(CXXFLAGS) -c -o lexer.o lexer.cpp 

ast.o: ast.cpp ast.hpp symbol.cpp
	$(CXX) $(CXXFLAGS) -c -o ast.o ast.cpp

parser.hpp parser.cpp: parser.y
	bison -dv -o parser.cpp parser.y

parser.o: parser.cpp lexer.hpp ast.hpp
	$(CXX) $(CXXFLAGS) -c -o parser.o parser.cpp 
 
gracec: lexer.o parser.o ast.o
	$(CXX) $(CXXFLAGS) -o gracec $^ $(LDFLAGS)

clean:
	$(RM) lexer.cpp parser.cpp parser.hpp parser.output *.o *.s

distclean: clean
	$(RM) gracec a.out
