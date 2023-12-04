.PHONY: clean distclean default
MAKEFLAGS += -j4
LLVMCONFIG=llvm-config

CXX=clang++
LDFLAGS=`$(LLVMCONFIG) --ldflags --system-libs --libs all`

# Directories
SRCDIR = src
LEXERDIR = $(SRCDIR)/lexer
PARSERDIR = $(SRCDIR)/parser
ASTDIR = $(SRCDIR)/ast
BUILDDIR = build
BINDIR = bin
CXXFLAGS=-Wall -g `$(LLVMCONFIG) --cxxflags` -std=c++17 -I$(SRCDIR) -I$(LEXERDIR) -I$(PARSERDIR) -I$(ASTDIR)

VPATH = $(LEXERDIR):$(PARSERDIR):$(ASTDIR)

default: $(BINDIR)/gracec

$(BUILDDIR)/lexer.cpp: $(LEXERDIR)/lexer.l
	flex -s -o $@ $<

$(BUILDDIR)/lexer.o: $(BUILDDIR)/lexer.cpp $(LEXERDIR)/lexer.hpp $(BUILDDIR)/parser.hpp $(ASTDIR)/ast.hpp
	$(CXX) $(CXXFLAGS) -c -o $@ $(BUILDDIR)/lexer.cpp

$(BUILDDIR)/ast.o: $(ASTDIR)/ast.cpp $(ASTDIR)/ast.hpp $(ASTDIR)/symbol.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $(ASTDIR)/ast.cpp

$(BUILDDIR)/parser.cpp $(BUILDDIR)/parser.hpp: $(PARSERDIR)/parser.y
	bison -dv -o $(BUILDDIR)/parser.cpp $<

$(BUILDDIR)/parser.o: $(BUILDDIR)/parser.cpp $(LEXERDIR)/lexer.hpp $(ASTDIR)/ast.hpp
	$(CXX) $(CXXFLAGS) -c -o $@ $(BUILDDIR)/parser.cpp

$(BINDIR)/gracec: $(BUILDDIR)/lexer.o $(BUILDDIR)/parser.o $(BUILDDIR)/ast.o
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)
clean:
	$(RM) -r $(BUILDDIR)/*
distclean: clean
	$(RM) -r $(BINDIR)/*
