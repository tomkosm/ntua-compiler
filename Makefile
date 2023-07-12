CC=g++
CFLAGS=-Wall

gracec: lexer.o parser.o
	$(CC) $(CFLAGS) -std=c++11 -o $@ $^
lexer.c: lexer.l parser.hpp
	flex -s -o $@ $<
parser.c parser.hpp: parser.y
	bison --debug -dv --debug -o $@ $<
clean:
	$(RM) *.o parser.c parser.hpp lexer.c core *~
distclean: clean
	$(RM) gracec

