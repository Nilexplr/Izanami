##
## koak
## Makefile
##

PROJECT_NAME	=	koak

MAIN 			= app/Main.hs

SRC 			=	src/Argument.hs \
					src/Compile.hs \
					src/JIT.hs \
					src/Lib.hs \
					src/LLVMtools.hs \
					src/Parser.hs \
					src/Prompt.hs \
					src/Tokenize.hs \
					src/Utils.hs

TU_SRC			= 	test/Spec.hs \
					test/TestToken.hs \

all: $(PROJECT_NAME)

$(PROJECT_NAME):
	ghc $(MAIN) $(SRC) -o -optl -no-keep-hi-files -no-keep-o-files

tests_run:
	ghc $(SRC) $(TU_SRC) -o koak -optl -no-keep-hi-files -no-keep-o-files

tests_run_coverage:
	stack test --coverage --allow-different-user

clean:
	rm -f $(PROJECT_NAME)
	stack clean

fclean: clean

re: fclean all