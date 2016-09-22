all:
	happy -gca Pargrammar.y
	alex -g Lexgrammar.x
	latex Docgrammar.tex; dvips Docgrammar.dvi -o Docgrammar.ps
	ghc --make Runner.hs -o interpreter
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Docgrammar.ps
distclean: clean
	-rm -f Docgrammar.* Lexgrammar.* Pargrammar.* TypeChecker.* Interpreter.* Layoutgrammar.* Printgrammar.* Runner.* Absgrammar.* interpreter ErrM.* SharedString.* grammar.dtd XMLgrammar.* Makefile*

