all:
	happy -gca ParE.y
	alex -g LexE.x
	ghc --make Main.hs -o Main

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocE.* LexE.* ParE.* LayoutE.* SkelE.* PrintE.* Main.* AbsE.* Main ErrM.* SharedString.* ComposOp.* E.dtd XMLE.* Makefile*
	

