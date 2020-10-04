run:
	ghc --make Main
	./Main

test:
	ghc Test
	./Test

clean:
	rm *.o *.hi Main Test