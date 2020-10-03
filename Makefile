run:
	ghc --make Main && ./Main

test:
	ghc --make Test && ./Test

clean:
	rm *.o *.hi Main Test