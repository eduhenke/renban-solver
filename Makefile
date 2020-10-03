run:
	ghc --make Main && ./Main

test:
	ghc --make -fno-ignore-asserts Test && ./Test

clean:
	rm *.o *.hi Main Test