
default: JTL/Parser.hs jtli jtlt

JTL/Parser.hs: JTL/Parser.y
	happy JTL/Parser.y

jtli:
	ghc --make Interpreter.hs -o jtli

jtlt:
	ghc --make Tester.hs -o jtlt

test:
	./jtlt tests/*.json

.PHONY: default jtli jtlt
