# More like a test file, but whatever.
TESTGEN := ./TestMainGen.hs
MAIN := ./Main.hs

.SILENT:

all: $(MAIN)
	runhaskell $(MAIN)

$(MAIN): $(TESTGEN)
	echo "Generating main file..."
	runhaskell $(TESTGEN)

clean:
	@rm -f $(MAIN)

clean-all:
	@rm -f $(MAIN)
	@find ./ -name '*.hi' -or -name '*.o' -exec rm {} \;
