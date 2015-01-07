
MODULES := modules/Warning.o $(subst .hs,.o,$(wildcard modules/*.hs))

all: $(MODULES) ; ghc -imodules -ferror-spans -Wall -Werror -c -fno-code yi.hs

%.o: %.hs ; ghc -imodules -ferror-spans -Wall -Werror -c $<

clean: ; find . -name '*.hi' -delete ; find . -name '*.o' -delete