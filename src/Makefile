all:
	FC="$(FC)" LINKER="$(DYLIB_LD)" LDFLAGS="$(FLIBS)" $(MAKE) --directory=../tools/x13as_html -f makefile.gf x13ashtml
	mkdir -p ../inst/bin
	cp -f ../tools/x13as_html/x13ashtml ../inst/bin/

clean:
	rm -f ../tools/x13as_html/x13ashtml
