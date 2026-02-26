all:
	@echo "using Fortran compiler: '`$(FC) --version | head -n 1`'" || true
        # Using `$(MAKE) --directory ...` is nicer but sadly outlawed here
	(cd ../tools/x13as_html && FC="$(FC)" FFLAGS="-O2" LINKER="$(FC)" $(MAKE) -f makefile.gf x13ashtml)
	mkdir -p ../inst/bin
	cp -f ../tools/x13as_html/x13ashtml ../inst/bin/

clean:
	@rm -f ../tools/x13as_html/x13ashtml
