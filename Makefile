# name of the Curry2Julia compiler executable
CJC=c2j

# standard options for the compiler
export CJOPTIONS=

# install compiler and system libraries
.PHONY: install
install:
	cypm install
	$(MAKE) libs

# translate system libraries
.PHONY: libs
libs:
	cd lib             && $(CJC) Prelude
	cd lib_backtrack   && $(CJC) Prelude --backtrack
	cd lib_pulltab     && $(CJC) Prelude --pulltab
	cd lib_pulltabonly && $(CJC) Prelude --pulltabonly

.PHONY: clean
clean:
	cypm uninstall
	cypm clean
	/bin/rm -f lib*/*.jl
