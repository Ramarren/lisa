# Makefile for Zebu

SHELL = /bin/sh
LISP = /lisp/700-non-kanji/non-kanji
# You may have to customize the variables LISP CLPATH CLDE Lisp-compiler Lisp-ae LATEX bin
CLPATH = $(LISP)

# Common Lisp development environment
CLDE = lucid-de

# Compiler
Lisp-compiler = $(CLPATH)/$(CLDE)

# Common Lisp application environment
Lisp-ae = $(CLPATH)/lucid-ae

LATEX = latex

cl-compile = $(CLPATH)/$(CLDE) -n \
	-l COMPILE-ZEBU.lisp -quit -f 

# this works in GNU make, and not in HPUX
# in HPUX use: bin = `./machine`bin
bin = `./machine`bin
# with gnu make use bin := `./machine`bin
B = binary/

lisp-obj = $(B)zebu-package.$(bin) $(B)zebu-aux.$(bin) $(B)zmg-dom.$(bin) $(B)zebu-regex.$(bin) $(B)zebu-loader.$(bin) $(B)zebu-driver.$(bin) $(B)zebu-actions.$(bin) $(B)zebu-oset.$(bin) $(B)zebu-g-symbol.$(bin) $(B)zebu-loadgram.$(bin) $(B)zebu-generator.$(bin) $(B)zebu-lr0-sets.$(bin) $(B)zebu-empty-st.$(bin) $(B)zebu-first.$(bin) $(B)zebu-follow.$(bin) $(B)zebu-tables.$(bin) $(B)zebu-slr.$(bin) $(B)zebu-closure.$(bin) $(B)zebu-lalr1.$(bin) $(B)zebu-dump.$(bin) $(B)zebu-compile.$(bin) $(B)zebu-printers.$(bin)

all: ZEBU-init.lisp $(lisp-obj) Zebu-Compiler Zebu-Parser Zebu_intro TAGS

$(B)zebu-package.$(bin): zebu-package.lisp
	$(cl-compile)

$(B)zebu-aux.$(bin): zebu-aux.lisp
	$(cl-compile)

$(B)zmg-dom.$(bin): zmg-dom.lisp
	$(cl-compile)

$(B)zebu-regex.$(bin): zebu-regex.lisp
	$(cl-compile)

$(B)zebu-loader.$(bin): zebu-loader.lisp
	$(cl-compile)

$(B)zebu-driver.$(bin): zebu-driver.lisp
	$(cl-compile)

$(B)zebu-actions.$(bin): zebu-actions.lisp
	$(cl-compile)

$(B)zebu-oset.$(bin): zebu-oset.lisp
	$(cl-compile)

$(B)zebu-g-symbol.$(bin): zebu-g-symbol.lisp
	$(cl-compile)

$(B)zebu-loadgram.$(bin): zebu-loadgram.lisp
	$(cl-compile)

$(B)zebu-generator.$(bin): zebu-generator.lisp
	$(cl-compile)

$(B)zebu-lr0-sets.$(bin): zebu-lr0-sets.lisp
	$(cl-compile)

$(B)zebu-empty-st.$(bin): zebu-empty-st.lisp
	$(cl-compile)

$(B)zebu-first.$(bin): zebu-first.lisp
	$(cl-compile)

$(B)zebu-follow.$(bin): zebu-follow.lisp
	$(cl-compile)

$(B)zebu-tables.$(bin): zebu-tables.lisp
	$(cl-compile)

$(B)zebu-slr.$(bin): zebu-slr.lisp
	$(cl-compile)

$(B)zebu-closure.$(bin): zebu-closure.lisp
	$(cl-compile)

$(B)zebu-lalr1.$(bin): zebu-lalr1.lisp
	$(cl-compile)

$(B)zebu-dump.$(bin): zebu-dump.lisp
	$(cl-compile)


$(B)zebu-compile.$(bin): zebu-compile.lisp
	$(cl-compile)

$(B)zebu-printers.$(bin): zebu-printers.lisp
	$(cl-compile)

Zebu-Compiler: $(lisp-obj) ZEBU-init.lisp
	@echo "Making the Zebu Compiler"
	$(Lisp-compiler) -n \
-l "ZEBU-init.lisp" \
-e '(progn (zb::zebu-compiler)(setq *enter-top-level-hook* (function zb::compile-from-command-line)) (discard-source-file-info) (disksave "./Zebu-Compiler" :FULL-GC T :WRITABLE T :VERBOSE T) (quit))'

Zebu-Parser: $(lisp-obj) ZEBU-init.lisp
	@echo "Making the Zebu Parser"
	$(Lisp-ae) -n \
-l "ZEBU-init.lisp" \
-e '(progn (in-package "CL-USER") (zb::zebu) (discard-source-file-info))' \
-e '(progn (disksave "./Zebu-Parser" :FULL-GC T :WRITABLE T :VERBOSE T :restart-function (function zb::load-from-command-line)) (quit))'

Zebu_intro: doc/Zebu_intro.tex
	cd doc; $(LATEX) Zebu_intro; makeindex Zebu_intro.idx;\
	$(LATEX) Zebu_intro;\
	dvips Zebu_intro.dvi -o Zebu_intro.ps

TRANSLATE = /usr/local/bin/latex2html
tohtml = $(TRANSLATE) -no_reuse -address laubsch@cup.hp.com -auto_navigation -contents_in_navigation -index_in_navigation 

html:	doc/Zebu_intro.html

doc/Zebu_intro.html:	doc/Zebu_intro.tex
	$(tohtml) doc/Zebu_intro.tex

.PHONY: distclean
distclean:
	make clean
	@echo Deleting tab files in directory test and test/binary
	chmod 777 ./test
	rm -rf ./test/*.tab
	@echo Deleting binary files in directory test/binary
	rm -f ./test/binary/*.$(bin) 
	@echo Deleting binary files in directory test/binary
	rm -f ./binary/*

.PHONY: clean
clean:
	@echo Deleting domain files 
	find .  \( -name '[0-9]*-dom*' -o -name '*~' \) -type f -print -exec rm {} \;
	find ./test  \( -name '[0-9]*.lisp' -o -name '*-dom*.lisp' -o -name '*~' \) -type f -print -exec rm {} \;

TAGS:   $(lisp-obj) 
	make clean; etags -o TAGS *.lisp

tar:
	./Tar-zebu

dos:
	./winzebu
