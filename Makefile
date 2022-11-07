.PHONY : test

EMACS ?= emacs
CASK ?= cask

LOADPATH = -L .
LOAD_HELPER = -l test/test-helper.el

ELPA_DIR = $(shell EMACS=$(EMACS) $(CASK) package-directory)

test: elpa
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) $(LOAD_HELPER) \
		-l test/test-lisp-utility.el \
		-l test/test-kill-utility.el \
		-l test/test-moving-utility.el \
		-l test/test-insertion-utility.el \
		-l test/test-editing-utility.el \
		-f ert-run-tests-batch-and-exit

elpa: $(ELPA_DIR)
$(ELPA_DIR): Cask
	$(CASK) install
	touch $@
