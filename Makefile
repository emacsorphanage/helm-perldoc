.PHONY : test

EMACS ?= emacs
CASK ?= cask
LOADPATH = -L .

test: elpa
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) \
		-l test/test-helm-perldoc.el \
		-f ert-run-tests-batch-and-exit

elpa: Cask
	${CASK} install
	touch $@
