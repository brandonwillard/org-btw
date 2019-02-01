#
# Inspired by https://github.com/leanprover/presentations/blob/master/org-reveal.mk#L9
# and https://github.com/bbatsov/projectile/blob/master/Makefile
#
.PHONY: all clean install-cask test elpa

CASK_DIR ?= ${HOME}/.cask
CASK_BIN ?= ${CASK_DIR}/bin/cask
EMACS_BIN ?= emacs

all: clean install-cask

install-cask:
	curl -fsSkL https://raw.github.com/cask/cask/master/go | python

elpa-$(EMACS_BIN):
	$(CASK_BIN) install
	$(CASK_BIN) update
	touch $@

elpaclean:
	rm -f elpa*
	rm -rf .cask

elpa: elpa-$(EMACS_BIN)

.cask: Cask
	@EMACS=$(EMACS_BIN) $(CASK_BIN)
	@touch .cask

test: unit

compile: elpa
	$(CASK_BIN) build

unit:
	$(CASK_BIN) exec ert-runner

clean:
	$(CASK_BIN) clean-elc
