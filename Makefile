SHELL = /bin/bash

LINKFILES := bashrc inputrc gitignore
DOTFILES := $(addprefix $(HOME)/.,$(LINKFILES))
DOTEMACS := $(addprefix $(HOME)/.emacs.d/,init.el)

.DEFAULT_GOAL := help
.PHONY: all help link unlink bash emacs

all: help

setup: | bash emacs $(DOTFILES)

help:
	echo hello world .$@.test $(HOME) $(addsuffix .sh,$(DOTFILES))

bash:
ifeq ($(wildcard $(HOME)/.bashrc),)
	@echo "No .bashrc, creating file"
	@touch $(HOME)/.bashrc)
endif
ifeq ($(shell grep "Load .bashrc from repo" $(HOME)/.bashrc),)
	@echo "Add line to source repo bashrc"
	@echo "## Load .bashrc from repo" >> $(HOME)/.bashrc)
	@echo "source ~/Repos/dotfiles/bashrc.sh" >> $(HOME)/.bashrc)
endif

emacs: | $(DOTEMACS)

$(DOTEMACS):
	@echo 'Linking repo init.el for Emacs'
	@mkdir -p $(dir $@)
	@ln -sv "$(CURDIR)/emacs/init.el" $@

### Linking files
# Modified from: https://polothy.github.io/post/2018-10-09-makefile-dotfiles/
link: | $(DOTFILES)
	@echo 'Linking repo dotfiles'

$(DOTFILES):
	@ln -sv "$(CURDIR)/$(notdir $(subst .,,$@)).sh" $@

# Interactively delete symbolic links.
unlink:
	@echo "Unlinking dotfiles"
	@for f in $(DOTFILES); do if [ -h $$f ]; then rm -i $$f; fi ; done
	@rm -i $(HOME)/.emacs.d/init.el

