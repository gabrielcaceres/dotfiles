
SHELL = /bin/bash
DOTFILES := bashrc inputrc

.PHONY: help all

help:
	echo hello world .$@.test $(HOME) $(addsuffix .sh,$(DOTFILES))

all: help

emacs:
	mkdir -p $(HOME)/.emacs.d
