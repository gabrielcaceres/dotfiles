
SHELL = /bin/bash
DOTFILES := bashrc inputrc

.DEFAULT_GOAL := help
.PHONY: help all

all: help

emacs:
	echo 'emacs'
	# mkdir -p $(HOME)/.emacs.d

help:
	echo hello world .$@.test $(HOME) $(addsuffix .sh,$(DOTFILES))
