DOTFILES := bashrc inputrc

init:
	echo hello world .$@.test $(DOTFILES).sh $(addsuffix .sh,$(DOTFILES))
