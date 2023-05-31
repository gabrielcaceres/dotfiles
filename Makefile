SHELL = /bin/bash

LINKFILES := bashrc inputrc gitignore
DOTFILES := $(addprefix $(HOME)/.,$(LINKFILES))
DOTEMACS := $(addprefix $(HOME)/.emacs.d/,init.el)

.DEFAULT_GOAL := help
.PHONY: all help link unlink bash emacs

## For now only repeats `help`
all: help

## Sets up bash and links dotfiles (as needed)
setup: | bash link
	@echo 'Setup repo dotfiles'

## Links dotfiles
link: | emacs $(DOTFILES)
	@echo 'Linking repo dotfiles'

## Add line to source config into .bashrc
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

## Simpler command to link Emacs init file
emacs: | $(DOTEMACS)

## Links Emacs init file
$(DOTEMACS):
	@echo "Linking Emacs init.el"
	@mkdir -p $(dir $@)
	@ln -sv "$(CURDIR)/emacs/init.el" $@


### Modified from: https://polothy.github.io/post/2018-10-09-makefile-dotfiles/

## Linking dotfiles files
$(DOTFILES):
	@echo "Linking $(notdir $@)"
	@ln -sv "$(CURDIR)/$(notdir $(subst .,,$@)).sh" $@

## Interactively delete symbolic links.
unlink:
	@echo "Unlinking dotfiles"
	@for f in $(DOTFILES); do if [ -h $$f ]; then rm -i $$f; fi ; done
	@rm -i $(HOME)/.emacs.d/init.el


#################################################################################
# Self Documenting Commands                                                     #
#################################################################################

#***TODO: adds a lot of white space after commands
# Taken from Cookiecutter DS Makefile
# <https://github.com/drivendata/cookiecutter-data-science/blob/master/%7B%7B%20cookiecutter.repo_name%20%7D%7D/Makefile>
# which was itself inspired by
# <http://marmelab.com/blog/2016/02/29/auto-documented-makefile.html>
# sed script explained:
# /^##/:
# 	* save line in hold space
# 	* purge line
# 	* Loop:
# 		* append newline + line to hold space
# 		* go to next line
# 		* if line starts with doc comment, strip comment character off and loop
# 	* remove target prerequisites
# 	* append hold space (+ newline) to line
# 	* replace newline plus comments by `---`
# 	* print line
# Separate expressions are necessary because labels cannot be delimited by
# semicolon; see <http://stackoverflow.com/a/11799865/1968>
help:
	@echo "$$(tput bold)Available rules:$$(tput sgr0)"
	@echo
	@sed -n -e "/^## / { \
		h; \
		s/.*//; \
		:doc" \
		-e "H; \
		n; \
		s/^## //; \
		t doc" \
		-e "s/:.*//; \
		G; \
		s/\\n## /---/; \
		s/\\n/ /g; \
		p; \
	}" ${MAKEFILE_LIST} \
	| LC_ALL='C' sort --ignore-case \
	| awk -F '---' \
		-v ncol=$$(tput cols) \
		-v indent=19 \
		-v col_on="$$(tput setaf 6)" \
		-v col_off="$$(tput sgr0)" \
	'{ \
		printf "%s%*s%s ", col_on, -indent, $$1, col_off; \
		n = split($$2, words, " "); \
		line_length = ncol - indent; \
		for (i = 1; i <= n; i++) { \
			line_length -= length(words[i]) + 1; \
			if (line_length <= 0) { \
				line_length = ncol - indent - length(words[i]) - 1; \
				printf "\n%*s ", -indent, " "; \
			} \
			printf "%s ", words[i]; \
		} \
		printf "\n"; \
	}' \
	| more $(shell test $(shell uname) = Darwin && echo '--no-init --raw-control-chars')
