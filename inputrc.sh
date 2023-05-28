# ignore case on TAB autocompletion
set completion-ignore-case on

# Details here: https://www.topbug.net/blog/2017/07/31/inputrc-for-humans/
# display possible completions using different colors to indicate their file type
set colored-stats on		       # Completion colors file/dir types
set colored-completion-prefix on       # Color matched prefix
set completion-prefix-display-length 5 # If longer completion, show `...`
set mark-symlinked-directories on      # Add / if syslink maps to dir
set show-all-if-ambiguous off          # Show all right away with partial
set show-all-if-unmodified off	       # Show all right away without partial
set visible-stats on	               # Completion appends file/dir type char 
