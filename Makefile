ORIG_DIR = obelisk
TGT_DIR  = ~
VERBOSE  = 2

link:
	stow -S -t $(TGT_DIR) --verbose=$(VERBOSE) --dotfiles $(ORIG_DIR);

overwrite:
	stow -S -t $(TGT_DIR) --verbose=$(VERBOSE) --dotfiles --adopt  $(ORIG_DIR);

unlink:
	stow -D -t $(TGT_DIR) --verbose=$(VERBOSE) --dotfiles $(ORIG_DIR);

relink:
	stow -R -t $(TGT_DIR) --verbose=$(VERBOSE) --dotfiles $(ORIG_DIR);
