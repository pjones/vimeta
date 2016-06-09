################################################################################
export TMPDIR = $(HOME)/tmp

################################################################################
TMP_DUMMY     = $(TMPDIR)/.dummy
STACK_OPTS    = --stack-yaml=build/stack.yaml
PKG_ROOT_PATH = $(shell stack $(STACK_OPTS) path --local-install-root)
DEST_DIR      = $(HOME)/bin

################################################################################
.PHONY: all test clean install

################################################################################
all: $(TMP_DUMMY)
	stack $(STACK_OPTS) setup
	stack $(STACK_OPTS) build
	hlint src

################################################################################
test:
	stack $(STACK_OPTS) test

################################################################################
clean:
	stack $(STACK_OPTS) clean

################################################################################
install: all
	install -m 0755 $(PKG_ROOT_PATH)/bin/vimeta $(DEST_DIR)/vimeta


################################################################################
$(TMP_DUMMY):
	mkdir -p $(dir $@)
	touch $@
