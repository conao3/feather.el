## Makefile-docker.mk

# Copyright (C) 2019  Naoya Yamashita

# This program is free software: you can redistribute it and/or modify
# it under the terms of the Affero GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the Affero GNU General Public License for more details.

# You should have received a copy of the Affero GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

all:

PACKAGE_NAME ?=
VERSION ?=
ELS ?=
TESTFILE ?=

EMACS ?= emacs
BATCH := $(EMACS) --batch

##################################################

.PHONY: all init build test clean

all: init

init: $(ELS) $(TESTFILE) .cask
build: init $(ELS:%.el=%.elc)
test: build
	docker exec -t $(PACKAGE_NAME)_emacs-$(VERSION) sh -c "cd $(PACKAGE_NAME); $(BATCH) -l $(TESTFILE) -f cort-test-run"

##############################

.cask: ../../.cask
	cp -rf $< $@

%.el: ../../%.el
	cp -rf $< $@

%.elc: %.el
	docker exec -t $(PACKAGE_NAME)_emacs-$(VERSION) sh -c "cd $(PACKAGE_NAME); $(BATCH) -f batch-byte-compile $<"

##############################

clean:
	rm -rf $(ELS:%.el=%.elc)
