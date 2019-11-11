## Makefile

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

PACKAGE_NAME := feather
REPO_USER    := conao3
REPO_NAME    := feather.el
VERSIONS     := 26.1 26.2

EMACS        ?= emacs
LOADPATH     := $$(cask load-path)
BATCH        := EMACSLOADPATH=$(LOADPATH) $(EMACS) -Q --batch

TESTFILE     := $(PACKAGE_NAME)-tests.el
ELS          := $(shell cask files)

GIT_HOOKS    := pre-commit

MAKEARGS     := --no-print-directory

export PACKAGE_NAME
export ELS
export TESTFILE

##################################################

.PHONY: all git-hook localbuild localtest up down build test clean
.PRECIOUS: $(VERSIONS:%=.docker/emacs-%/Makefile)

all: git-hook help

git-hook: $(GIT_HOOKS:%=.git/hooks/%)

help:
	$(info )
	$(info Commands)
	$(info ========)
	$(info   - make             # Install git-hook to your local .git folder)
	$(info   - make test        # Test $(PACKAGE_NAME) via docker-compose)
	$(info   - make down        # Down docker-compose)
	$(info )
	$(info Commands using your emacs)
	$(info =========================)
	$(info   - make localtest   # Test $(PACKAGE_NAME) via your `emacs`)
	$(info )
	$(info Cleaning)
	$(info ========)
	$(info   - make clean       # Clean compiled files, docker conf files)
	$(info )
	$(info Required `cask`, `docker` and `docker-compose`.)
	$(info See https://github.com/$(REPO_USER)/$(REPO_NAME)#contribution)
	$(info )

##############################

localbuild: $(ELS:%.el=%.elc)
localtest: localbuild
	$(BATCH) -l $(TESTFILE) -f cort-test-run

##############################

.git/hooks/%: git-hooks/%
	cp -a $< $@

%.elc: %.el .cask
	$(BATCH) -f batch-byte-compile $<

##############################
#
#  dynamic resources
#

# listed below will remove via `make clean`
GABAGES := .cask .env .docker.env .docker

.cask: Cask
	cask install
	find $@ -name "*.elc" | xargs rm -f
	touch $@

.docker/emacs-%/Makefile: Makefile-docker.mk
	mkdir -p $(@D)
	cp $< $@

.env:
	echo "PACKAGE_NAME=$(PACKAGE_NAME)" > $@

.docker.env: .cask
	echo "EMACSLOADPATH=$$(cask load-path | tr ':' '\n' | \
grep -o '\.cask.*' | sed 's|^|/$(PACKAGE_NAME)/|g' | \
tr '\n' ':' | sed 's|^|/$(PACKAGE_NAME)/:|g')" > $@

##############################
#
#  docker managemant
#

.docker/up: .cask .env .docker.env $(VERSIONS:%=.docker/emacs-%/Makefile)
	docker-compose up -d --no-recreate
	touch .docker/up

up: .docker/up
down:
	if [ -e .docker/up ]; then docker-compose down; fi
	rm -rf .docker/up

build: $(VERSIONS:%=.make-build-%)
test:  $(VERSIONS:%=.make-test-%)

.make-build-%: up
	$(MAKE) $(MAKEARGS) build -C .docker/emacs-$* VERSION=$* 

.make-test-%: up
	$(MAKE) $(MAKEARGS) test -C .docker/emacs-$* VERSION=$*

##############################

clean: down
	rm -rf $(ELS:%.el=%.elc) $(GABAGES)
