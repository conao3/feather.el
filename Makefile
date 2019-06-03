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

include Makefunc.mk

TOP         := $(dir $(lastword $(MAKEFILE_LIST)))
EMACS_RAW   := $(filter-out emacs-undumped, $(shell compgen -c emacs- | xargs))
AVAILABLE   := $(strip $(sort $(EMACS_RAW)))
ALL_EMACS   := $(filter $(AVAILABLE),emacs-24.5 emacs-25.3 emacs-26.1)

EMACS       ?= emacs

BATCH       := $(EMACS) -Q --batch -L $(TOP)

TESTFILE    := feather-tests.el
ELS         := feather.el
ELS           += feather-polyfill.el

CORTELS     := $(TESTFILE) cort-test.el
CORT_ARGS   := -l $(TESTFILE) -f cort-run-tests

LOGFILE     := .make-check.log

##################################################
# $(if $(findstring 22,$(shell $* --version)),[emacs-22],[else emacs-22])

all: git-hook $(ELS:.el=.elc)

git-hook:
	cp -a git-hooks/* .git/hooks/

include Makefile-check.mk

##############################
#  test on all Emacs

allcheck: $(ALL_EMACS:%=.make-check-%)
	@echo ""
	@cat $(LOGFILE) | grep =====
	@rm $(LOGFILE)

.make-check-%:
	mkdir -p .make-$*
	cp -f $(ELS) $(CORTELS) .make-$*/
	cp -f Makefile-check.mk .make-$*/Makefile
	$(MAKE) -C .make-$* clean
	$(call EXPORT,ELS CORT_ARGS) \
	  EMACS=$* $(MAKE) -C .make-$* check 2>&1 | tee -a $(LOGFILE)
	rm -rf .make-$*

##############################
#  silent `allcheck' job

test: $(ALL_EMACS:%=.make-test-%)
	@echo ""
	@cat $(LOGFILE) | grep =====
	@rm $(LOGFILE)

.make-test-%:
	mkdir -p .make-$*
	cp -f $(ELS) $(CORTELS) .make-$*/
	cp -f Makefile-check.mk .make-$*/Makefile
	$(MAKE) -C .make-$* clean
	$(call EXPORT,ELS CORT_ARGS) \
	  EMACS=$* $(MAKE) -C .make-$* check 2>&1 >> $(LOGFILE)
	rm -rf .make-$*
