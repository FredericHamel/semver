
##CURRENT_BRANCH_OR_TAG=$(shell git describe --all | grep -Eo '[A-Za-z0-9.-]+$$')
CURRENT_BRANCH_OR_TAG=$(notdir $(CURDIR))

all:
	@echo "Current Branch or tag $(CURRENT_BRANCH_OR_TAG)"

ut: _setup_.scm
	@gsi tests.scm

_setup_.scm:
	@echo '; Automatically generated' > "$@"
	@echo "(define-library-alias (semver) (github.com/FredericHamel/semver/tree/$(CURRENT_BRANCH_OR_TAG)))" >> "$@"

clean:
	@rm -f _setup_.scm
.PHONY: all ut clean
