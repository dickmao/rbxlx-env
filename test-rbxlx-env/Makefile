LIBDIR := ..
CASK_LOC := ..
include ../Makefile

.DEFAULT_GOAL := test-rbxlx-env

.PHONY: test-rbxlx-env
test-rbxlx-env:
	cp Baseplate.rbxlx.orig Baseplate.rbxlx
	$(MAKE) unfurl
	sed -i 's/foobar/foobaz/' $(DESTINATION)/ReplicatedStorage/ModuleScript/Folder/LocalScript/LocalScript/{1996D5C4-6DD8-4E53-8432-FBD2ED97483A}.lua
	$(MAKE) furl
	bash -c "diff <(cat Baseplate.rbxlx.orig | sed -e 's/foobar/foobaz/') Baseplate.rbxlx"
	@echo "*** Passed"

.PHONY: test-rbxlx-env-clean
test-rbxlx-env-clean:
	rm -rf $(DESTINATION) Baseplate.rbxlx
