EMACS ?= emacs
USER ?= $(shell whoami)
DESTINATION ?= ./src
LIBDIR ?= .
RBXLX ?= $(shell ls -C *.rbxlx 2>/dev/null | cut -d' ' -f1)
WINE ?= $(shell which wine)
ifeq ($(WINE),)
WINE := true
endif

.PHONY: all
all: furl
	pkill RobloxStudio || true
	env WINEPREFIX="$$HOME/.wine" $(WINE) start C:\\users\\$(USER)\\Desktop\\Roblox\ Studio.lnk >/dev/null 2>&1

.PHONY: proceed
proceed:
	@bash -c "[ -n \"$(RBXLX)\" ] || ( echo \"*** No .rbxlx files found\" && false )"

.PHONY: furl
furl: $(DESTINATION)
	$(EMACS) -Q -batch -L $(LIBDIR) -f package-initialize -l rbxlx-env --eval "(setq make-backup-files nil)" --eval "(setq debug-on-error t)" --eval "(delete-directory (rbxlx-unfurl \"./$(RBXLX)\") t)" --eval "(rbxlx-furl \"./$(RBXLX)\" \"$(DESTINATION)\")"

$(DESTINATION):
	$(MAKE) unfurl

.PHONY: unfurl
unfurl: proceed
	$(EMACS) -Q -batch -L $(LIBDIR) -f package-initialize -l rbxlx-env --eval "(delete-directory \"$(DESTINATION)\" t)" --eval "(copy-directory (rbxlx-unfurl \"./$(RBXLX)\") \"$(DESTINATION)\")"

.PHONY: clean
clean:
	rm -rf $(DESTINATION)

README.rst: README.in.rst rbxlx-env.el
	grep ';;' rbxlx-env.el \
	    | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	    | sed -e 's/^\s*;;*\s*//g' \
	    | tools/readme-sed.sh "COMMENTARY" README.in.rst > README.rst
