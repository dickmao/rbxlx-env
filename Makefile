EMACS ?= emacs
USER ?= $(shell whoami)
DESTINATION ?= ./src
RBXLX ?= $(shell ls -C *.rbxlx 2>/dev/null | cut -d' ' -f1)
ifeq ($(RBXLX),)
$(error No .rbxlx files found)
endif
WINE ?= $(shell which wine)
ifeq ($(WINE),)
WINE := true
endif

.PHONY: all
all: furl
	pkill RobloxStudio || true
	env WINEPREFIX="$$HOME/.wine" $(WINE) start C:\\users\\$(USER)\\Desktop\\Roblox\ Studio.lnk >/dev/null 2>&1

.PHONY: furl
furl: $(DESTINATION)
	$(EMACS) -Q -batch -L . -f package-initialize -l rbxlx --eval "(setq debug-on-error t)" --eval "(delete-directory (rbxlx-unfurl \"./$(RBXLX)\") t)" --eval "(rbxlx-furl \"./$(RBXLX)\" \"$(DESTINATION)\")"

$(DESTINATION):
	$(MAKE) unfurl

.PHONY: unfurl
unfurl:
	$(EMACS) -Q -batch -L . -f package-initialize -l rbxlx --eval "(delete-directory \"$(DESTINATION)\" t)" --eval "(copy-directory (rbxlx-unfurl \"./$(RBXLX)\") \"$(DESTINATION)\")"

.PHONY: clean
clean:
	rm -rf $(DESTINATION)
