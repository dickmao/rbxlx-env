EMACS ?= emacs

CASK := $(shell which cask)
ifeq ($(CASK),)
$(error Please install CASK at https://cask.readthedocs.io/en/latest/guide/installation.html)
endif
CASK_LOC ?= .
CASK_DIR := $(shell $(CASK) package-directory --path $(CASK_LOC) || exit 1)

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


.PHONY: cask
cask: $(CASK_DIR)

$(CASK_DIR): $(CASK_LOC)/Cask
	$(CASK) install --path $(CASK_LOC)
	touch $(CASK_DIR)

.PHONY: proceed
proceed:
	@bash -c "[ -n \"$(RBXLX)\" ] || ( echo \"*** No .rbxlx files found\" && false )"

.PHONY: furl
furl: cask $(DESTINATION)
	EMACSLOADPATH=`$(CASK) load-path --path $(CASK_LOC)` PATH=`$(CASK) path --path $(CASK_LOC)` $(EMACS) -Q -batch  -f package-initialize -L $(LIBDIR) -l rbxlx-env --eval "(setq make-backup-files nil)" --eval "(setq debug-on-error t)" --eval "(delete-directory (rbxlx-unfurl \"./$(RBXLX)\") t)" --eval "(rbxlx-furl \"./$(RBXLX)\" \"$(DESTINATION)\")"

$(DESTINATION):
	$(MAKE) unfurl

.PHONY: cask unfurl
unfurl: proceed
	EMACSLOADPATH=`$(CASK) load-path --path $(CASK_LOC)` PATH=`$(CASK) path --path $(CASK_LOC)` $(EMACS) -Q -batch -f package-initialize -L $(LIBDIR) -l rbxlx-env --eval "(setq debug-on-error t)" --eval "(when (file-exists-p \"$(DESTINATION)\") (delete-directory \"$(DESTINATION)\" t))" --eval "(copy-directory (rbxlx-unfurl \"./$(RBXLX)\") \"$(DESTINATION)\")"

.PHONY: clean
clean:
	rm -rf $(DESTINATION)
	$(MAKE) -C test-rbxlx-env test-rbxlx-env-clean

README.rst: README.in.rst rbxlx-env.el
	grep ';;' rbxlx-env.el \
	    | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	    | sed -e 's/^\s*;;*\s*//g' \
	    | tools/readme-sed.sh "COMMENTARY" README.in.rst > README.rst

.PHONY: test
test: cask
	! ($(CASK) eval --path $(CASK_LOC) "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; $(CASK) clean-elc --path $(CASK_LOC) && exit $$ret)
	$(MAKE) -C test-rbxlx-env
