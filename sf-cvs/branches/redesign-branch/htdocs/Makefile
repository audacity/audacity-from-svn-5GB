POT_FILE=locale/audacity_website.pot
PO_FILES := $(wildcard locale/*/LC_MESSAGES/audacity_website.po)
MO_FILES := $(PO_FILES:%.po=%.mo)

all: $(MO_FILES)

# Build the POT file (message catalog template).
SOURCES := $(wildcard *.php */*.php)
XGETTEXT=xgettext
XGETTEXT_ARGS=-L php --add-comments="i18n-hint" --from-code="UTF-8"
$(POT_FILE): $(SOURCES)
	find . -name '*.php' ! -path './_darcs/*' | xargs $(XGETTEXT) $(XGETTEXT_ARGS) -o $@
	chmod go+r $@

# Update the PO files (translated message catalogs).
MSGMERGE=msgmerge
MSGMERGE_ARGS=-U
%.po: $(POT_FILE)
	$(MSGMERGE) $(MSGMERGE_ARGS) $@ $<
	touch $@

# Compile the PO files into MO files.
MSGFMT=msgfmt
MSGFMT_ARGS=-fv
%.mo: %.po
	$(MSGFMT) $(MSGFMT_ARGS) -o $@ $<
