POT_FILE=locale/audacity_website.pot
PO_FILES_ORIG := $(wildcard locale/*.po)
PO_FILES := $(PO_FILES_ORIG:%.po=%/LC_MESSAGES/audacity_website.po)
MO_FILES := $(PO_FILES:%.po=%.mo)

all: $(MO_FILES)

# Build the POT file (message catalog template).
SOURCES := $(wildcard *.php */*.php)
XGETTEXT=xgettext
XGETTEXT_ARGS=-L php --add-comments="i18n-hint" --from-code="UTF-8"

$(POT_FILE): $(SOURCES)
	find . -name '*.php' ! -path './old/*' \
	  | xargs $(XGETTEXT) $(XGETTEXT_ARGS) -o $@
	#chmod go+r $@

# Update the PO files (translated message catalogs).
%/LC_MESSAGES/audacity_website.po: %.po $(POT_FILE)
	mkdir -p $(@:%/audacity_website.po=%)
	locale/smartmsgmerge.py $< $(POT_FILE) $@
	cp -f $@ $<

# Compile the PO files into MO files.
# -f is for fuzzy string matching
# -v is for verbose
MSGFMT=msgfmt
MSGFMT_ARGS=-fv

%.mo: %.po
	$(MSGFMT) $(MSGFMT_ARGS) -o $@ $<
