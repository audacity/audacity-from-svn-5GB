# locale/Makefile
#
# Matt Brubeck 2002-01-12
# based on the locale makefile from wxWindows 2.2.9
#  
# This is the makefile for generating the message catalog file and
# building lang.mo files from the translated lang.po catalogs.
# This makefile may be invoked to build either audacity.pot or any lang.mo

PROGNAME=audacity

all: allmo

# Files 
POFILES=*.po
POTFILE=$(PROGNAME).pot 

# Update the POT template file.
update: force-update $(POTFILE)

# Merge POT file with all existing message catalogs
allpo: $(POFILES)
allmo: $(POFILES:%.po=../%/$(PROGNAME).mo)

# the programs we use (TODO: use configure to detect them)
MSGFMT=msgfmt -v
MSGMERGE=msgmerge
XGETTEXT=xgettext
XARGS=xargs

# common xgettext args: C++ syntax, use the specified macro names as markers
XGETTEXT_ARGS=-C -k_ -kwxGetTranslation -kwxTRANSLATE -s -j

# implicit rules
../%/$(PROGNAME).mo: %.po
	$(MSGFMT) -o $@ $<


%.po: $(POTFILE)
	@if [ -f $@ ]; then \
		echo "Updating catalog: $@"; \
		$(MSGMERGE) $@ $(POTFILE) > $@.new && mv $@.new $@; \
	else \
		echo "Creating new catalog: $@"; \
		cp $(POTFILE) $@; \
	fi

$(POTFILE):
	touch $@
	find ../src -name "*.cpp" -or -name "*.h" \
		| $(XARGS) $(XGETTEXT) $(XGETTEXT_ARGS) -o $(POTFILE)

force-update:
	rm -f $(POTFILE)

.PHONY: allpo allmo update-pot
FORCE:
