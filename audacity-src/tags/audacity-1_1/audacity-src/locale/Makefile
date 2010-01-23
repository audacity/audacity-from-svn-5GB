# locale/Makefile
#
# Matt Brubeck 2002-01-12
# based on the locale makefile from wxWindows 2.2.9
#  
# This is the makefile for generating the message catalog file and
# building lang.mo files from the translated lang.po catalogs.
# This makefile may be invoked to build either audacity.pot or any lang.mo

PROGNAME=audacity
LINGUAS=es fr ru

all: allmo

# Merge POT file with all existing message catalogs
allpo: $(LINGUAS:%=%.po) FORCE
allmo: $(LINGUAS:%=../%/$(PROGNAME).mo) FORCE

# Update the POT template file.
POTFILE=$(PROGNAME).pot 
update: force-update $(POTFILE) FORCE

# the programs we use (TODO: use configure to detect them)
MSGFMT=msgfmt -v
MSGMERGE=msgmerge
XGETTEXT=xgettext
XARGS=xargs

# common xgettext args: C++ syntax, use the specified macro names as markers
XGETTEXT_ARGS=-C -k_ -s -j

# implicit rules

../%/$(PROGNAME).mo: %.po
	@test -d ../$* || mkdir ../$*
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

force-update: FORCE
	rm -f $(POTFILE)

FORCE:
