# locale/Makefile
#
# Matt Brubeck 2002-01-12
# based on the locale makefile from wxWindows 2.2.9
#  
# This is the makefile for generating the message catalog file and
# building lang.mo files from the translated lang.po catalogs.
# This makefile may be invoked to build either audacity.pot or any lang.mo

# Autodetect the languages we support.	Currently this relies on make
# being called with this dir as the cwd, but if we generate this file
# with configure an explicit path should be specified -- RL.

WX_LINGUAS := `ls *.po 2> /dev/null | sed -n 's/^\(.*\)\.po$$/\1/p'`

# the programs we use (TODO: use configure to detect them)
MSGFMT=msgfmt -v
MSGMERGE=msgmerge
XGETTEXT=xgettext
XARGS=xargs

# common xgettext args: C++ syntax, use the specified macro names as markers
XGETTEXT_ARGS=-C -k_ -kwxGetTranslation -kwxTRANSLATE -s -j

# implicit rules
%.mo: %.po
	$(MSGFMT) -o $@ $<

# a PO file must be updated from the template to include new translations
POTFILE=audacity.pot 

%.po: $(POTFILE)
	if [ -f $@ ]; then $(MSGMERGE) $@ $(POTFILE) > $@.new && mv $@.new $@; \
	else cp $(POTFILE) $@; fi

$(POTFILE):
	touch $@
	find ../src -name "*.h" \
		| $(XARGS) $(XGETTEXT) $(XGETTEXT_ARGS) -o $(POTFILE)
	find ../src -name "*.cpp" \
		| $(XARGS) $(XGETTEXT) $(XGETTEXT_ARGS) -o $(POTFILE)

allpo: update
	@for t in $(WX_LINGUAS); do $(MAKE) $$t.po; done

allmo:
	@for t in $(WX_LINGUAS); do $(MAKE) $$t.mo; done

update:
	$(RM) $(POTFILE)
	$(MAKE) $(POTFILE)

# targets depending on this one will be always remade
FORCE:

.PHONY: allpo allmo force-update
