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

# Host / Project configuration
# ============================
# Variables in this section configure the project, host etc to use at
# Sourceforge.
# $SFUSER must be set from outside to a valid SF user name, but it will be 
# different for everyone who uses this file, so no point setting it here.
# project unix name
PROJECT=audacity
# host files are on
SFHOST=web.sourceforge.net
# where the files are kept. The trailing / is important.
PROJDIR=/home/groups/a/au/audacity/htdocs/

# Internal configuration stuff applies to all projects
# options to all rsync commands
RSYNCOPTS=-vrtchlO --delete-during --rsh=ssh --exclude '*~' --exclude '*.po'
# -v Verbose mode transfers
# -r Recursive transfer of whole dir tree
# -t transfer modification times of files
# -c Use file's md4 checksums to see if they need to be transfered (slower but
#    more reliable than using size and modtimes of files)
# -h Output data in human-readable formats
# -l Preserve links in the uploaded data
# -O Ommit directory modification times (because they are largely meaningless)
#--delete-during	delete deleted files as they are found (uses minimum disk
#					space but is safe against aborted transfers)
#--exclude '*~'		avoid moving editor temp files
#--exclude '*.po'	don't move .po files (source code of translations) to/from
#					server because it can't use them (it only wants the .mo
#					compiled versions)
RSYNCOPTS+=--exclude 'CVS' --exclude '.cvsignore'
# Exclude CVS files we don't need on the server
RSYNCOPTS+=--exclude '*.bak' --exclude '/files' --itemize-changes
# Exclude backup files from windows editors as well
# Exclude the contents of the /files directory used to host temporary download
# files.
# --itemize-changes 	show what is being changed on each file as it happens
# construct some longer strings to use in commands
UPLOADURL=$(SFUSER),$(PROJECT)@$(SFHOST):$(PROJDIR)

pull:
	rsync $(RSYNCOPTS) $(UPLOADURL) .

pretend-pull:
	rsync $(RSYNCOPTS) --dry-run $(UPLOADURL) .


push:
	rsync $(RSYNCOPTS) . $(UPLOADURL)

pretend-push:
	rsync $(RSYNCOPTS) --dry-run . $(UPLOADURL)
