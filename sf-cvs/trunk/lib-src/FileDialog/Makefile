
CXXFLAGS = -O2
CXXFLAGS += $(shell wx-config --unicode=yes --cxxflags)

ifneq ($(findstring __WXGTK__,$(CXXFLAGS)),)
CXXFLAGS += $(shell pkg-config gtk+-2.0 --cflags)
endif

all: FileDialog.a

clean:
	rm -f *.o *.a

distclean: clean

FileDialog.a: FileDialog.o
	$(AR) $(ARFLAGS) $@ $<

FileDialog.o: FileDialog.cpp FileDialog.h

