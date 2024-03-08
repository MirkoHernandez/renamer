SOURCES =			\
	renamer/renamer.scm	\
	renamer/regex.scm	\
	renamer/utils.scm	\
	renamer/operations.scm

GOBJECTS = $(SOURCES:%.scm=%.go)

# Installation directories.
moddir=$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

# Install source and go files.
nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Trick to have mtime of  installed compiled  files greater than  that of
# installed source files. 
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

CLEANFILES = $(GOBJECTS)

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat

# Relate scm and go files
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"

bin_SCRIPTS =		\
	scripts/renamer \
	scripts/.renamer-real

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
