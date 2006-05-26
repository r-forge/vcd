SOURCES = JSS.Rnw

EVAL = TRUE

OBJECTS_LTX = $(SOURCES:.Rnw=.tex)
OBJECTS_DVI = $(SOURCES:.Rnw=.dvi)
OBJECTS_PDF = $(SOURCES:.Rnw=.pdf)

R = LC_ALL=en_US R --no-save --no-restore --quiet

all: pdf
pdf: $(OBJECTS_LTX) $(OBJECTS_PDF)
dvi: $(OBJECTS_LTX) $(OBJECTS_DVI)

.SUFFIXES: 
.SUFFIXES: .Rnw .tex .pdf .dvi

.Rnw.tex:
	@(echo "Sweave(file = \"$<\", out = \"$@\", \
	          eps = FALSE, stylepath = FALSE, eval = $(EVAL))") | $(R)
.tex.pdf:
	@$(R) CMD texi2dvi --pdf $<
.tex.dvi:
	@$(R) CMD texi2dvi $<

clean:
	@rm -f *.aux *.bbl *.blg *.dvi *.html *.log *.out *.Rout *.tpt \
	  $(SOURCES:.Rnw=)-*.{eps,pdf}
	@rm -f $(OBJECTS_LTX) Rplots.ps *.idx *.ilg *.ind
