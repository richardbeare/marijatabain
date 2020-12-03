HTML_FILES := $(patsubst %.Rmd, %.html ,$(wildcard *.Rmd))

all: html


html: $(HTML_FILES)

%.html: %.Rmd compile.R mkMaps.R mkMaps2.R ipastuff.Rda IPA_Illustrations_enc.xlsx
	Rscript compile.R "$<"

.PHONY: clean
clean:
	$(RM) $(HTML_FILES)
	$(RM) -r libs
