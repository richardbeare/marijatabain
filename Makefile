HTML_FILES := $(patsubst %.Rmd, %.html ,$(wildcard *.Rmd))

all: html


html: $(HTML_FILES)

%.html: %.Rmd compile.R
	Rscript compile.R "$<"

.PHONY: clean
clean:
	$(RM) $(HTML_FILES)
	$(RM) -r libs
