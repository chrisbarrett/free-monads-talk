CSS = https://gist.githubusercontent.com/ryangray/1882525/raw/2a6e53f645b960f0bed16d686ba3df36505f839f/buttondown.css

.PHONY: all clean

all: FreeMonads.html

FreeMonads.html: FreeMonads.lhs
	pandoc -f markdown_github+lhs -c $(CSS) --self-contained -t html $< -o $@

clean:
	rm *.html
