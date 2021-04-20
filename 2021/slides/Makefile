actors.html: actors.md
	pandoc -t revealjs -i -s --slide-level 2 --highlight-style breezedark \
		-o actors.html actors.md

watch: *.md
	  fswatch -o $^ | xargs -n1 -I{} make
