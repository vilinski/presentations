actors.md: slides/actors.md
	pandoc -t revealjs -i -s --slide-level 2 --highlight-style breezedark \
		-o slides/actors.html slides/actors.md

watch: slides/*.md
	  fswatch -o $^ | xargs -n1 -I{} make

reveal: 2019-ActorSystems/slides/index.md
	reveal-md --preprocessor preprocessor.js 2019-ActorSystems/slides/index.md
