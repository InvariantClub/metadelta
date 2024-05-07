watch-css: ## generate the css and watch for changes
	echo css/Css.hs | entr -cs 'runghc css/Css.hs>/tmp/css.css && cp /tmp/css.css ./static/css.css'

serve: ## run the elm-land server
	npx elm-land server

help: ## See a list of all available targets
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' Makefile | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-10s\033[0m %s\n", $$1, $$2}'

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
