build:
	elm make src/Main.elm --output=main.js

# TODO: broken due to Debug usage
release:
	elm make src/Main.elm --output=main.js --optimize

dist: build
	mkdir -p dist/style
	cp -r style/* dist/style
	cp -f index.html dist/index.html
	cp main.js dist/main.js

deploy: dist
	rsync -rv --delete \
		--exclude=".DS_Store" \
		--exclude="*.swp" \
		./dist/* mordheim.app:/home/caddy/mordheim.app/

.PHONY: build release deploy
