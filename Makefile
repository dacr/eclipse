# eclipse - aide-mémoire
#
# Les photos sont attendues dans le lien `photos-eclipse` du répertoire courant.
# Tout se règle par variables :
#
#   make analyze PHOTOS=/ailleurs/mes-photos
#   make compose OUT=posters ARGS="--annotate --caption 'Éclipse du 12 août 2026'"
#
# L'étape `analyze` est la seule longue : elle décode les RAW et mesure chaque
# photo, une fois pour toutes. Tout le reste repart du fichier de mesures.

PHOTOS       ?= photos-eclipse
OUT          ?= out
CACHE        ?= .eclipse-cache
MEASUREMENTS ?= $(OUT)/measurements.csv
COMPOSITE    ?= $(OUT)/composite.png
SBT          ?= sbt
ARGS         ?=

# position de l'observateur, utile seulement si les EXIF ne portent pas de GPS
# exemple : make analyze OBSERVER=43.6047,1.4442,150
OBSERVER ?=
OBSERVER_OPTION := $(if $(OBSERVER),--observer $(OBSERVER),)


.DEFAULT_GOAL := help

## ---------------------------------------------------------------- mise en route

.PHONY: help
help: ## affiche cette aide
	@echo "eclipse - composition d'une séquence d'éclipse solaire"
	@echo
	@grep -hE '^[a-zA-Z0-9_-]+:.*?## ' $(MAKEFILE_LIST) \
	  | sort \
	  | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}'
	@echo
	@echo "variables : PHOTOS=$(PHOTOS) OUT=$(OUT) CACHE=$(CACHE) OBSERVER=$(OBSERVER)"
	@echo "            ARGS=\"...\" pour passer n'importe quelle option à la commande"

.PHONY: check
check: ## vérifie que les photos et les outils sont là
	@test -e $(PHOTOS) || { echo "absent : $(PHOTOS) (lien ou répertoire de photos)"; exit 1; }
	@echo "photos    : $(PHOTOS) -> $$(readlink -f $(PHOTOS))"
	@echo "fichiers  : $$(find -L $(PHOTOS) -maxdepth 2 -type f | wc -l)"
	@echo "extensions: $$(find -L $(PHOTOS) -maxdepth 2 -type f | sed 's/.*\.//' | sort -u | tr '\n' ' ')"
	@for tool in dcraw_emu darktable-cli rawtherapee-cli magick; do \
	  command -v $$tool >/dev/null && echo "décodeur  : $$tool"; \
	done; \
	command -v dcraw_emu darktable-cli rawtherapee-cli magick >/dev/null 2>&1 \
	  || echo "décodeur  : AUCUN, installer libraw-bin (dcraw_emu) ou darktable"

## ---------------------------------------------------------------- le travail

.PHONY: analyze
analyze: ## mesure toutes les photos -> $(MEASUREMENTS)  (long, une seule fois)
	@mkdir -p $(OUT)
	$(SBT) -batch "cli/run analyze $(PHOTOS) --out $(MEASUREMENTS) --cache $(CACHE) $(OBSERVER_OPTION) $(ARGS)"

.PHONY: plan
plan: ## dit ce qui serait dessiné, sans rien dessiner
	$(SBT) -batch "cli/run plan $(MEASUREMENTS) --cache $(CACHE) $(ARGS)"

.PHONY: compose
compose: ## composite sur la trajectoire réelle -> $(COMPOSITE)
	@mkdir -p $(OUT)
	$(SBT) -batch "cli/run compose $(MEASUREMENTS) --cache $(CACHE) --out $(COMPOSITE) $(ARGS)"

.PHONY: preview
preview: ## composite rapide en petit, pour juger avant de lancer le grand
	@mkdir -p $(OUT)
	$(SBT) -batch "cli/run compose $(MEASUREMENTS) --cache $(CACHE) --out $(OUT)/preview.png --disc-radius 26 $(ARGS)"

## ---------------------------------------------------------------- variantes

.PHONY: compose-even
compose-even: ## même trajectoire, espacement régularisé (rendu affiche)
	@mkdir -p $(OUT)
	$(SBT) -batch "cli/run compose $(MEASUREMENTS) --cache $(CACHE) --layout sky-path-even --out $(OUT)/composite-even.png $(ARGS)"

.PHONY: compose-timeline
compose-timeline: ## bande chronologique rectiligne, l'évolution seule
	@mkdir -p $(OUT)
	$(SBT) -batch "cli/run compose $(MEASUREMENTS) --cache $(CACHE) --layout timeline --out $(OUT)/composite-timeline.png $(ARGS)"

.PHONY: compose-grid
compose-grid: ## planche contact
	@mkdir -p $(OUT)
	$(SBT) -batch "cli/run compose $(MEASUREMENTS) --cache $(CACHE) --layout grid --out $(OUT)/composite-grid.png $(ARGS)"

.PHONY: compose-annotated
compose-annotated: ## composite avec l'heure sous chaque vue
	@mkdir -p $(OUT)
	$(SBT) -batch "cli/run compose $(MEASUREMENTS) --cache $(CACHE) --annotate --out $(OUT)/composite-annotated.png $(ARGS)"

.PHONY: all-layouts
all-layouts: compose compose-even compose-timeline compose-grid ## produit les quatre mises en page

## ---------------------------------------------------------------- développement

.PHONY: build
build: ## compile tout
	$(SBT) -batch compile

.PHONY: test
test: ## joue les tests
	$(SBT) -batch test

.PHONY: sample
sample: ## fabrique une séance d'exemple dans $(OUT)/sample, sans aucun RAW
	$(SBT) -batch "composer/Test/runMain fr.janalyse.eclipse.composer.SampleSessionGenerator $(OUT)/sample 120"
	$(SBT) -batch "cli/run compose $(OUT)/sample/measurements.csv --cache $(OUT)/sample/cache --out $(OUT)/sample/composite.png"

.PHONY: usage
usage: ## affiche l'aide de la ligne de commande
	$(SBT) -batch "cli/run"

## ---------------------------------------------------------------- ménage

.PHONY: clean-cache
clean-cache: ## efface les RAW décodés ($(CACHE))
	rm -rf $(CACHE)

.PHONY: clean-out
clean-out: ## efface les images produites ($(OUT)), mesures comprises
	rm -rf $(OUT)

.PHONY: clean
clean: ## efface les produits de compilation
	$(SBT) -batch clean

.PHONY: distclean
distclean: clean clean-cache clean-out ## efface tout ce qui est reproductible
