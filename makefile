# Makefile
PROJECT_NAME = Juego
CABAL = cabal

.PHONY: all run build clean

all: run


build:
	@echo "--- Compilando el proyecto (usando un solo núcleo para ahorrar RAM) ---"
	$(CABAL) build --jobs=1
run:
	@echo "--- Ejecutando el juego... ---"
	$(CABAL) run

clean:
	@echo "--- Limpiando artefactos de compilación ---"
	$(CABAL) clean
	rm -rf dist-newstyle