
# Define la herramienta principal 
CABAL = cabal

# Lista los targets que no corresponden a nombres de archivos
.PHONY: all run build clean

# -----------------------------------------------------------------
# Default Target (El que se ejecuta solo con 'make')
# -----------------------------------------------------------------
all: run

# -----------------------------------------------------------------
# build: Compila el código (No lo ejecuta)
# -----------------------------------------------------------------
build:
	@echo "--- Compilando el proyecto... ---"
	$(CABAL) build

# -----------------------------------------------------------------
# run: Compila si es necesario y ejecuta el juego
# -----------------------------------------------------------------
run:
	@echo "--- Ejecutando el juego... ---"
	$(CABAL) run

# -----------------------------------------------------------------
# clean: Elimina artefactos de compilación
# -----------------------------------------------------------------
clean:
	@echo "--- Limpiando artefactos de compilación ---"
	$(CABAL) clean
	# Elimina la carpeta principal de compilación para garantizar una limpieza total:
	rm -rf dist-newstyle