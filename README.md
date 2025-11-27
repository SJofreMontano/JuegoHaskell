# 🕹️ HASKELL SURVIVORS: State Monad Edition

Juego estilo **Action RPG / Reverse Bullet Hell** (similar a Vampire Survivors), implementado completamente en **Haskell** utilizando la librería `gloss` y la arquitectura de la **Mónada `State`** para el manejo inmutable y secuencial del estado del juego.

### ✨ Características Implementadas

* **Arquitectura Funcional:** Uso de `Control.Monad.State` para manejar la lógica de actualización del juego de manera secuencial y legible.
* **Modularización:** Código dividido en módulos (`Types`, `Logic`, `Render`, `Input`) para facilitar la escalabilidad.
* **Sistema de Combate:** Disparo manual (Twin Stick Shooter) con cooldown.
* **Enemigos Variados:** Enemigos estándar (Grunt, 1 HP) y enemigos Tanque (Tank, 3 HP, más grandes y lentos).
* **Power-Ups:** Ítem coleccionable que permite lanzar un Burst Shot (onda expansiva de 128 balas) al presionar ESPACIO.
* **Delimitación de Arena:** Zona de juego visible y delimitada (1200x640) para el jugador y las balas.
* **Modo Display:** Pantalla completa (`FullScreen`).

---

## 🛠️ Requisitos del Sistema

Para compilar y ejecutar este proyecto, necesitas el entorno de Haskell y las librerías de gráficos de tu sistema operativo.

1.  **GHC** (Glasgow Haskell Compiler, versión 9.4+).
2.  **Cabal** (Herramienta de compilación y gestión de paquetes).
3.  **Librerías de OpenGL (Linux/Debian):** `gloss` necesita `freeglut`.

**Instalación de librerías en Linux (Obligatorio):**
```bash
sudo apt install freeglut3-dev