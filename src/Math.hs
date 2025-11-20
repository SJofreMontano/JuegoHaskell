module Math where


--Funciones matematicas 
import Types (Pos, Vel)

--Resta de vectores
--Se usa para calcular la direccion que un enemigo debe de seguir
--para perseguir al jugador
sub :: Pos -> Pos -> (Float, Float)
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)


--Calcula la longitud de un vector
--Calcula la distancia entre dos objetos para poder calcular la colision
mag :: (Float, Float) -> Float
mag (x, y) = sqrt (x*x + y*y)


--Normaliza un vector y se asegura que el movimiento diagonal no se mas rapido
normalize :: (Float, Float) -> (Float, Float)
normalize v@(x, y) 
  | m == 0    = (0, 0)
  | otherwise = (x/m, y/m)
  where m = mag v


--Restringe un valor numerico 
--Se usa para poder delimitar la arena de juego
clamp :: Float -> Float -> Float -> Float
clamp minVal maxVal val = max minVal (min maxVal val)


--Convertir la informacion booleana de las teclas en un vector
getDirectionFromKeys :: (Bool, Bool, Bool, Bool) -> Vel
getDirectionFromKeys (u, d, l, r) = 
    let vx = (if r then 1 else 0) + (if l then -1 else 0)
        vy = (if u then 1 else 0) + (if d then -1 else 0)
    in normalize (vx, vy)