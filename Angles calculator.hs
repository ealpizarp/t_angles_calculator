{-

                                            Tecnológico de Costa Rica
                                Proyecto Programado Lenguajes de programacion (IC4700)
Autores:Eric Alpizar.
        Jacob Picado.
        Natalia Vargas.
        Patrick Vindas.

Fecha de ultima modificación: 07/06/2021.
-}

import Data.List
import System.IO
import Debug.Trace




collinearPoints :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool

{- 
Function: Calculates if three given points are collinear or not
 * --------------------------------------------------
 *  
 *   Recieves
 *
 *   1 (Float, Float): A valid pair of coordinates in R2 
 *   2 (Float, Float): A valid pair of coordinates in R2
 *   3 (Float, Float): A valid pair of coordinates in R2
 *
 *   Returns: True if the points given are collinear, false if not
-}

collinearPoints (x1, y1) (x2, y2) (x3,y3)

    -- A comparation of the slope created by the points (x1, y1) (x2, y2)
    -- and (x2, y2) (x3,y3) is made here.  

    | (snd (x3, y3) - snd (x2, y2)) * (fst (x2, y2) - fst (x1, y1)) == (snd (x2, y2) - 
    snd (x1, y1)) * (fst (x3, y3) - fst (x2, y2)) = True 

    -- The formula for calculating a slope between two points is y2 - y1 / x2 - x1. A simplified
    -- version of this is used above

    | otherwise = False





dotProduct2P :: (Float, Float) -> (Float, Float) -> Float

{-
Function: Calculates if three given points are collinear or not
 * --------------------------------------------------
 *  
 *   Recieves
 *
 *   1 (Float, Float): A valid pair of coordinates in R2 
 *   2 (Float, Float): A valid pair of coordinates in R2
 *   3 (Float, Float): A valid pair of coordinates in R2
 *
 *   Returns: True if the points given are collinear and false if not
-}

-- The formula for calculating the dot product of two given vectors is used here

dotProduct2P (x1, y1) (x2, y2) = fst(x1, y1) * fst (x2, y2) + snd (x1, y1) * snd (x2, y2)





computeMagnitudeVect :: (Float, Float) -> Float

{-
Function: Calculates the magnitude of given vector in R2
 * --------------------------------------------------
 *  
 *   Recieves
 *
 *   (Float, Float): A valid pair of coordinates in R2 
 *
 *   Returns: The calculated magnitude of the vector
-}

computeMagnitudeVect (x1, y1) = sqrt( ( fst (x1, y1) ) ** 2 + ( snd (x1, y1) ) ** 2 ) 





computeAngle3P :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Float

{-
Function: Calculates the angle of a vertex made by three points given
 * --------------------------------------------------
 *  
 *   Recieves
 *
 *   1 (Float, Float): A valid pair of coordinates in R2
 *   2 (Float, Float): A valid pair of coordinates in R2 
 *   3 (Float, Float): A valid pair of coordinates in R2 
 *
 *   Returns: The value of an angle from a given triangle made by three points
-}

computeAngle3P (x1, y1) (x2, y2) (x3, y3) = theta where 

    -- The angle of theta is calculated using the following formula arcos( dotProduct / magnitudePQ * magnitudePR)

    theta = verifyInternalAngle( acos( ((dotp) / (magPQ * magPR))) )

    -- The two vectors that generate our three points are calculated and stored here

    vectPQ = ( fst (x2, y2) - fst (x1, y1) , snd (x2, y2) - snd (x1, y1))
    vectPR = ( fst (x3, y3) - fst (x1, y1) , snd (x3, y3) - snd (x1, y1))

    dotp = x where x = dotProduct2P vectPQ vectPR
    magPQ = x where x = computeMagnitudeVect vectPQ
    magPR = x where x = computeMagnitudeVect vectPR





computeTriangleAngles :: (Float, Float) -> (Float, Float) -> (Float, Float) -> String

{-
Function: Calculates the angles of a triangle made by three points in R2 if they are not collinear
 * --------------------------------------------------
 *  
 *   Recieves
 *
 *   1 (Float, Float): A valid pair of coordinates in R2
 *   2 (Float, Float): A valid pair of coordinates in R2 
 *   3 (Float, Float): A valid pair of coordinates in R2 
 *
 *   Returns: The value of the internal angles of the triangle derivative from the three given points in radians
-}

computeTriangleAngles (x1, y1) (x2, y2) (x3, y3)

    -- Checks if the three points are collinear or not

    | collinearPoints (x1, y1) (x2, y2) (x3, y3) == True = "Los puntos ingresados son colineales \n"

    | otherwise =   "El valor de los angulos son los siguientes: " ++ "\n" ++
                    "\tTheta P = " ++ show thetaP ++ 
                    "\n\tTheta Q = " ++ show thetaQ ++
                    "\n\tTheta R = " ++ show thetaR ++
                    "\n\tLa suma de los angulos = " ++ show sumofAngles ++ "\n" where

                    thetaP = computeAngle3P (x1, y1) (x2, y2) (x3, y3)
                    thetaQ = computeAngle3P (x2, y2) (x3, y3) (x1, y1)
                    thetaR = computeAngle3P (x3, y3) (x1, y1) (x2, y2) 
                    sumofAngles = thetaP + thetaQ + thetaR





verifyInternalAngle :: Float -> Float 

{-
Function: Verifies if the an angle is internal or external, in the case that the angle is 
          external then is converted to it's internal form
 * --------------------------------------------------
 *  
 *   Recieves
 *
 *   Float The measure of the angle in radians
 *
 *   Returns: The internal angle
-}

verifyInternalAngle x
    | x >= pi = pi - x
    | otherwise = x
    
    
    

main :: IO ()
main =
  do
    putStrLn " ____                                               __                __ __         ___     "
    putStrLn "/\\  _`\\                                            /\\ \\__            _\\ \\\\ \\__    /'___`\\   "
    putStrLn "\\ \\ \\L\\ \\ _ __    ___    __  __       __     ___   \\ \\ ,_\\    ___   /\\__  _  _\\  /\\_\\ /\\ \\  "
    putStrLn " \\ \\ ,__//\\`'__\\ / __`\\ /\\ \\/\\ \\    /'__`\\  /'___\\  \\ \\ \\/   / __`\\ \\/_L\\ \\\\ \\L_ \\/_/// /__ "
    putStrLn "  \\ \\ \\/ \\ \\ \\/ /\\ \\L\\ \\\\ \\ \\_\\ \\  /\\  __/ /\\ \\__/   \\ \\ \\_ /\\ \\L\\ \\  /\\_   _  _\\   // /_\\ \\"
    putStrLn "   \\ \\_\\  \\ \\_\\ \\ \\____/ \\/`____ \\ \\ \\____\\\\ \\____\\   \\ \\__\\\\ \\____/  \\/_/\\_\\\\_\\/  /\\______/"
    putStrLn "    \\/_/   \\/_/  \\/___/   `/___/> \\ \\/____/ \\/____/    \\/__/ \\/___/      \\/_//_/   \\/_____/ "
    putStrLn "                             /\\___/                                                         "
    putStrLn "                             \\/__/                                                          "
    putStrLn "============================================================================================\n"
    

    
    
    putStrLn "\t ====================================== NOTA ==================================="
    putStrLn "\t|\t      Los puntos en un plano 2D está compuesto por (X,Y)        \t|"
    putStrLn "\t|\tSe le solicitarán los valores X , Y por separado para cada punto\t|"
    putStrLn "\t ===============================================================================\n"
    
    --------------------INPUT DEL LOS VALORES DEL PRIMER PUNTO
    putStrLn "\nPrimer Punto."
    putStrLn "\tIngrese el valor de X para el primer punto: "
    primerX <- getLine
    putStrLn "\tIngrese el valor de Y para el primer punto: "
    primerY <- getLine
    
    
    --------------------INPUT DEL LOS VALORES DEL SEGUNDO PUNTO
    putStrLn "\nSegundo Punto."
    putStrLn "\tIngrese el valor de X para el segundo punto: "
    segundoX <- getLine
    putStrLn "\tIngrese el valor de Y para el segundo punto: "
    segundoY <- getLine
    
    
    --------------------INPUT DEL LOS VALORES DEL TERCER PUNTO
    putStrLn "\nTercer Punto."
    putStrLn "\tIngrese el valor de X para el tercer punto: "
    tercerX <- getLine
    putStrLn "\tIngrese el valor de Y para el tercer punto: "
    tercerY <- getLine
    
    
    putStrLn "\nResultado:\n"
    putStrLn $ computeTriangleAngles (read primerX, read primerY) (read segundoX, read segundoY) (read tercerX, read tercerY)
    
    
    
