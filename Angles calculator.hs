
import Data.List
import System.IO
import Debug.Trace


main = do

    putStrLn "Welcome to the internal angles calculator"





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

    | collinearPoints (x1, y1) (x2, y2) (x3, y3) == True = "The three given points are collinear \n"

    | otherwise =   "The calculations for the angles are the following: " ++ "\n" ++
                    "Theta P = " ++ show thetaP ++ 
                    "\nTheta Q = " ++ show thetaQ ++
                    "\nTheta R = " ++ show thetaR ++
                    "\nSum of angles = " ++ show sumofAngles ++ "\n" where

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
