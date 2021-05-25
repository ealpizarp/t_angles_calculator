
import Data.List
import System.IO
import Debug.Trace


main = do

    putStrLn "Welcome to my haskell program"




collinearPoints :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool

collinearPoints (x1, y1) (x2, y2) (x3,y3)
    | (snd (x3, y3) - snd (x2, y2)) * (fst (x2, y2) - fst (x1, y1)) == (snd (x2, y2) - 
    snd (x1, y1)) * (fst (x3, y3) - fst (x2, y2)) = True 
    | otherwise = False

dotProduct2P :: (Float, Float) -> (Float, Float) -> Float

dotProduct2P (x1, y1) (x2, y2) = fst(x1, y1) * fst (x2, y2) + snd (x1, y1) * snd (x2, y2)

{-
computeDistance2P :: (Float, Float) -> (Float, Float) -> Float

computeDistance2P (x1, y1) (x2, y2) = sqrt( (fst (x2, y2) - fst (x1, y1)) ** 2 + (snd (x2, y2) - snd (x1, y1)) ** 2)
-}


computeMagnitudeVect :: (Float, Float) -> Float

computeMagnitudeVect (x1, y1) = sqrt( ( fst (x1, y1) ) ** 2 + ( snd (x1, y1) ) ** 2 ) 


computeAngle3P :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Float

computeAngle3P (x1, y1) (x2, y2) (x3, y3) = theta where 
    theta = verifyInternalAngle( acos( ((dotp) / (magPQ * magPR))) )

    vectPQ = ( fst (x2, y2) - fst (x1, y1) , snd (x2, y2) - snd (x1, y1))
    vectPR = ( fst (x3, y3) - fst (x1, y1) , snd (x3, y3) - snd (x1, y1))
    dotp = x where x = dotProduct2P vectPQ vectPR
    magPQ = x where x = computeMagnitudeVect vectPQ
    magPR = x where x = computeMagnitudeVect vectPR


computeTriangleAngles :: (Float, Float) -> (Float, Float) -> (Float, Float) -> String

computeTriangleAngles (x1, y1) (x2, y2) (x3, y3)
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

verifyInternalAngle x
    | x >= pi = pi - x
    | otherwise = x




    