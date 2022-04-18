module Properties where

{-***Defining Costants***-}
eartRadius :: Double 
eartRadius = 6372.795477598

{-***Defining Functions to calculate the course properties***-}

{-Calculate the distance between two detections
 -Formula implemented:
 -distance (A,B) = R * arccos(sin(latA) * sin(latB) + cos(latA) * cos(latB) * cos(lonA-lonB))-}
distance :: [Double] -> [Double] -> Double 
distance detA detB = eartRadius * acos(sin lat1 * sin lat2 + cos lat1 * cos lat2 * cos(lat1 * pi / 180)) 
    where
    lat1 = head detA * pi / 180
    lat2 = head detB * pi / 180

{-Calculate the directions between two detections
 - Formula Implemented: 
 - Δφ = ln( tan( latB / 2 + π / 4 ) / tan( latA / 2 + π / 4) )
 - Δlon = abs( lonA - lonB ) 
 - direction :  θ = atan2( Δlon ,  Δφ )-}

{-Defining the first part of formula-}
phi :: [Double] -> [Double] -> Double 
phi detA detB = log(tan(lat2 / 2 + pi / 4) / tan(lat1 / 2 + pi / 4))
    where
        lat1 = head detA * pi / 180
        lat2 = head detB * pi / 180

{-Defining a lon nomralizer-}
verLon :: Double -> Double 
verLon val
          | (val * pi / 180) > 180 = fromInteger(rem (round val :: Integer) 180)   
          | otherwise = val

{-Defining the second part of formula-}
lon :: [Double] -> [Double] -> Double
lon detA detB = verLon(abs(detA !! 1 - detB !! 1)) 

{-Defining directions calculator-}
direction :: [Double] -> [Double] -> Double 
direction det1 det2 = atan2(lon det1 det2 * pi / 180) (abs (phi det1 det2)) / pi * 180

{-Defining a funciton that calculate the contrary direction-}
invDirection :: [Double] -> [Double] -> Double
invDirection det1 det2 = direction det1 det2 + 180