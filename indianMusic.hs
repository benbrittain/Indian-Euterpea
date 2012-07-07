import Euterpea

data Modes = Bilawal | Khamaj  | Kafi | Asavari | Bhairavi | Bhairav | Kalyan  | Marva | Purvi | Todi
    deriving (Show)

genScale :: Modes -> [Int]
genScale Bilawal    = [2,2,1,2,2,2,1]
genScale Khamaj     = [2,2,1,2,2,1,2]
genScale Kafi       = [2,1,2,2,2,1,2]
genScale Asavari    = [2,1,2,2,1,2,2]
genScale Bhairavi   = [1,2,2,2,1,2,2]
genScale Bhairav    = [1,3,1,2,1,3,1]
genScale Kalyan     = [2,2,2,1,2,2,1]
genScale Marva      = [1,3,2,1,2,2,1]
genScale Purvi      = [1,3,2,1,1,3,1] 
genScale Todi       = [1,2,3,1,1,3,1]


mkScale :: Pitch -> [Int] -> Music Pitch
mkScale p []     = note qn p
mkScale p (x:xs) = note qn p :+: mkScale (trans x p) xs

apToHz' :: Floating a => AbsPitch -> AbsPitch -> a -> a
apToHz' ap sp hz = hz * 2 ** (fromIntegral (ap - sp) /12)
