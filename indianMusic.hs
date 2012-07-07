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



ssfMelTrills :: Music Pitch
ssfMelTrills = instrument Sitar $ line (l1 ++ l2 ++ l3 ++ l4)
    where   l1 = [trilln 2 5 (bf 6 en), ef 7 en, ef 6 en, ef 7 en]
            l2 = [bf 6 sn, c 7 sn, bf 6 sn, g 6 sn, ef 6 en, bf 5 en]
            l3 = [ef 6 sn, f 6 sn, g 6 sn, af 6 sn, bf 6 en, ef 7 en]
            l4 = [trill 2 tn (bf 6 qn), bf 6 sn, denr]

