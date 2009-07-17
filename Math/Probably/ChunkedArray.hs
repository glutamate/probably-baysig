module Math.Probably.ChunkedArray where

import Data.Array.Vector
import Data.List (foldl')
class ReifyInt a where
    reifyInt :: a -> Int

data D4 = D4
instance ReifyInt D4 where reifyInt _ = 4

data D512 = D512
instance ReifyInt D512 where reifyInt _ = 512

data D10000 = D10000
instance ReifyInt D10000 where reifyInt _ = 10000

newtype Chunked chsz a = CU [UArr a] deriving (Eq, Show)

chunkedTypedChunkSize :: ReifyInt chsz => Chunked chsz a -> chsz
chunkedTypedChunkSize = undefined

chunkedChunkSize :: ReifyInt chsz => Chunked chsz a -> Int
chunkedChunkSize ch = reifyInt (chunkedTypedChunkSize ch)

consChunks :: UArr a -> Chunked chsz a -> Chunked chsz a
consChunks arr (CU arrs) = CU (arr:arrs)

replicateCU :: (ReifyInt chsz, UA a) => chsz -> Int -> a -> Chunked chsz a
replicateCU chsz n e | n > reifyInt chsz = consChunks (replicateU (reifyInt chsz) e) $ replicateCU  chsz (n-reifyInt chsz) e
                     | otherwise = CU [replicateU n e]

enumFromToCU :: ReifyInt chsz => chsz -> Int -> Int -> Chunked chsz Int
enumFromToCU chsz from to = let sz = reifyInt chsz in
                            if to-from > sz 
                               then consChunks (enumFromToU from (from+sz-1)) $ enumFromToCU chsz (from+sz) to
                               else CU [enumFromToU from to]

mapCU :: (UA a, UA b) => (a->b) -> Chunked chsz a -> Chunked chsz b
mapCU _ (CU []) = CU []
mapCU f (CU arrs) = CU $ map (mapU f) arrs

foldlCU :: UA b => (a -> b -> a) -> a -> Chunked chsz b -> a
foldlCU f init (CU arrs) = foldl' (foldlU f) init arrs