{-# LANGUAGE TypeFamilies #-}

module Math.Probably.Distribution where

import Math.Probably.Sampler as S
import Math.Probably.PDF as P
import Math.Probably.FoldingStats as F

class Distribution f where
      type Elem f :: * 
      pdf :: f -> P.PDF (Elem f)
      sampler :: f -> S.Sampler (Elem f)
      estimator :: F.Fold (Elem f) f

data Normal = Normal Double Double


instance Distribution Normal where
      type Elem (Normal) = Double
      pdf (Normal mu sd) = P.gaussD mu sd
      sampler (Normal mu sd) = S.gauss mu sd
      estimator = fmap (uncurry Normal) meanSDF

