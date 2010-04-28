{-# LANGUAGE TypeFamilies, FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances #-}

module Math.Probably.Distribution where

import Math.Probably.Sampler as S
import Math.Probably.PDF as P
import Math.Probably.FoldingStats as F

class Distribution f where
      type Elem f :: * 
      pdf :: f -> P.PDF (Elem f)

class Distribution f => ProperDistribution f where
      sampler :: f -> S.Sampler (Elem f)
      estimator :: F.Fold (Elem f) f

data Normal = Normal Double Double

instance Distribution Normal where
      type Elem (Normal) = Double
      pdf (Normal mu sd) = P.gaussD mu sd

instance ProperDistribution Normal where
      sampler (Normal mu sd) = S.gauss mu sd
      estimator = fmap (uncurry Normal) meanSDF

data Gamma = Gamma Double Double

instance Distribution Gamma where
      type Elem (Gamma) = Double
      pdf (Gamma a b) = P.gammaD a b

instance ProperDistribution Gamma where
      sampler (Gamma a b) = S.gamma a b
      estimator = fmap (uncurry Gamma) gammaF

data ImproperUniform a = ImproperUniform

instance Distribution (ImproperUniform a) where
      type Elem (ImproperUniform a) = a
      pdf ImproperUniform = const 1

class BayesUp prior like post | prior like -> post where
    bayesUp :: prior -> like -> post

instance (ProperDistribution f, Elem f ~ a) => BayesUp (ImproperUniform a) f f where
    bayesUp _ d = d

--instance BayesUp Gamma Normal Normal where
    
