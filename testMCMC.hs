{-# LANGUAGE RankNTypes, BangPatterns, ScopedTypeVariables, OverloadedStrings #-}

module Main where

import Math.Probably.MCMC
import qualified Math.Probably.PDF as PDF
import Math.Probably.Sampler
import Data.Text as T

import Strategy.RandomWalkMetropolis (rwm)
import Strategy.Hamiltonian (hmc)

import qualified Target.Regression
import qualified Target.Heston
import qualified Target.Logistic
import qualified Target.Prelude


main = do
  (posterior, postgrad,v2rec,inisam) <- Target.Heston.target
  iniv <- sampleIO inisam
  let rec = v2rec iniv
--  chain <- sampleIO $ runChain inisam posterior postgrad 1 100 (hmc 100)
--  chain <- sampleIO $ runChain inisam posterior postgrad 1 100 rwm
  Target.Prelude.printC  "initial" $ v2rec iniv
--  Target.Prelude.printC "pars" $ fmap v2rec chain
  return ()

   