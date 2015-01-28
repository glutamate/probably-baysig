{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Math.Probably.Datasets where


import Data.Csv
import GHC.Generics
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL

--http://www.stephendiehl.com/what/#csv


data Plant = Plant
  { sepal_length :: Double
  , sepal_width  :: Double
  , petal_length :: Double
  , petal_width  :: Double
  , species :: String
  } deriving (Generic, Show)

instance FromNamedRecord Plant
instance ToNamedRecord Plant

type CsvData = (Header, V.Vector Plant)

plantToVector (Plant x y z w _) = [x,y,z,w]

parseCSV :: FilePath -> IO (Either String CsvData)
parseCSV fname = do
  contents <- BL.readFile fname
  return $ decodeByName contents


--e.g. https://raw.githubusercontent.com/uiuc-cse/data-fa14/gh-pages/data/iris.csv
iris :: FilePath -> IO (Either String [Plant])
iris = fmap (fmap (V.toList . snd)) . parseCSV
