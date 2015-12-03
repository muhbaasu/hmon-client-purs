module Model where

import Prelude
import Data.JSON

data Cpu = Cpu {
              cpuData :: Array Number
           }

instance showCpu :: Show Cpu where
  show (Cpu cData) = "CPU: "

instance cpuFromJSON :: FromJSON Cpu where
  parseJSON (JObject o) = do
    d <- o .: "cpu"
    return $ Cpu {cpuData: d}
