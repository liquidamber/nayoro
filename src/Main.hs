{-# LANGUAGE OverloadedStrings #-}

import           Nayoro.Config
import           Nayoro.Model
import           System.Environment (getArgs)
import qualified Nayoro.Web as Web

main :: IO ()
main = do
  argv <- getArgs
  let conffile = head argv
  mconf <- loadConfigFromFile conffile
  case mconf of
    Left _ -> putStrLn "Errornous configuration file"
    Right conf -> Web.runApp conf
