import Nayoro.Config
import System.Environment (getArgs)

main :: IO ()
main = do
  argv <- getArgs
  let conf = head argv
  print =<< loadConfigFromFile conf
