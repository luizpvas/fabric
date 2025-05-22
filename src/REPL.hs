module REPL where


import System.IO
import qualified Text.Megaparsec as Megaparsec
import qualified Compiler.Parser.SQL as SQL


loop :: IO ()
loop = do
  putStr "fabric> "
  hFlush stdout
  code <- getLine
  putStr (eval code)
  hFlush stdout
  loop


eval :: String -> String
eval code =
  case Megaparsec.parse SQL.expression "REPL" code of
    Left err  -> Megaparsec.errorBundlePretty err
    Right ast -> "All good!\n"
