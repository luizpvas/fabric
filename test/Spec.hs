import Test.Hspec
import qualified Compiler.Parser.NumberSpec
import qualified Compiler.SQL.ParserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Compiler.Parser.Number" Compiler.Parser.NumberSpec.spec
  describe "Compiler.SQL.Parser" Compiler.SQL.ParserSpec.spec
