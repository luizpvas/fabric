import qualified Compiler.SQL.ParserSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Compiler.SQL.Parser" Compiler.SQL.ParserSpec.spec
