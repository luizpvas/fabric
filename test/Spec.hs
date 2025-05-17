import Test.Hspec
import qualified Compiler.Parser.NumberSpec
import qualified Compiler.Parser.StringSpec
import qualified Compiler.SQL.ExpressionSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Compiler.Parser.Number" Compiler.Parser.NumberSpec.spec
  describe "Compiler.Parser.String" Compiler.Parser.StringSpec.spec
  describe "Compiler.SQL.Parser" Compiler.SQL.ExpressionSpec.spec
