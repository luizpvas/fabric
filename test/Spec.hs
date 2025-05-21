import Test.Hspec
import qualified Compiler.Parser.NameSpec
import qualified Compiler.Parser.NumberSpec
import qualified Compiler.Parser.StringSpec
import qualified Compiler.Parser.SQL.ExpressionSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Compiler.Parser.Name"   Compiler.Parser.NameSpec.spec
  describe "Compiler.Parser.Number" Compiler.Parser.NumberSpec.spec
  describe "Compiler.Parser.String" Compiler.Parser.StringSpec.spec
  describe "Compiler.Parser.SQL.Expression" Compiler.Parser.SQL.ExpressionSpec.spec
