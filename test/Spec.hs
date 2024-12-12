import qualified TestPrismaParser
import qualified GeneratorToolkitSpec
import qualified FormatStringSpec

main :: IO ()
main = do
    putStrLn "running toolkit tests..."
    GeneratorToolkitSpec.main
    putStrLn "\nrunning format string tests..."
    FormatStringSpec.main
    putStrLn "\nrunning parser tests..."
    TestPrismaParser.runQC