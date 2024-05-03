import qualified System.Info

main :: IO ()
main = do
    putStrLn System.Info.os
