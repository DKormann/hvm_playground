import GHC.Internal.TopHandler (runIO)

comp :: Int -> IO Int
comp x = print("done.") >> return (x * 2)

main :: IO ()
main = do
  let x = (comp 2)
  x >>= \xx -> print xx