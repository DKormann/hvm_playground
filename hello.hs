

hs = "hello\nworld"

parse s = case s of 
  s -> s

main = do
  putStrLn $ parse hs