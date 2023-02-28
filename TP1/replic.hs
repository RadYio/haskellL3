replic :: Int -> a -> [a]
replic 0 _ = []
replic n x = x : replic (n-1) x
