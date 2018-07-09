-- curry
mult’ :: Int ->(Int-> (Int->Int ))
mult’ = \x -> \y -> \z -> x*y*z


soma 1 :: Int->
