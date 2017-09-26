module Util where

filterJust :: [Maybe a] -> [a]
filterJust ls = [x | Just x <- ls]