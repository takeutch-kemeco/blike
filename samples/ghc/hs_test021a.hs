module Test021a where

import Blike

foreign export ccall
  hs_bl_main :: IO ()

hs_bl_main :: IO ()
hs_bl_main = do
  bl_drawPtrn_d 20 20 448 0
    "%name=block a=#AAAAAA b=#FFFFFF" $
    "abbbbbbbbbbbbbbbbbba" ++
    "baaaaaaaaaaaaaaaaaab" ++
    "baaaaaaaaaaaaaaaaaab" ++
    "baaaaaaaaaaaaaaaaaab" ++
    "baaaaaaaaaaaaaaaaaab" ++
    "baaaaaaaaaaaaaaaaaab" ++
    "baaaaaaaaaaaaaaaaaab" ++
    "baaaaaaaaaaaaaaaaaab" ++
    "baaaaaaaaaaaaaaaaaab" ++
    "baaaaaaaaaaaaaaaaaab" ++
    "baaaaaaaaaaaaaaaaaab" ++
    "baaaaaaaaaaaaaaaaaab" ++
    "baaaaaaaaaaaaaaaaaab" ++
    "baaaaaaaaaaaaaaaaaab" ++
    "baaaaaaaaaaaaaaaaaab" ++
    "baaaaaaaaaaaaaaaaaab" ++
    "baaaaaaaaaaaaaaaaaab" ++
    "baaaaaaaaaaaaaaaaaab" ++
    "baaaaaaaaaaaaaaaaaab" ++
    "abbbbbbbbbbbbbbbbbba"

  bl_drawPtrn_d 20 20 448 24
    "%name=brick a=#CCCCCC b=#FFCCCCC" $
    "aaaaaaaaaaaaaaaaaaaa" ++
    "bbbbbabbbbbbbbbbabbb" ++
    "bbbbbabbbbbbbbbbabbb" ++
    "bbbbbabbbbbbbbbbabbb" ++
    "bbbbbabbbbbbbbbbabbb" ++
    "aaaaaaaaaaaaaaaaaaaa" ++
    "abbbbbbbbbabbbbbbbbb" ++
    "abbbbbbbbbabbbbbbbbb" ++
    "abbbbbbbbbabbbbbbbbb" ++
    "abbbbbbbbbabbbbbbbbb" ++
    "aaaaaaaaaaaaaaaaaaaa" ++
    "bbbbbabbbbbbbbbbabbb" ++
    "bbbbbabbbbbbbbbbabbb" ++
    "bbbbbabbbbbbbbbbabbb" ++
    "bbbbbabbbbbbbbbbabbb" ++
    "aaaaaaaaaaaaaaaaaaaa" ++
    "abbbbbbbbbabbbbbbbbb" ++
    "abbbbbbbbbabbbbbbbbb" ++
    "abbbbbbbbbabbbbbbbbb" ++
    "abbbbbbbbbabbbbbbbbb"

  bl_drawPtrn_d 20 20 448 48
    "%name=smile a=#CCCCCC b=#FFCCCCC c=#FFFF00 d=#000000" $
    "aaaaaaaaaaaaaaaaaaaa" ++
    "bbbbbabbbbbbbbbbabbb" ++
    "bbbbbabbbbbbbbbbabbb" ++
    "bbbbbabbbbbbbbbbabbb" ++
    "bbbbbabbbbbbbbbbabbb" ++
    "aaaaaaacccccaaaaaaaa" ++
    "abbbbbbccccccccbbbbb" ++
    "abbbbcccccccccccbbbb" ++
    "abbbccccdcccdcccbbbb" ++
    "abbbccccccccccccbbbb" ++
    "aaaaccccccccccccaaaa" ++
    "bbbbbccccdddcccbabbb" ++
    "bbbbbabccccccbbbabbb" ++
    "bbbbbabbbbbbbbbbabbb" ++
    "bbbbbabbbbbbbbbbabbb" ++
    "aaaaaaaaaaaaaaaaaaaa" ++
    "abbbbbbbbbabbbbbbbbb" ++
    "abbbbbbbbbabbbbbbbbb" ++
    "abbbbbbbbbabbbbbbbbb" ++
    "abbbbbbbbbabbbbbbbbb"

  bl_drawPtrn_d 20 20 448 72
    "%name=card a=#CCCCCC b=#FFCCCCC c=#00FF00 d=#000000" $
    "aaaaaaaaaaaaaaaaaaaa" ++
    "bbbbbabbbbbbbbbbabbb" ++
    "bbbbbabbbbbbbbbbabbb" ++
    "bbbbbabbbbbbbbbbabbb" ++
    "bbbbbabbbbbbbbbbabbb" ++
    "aaacccccccccccccaaaa" ++
    "abbcccccccccccccbbbb" ++
    "abbcccccccccccccbbbb" ++
    "abbcccccccccccccbbbb" ++
    "abbcccccccccccccbbbb" ++
    "aaacccccccccccccdaaa" ++
    "bbbcccccccccccccdbbb" ++
    "bbbcccccccccccccdbbb" ++
    "bbbbbddddddddddddbbb" ++
    "bbbbbddddddddddddbbb" ++
    "aaaaaaaaaaaaaaaaaaaa" ++
    "abbbbbbbbbabbbbbbbbb" ++
    "abbbbbbbbbabbbbbbbbb" ++
    "abbbbbbbbbabbbbbbbbb" ++
    "abbbbbbbbbabbbbbbbbb"

  bl_drawPtrn_d 20 20 448 96
--  "%name=map A=#000000 B=#000001 C=#000002 D=#000003" $
    "%name=map A=0 B=1 C=2 D=3" $
    "AAAAAAAAAAAAAAAAAAAA" ++
    "ACABBBBBABBBBBBBBBBA" ++
    "ABABABABABBBBBBBBBBA" ++
    "ABABABABABBBBBBBBBBA" ++
    "ABABABABABBBBBBBBBBA" ++
    "ABBBABBBBBBBBBBBBBBA" ++
    "ABAAABBBBBBBBBBBBBBA" ++
    "ABABBBBBBBBBBBBBBBBA" ++
    "ABABABBBBBBBBBBBBBBA" ++
    "ABBBABBBBBBBBBBBBBBA" ++
    "ABABABBBBBBBBBBBBBBA" ++
    "ADABABBBBBBBBBBBBBBA" ++
    "ABABABBBBBBBBBBBBBBA" ++
    "ABBBBBBBBBBBBBBBBBBA" ++
    "ABBBBBBBBBBBBBBBBBBA" ++
    "ABBBBBBBBBBBBBBBBBBA" ++
    "ABBBBBBBBBBBBBBBBBBA" ++
    "ABBBBBBBBBBBBBBBBBBA" ++
    "ABBBBBBBBBBBBBBBBBBA" ++
    "AAAAAAAAAAAAAAAAAAAA"

  mapM_
    (\y -> mapM_
           (\x -> bl_getPix (448 + x) (96 + y) >>=
                  (\c -> bl_copyRct0 20 20 0 448 (c * 24) 0 (x * 20) (y * 20))
           )
           [0..19]
    )
    [0..19]

  bl_wait (-1)
