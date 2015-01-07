module Main where
import Web.Blog

main :: IO ()
main =
    do cfg <- parseConfig "blog.cfg"
       runBlog cfg
