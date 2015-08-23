module Main where

import Web.Blog

import System.Environment

main :: IO ()
main =
    do args <- getArgs
       if "-h" `elem` args || "--help" `elem` args
       then do putStrLn "Usage: funblog"
               putStrLn ""
               putStrLn "Configure using the blog.cfg file"
       else do cfg <- parseConfig "blog.cfg"
               runBlog cfg
