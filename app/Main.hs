module Main where


import Application
import Yesod


main :: IO ()
main = warp 3000 HelloWorld