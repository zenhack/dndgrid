module Main (main) where

import Zhp

import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
    get "/" $ file "static/app.html"
    get "/dndgrid.js" $ file "build/dndgrid.js"
    get "/setup.js" $ file "static/setup.js"
