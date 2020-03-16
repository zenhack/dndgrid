module Main (main) where

import Zhp

import Web.Scotty

staticFiles =
    [ ("/", "static/app.html", "text/html")
    , ("/dndgrid.js", "build/dndgrid.js", "application/javascript")
    , ("/setup.js", "static/setup.js", "application/javascript")
    ]

main :: IO ()
main = scotty 3000 $ do
    for_ staticFiles $ \(url, filePath, contentType) ->
        get url $ do
            setHeader "Content-Type" contentType
            file filePath
