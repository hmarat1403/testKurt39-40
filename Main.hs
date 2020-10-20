{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as BC 
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Network.HTTP.Types (Status(..))
import Data.Aeson
import Json

takeData :: IO ()
takeData = do
    response <- httpLBS request
    let code = statusCode . getResponseStatus $ response
    let error = statusMessage . getResponseStatus $ response
    if code == 200
    then do
        putStrLn "saving request to file\n\n"
        let jsonBody = getResponseBody response
        L.writeFile "data.json" jsonBody
    else print $ "request failed: code-" <> show code <> "; message-" <> show error
    
main :: IO ()
main = do
    takeData
    jsonData <- L.readFile "data.json"
    let noaaResponse = decode jsonData :: Maybe NOAAResponse
    let noaaResults = results <$> noaaResponse
    printResults noaaResults

        
      

myToken :: BC.ByteString
myToken = "KmLKiqKWaSNkNWZOMtcxnXtykAXxWVlz"
noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"
apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

--The code for building an HTTPS request for the API 
buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString
                -> BC.ByteString -> Request
buildRequest token host method path = setRequestMethod method
                                    $ setRequestHost host
                                    $ setRequestHeader "token" [token]
                                    $ setRequestPath path
                                    $ setRequestSecure True
                                    $ setRequestPort 443
                                    $ defaultRequest
request :: Request
request = buildRequest myToken noaaHost "GET" apiPath   

buildRequestNOSSL :: BC.ByteString -> BC.ByteString -> BC.ByteString
                   -> BC.ByteString -> Request
buildRequestNOSSL token host method path = setRequestMethod method
                                    $ setRequestHost host
                                    $ setRequestHeader "token" [token]
                                    $ setRequestPath path
                                    $ setRequestSecure False
                                    $ setRequestPort 80
                                    $ defaultRequest