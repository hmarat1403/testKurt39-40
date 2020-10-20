{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Json where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics 
import Control.Monad

data NOAAResult = NOAAResult
                { uid :: T.Text
                , mindate :: T.Text
                , maxdate :: T.Text
                , name :: T.Text
                , datacoverage :: Float
                , resultId :: T.Text
                } deriving Show
instance FromJSON NOAAResult where
    parseJSON (Object v) = 
        NOAAResult <$> v .: "uid"
                   <*> v .: "mindate"
                   <*> v .: "maxdate"
                   <*> v .: "name"
                   <*> v .: "datacoverage"
                   <*> v .: "id"     
data Resultset = Resultset
                { offset :: Int
                , count :: Int
                , limit :: Int
                } deriving (Show, Generic) 
instance FromJSON Resultset 

data Metadata = Metadata 
                { resultset :: Resultset
                } deriving (Show, Generic)
instance FromJSON Metadata 

data NOAAResponse = NOAAResponse
                { metadata :: Metadata
                , results :: [NOAAResult]
                } deriving (Show,Generic)
instance FromJSON NOAAResponse

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just results) = do
    forM_ results (print . name)

{-Q40.1 Make your NOAAResponse type an instance of ToJSON. This requires making 
all the types used by this type instances of ToJSON as well.-}  

instance ToJSON NOAAResult where
    toJSON (NOAAResult uid mindate maxdate name datacoverage resultId)
            = object [ "uid" .= uid
                     , "mindate" .= mindate
                     , "maxdate" .= maxdate
                     , "name" .= name
                     , "datacoverage" .= datacoverage
                     , "id" .= resultId
                     ]  
instance ToJSON Resultset 
instance ToJSON Metadata
instance ToJSON NOAAResponse 

{-Q40.2 Make a Sum type called IntList and use DerivingGeneric to make it an instance 
of ToJSON. Don’t use the existing List type, but rather write it from scratch. 
Here’s an example of an IntList:
-}
intListExample :: IntList
intListExample = Cons 1 $ Cons 2 EmptyList 

data IntList = EmptyList | Cons Int IntList
    deriving (Show, Eq, Generic)
instance FromJSON IntList
instance ToJSON IntList

test :: IO ()
test = print $ encode intListExample