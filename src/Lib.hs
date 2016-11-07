{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Lib
    ( startApp
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString, pack)
import qualified Data.ByteString.Lazy.Char8       as BC
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Text.Pandoc
import Text.Pandoc.Error

markdownToHtmlString :: String -> String
markdownToHtmlString =
  writeHtmlString def .
  handleError . readMarkdown def

jsonToDoc :: String -> Pandoc
jsonToDoc =
  handleError . readJSON def

data HTML

instance Accept HTML where
  contentType _ = "text" // "html"

instance MimeRender HTML BC.ByteString where
  mimeRender _ a = a

type ConvertAPI =
        QueryParam "content" String :> Get '[HTML] BC.ByteString
   :<|> ReqBody '[JSON] ConvertRequest :> Post '[OctetStream] BC.ByteString

data ConvertRequest = ConvertRequest
  { from :: String
  , to :: String
  , content :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ConvertRequest)

doConvert :: ConvertRequest -> IO BC.ByteString
doConvert (ConvertRequest "json" "html" content) = return . BC.pack $ writeHtmlString def doc
                  where doc = jsonToDoc content
doConvert (ConvertRequest "json" "epub" content) = writeEPUB def doc
                  where doc = jsonToDoc content
doConvert _ = return $ BC.pack "Not Supported!"

convertServer :: Server ConvertAPI
convertServer = test :<|> convert

  where test :: Maybe String -> Handler BC.ByteString
        test content =
          return . BC.pack . markdownToHtmlString $ case content of
            Nothing -> "#Nothing to see here"
            Just n -> n

        convert :: ConvertRequest -> Handler BC.ByteString
        convert request = liftIO . doConvert $ request

type API = "convert" :> ConvertAPI

startApp :: IO ()
startApp = run 8081 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = convertServer

