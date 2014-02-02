{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      :  Utils.PastePipe
-- Copyright   :  (c) Ragon Creswick, 2009-2012
--                    Mateusz Kowalczyk, 2014
-- License     :  GPL-3
--
-- Configuration and communication with lpaste.net
module Utils.PastePipe where

import Control.Monad (when)
import Data.Maybe
import Network.Browser
import Network.HTTP.Base
import Network.URI
import System.Console.CmdArgs
import System.Environment (getEnv)

-- | Configuration type for PastePipe:
data Config = Config { userName :: String
                     , language :: String
                     , title :: String
                     , uri :: String
                     , test :: Bool }
              deriving (Show, Data, Typeable)

config :: String -> Config
config realUser = Config { userName = realUser
                                &= help "Your user name"
                                &= typ "USER"
                                &= explicit
                                &= name "user"
                         , language = "haskell"
                                &= help "The language used for syntax highlighting"
                                &= typ "LANGUAGE"
                         , title = ""
                                &= help "The title of the snippet"
                                &= typ "TITLE"
                                &= explicit
                                &= name "title"
                                &= name "t"
                         , uri = defaultUri
                                &= help "The URI of the lpaste instance to post to"
                                &= typ "URL"
                         , test = False
                                &= help "Prevents PastePipe from actually posting content, just echos the configuration and input"
                         }
                         &= summary "PastePipe v1.3, (C) Rogan Creswick 2009"
                         &= program "pastepipe"

-- | Takes a string to post to the default and returns the URI.
-- Client code is expected to catch any exceptions.
postWithDefaults :: String -> IO URI
postWithDefaults s = getEnv "USER" >>= \u -> post (config u) s

-- | Define an output handler based on the user-specified verbosity.
outHandler :: String -> IO ()
outHandler str = do
  loud <- isLoud -- are we running in verbose mode?
  when loud $ putStr str

-- | The "root" uri for lpaste.net
defaultUri :: String
defaultUri = "http://lpaste.net/"

-- | The URI for posting new pastes to lpaste.
-- This isn't guaranteed to trigger a failure on all execution paths, as-is.
saveUri :: String -> URI
saveUri coreUri = buildURI coreUri "new"

-- | composes the core uri and a string to create a usable URI
buildURI :: String -> String -> URI
buildURI coreUri str = fromJust $ parseURI $ coreUri ++ str

-- | Posts the given content to lpaste.net, returning the new uri.
post :: Config -> String -> IO URI
post conf str = do
  (url, _) <- Network.Browser.browse $ do
                  setOutHandler outHandler
                  setAllowRedirects True -- handle HTTP redirects
                  request $ buildRequest conf str
  return url

-- | Creates the request to post a chunk of content.
buildRequest :: Config -> String -> Request String
buildRequest conf str = formToRequest $ Form POST (saveUri $ uri conf)
                             [ ("title", title conf)
                             , ("author", userName conf)
                             , ("paste", str)
                             , ("language", language conf)
                             , ("channel", "")
                             , ("email", "")
                             ]

fakePost ::  Config -> String -> IO URI
fakePost conf str = do
  putStrLn $ "uri: "++uri conf
  putStrLn $ "user: "++userName conf
  putStrLn $ "lang: "++language conf
  putStrLn $ "title: "++title conf
  putStrLn $ "content: "++str
  return $ fromJust $ parseURI $ uri conf
