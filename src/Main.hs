-- |
-- Module      :  Main
-- Copyright   :  (c) Ragon Creswick, 2009-2012
--                    Mateusz Kowalczyk, 2014
-- License     :  GPL-3
--
-- Entry point for the executable, using "Utils.PastePipe".
--
-- A CLI for lpaste.org.
--
--  Authored by Rogan Creswick (creswick_at_googles_mail_service.)
--
-- Pastepipe reads from stdin, posting to lpaste, and prints out the
-- resulting url (the last line of output).  Parameters control various
-- lpaste form fields:
--
--   --user         (defaults to $USER)
--   -l --language  (defaults to haskell, of course)
--   -t --title     (defaults to the empty string)
--   -u --uri       (defaults to <http://lpaste.net>)
--
-- It will auto-detect your local username, but --user overrides this detection.
--
-- Use @cabal install@ to install this. The executable will be called
-- @pastepipe@.
module Main where

import System.Console.CmdArgs
import System.Environment (getEnv)
import Utils.PastePipe

main :: IO ()
main = do
  realUser <- getEnv "USER"
  conf <- cmdArgs $ config realUser
  content <- getContents
  let postFn = if test conf then fakePost else post
  resultUrl <- postFn conf content
  print resultUrl
