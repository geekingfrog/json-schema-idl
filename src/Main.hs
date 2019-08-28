{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.JSON.Schema as S
import qualified Data.Aeson as JSON

main :: IO ()
main = do
  let s = S.Schema (Just "description") (Just "title") S.ValNumeric
  print $ JSON.encode s
  let (rt :: Either String S.Schema) = JSON.eitherDecode (JSON.encode s)
  print rt
  putStrLn "hello world"
