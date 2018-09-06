{-# LANGUAGE OverloadedStrings #-}

module CmdLineParserTests (tests) where

import Test.Hspec
import Text.Printf (printf)
import Text.Megaparsec
import Data.Text

import TestData

import Cards
import Protocol
import CmdLineParser

testParseHand :: Text -> Hand -> Spec
testParseHand input hand =
  it (printf "should parse the hand for : %s -> %s \n" input (show hand)) $
    parse handParser "" input `shouldBe` Right hand

testParseMsg :: Text -> Msg -> Spec
testParseMsg input msg =
  it (printf "should parse the msg for : %s -> %s \n" input (show msg)) $
    parse msgParser "" input `shouldBe` Right msg

tests = hspec $ do
  describe "parseHand" $ do 
      testParseHand "(7c,8c,9c,10c,Jc,8h,9h,10h,Jh,Js,Jd,Qd)" handElder 
  describe "parseExchangeMsg" $ do 
      testParseMsg "e (7c,8c,9c,10c,Jc,8h,9h,10h,Jh,Js,Jd,Qd)" (Exchange handElder) 


