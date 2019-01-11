{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module Main where

import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as BL
import Data.Bits ((.&.))
import System.Environment (getArgs)

data TwiddlerConfig = TwiddlerConfig {
    keyRepeat :: Bool,
    directKey :: Bool,
    joystickLeftClick :: Bool,
    disableBluetooth :: Bool,
    stickyNum :: Bool,
    stickyShift :: Bool,
    hapticFeedback :: Bool,

    sleepTimeout :: Int,
    mouseLeftClickAction :: Int,
    mouseMiddleClickAction :: Int,
    mouseRightClickAction :: Int,
    mouseAccelFactor :: Int,
    keyRepeatDelay :: Int,

    nchords :: Int,
    chords :: [Int]
  }
  deriving Show


readConfig :: BL.ByteString -> TwiddlerConfig
readConfig contents = flip G.runGet contents $ do
  version <- fromIntegral <$> G.getWord8
  _ <- if version /= 5 then error "Only works on version 5" else return ()
  flagsA <- fromIntegral <$> G.getWord8 :: G.Get Int

  keyRepeat <- return $ flagsA .&. 0x01 /= 0
  directKey <- return $ flagsA .&. 0x02 /= 0
  joystickLeftClick <- return $ flagsA .&. 0x04 /= 0
  disableBluetooth <- return $ flagsA .&. 0x08 /= 0
  stickyNum <- return $ flagsA .&. 0x10 /= 0
  stickyShift <- return $ flagsA .&. 0x80 /= 0

  nchords <- fromIntegral <$> G.getWord16le :: G.Get Int
  sleepTimeout <- fromIntegral <$> G.getWord16le
  mouseLeftClickAction <- fromIntegral <$> G.getWord16le
  mouseMiddleClickAction <- fromIntegral <$> G.getWord16le
  mouseRightClickAction <- fromIntegral <$> G.getWord16le

  mouseAccelFactor <- fromIntegral <$> G.getWord8
  keyRepeatDelay <- fromIntegral <$> G.getWord8

  flagsA <- fromIntegral <$> G.getWord8 :: G.Get Int
  hapticFeedback <- return $ flagsA .&. 0x01 /= 0

  return $ TwiddlerConfig {
    keyRepeat = keyRepeat,
    directKey = directKey,
    joystickLeftClick = joystickLeftClick,
    disableBluetooth = disableBluetooth,
    stickyNum = stickyNum,
    stickyShift = stickyShift,
    nchords = nchords,
    sleepTimeout = sleepTimeout,
    mouseLeftClickAction = mouseLeftClickAction,
    mouseMiddleClickAction = mouseMiddleClickAction,
    mouseRightClickAction = mouseRightClickAction,
    mouseAccelFactor = mouseAccelFactor,
    keyRepeatDelay = keyRepeatDelay,
    hapticFeedback = hapticFeedback,
    chords = [] }

main :: IO ()
main = do
  args <- getArgs
  filename <- case args of
      [ f ] -> return f
      _ -> error "Requires a filename as argument"
  contents <- BL.readFile filename
  config <- return $ readConfig contents
  print config
