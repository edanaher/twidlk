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

    nchords :: Int,
    chords :: [Int]
  }
  deriving Show


readConfig :: BL.ByteString -> TwiddlerConfig
readConfig contents = flip G.runGet contents $ do
  version <- fromIntegral <$> G.getWord8
  _ <- if version /= 5 then error "Only works on version 5" else return ()
  flags <- fromIntegral <$> G.getWord8 :: G.Get Int

  keyRepeat <- return $ flags .&. 0x01 /= 0
  directKey <- return $ flags .&. 0x02 /= 0
  joystickLeftClick <- return $ flags .&. 0x04 /= 0
  disableBluetooth <- return $ flags .&. 0x08 /= 0
  stickyNum <- return $ flags .&. 0x10 /= 0
  stickyShift <- return $ flags .&. 0x80 /= 0

  nchords <- fromIntegral <$> G.getWord16le :: G.Get Int

  return $ TwiddlerConfig {
    keyRepeat = keyRepeat,
    directKey = directKey,
    joystickLeftClick = joystickLeftClick,
    disableBluetooth = disableBluetooth,
    stickyNum = stickyNum,
    stickyShift = stickyShift,
    nchords = nchords,
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
