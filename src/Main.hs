{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module Main where

import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as BL
import Data.Bits ((.&.), shiftL)
import Data.Char (chr, ord)
import Data.List (intercalate)
import Numeric (showHex)
import System.Environment (getArgs)

import Debug.Trace (trace)

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
    chords :: [RawChord]
  }
  deriving Show

data ChordOutput =
    SingleChord { modifier :: Int, keyCode :: Int }
  | MultipleChord { stringIndex :: Int }
  deriving Show

data RawChord = RawChord { keys :: [Int], output :: ChordOutput }
  deriving Show

readChord :: G.Get RawChord
readChord = do
  rawKeys <- fromIntegral <$> G.getWord16le :: G.Get Int
  keys <- return $ [i | i <- [0..15], rawKeys .&. (1 `shiftL` i) /= 0]

  mappingL <- fromIntegral <$> G.getWord8
  mappingH <- fromIntegral <$> G.getWord8

  chord <- return $ case mappingL of
            0xFF -> MultipleChord mappingH
            _ -> SingleChord { modifier = mappingL, keyCode = mappingH }

  return $ RawChord keys chord

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

  flagsB <- fromIntegral <$> G.getWord8 :: G.Get Int
  flagsC <- fromIntegral <$> G.getWord8 :: G.Get Int
  hapticFeedback <- return $ flagsC .&. 0x01 /= 0

  chords <- mapM (\() -> readChord) (take nchords $ repeat ())

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
    chords = chords }

generateTextForKeys :: [Int] -> String
generateTextForKeys keys =
  let generateRow n =
        let keys' = [k - 4*n | k <- keys, k > 4*n, k < 4*(n+1)] in
        case keys' of
          [] -> "0"
          (1:r) -> "L"
          (2:r) -> "M"
          (3:r) -> "R"
      modifiers = (if 0  `elem` keys then "N" else "") ++
                  (if 4  `elem` keys then "A" else "") ++
                  (if 8  `elem` keys then "C" else "") ++
                  (if 12 `elem` keys then "S" else "")
      modifier' = if modifiers == "" then "" else modifiers ++ "+"
      modifier = [' ' | _ <- [length modifier'..4]] ++ modifier'
  in
  modifier ++ (intercalate "" [generateRow n | n <- [0..3]])

usbHidToText :: Int -> String
usbHidToText n = case n of
  n | n >= 0x04 && n <= 0x1d -> [chr (n - 0x04 + (ord 'a'))]
  n | n >= 0x1e && n <= 0x26 -> [chr (n - 0x1e + (ord '1'))]
  0x27 -> "0"
  0x2a -> "<backspace>"
  0x2c -> "<space>"
  _ -> "0x" ++ showHex n ""


generateTextConfig :: TwiddlerConfig -> [String]
generateTextConfig config =
  let renderModifiers m =
        let m' = (if m .&. 0x01 /= 0 then "C" else "") ++
                 (if m .&. 0x02 /= 0 then "S" else "") ++
                 (if m .&. 0x04 /= 0 then "A" else "") ++
                 (if m .&. 0x08 /= 0 then "4" else "") ++
                 (if m .&. 0x10 /= 0 then "C" else "") ++
                 (if m .&. 0x20 /= 0 then "S" else "") ++
                 (if m .&. 0x40 /= 0 then "A" else "") ++
                 (if m .&. 0x80 /= 0 then "4" else "")
        in if m' == "" then "" else m' ++ "-"
      renderChord (RawChord { keys=keys, output = output }) =
        case output of
          SingleChord m c -> generateTextForKeys keys ++ ": " ++ renderModifiers m ++ usbHidToText c
          MultipleChord m -> generateTextForKeys keys ++ ": " ++ show output
  in
  map renderChord (chords config)

main :: IO ()
main = do
  args <- getArgs
  filename <- case args of
      [ f ] -> return f
      _ -> error "Requires a filename as argument"
  contents <- BL.readFile filename
  config <- return $ readConfig contents
  print config
  putStr $ unlines $ generateTextConfig config
