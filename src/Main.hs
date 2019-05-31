{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module Main where

import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
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
  | MultipleChordIndex { stringIndex :: Int }
  | MultipleChord [ChordOutput]
  deriving Show

data RawChord = RawChord { keys :: [Int], output :: ChordOutput }
  deriving Show

readChordMapping :: G.Get ChordOutput
readChordMapping = do
  mappingL <- fromIntegral <$> G.getWord8
  mappingH <- fromIntegral <$> G.getWord8

  return $ case mappingL of
      0xFF -> MultipleChordIndex mappingH
      _ -> SingleChord { modifier = mappingL, keyCode = mappingH }

readChord :: G.Get RawChord
readChord = do
  rawKeys <- fromIntegral <$> G.getWord16le :: G.Get Int
  keys <- return $ [i | i <- [0..15], rawKeys .&. (1 `shiftL` i) /= 0]

  chord <- readChordMapping

  return $ RawChord keys chord

readLocation :: G.Get Int
readLocation = do
  fromIntegral <$> G.getWord32le :: G.Get Int

readStringContents :: BL.ByteString -> Int -> [ChordOutput]
readStringContents contents offset =
  let tail = BL.drop (fromIntegral offset) contents in
  flip G.runGet tail $ do
    len <- fromIntegral <$> G.getWord16le
    mapM (\() -> readChordMapping) (take (len `div` 2 - 1) $ repeat ())

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

  maxStringLocation <- return $ foldl (\n c -> max n $ case output c of MultipleChordIndex i -> i; _ -> 0) 0 chords
  stringLocations <- mapM (\() -> readLocation) (take (maxStringLocation + 1) $ repeat ())

  chords' <- return $ flip map chords $ \(RawChord keys output) -> case output of
    MultipleChordIndex i -> RawChord keys (MultipleChord $ readStringContents contents (stringLocations !! i))
    _ -> RawChord keys output


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
    chords = chords' }

generateTextForKeys :: [Int] -> String
generateTextForKeys keys =
  let generateRow n =
        let keys' = [k - 4*n | k <- keys, k > 4*n, k < 4*(n+1)]
            letters k =
              case k of
                [] -> ""
                (1:r) -> 'L':letters r
                (2:r) -> 'M':letters r
                (3:r) -> 'R':letters r in
        case letters keys' of
          [] -> "O"
          [k] -> [k]
          letters -> "(" ++ letters ++ ")"
      modifiers = (if 0  `elem` keys then "N" else "") ++
                  (if 4  `elem` keys then "A" else "") ++
                  (if 8  `elem` keys then "C" else "") ++
                  (if 12 `elem` keys then "S" else "")
      modifier' = if modifiers == "" then "" else modifiers ++ "+"
      modifier = [' ' | _ <- [length modifier'..4]] ++ modifier'
  in
  modifier ++ (intercalate "" [generateRow n | n <- [0..3]])

usbHidToText :: Bool -> Int -> (Bool, String)
usbHidToText shift n = case (shift, n) of
  (False, n) | n >= 0x04 && n <= 0x1d -> (False, [chr (n - 0x04 + (ord 'a'))])
  (True, n) | n >= 0x04 && n <= 0x1d -> (False, [chr (n - 0x04 + (ord 'A'))])
  (False, n) | n >= 0x1e && n <= 0x26 -> (shift, [chr (n - 0x1e + (ord '1'))])
  (False, 0x27) -> (False, "0")
  (shift, n) | n >= 0x3a && n <= 0x45 -> (shift, "F" ++ show (n - 0x39))
  (True, 0x1e) -> (False, "!")
  (True, 0x1f) -> (False, "@")
  (True, 0x20) -> (False, "#")
  (True, 0x21) -> (False, "$")
  (True, 0x22) -> (False, "%")
  (True, 0x23) -> (False, "^")
  (True, 0x24) -> (False, "&")
  (True, 0x25) -> (False, "*")
  (True, 0x26) -> (False, "(")
  (True, 0x27) -> (False, ")")
  (_, 0x28) -> (shift, "<return>")
  (_, 0x29) -> (shift, "<escape>")
  (_, 0x2a) -> (shift, "<backspace>")
  (_, 0x2b) -> (shift, "<tab>")
  (_, 0x2c) -> (shift, "<space>")
  (False, 0x2d) -> (False, "-")
  (True, 0x2d) -> (False, "_")
  (False, 0x2e) -> (False, "=")
  (True, 0x2e) -> (False, "+")
  (False, 0x2f) -> (False, "[")
  (True, 0x2f) -> (False, "{")
  (False, 0x30) -> (False, "]")
  (True, 0x30) -> (False, "}")
  (False, 0x31) -> (False, "\\")
  (True, 0x31) -> (False, "|")
  -- 0x32: "Non-US # and ~"
  (False, 0x33) -> (False, ";")
  (True, 0x33) -> (False, ":")
  (False, 0x34) -> (False, "'")
  (True, 0x34) -> (False, "\"")
  (False, 0x35) -> (False, "`")
  (True, 0x35) -> (False, "~")
  (False, 0x36) -> (False, ",")
  (True, 0x36) -> (False, "<")
  (False, 0x37) -> (False, ".")
  (True, 0x37) -> (False, ">")
  (False, 0x38) -> (False, "/")
  (True, 0x38) -> (False, "?")
  (shift, 0x39) -> (shift, "<capslock>")
  (shift, 0x46) -> (shift, "<printscreen>")
  (shift, 0x47) -> (shift, "<scrolllock>")
  (shift, 0x48) -> (shift, "<pause>")
  (shift, 0x49) -> (shift, "<insert>")
  (shift, 0x4a) -> (shift, "<home>")
  (shift, 0x4b) -> (shift, "<pageup>")
  (shift, 0x4c) -> (shift, "<delete>")
  (shift, 0x4d) -> (shift, "<end>")
  (shift, 0x4e) -> (shift, "<pagedown>")
  (shift, 0x4f) -> (shift, "<right>")
  (shift, 0x50) -> (shift, "<left>")
  (shift, 0x51) -> (shift, "<down>")
  (shift, 0x52) -> (shift, "<up>")
  (shift, 0x53) -> (shift, "<numlock>")
  _ -> (shift, "<0x" ++ showHex n ">")


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
      renderSingleChord (SingleChord m c) =
        let (shift, c') = usbHidToText (m .&. 0x22 /= 0) c
        in renderModifiers (m .&. (if shift then 0xFF else 0xDD)) ++ c'
      renderSingleChord _ = error "Rending multichord as singlechord"
      renderChord (RawChord { keys=keys, output = output }) =
        case output of
          SingleChord m c -> generateTextForKeys keys ++ ": " ++ renderSingleChord output
          MultipleChordIndex m -> generateTextForKeys keys ++ ": " ++ show output
          MultipleChord m -> generateTextForKeys keys ++ ": " ++ intercalate "" (map renderSingleChord m)
  in
  map renderChord (chords config)

generateBinConfig :: TwiddlerConfig -> BL.ByteString
generateBinConfig config =
  let flagInt n b = if b then 1 `shiftL` n else 0
      flags =  foldl (+) 0 [
        flagInt 0 $ keyRepeat config,
        flagInt 1 $ directKey config,
        flagInt 2 $ joystickLeftClick config,
        flagInt 3 $ disableBluetooth config,
        flagInt 4 $ stickyNum config,
        flagInt 7 $ stickyShift config]
      chordToInt :: RawChord -> P.Put
      chordToInt (RawChord keys output) = do
        P.putWord16le $ foldl (+) 0 $ map (1 `shiftL`) keys
        case output of
          SingleChord m k -> do
            P.putWord8 $ fromIntegral m
            P.putWord8 $ fromIntegral k
          MultipleChord mcc -> do
            P.putWord8 0xFF
            P.putWord8 0

      -- TODO: Sort the chords "in ascending order of Chord Representation Value"
      chordsTable = mapM_ chordToInt $ chords config
  in P.runPut $ do
  P.putWord8 5
  P.putWord8 $ flags
  P.putWord16le $ fromIntegral $ nchords config
  P.putWord16le $ fromIntegral $ sleepTimeout config
  P.putWord16le $ fromIntegral $ mouseLeftClickAction config
  P.putWord16le $ fromIntegral $ mouseMiddleClickAction config
  P.putWord16le $ fromIntegral $ mouseRightClickAction config
  P.putWord8 $ fromIntegral $ mouseAccelFactor config
  P.putWord8 $ fromIntegral $ keyRepeatDelay config
  P.putWord8 0
  P.putWord8 $ if hapticFeedback config then 1 else 0
  chordsTable

main :: IO ()
main = do
  args <- getArgs
  filename <- case args of
      [ f ] -> return f
      _ -> error "Requires a filename as argument"
  contents <- BL.readFile filename
  config <- return $ readConfig contents
  putStr $ unlines $ generateTextConfig config
  BL.writeFile "output.cfg" $ generateBinConfig config
