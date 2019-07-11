{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module Main where

import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL.Char8
import Data.Bits ((.&.), (.|.), shiftL)
import Data.Char (chr, ord, isSpace)
import Data.List (intercalate, elemIndex, find)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
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

    chords :: [RawChord]
  }
  deriving Show

defaultTwiddlerConfig = TwiddlerConfig {
    keyRepeat = False,
    directKey = False,
    joystickLeftClick = True,
    disableBluetooth = False,
    stickyNum = False,
    stickyShift = False,
    hapticFeedback = False,

    sleepTimeout = 1500,
    mouseLeftClickAction = 0,
    mouseMiddleClickAction = 0,
    mouseRightClickAction = 0,
    mouseAccelFactor = 10,
    keyRepeatDelay =  100,

    chords = []
  }


data ChordOutput =
    SingleChord { modifier :: Int, keyCode :: Int }
  | MultipleChordIndex { stringIndex :: Int }
  | MultipleChord [ChordOutput]
  deriving (Show, Eq)

data RawChord = RawChord { keys :: [Int], output :: ChordOutput }
  deriving Show

usbHidTable =
  [((Unshifted, n), (Unshifted, [chr (n - 0x04 + (ord 'a'))])) | n <- [0x04 ..  0x1d]] ++
  [((Shifted,  n), (Unshifted, [chr (n - 0x04 + (ord 'A'))])) | n <- [0x04 .. 0x1d]] ++
  [((Unshifted, n), (Unshifted, [chr (n - 0x1e + (ord '1'))])) | n <- [0x1e .. 0x26]] ++
  [((Unshifted, 0x27), (Unshifted, "0"))] ++
  [((ShiftAgnostic, n), (ShiftAgnostic, "<F" ++ show (n - 0x39) ++ ">")) | n <- [0x3a .. 0x45]] ++ [
  ((Shifted, 0x1e), (Unshifted, "!")),
  ((Shifted, 0x1f), (Unshifted, "@")),
  ((Shifted, 0x20), (Unshifted, "#")),
  ((Shifted, 0x21), (Unshifted, "$")),
  ((Shifted, 0x22), (Unshifted, "%")),
  ((Shifted, 0x23), (Unshifted, "^")),
  ((Shifted, 0x24), (Unshifted, "&")),
  ((Shifted, 0x25), (Unshifted, "*")),
  ((Shifted, 0x26), (Unshifted, "(")),
  ((Shifted, 0x27), (Unshifted, ")")),
  ((ShiftAgnostic, 0x28), (ShiftAgnostic, "<return>")),
  ((ShiftAgnostic, 0x29), (ShiftAgnostic, "<escape>")),
  ((ShiftAgnostic, 0x2a), (ShiftAgnostic, "<backspace>")),
  ((ShiftAgnostic, 0x2b), (ShiftAgnostic, "<tab>")),
  ((ShiftAgnostic, 0x2c), (ShiftAgnostic, "<space>")),
  ((Unshifted, 0x2d), (Unshifted, "-")),
  ((Shifted, 0x2d), (Unshifted, "_")),
  ((Unshifted, 0x2e), (Unshifted, "=")),
  ((Shifted, 0x2e), (Unshifted, "+")),
  ((Unshifted, 0x2f), (Unshifted, "[")),
  ((Shifted, 0x2f), (Unshifted, "{")),
  ((Unshifted, 0x30), (Unshifted, "]")),
  ((Shifted, 0x30), (Unshifted, "}")),
  ((Unshifted, 0x31), (Unshifted, "\\")),
  ((Shifted, 0x31), (Unshifted, "|")),
  -- 0x32: "Non-US # and ~"
  ((Unshifted, 0x33), (Unshifted, ";")),
  ((Shifted, 0x33), (Unshifted, ":")),
  ((Unshifted, 0x34), (Unshifted, "'")),
  ((Shifted, 0x34), (Unshifted, "\"")),
  ((Unshifted, 0x35), (Unshifted, "`")),
  ((Shifted, 0x35), (Unshifted, "~")),
  ((Unshifted, 0x36), (Unshifted, ",")),
  ((Shifted, 0x36), (Unshifted, "<")),
  ((Unshifted, 0x37), (Unshifted, ".")),
  ((Shifted, 0x37), (Unshifted, ">")),
  ((Unshifted, 0x38), (Unshifted, "/")),
  ((Shifted, 0x38), (Unshifted, "?")),
  ((ShiftAgnostic, 0x39), (ShiftAgnostic, "<capslock>")),
  ((ShiftAgnostic, 0x46), (ShiftAgnostic, "<printscreen>")),
  ((ShiftAgnostic, 0x47), (ShiftAgnostic, "<scrolllock>")),
  ((ShiftAgnostic, 0x48), (ShiftAgnostic, "<pause>")),
  ((ShiftAgnostic, 0x49), (ShiftAgnostic, "<insert>")),
  ((ShiftAgnostic, 0x4a), (ShiftAgnostic, "<home>")),
  ((ShiftAgnostic, 0x4b), (ShiftAgnostic, "<pageup>")),
  ((ShiftAgnostic, 0x4c), (ShiftAgnostic, "<delete>")),
  ((ShiftAgnostic, 0x4d), (ShiftAgnostic, "<end>")),
  ((ShiftAgnostic, 0x4e), (ShiftAgnostic, "<pagedown>")),
  ((ShiftAgnostic, 0x4f), (ShiftAgnostic, "<right>")),
  ((ShiftAgnostic, 0x50), (ShiftAgnostic, "<left>")),
  ((ShiftAgnostic, 0x51), (ShiftAgnostic, "<down>")),
  ((ShiftAgnostic, 0x52), (ShiftAgnostic, "<up>")),
  ((ShiftAgnostic, 0x53), (ShiftAgnostic, "<numlock>"))]

usbHidMap = Map.fromList usbHidTable

usbHidUnmap = Map.fromList $ flip map usbHidTable $ \((s, k), (s', k')) -> (k', (s, s', k))

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
    sleepTimeout = sleepTimeout,
    mouseLeftClickAction = mouseLeftClickAction,
    mouseMiddleClickAction = mouseMiddleClickAction,
    mouseRightClickAction = mouseRightClickAction,
    mouseAccelFactor = mouseAccelFactor,
    keyRepeatDelay = keyRepeatDelay,
    hapticFeedback = hapticFeedback,
    chords = chords' }

flagFields = [
  "version",
  "keyRepeat",
  "directKey",
  "joystickLeftClick",
  "disableBluetooth",
  "stickyNum",
  "stickyShift",
  "sleepTimeout",
  "mouseLeftClickAction",
  "mouseMiddleClickAction",
  "mouseRightClickAction",
  "mouseAccelFactor",
  "keyRepeatDelay",
  "hapticFeedback"
  ]

readTextConfig :: BL.ByteString -> TwiddlerConfig
readTextConfig contents =
  let trim = dropWhile isSpace
      conf = lines $ BL.Char8.unpack contents
      split s = (takeWhile (not . isSpace) (trim s), trim (dropWhile (not . isSpace) $ trim s))
      parseModifiers k = case k of
        [] -> []
        ('N':r) -> 0:parseModifiers r
        ('A':r) -> 4:parseModifiers r
        ('C':r) -> 8:parseModifiers r
        ('S':r) -> 12:parseModifiers r
        (c:r) -> error $ "Unknown modifier" ++ [c]
      parseMainChord :: String -> Int -> [Int]
      parseMainChord k n = if n == 4 then [] else
        let parseLetter l = case l of
              'O' -> []
              'L' -> [4*n + 1]
              'M' -> [4*n + 2]
              'R' -> [4*n + 3]
              _ -> error $ "Unexpected key: " ++ [l]
        in case k of
          '(':r ->
            let prefix = tail $ takeWhile (not . (== ')')) k
                suffix = tail $ dropWhile (not . (== ')')) k in
                concatMap parseLetter prefix ++ parseMainChord suffix (n+1)
          l:r -> parseLetter l ++ parseMainChord r (n + 1)
      parseChord k = case find (== '+') k of
        Nothing -> parseMainChord k 0
        Just _ -> parseModifiers (takeWhile (not . (== '+')) k) ++ parseMainChord (tail $ dropWhile (not . (== '+')) k) 0
      parseOutput v = textToUsb v
      parseRow row = case split row of
        (key, value) -> case find (== key) flagFields of
          Just f -> Left (key, value)
          Nothing -> Right (RawChord (parseChord key) (parseOutput value))
      parsed = map parseRow conf

      readBool str = case read str of
        0 -> False
        1 -> True
        _ -> error $ "Invalid boolean: " ++ str

      addRowToConfig config row = case row of
        Left ("version", "0") -> config
        Left ("keyRepeat", v) -> config { keyRepeat = readBool v }
        Left ("directKey", v) -> config { directKey = readBool v }
        Left ("joystickLeftClick", v) -> config { joystickLeftClick = readBool v }
        Left ("disableBluetooth", v) -> config { disableBluetooth = readBool v }
        Left ("stickyNum", v) -> config { stickyNum = readBool v }
        Left ("stickyShift", v) -> config { stickyShift = readBool v }
        Left ("hapticFeedback", v) -> config { hapticFeedback = readBool v }

        Left ("sleepTimeout", v) -> config { sleepTimeout = read v }
        Left ("mouseLeftClickAction", v) -> config { mouseLeftClickAction = read v }
        Left ("mouseMiddleClickAction", v) -> config { mouseMiddleClickAction = read v }
        Left ("mouseRightClickAction", v) -> config { mouseRightClickAction = read v }
        Left ("mouseAccelFactor", v) -> config { mouseAccelFactor = read v }
        Left ("keyRepeatDelay", v) -> config { keyRepeatDelay = read v }

        Right (chord) -> config { chords = chords' }
          where chords' = chords config ++ [chord]
        _ -> error $ "Bad row: " ++ show row
  in
  foldl addRowToConfig defaultTwiddlerConfig parsed

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

data Shifted =
    Shifted
  | Unshifted
  | ShiftAgnostic
  deriving (Ord, Eq, Show)


usbHidToText :: Bool -> Int -> (Bool, String)
usbHidToText shift n =
  let mapShift shift' = case shift' of
        ShiftAgnostic -> shift
        Shifted -> True
        Unshifted -> False
      unmapShift shift = if shift then Shifted else Unshifted
  in
  case usbHidMap Map.!? (unmapShift shift, n) of
    Just (shift', k) -> (mapShift shift', k)
    _ -> case usbHidMap Map.!? (ShiftAgnostic, n) of
      Just (shift', k) -> (mapShift shift', k)
      _ -> (shift, "<0x" ++ showHex n ">")

textToUsb' :: String -> [ChordOutput]
textToUsb' str =
  let unmapChar :: String -> Bool -> (Bool, Int)
      unmapChar rawC shift =
        case usbHidUnmap Map.!? c of
          Nothing -> error $ "Error: Unknown string: " ++ c
          Just (Shifted, s', c') -> (True, c')
          Just (Unshifted, s', c') -> (False, c')
          Just (ShiftAgnostic, s', c') -> (shift, c')
        where c = case rawC of
                [c] -> [c]
                _ -> '<':rawC++">"
      isShifted mods = 'S' `elem` mods
      unmapMods mods = case mods of
        'C':r -> 0x01 .|. unmapMods r
        'S':r -> 0x02 .|. unmapMods r
        'A':r -> 0x04 .|. unmapMods r
        '4':r -> 0x08 .|. unmapMods r
        [] -> 0
      mergeMods mods (shift, code) =
        SingleChord shiftedMods code
        where shiftedMods = if shift then mods .|. 0x02 else mods
  in case str of
    '\\':c:r -> mergeMods 0 (unmapChar [c] False) :textToUsb' r
    '<':r -> mergeMods (unmapMods mod) (shift, keycode):textToUsb' (tail rest')
      where (prefix, rest) = break (== '>') r
            (mod', char) =  break (== '-') prefix
            (mod, char') = if char == "" then ("", mod') else (mod', tail char)
            (char'', rest') = case (char', rest) of
                ("", ('>':'>':r')) -> (">", '>':r')
                _ -> (char', rest)
            (shift, keycode) = unmapChar char'' (isShifted mod)
    c:r -> mergeMods 0 (unmapChar [c] False):textToUsb' r
    [] -> []

textToUsb :: String -> ChordOutput
textToUsb str =
  case textToUsb' str of
    [] -> error "Empty chord?"
    [h] -> h
    l -> MultipleChord l

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
        in if m' == "" then "" else "<" ++ m' ++ "-"
      renderSingleChord (SingleChord m c) =
        let (shift, c') = usbHidToText (m .&. 0x22 /= 0) c
            mods = renderModifiers $ m .&. (if shift then 0xFF else 0xDD)
            suffix = if mods == "" then "" else ">"
            escape = if c' == "<" && mods == "" then "\\" else ""
            c'' = if mods /= "" && head c' == '<' && last c' == '>' then reverse (tail $ reverse $ tail c') else c'
        in  mods ++ escape ++ c'' ++ suffix
      renderSingleChord _ = error "Rending multichord as singlechord"
      renderChord (RawChord { keys=keys, output = output }) =
        case output of
          SingleChord m c -> generateTextForKeys keys ++ " " ++ renderSingleChord output
          MultipleChordIndex m -> generateTextForKeys keys ++ " " ++ show output
          MultipleChord m -> generateTextForKeys keys ++ " " ++ intercalate "" (map renderSingleChord m)
      renderField field = if field config then "1" else "0"
  in
  ["version 0",
   "keyRepeat " ++ renderField keyRepeat,
   "directKey " ++ renderField directKey,
   "joystickLeftClick " ++ renderField joystickLeftClick,
   "disableBluetooth " ++ renderField disableBluetooth,
   "stickyNum " ++ renderField stickyNum,
   "stickyShift " ++ renderField stickyShift,
   "hapticFeedback " ++ renderField hapticFeedback,
   "sleepTimeout " ++ show (sleepTimeout config),
   "mouseLeftClickAction " ++ show (mouseLeftClickAction config),
   "mouseMiddleClickAction " ++ show (mouseMiddleClickAction config),
   "mouseRightClickAction " ++ show (mouseRightClickAction config),
   "mouseAccelFactor " ++ show (mouseAccelFactor config),
   "keyRepeatDelay " ++ show (keyRepeatDelay config)] ++
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
      -- TODO: Dedupe repeated stringContents
      stringContents = map (\(RawChord _ (MultipleChord cs)) -> cs) $
          filter (\(RawChord _ output) -> case output of
              MultipleChord _ -> True
              _ -> False) $ chords config
      baseStringContentLocation = 16 + 4 * length (chords config) + 4 * length stringContents
      stringLocations = reverse $ snd $ foldl (\(last, acc) str -> (last + 2 + 2*length str, last:acc)) (baseStringContentLocation, []) stringContents
      chordToInt :: RawChord -> P.Put
      chordToInt (RawChord keys output) = do
        P.putWord16le $ foldl (+) 0 $ map (1 `shiftL`) keys
        case output of
          SingleChord m k -> do
            P.putWord8 $ fromIntegral m
            P.putWord8 $ fromIntegral k
          MultipleChord mcc -> do
            P.putWord8 0xFF
            P.putWord8 $ fromIntegral $ fromJust $ elemIndex mcc stringContents

      -- TODO: Sort the chords "in ascending order of Chord Representation Value"
      chordsTable = mapM_ chordToInt $ chords config
      writeStringContent :: [ChordOutput] -> P.Put
      writeStringContent cs = do
        P.putWord16le $ fromIntegral $ 2 + 2 * length cs
        flip mapM_ cs $ \output -> case output of
          SingleChord m k -> do
            P.putWord8 $ fromIntegral m
            P.putWord8 $ fromIntegral k
          _ -> error "Non-single chord in MCC"
  in P.runPut $ do
  P.putWord8 5
  P.putWord8 $ flags
  P.putWord16le $ fromIntegral $ length $ chords config
  P.putWord16le $ fromIntegral $ sleepTimeout config
  P.putWord16le $ fromIntegral $ mouseLeftClickAction config
  P.putWord16le $ fromIntegral $ mouseMiddleClickAction config
  P.putWord16le $ fromIntegral $ mouseRightClickAction config
  P.putWord8 $ fromIntegral $ mouseAccelFactor config
  P.putWord8 $ fromIntegral $ keyRepeatDelay config
  P.putWord8 0
  P.putWord8 $ if hapticFeedback config then 1 else 0
  chordsTable
  mapM_ (P.putWord32le . fromIntegral) stringLocations
  mapM_ (writeStringContent) stringContents

main :: IO ()
main = do
  args <- getArgs
  filename <- case args of
      [ f ] -> return f
      _ -> error "Requires a filename as argument"
  contents <- BL.readFile filename
  config <-
    if BL.take 9 contents == BL.Char8.pack "version 0" then
      return $ readTextConfig contents
    else
      return $ readConfig contents
  writeFile "output.txt" $ unlines $ generateTextConfig config
  BL.writeFile "output.cfg" $ generateBinConfig config
