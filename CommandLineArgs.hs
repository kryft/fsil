module CommandLineArgs (FsilOptions,
                        charDumpFile,
                        monsterName,
                        evBonus,
                        nSamples,
                        meleeBonus,
                        singing,
                        alertness,
                        playerOnLitSquare,
                        monsterOnLitSquare,
                        invisibleMonster,
                        parseArgs) where

import System.Environment
import Control.Monad
import System.Console.GetOpt
import System.IO.Error
import System.IO
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Types as T
import qualified Monster as M
import Data.Either(partitionEithers)
import GeneralParse as G

data FsilOptions =
  FsilOptions { charDumpFile :: String,
                monsterName :: String,
                singing :: T.Singing,
                alertness :: M.Alertness,
                meleeBonus :: Int,
                evBonus :: Int,
                nSamples :: Int,
                playerOnLitSquare :: Bool,
                monsterOnLitSquare :: Bool,
                invisibleMonster :: Bool}

defaultOptions = 
  FsilOptions { charDumpFile = "",
                monsterName = "",
                singing = T.Quiet,
                alertness = M.Alert,
                nSamples = 10000,
                meleeBonus = 0,
                evBonus = 0,
                playerOnLitSquare = False,
                monsterOnLitSquare = False,
                invisibleMonster = False}


options :: [OptDescr (FsilOptions -> IO FsilOptions )]
options =
 [ Option ['s'] ["singing"] 
    (ReqArg 
      (\arg opt -> parseStr arg parseSinging 
        >>= \s -> return opt {singing = s}) 
      "SONG[,SONG]") 
    "player character is singing; SONG = trees | staying | sharpness"
 , Option ['a'] ["alertness"] 
    (ReqArg 
      (\arg opt -> parseStr arg parseAlertness 
        >>= \s -> return opt {alertness = s}) 
      "ALERTNESS") 
    "set monster alertness; ALERTNESS = unwary | sleeping"
  , Option [] ["evbonus"] 
    (ReqArg 
      (\arg opt -> parseStr arg G.parseInt
        >>= \n -> return opt {evBonus = n}) 
      "EV_BONUS") 
    "Evasion bonus for the player (integer)"
   , Option [] ["meleebonus"] 
    (ReqArg 
      (\arg opt -> parseStr arg G.parseInt 
        >>= \n -> return opt {meleeBonus = n}) 
      "MELEE_BONUS") 
    "Melee accuracy bonus for the player (integer)"
  , Option ['n'] ["nsamples"] 
    (ReqArg 
      (\arg opt -> parseStr arg G.parseInt 
        >>= \n -> return opt {nSamples = n}) 
      "N_SAMPLES") 
    "Number of samples to use in estimating distributions"
  , Option [] ["plit"] 
    (NoArg 
      (\opt -> return opt {playerOnLitSquare = True})) 
    "Player is standing on a lit square"
  , Option [] ["mlit"] 
    (NoArg 
      (\opt -> return opt {monsterOnLitSquare = True})) 
    "Monster is standing on a lit square"
  , Option [] ["minvisible"] 
    (NoArg 
      (\opt -> return opt {invisibleMonster = True})) 
    "Force monster to be invisible"
 ]

header :: String
header = 
  "Usage: Fsil [-s SONG[,SONG]] [-m ALERTNESS] [--evbonus EV_BONUS] \
    \[--meleebonus MELEE_BONUS] CHAR_DUMP_FILE MONSTER_NAME"

parseStr :: String -> Parser a -> IO a
parseStr str parser = do
  let parseRes = parse parser "" str
  case parseRes of
    Left err -> ioError $ userError (show err)
    Right result -> return result

parseAlertness :: Parser M.Alertness
parseAlertness = 
  try (string "unwary" >> return M.Unwary)
  <|> (string "sleeping" >> return M.Sleeping)

parseSinging :: Parser T.Singing
parseSinging = try (song >>= \s -> eof >> return ( T.OneSong s))
  <|> try wovenThemes 
--  try (song >>= \s -> return $ T.OneSong s)
  
wovenThemes :: Parser T.Singing
wovenThemes = try $ do
  song1 <- song
  char ','
  song2 <- song
  return $ T.WovenThemes (song1,song2)

song :: Parser T.Song
song = 
  try (string "trees" >> return T.SongTrees)
  <|> try (string "staying" >> return T.SongStay)
  <|> try (string "sharpness" >> return T.SongSharp)

parseArgs :: IO (FsilOptions)
parseArgs = do
  args <- getArgs 
  let (actions, nonOptions, errs) = getOpt RequireOrder options args
      info = usageInfo header options
      die err = ioError (userError $ err ++ "\n" ++ info)
  when (not $ null errs)
    $ die (head errs)
  when (not $ 2 == length nonOptions)
    $ die ""
  opts <- foldl (>>=) (return defaultOptions) actions
  let (charDumpFile:monsterName:_) = nonOptions
  return $ opts { charDumpFile = charDumpFile,
                  monsterName = monsterName }
  
