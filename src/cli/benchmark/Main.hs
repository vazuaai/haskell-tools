
{-# LANGUAGE LambdaCase
           , ViewPatterns
           , TypeFamilies
           , RecordWildCards
           , DeriveGeneric
           #-}
module Main where

import Criterion.Measurement hiding (runBenchmark)
import Criterion.Types hiding(measure)

import qualified Data.ByteString.Lazy.Char8 as LazyBS (pack, unpack)
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import Data.Aeson
import Data.Knob
import GHC.Generics
import System.FilePath
import System.Directory
import System.IO
import Control.Monad
import Control.Exception
import System.Environment
import Data.List
import Data.List.Split
import Data.Time.Clock
import Data.Time.Calendar

import Language.Haskell.Tools.Refactor.CLI

rootDir = ".." </> ".." </> "examples"

main :: IO ()
main = do
    args <- getArgs
    (year, month, day) <- date
    exists <- doesDirectoryExist (rootDir </> "CppHs_bench")
    when exists $ removeDirectoryRecursive (rootDir </> "CppHs_bench")
    copyDir (rootDir </> "CppHs") (rootDir </> "CppHs_bench")
    benches <- bms
    cases <- bms2Mcases (Date {..}) benches
    case args of 
      [file] -> writeFile file (show $ encode cases)
      _ -> putStrLn $ LazyBS.unpack $ encode cases
    putStrLn "Execution times (cycles):"
    mapM_ (\c -> putStrLn $ "# " ++ bmId (bm c) ++ ": " ++ showGrouped (measCycles (ms c))) cases
    removeDirectoryRecursive (rootDir </> "CppHs_bench")
  where showGrouped = reverse . concat . intersperse " " . chunksOf 3 . reverse . show

date :: IO (Integer,Int,Int) -- (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay

data Date = Date { year  :: Integer
                 , month :: Int
                 , day   :: Int
                 }
  deriving (Eq, Show, Generic)

data BM = BM { bmId       :: String
             , workingDir :: FilePath
             , refactors  :: [String]
             }
  deriving (Eq, Show, Generic)

data BMCase = BMCase { bm :: BM
                     , ms :: Measured
                     , dt :: Date
                     }
  deriving (Eq, Show, Generic)

instance FromJSON Date
instance ToJSON Date
instance FromJSON BM
instance ToJSON BM
instance FromJSON BMCase
instance ToJSON BMCase

bms :: IO [BM]
bms = forM ["full-1", "full-2", "full-3", "three-gen-sigs", "selects", "empty"] $ \id -> do
        commands <- readFile ("bench-tests" </> id <.> "txt")
        return (BM { bmId = id, workingDir = rootDir </> "CppHs_bench", refactors = lines commands })

bms2Mcases :: Date -> [BM] -> IO [BMCase]
bms2Mcases = mapM . bm2Mcase

runBenchmark :: Benchmarkable -> IO Measured
runBenchmark bm = fst <$> (measure bm 1)

bm2Mms :: BM -> IO Measured
bm2Mms (BM { .. }) = runBenchmark $ benchmakable workingDir refactors

bm2Mcase :: Date -> BM -> IO BMCase
bm2Mcase d bm = BMCase bm <$> (bm2Mms bm) <*> (return d)

benchmakable :: String -> [String] -> Benchmarkable -- IO (Either String String)
benchmakable wd rfs = Benchmarkable $ \ _ -> makeCliTest wd rfs

makeCliTest :: String -> [String] -> IO ()
makeCliTest wd rfs = do   
  inKnob <- newKnob (BS.pack $ unlines rfs)
  inHandle <- newFileHandle inKnob "<input>" ReadMode
  outKnob <- newKnob (BS.pack [])
  outHandle <- newFileHandle outKnob "<output>" WriteMode
  refactorSession inHandle outHandle [wd]

copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dst = do
  createDirectory dst
  content <- getDirectoryContents src
  let xs = filter (`notElem` [".", ".."]) content
  forM_ xs $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then copyDir srcPath dstPath
      else copyFile srcPath dstPath
