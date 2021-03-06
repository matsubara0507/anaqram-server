module Main where

import           Paths_anaqram_server      (version)
import           RIO

import qualified AnaQRam
import           Configuration.Dotenv      (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           Data.Fallible
import           GetOpt                    (withGetOpt')
import           Mix
import           Mix.Plugin.Logger         as MixLogger
import qualified Mix.Plugin.Persist.Sqlite as MixDB
import qualified Version

main :: IO ()
main = withGetOpt' "[options] [input-file]" opts $ \r args usage -> do
  _ <- tryIO $ loadFile defaultConfig
  if | r ^. #help    -> hPutBuilder stdout (fromString usage)
     | r ^. #version -> hPutBuilder stdout (Version.build version <> "\n")
     | otherwise     -> runCmd r (fromMaybe defaultPath $ listToMaybe args)
  where
    defaultPath = "./.anaqram-server.yaml"
    opts = #help    @= optFlag ['h'] ["help"] "Show this help text"
        <: #version @= optFlag [] ["version"] "Show version"
        <: #verbose @= optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""
        <: #migrate @= optFlag [] ["migrate"] "Migrate SQLite"
        <: nil

type Options = Record
  '[ "help"    >: Bool
   , "version" >: Bool
   , "verbose" >: Bool
   , "migrate" >: Bool
   ]

runCmd :: Options -> FilePath -> IO ()
runCmd opts path = evalContT $ do
  config <- AnaQRam.readConfig path !?= exit . displayErr
  let plugin = hsequence
             $ #logger <@=> MixLogger.buildPlugin logOpts
            <: #config <@=> pure config
            <: #sqlite <@=> MixDB.buildPlugin (fromString $ config ^. #sqlite_path) 2
            <: nil
  if | opts ^. #migrate -> lift (Mix.run plugin AnaQRam.migrate)
     | otherwise        -> lift (Mix.run plugin AnaQRam.app)
  where
    logOpts = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
    displayErr err = hPutBuilder stderr $ "cannot read config: " <> fromString (show err)
