module Main where

import           Paths_anaqram_server   (version)
import           RIO

import           AnaQRam.Cmd
import           AnaQRam.Env
import           Configuration.Dotenv   (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           GetOpt                 (withGetOpt')
import           Mix
import           Mix.Plugin.Logger      as MixLogger
import qualified Version

main :: IO ()
main = withGetOpt' "[options] [input-file]" opts $ \r args usage -> do
  _ <- tryIO $ loadFile defaultConfig
  if | r ^. #help    -> hPutBuilder stdout (fromString usage)
     | r ^. #version -> hPutBuilder stdout (Version.build version)
     | otherwise     -> runCmd r (fromMaybe defaultPath $ listToMaybe args)
  where
    defaultPath = "./.anaqram-server.yaml"
    opts = #help    @= optFlag ['h'] ["help"] "Show this help text"
        <: #version @= optFlag [] ["version"] "Show version"
        <: #verbose @= optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""
        <: nil

type Options = Record
  '[ "help"    >: Bool
   , "version" >: Bool
   , "verbose" >: Bool
   ]

runCmd :: Options -> FilePath -> IO ()
runCmd opts path = do
  config <- readConfig path
  let plugin = hsequence
             $ #logger <@=> MixLogger.buildPlugin logOpts
            <: #config <@=> pure config
            <: nil
  Mix.run plugin cmd
  where
    logOpts = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
