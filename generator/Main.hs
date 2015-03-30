{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      :  $Header$
-- Copyright   :  (c) 2014-2015 Galois, Inc.
-- License     :  BSD3
-- Maintainer  :  cryptol@galois.com
-- Stability   :  provisional
-- Portability :  portable

module Main where

import CodeGen
import Cryptol.ModuleSystem (loadModuleByPath, initialModuleEnv)
import Cryptol.ModuleSystem.Env (moduleDeps,ModuleEnv(..))
import Cryptol.ModuleSystem.Monad (runModuleM, ModuleM, io, modifyModuleEnv)
import Cryptol.Utils.PP (pp)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.String (fromString)
import Options
import System.Directory (doesFileExist,getDirectoryContents)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (takeExtension,splitSearchPath)
import System.IO (hPrint, hPutStrLn, stderr)

data CGOptions = CGOptions
  { optLoad   :: [FilePath]
  , optOutput :: Maybe FilePath
  , optRoot   :: Maybe GenerationRoot
  , optTarget :: GenerationTarget
  , optCryptolPathOnly :: Bool
  } deriving (Show)

argParser :: ArgParser CGOptions
argParser = ArgParser
  { nonOptions  = ReturnInOrder addFile
  , defOptions  = defaultOptions CGOptions
    { optLoad   = []
    , optOutput = Nothing
    , optRoot   = Nothing
    , optTarget = SBVC
    , optCryptolPathOnly = False
    }
  , description = defaultDescription
    [ Option "o" ["output-dir"] (ReqArg setOutput "DIR")
      "output directory for code generation (default stdout)"

    , Option ""  ["root"] (ReqArg setRoot "UNIT")
      "generate code for the specified identifier, module, file, or directory"

    , Option "t" ["target"] (ReqArg setTarget "BACKEND")
      "code generation backend (default SBV-C)"

    , Option ""  ["cryptolpath-only"] (NoArg setCryptolPathOnly)
      "only look for .cry files in CRYPTOLPATH; don't use built-in locations"
    ]
  , toolName = "Cryptol Code Generator"
  }

-- | Set a single file to be loaded.  This should be extended in the future, if
-- we ever plan to allow multiple files to be loaded at the same time.
addFile :: String -> OptParser (Options CGOptions)
addFile path = modifyOpt $ \ opts -> opts { optLoad = path : optLoad opts }

-- | Choose a unit for code generation. Heuristic: it's always an identifier.
-- This also signals that code generation should be performed instead of
-- dropping into the REPL.
-- XXX Use a better heuristic.
setRoot :: String -> OptParser (Options CGOptions)
setRoot id = modifyOpt $ \opts -> opts { optRoot = Just (Identifier id) }

-- | Choose an output directory.
setOutput :: FilePath -> OptParser (Options CGOptions)
setOutput path = modifyOpt $ \opts -> opts { optOutput = Just path }

-- | Choose a code generation target.
setTarget :: String -> OptParser (Options CGOptions)
setTarget target = case fromString target of
  Just t  -> modifyOpt $ \opts -> opts { optTarget = t }
  Nothing -> report $ "Unknown backend " ++ target ++
                      ". Choices are " ++ intercalate ", " knownTargets

setCryptolPathOnly :: OptParser (Options CGOptions)
setCryptolPathOnly  = modifyOpt $ \opts -> opts { optCryptolPathOnly = True }

main :: IO ()
main = getOpts argParser >>= codeGenFromOpts

-- | Precondition: the generation root must be 'Just'.
codeGenFromOpts :: CGOptions -> IO ()
codeGenFromOpts CGOptions { .. } = inFreshEnv $
  do mCryptolPath <- io (lookupEnv "CRYPTOLPATH")
     let cryptolPath = case mCryptolPath of
           Nothing -> []

           Just path ->
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
                  -- Windows paths search from end to beginning
                  reverse (splitSearchPath path)
#else
                  splitSearchPath path
#endif

     modifyModuleEnv $ \ ModuleEnv { .. } ->
       ModuleEnv { meSearchPath = if optCryptolPathOnly
                                     then cryptolPath
                                     else cryptolPath ++ meSearchPath
                 , .. }

     case optRoot of
       Just root -> codeGen optOutput root optTarget
       Nothing   -> io $ do putStrLn "No root specified"
                            exitFailure


inFreshEnv :: ModuleM () -> IO ()
inFreshEnv body =
  do env         <- initialModuleEnv
     (res,warns) <- runModuleM env body
     mapM_ (print . pp) warns
     case res of
       Right (a,_) ->    return ()
       Left err    -> do print (pp err)
                         exitFailure
