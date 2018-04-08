{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Monad.IO.Class
import System.FilePath
import System.Directory

import Futhark.Pipeline
import Futhark.Passes
import qualified Futhark.CodeGen.Backends.SequentialCSharp as SequentialCS
import Futhark.Util.Pretty (prettyText)
import Futhark.Compiler.CLI

main :: IO ()
main = compilerMain () []
       "Compile sequential C#" "Generate sequential C# code from optimised Futhark program."
       sequentialCpuPipeline $ \() mode outpath prog -> do
          let class_name =
                case mode of ToLibrary -> Just $ takeBaseName outpath
                             ToExecutable -> Nothing
          csprog <- either (`internalError` prettyText prog) return =<<
                    SequentialCS.compileProg class_name prog


          let outpath' = outpath `addExtension` "cs"
          liftIO $ writeFile outpath' csprog
          case mode of
            ToLibrary ->
              return ()
            ToExecutable -> liftIO $ do
              perms <- liftIO $ getPermissions outpath'
              setPermissions outpath' $ setOwnerExecutable True perms
