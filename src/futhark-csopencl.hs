{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Monad.IO.Class
import System.FilePath
import System.Directory

import Futhark.Pipeline
import Futhark.Passes
import qualified Futhark.CodeGen.Backends.CSOpenCL as CSOpenCL
import Futhark.Util.Pretty (prettyText)
import Futhark.Compiler.CLI

main :: IO ()
main = compilerMain () []
       "Compile OpenCL C#" "Generate OpenCL C# code from optimised Futhark program."
       gpuPipeline $ \() mode outpath prog -> do
          let class_name =
                case mode of ToLibrary -> Just $ takeBaseName outpath
                             ToExecutable -> Nothing
          csprog <- either (`internalError` prettyText prog) return =<<
                    CSOpenCL.compileProg class_name prog


          let outpath' = outpath `addExtension` "cs"
          liftIO $ writeFile outpath' csprog
          case mode of
            ToLibrary ->
              return ()
            ToExecutable -> liftIO $ do
              perms <- liftIO $ getPermissions outpath'
              setPermissions outpath' $ setOwnerExecutable True perms
