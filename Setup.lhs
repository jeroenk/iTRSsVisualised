#! /usr/bin/env runhaskell

> import Prelude hiding (catch)
> import Control.Exception
> import Distribution.PackageDescription
> import Distribution.Simple
> import Distribution.Simple.LocalBuildInfo
> import System.Directory
> import System.FilePath
> import System.IO.Error hiding (catch)
> import System.Process

The following is a hack to be able to use visualization immediately after build

>
> progName = "visualization"
>
> pre_build _ _ = do
>    current <- getCurrentDirectory
>    removeFile (current ++ pathSeparator : progName) `catch` handleExists
>    return emptyHookedBuildInfo
>    where handleExists e
>              | isDoesNotExistError e = return ()
>              | otherwise             = throwIO e
>
> post_build _ _ _ build_info = do
>    current <- getCurrentDirectory
>    copyFile build (current ++ vis)
>    where build = (buildDir build_info) ++ vis ++ vis
>          vis   = pathSeparator : progName
>
> post_clean _ _ _ _ = do
>    current <- getCurrentDirectory
>    removeFile (current ++ pathSeparator : progName)
>
> myUserHooks = simpleUserHooks {preBuild  = pre_build,
>                                postBuild = post_build,
>                                postClean = post_clean}
>
> main = defaultMainWithHooks myUserHooks
>
