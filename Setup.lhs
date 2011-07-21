#! /usr/bin/env runhaskell

> import Distribution.PackageDescription
> import Distribution.Simple
> import Distribution.Simple.LocalBuildInfo
> import System.Directory
> import System.FilePath
> import System.Process

The following is a hack to be able to use visualization immediately after build

>
> prog_name = "visualization"
>
> pre_build _ _ = do
>    current <- getCurrentDirectory
>    ignore $ removeFile (current ++ pathSeparator:prog_name)
>    return emptyHookedBuildInfo
>    where ignore c = catch c (\_ -> return ())
>
> post_build _ _ _ build_info = do
>    current <- getCurrentDirectory
>    copyFile build (current ++ vis)
>    where build = (buildDir build_info) ++ vis ++ vis
>          vis = pathSeparator:prog_name
>
> post_clean _ _ _ _ = do
>    current <- getCurrentDirectory
>    removeFile (current ++ pathSeparator:prog_name)
>
> myUserHooks = simpleUserHooks {preBuild  = pre_build,
>                                postBuild = post_build,
>                                postClean = post_clean}
>
> main = defaultMainWithHooks myUserHooks

