#! /usr/bin/env runhaskell

> import Distribution.Simple
> import Distribution.Simple.LocalBuildInfo
> import System.Process

The following is a hack to be able to use visualization immediately after build

> pbuild _ _ _ build_info = do
>    pid <- runCommand ("cp " ++ build ++ " .")
>    waitForProcess pid
>    return ()
>    where build = (buildDir build_info) ++ vis ++ vis
>          vis = "/visualization"
>
> pclean _ _ _ _ = do
>    pid <- runCommand "rm visualization"
>    waitForProcess pid
>    return ()
>
> myUserHooks = simpleUserHooks {postBuild = pbuild,
>                                postClean = pclean}
>
> main = defaultMainWithHooks myUserHooks
