{-
Copyright (C) 2011, 2012 Jeroen Ketema

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

-- This module defines some I/O related helper functions.

module Utilities (
    loadImageTexture,
    loadFontTexture,
    loadReduction
) where

import DynamicReduction

import Prelude
import Codec.Image.STB
import Data.Bitmap.OpenGL
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import System.Directory
import System.FilePath
import System.Plugins

import Paths_Visualization

-- Try to find a file by probing both the current and data directory.
prefixPath :: FilePath -> IO FilePath
prefixPath s = do
    file_ok <- doesFileExist s
    if file_ok
        then return s
        else do
            data_dir <- getDataDir
            let s' = data_dir ++ pathSeparator : s
            file_ok' <- doesFileExist s'
            if file_ok'
                then return s'
                else error ("Cannot find " ++ s)

-- Load a texture from an image file.
loadImageTexture :: FilePath -> IO TextureObject
loadImageTexture s = do
    file <- prefixPath s
    stat <- loadImage file
    case stat of
        Left  err -> error $ "loadNode: " ++ err
        Right img -> makeSimpleBitmapTexture img

-- Load a font from a font file.
loadFontTexture :: FilePath -> IO Font
loadFontTexture s = do
    file <- prefixPath s
    createTextureFont file

-- Helper function for cleaning up after loading of a reduction
removeObjects :: IO ()
removeObjects = do
    contents <- getDirectoryContents "."
    let objects = filter (\s -> takeExtension s `elem` [".o", ".hi"]) contents
    mapM_ removeFile objects

-- Load a reduction (possibly compiling it on-the-file). After compilation
-- and loading, the remainders are cleaned-up.
loadReduction :: FilePath -> IO DynamicReduction
loadReduction s = do
    let toString = foldr (\x y -> x ++ "\n" ++ y) ""
    putStrLn ("Compiling " ++ s)
    make_stat <- makeAll (s ++ ".hs") ["-i.."]
    case make_stat of
        MakeFailure err -> error $ toString err
        MakeSuccess _ _ -> putStrLn ("Done compiling " ++ s)
    load_stat <- load_ (s ++ ".o") [".", ".."] "cReduction"
    reduction <- case load_stat of
        LoadFailure err -> error $ toString err
        LoadSuccess _ v -> return v
    removeObjects
    return reduction
