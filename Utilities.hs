{-
Copyright (C) 2011 Jeroen Ketema

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

import Codec.Image.STB
import Data.Bitmap.OpenGL
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import System.Directory
import System.Plugins
import System.Process

import Paths_Visualization

-- Try to find a file by probing both the current and data directory.
prefix_path :: FilePath -> IO FilePath
prefix_path s = do
    file_ok <- doesFileExist s
    case file_ok of
        True  -> return s
        False -> do
            data_dir <- getDataDir
            let s' = data_dir ++ "/" ++ s
            file_ok' <- doesFileExist s'
            if file_ok' then return s' else error $ "Cannot find " ++ s

-- Load a texture from an image file.
loadImageTexture :: FilePath -> IO TextureObject
loadImageTexture s = do
    file <- prefix_path s
    stat <- loadImage file
    case stat of
        Left  err -> error $ "loadNode: " ++ err
        Right img -> makeSimpleBitmapTexture img

-- Load a font from a font file.
loadFontTexture :: FilePath -> IO Font
loadFontTexture s = do
    file <- prefix_path s
    createTextureFont file

-- Load a reduction (possibly compiling it on-the-file). After compilation
-- and loading, the remainders are cleaned-up.
loadReduction :: FilePath -> IO (DynamicReduction)
loadReduction s = do
    let to_string = foldr (\x y -> x ++ "\n" ++ y) ""
    putStrLn ("Compiling " ++ s)
    make_stat <- makeAll (s ++ ".hs") ["-i.."]
    case make_stat of
        MakeFailure err -> error $ to_string err
        MakeSuccess _ _ -> putStrLn ("Done compiling " ++ s)
    load_stat <- load (s ++ ".o") [".."] [] "c_reduction"
    reduction <- case load_stat of
        LoadFailure err -> error $ to_string err
        LoadSuccess _ v -> return v
    _ <- runCommand "rm -f *.o *.hi"
    return reduction
