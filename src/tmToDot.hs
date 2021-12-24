{-|
Module       : tmToDot program
Description  : The main program for compiling tm in text to dotfile
Maintainer   : Natalia OM and Xoaquin Baca
Fully based on CS131 hmc pic2ps file
You can load this file into ghci with
    :load tmToDot.hs
and then run
    run "../examples/example1.txt"
and so on, or you can compile it on the command line with

-}

module Main where

import Parser  (turingMachine, parseFile)
--import PostScript (emitCode)
import DotFile (emitCode, doElems)
import TuringEval

import qualified System.Environment    as SysEnv
import qualified System.FilePath.Posix as FilePath


-- | Given a filename, translates the pic program in that file to PostScript,
--   then write the PostScript code to a .ps file.
run :: String -> IO ()

-- You can mostly ignore the code beyond this point.

-- However, you can uncomment the putStr line below if you want to see
-- the abstract syntax being returned by the parser.  Please
-- re-comment this before submitting your final code.
run filename =
  do putStrLn ("Reading " ++ filename)
     absyn <- parseFile turingMachine filename
     -- This error check is really done just make sure the absyn is
     -- fully read before we continue.  Sometimes we *want* sequencing.
     if length absyn == 0 then error "Internal error: No elements!"
                          else return ()
     -- putStr (show absyn ++ "\n")      -- Uncomment if you want to see the parsed abstract syntax.
     let dotCode = doElems absyn
     let dotFileName = FilePath.replaceExtension filename "dot"
     DotFile.emitCode dotFileName dotCode


--------------------------------------------------------------------------------
-- The main program 
--------------------------------------------------------------------------------

-- | Reads the txt filename from the command line, then translates the txt file
--   to dot.
main :: IO ()
main =
  do args <- SysEnv.getArgs
     let filename = case args of
                      [arg] -> arg
                      _ -> error "exactly one filename expected"
     run filename