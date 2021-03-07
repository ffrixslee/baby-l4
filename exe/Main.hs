module Main where

import Parser (parseProgram)
import Typing (tpProgram)
import System.Environment
import Options.Applicative
import qualified ToGF as GF

process :: InputOpts -> String -> IO ()
process args input = do
  let fpath = filepath args
      ast = parseProgram fpath input
  case ast of
    Right ast -> do
      print (tpProgram $ () <$ ast)
      --print ast
      GF.nlg (getGFL $ format args) ast
    Left err -> do
      putStrLn "Parser Error:"
      print err
  where
    getGFL (Fgf gfl) = gfl
    getGFL (Fall) = GF.GFall

data Format  = Fall | Fgf GF.GFlang deriving Show

data InputOpts = InputOpts
  { format   :: Format
  , filepath :: FilePath
  } deriving Show

optsParse :: Parser InputOpts
optsParse = InputOpts <$>
              subparser
                ( command "all" (info (pure Fall) (progDesc "Prints all available formats"))
               <> command "gf" (info gfSubparser gfHelper))
            <*> argument str (metavar "Filename")
        where
          gfSubparser = subparser ( command "all" (info (pure (Fgf GF.GFall)) (progDesc "tell GF to output all languages"))
                                 <> command "en" (info (pure (Fgf GF.GFeng))   (progDesc "tell GF to output english"))
                                 <> command "swe" (info (pure (Fgf GF.GFswe)) (progDesc "tell GF to output swedish"))
                                  )
                        <**> helper
          gfHelper = fullDesc
                  <> header "l4 gf - specialized for natLang output"
                  <> progDesc "Prints natLang format (subcommands: en, my)"


main :: IO ()
main = do
  let optsParse' = info (optsParse <**> helper) ( fullDesc
                                               <> header "mini-l4 - minimum l4? miniturised l4?")
  opts <- customExecParser (prefs showHelpOnError) optsParse'

  contents <- readFile (filepath opts)
  process opts contents
