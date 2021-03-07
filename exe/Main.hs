module Main where

import Parser (parseProgram)
import Typing (tpProgram)
import System.Environment
import Options.Applicative
import qualified ToGF as GF

process :: FilePath -> String -> IO ()
process filepath input = do
  let ast = parseProgram filepath input
  case ast of
    Right ast -> do
      print (tpProgram $ () <$ ast)
      --print ast
      GF.nlg ast
    Left err -> do
      putStrLn "Parser Error:"
      print err

data Format  = Fall | Fgf GFlang deriving Show
data GFlang  = GFeng | GFmalay deriving Show

data InputOpts = InputOpts
  { format   :: Format
  , filepath :: FilePath --Maybe FilePath
  } deriving Show

optsParse :: Parser InputOpts
optsParse = InputOpts <$>
              subparser
                ( command "all" (info (pure Fall) (progDesc "Prints all available formats"))
               <> command "gf" (info gfSubparser gfHelper))
            <*> argument str (metavar "Filename")
        where
          gfSubparser = subparser ( command "en" (info (pure (Fgf GFeng))   (progDesc "tell GF to output english"))
                                 <> command "my" (info (pure (Fgf GFmalay)) (progDesc "tell GF to output malay"))
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

  contents <- readFile $ filepath opts
  process (filepath opts) contents
