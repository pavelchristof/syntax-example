{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Lens.SemiIso
import           Control.Lens.TH
import qualified Data.Attoparsec.Text as AP
import           Data.Char
import           Data.SemiIsoFunctor
import           Data.Syntax (Syntax)
import qualified Data.Syntax as S
import qualified Data.Syntax.Attoparsec.Text as S
import qualified Data.Syntax.Char as S
import qualified Data.Syntax.Combinator as S
import qualified Data.Syntax.Pretty as S
import           Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Text.PrettyPrint as P

-- | A simple untyped lambda calculus.
data AST = Var Text
         | App AST AST
         | Abs Text AST
    deriving (Show)

$(makePrisms ''AST)

-- | A variable name.
name :: Syntax syn Text => syn Text
name = S.takeWhile1 isAlphaNum

-- | Encloses a symbol in parentheses.
parens :: Syntax syn Text => syn a -> syn a
parens m = S.char '(' */ S.spaces_ */ m /* S.spaces_ /* S.char ')'

-- | An atom is a variable or an expression in parentheses.
atom :: Syntax syn Text => syn AST
atom =  _Var /$/ name
    /|/ parens expr

-- | Parsers a list of applications.
apps :: Syntax syn Text => syn AST
apps = bifoldl1 (attemptAp_ _App) /$/ S.sepBy1 atom S.spaces1

-- | An expression of our lambda calculus.
expr :: Syntax syn Text => syn AST
expr =  _Abs /$/ S.char '\\'   /* S.spaces_
              */ name          /* S.spaces
             /*  S.string "->" /* S.spaces
             /*/ expr
    /|/ apps

main :: IO ()
main = do
    -- Load the standard input.
    t <- T.getContents

    -- Try to parse it.
    case AP.parseOnly (S.getParser expr <* AP.skipSpace <* AP.endOfInput) t of
      Left err -> putStrLn err
      Right ast -> do
        -- If parsing succeeded print the AST.
        print ast
        
        -- Try to pretty print it.
        -- (Printing cannot really fail in this example)
        case S.runPrinter expr ast of
          Left err -> putStrLn err
          Right doc -> putStrLn (P.render doc)

    return ()
