{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Lens.Cons
import           Control.Lens.SemiIso
import           Control.Lens.TH
import qualified Data.Attoparsec.Text as AP
import           Data.Char
import           Data.Scientific (Scientific)
import           Data.SemiIsoFunctor
import           Data.Syntax (Syntax)
import qualified Data.Syntax as S
import qualified Data.Syntax.Attoparsec.Text as S
import           Data.Syntax.Char (SyntaxChar)
import qualified Data.Syntax.Char as S
import qualified Data.Syntax.Combinator as S
import qualified Data.Syntax.Pretty as S
import           Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Text.PrettyPrint as P

-- A simple untyped lambda calculus.

data Literal = LitStr Text
             | LitNum Scientific
    deriving (Show)

$(makePrisms ''Literal)

data AST = Var Text
         | Lit Literal
         | App AST AST
         | Abs Text AST
         | Let Text AST AST
    deriving (Show)

$(makePrisms ''AST)

-- | A variable name.
name :: Syntax syn Text => syn Text
name = _Cons /$/ S.satisfy isAlpha /*/ S.takeWhile isAlphaNum

-- | A quoted string.
quoted :: SyntaxChar syn seq => syn seq
quoted = S.char '"' */ S.takeTill (=='"') /* S.char '"'

-- | Encloses a symbol in parentheses.
parens :: SyntaxChar syn seq => syn a -> syn a
parens m = S.char '(' */ S.spaces_ */ m /* S.spaces_ /* S.char ')'

-- | A literal.
literal :: SyntaxChar syn Text => syn Literal
literal =  _LitNum /$/ S.scientific
       /|/ _LitStr /$/ quoted

-- | An atom is a variable, literal or an expression in parentheses.
atom :: SyntaxChar syn Text => syn AST
atom =  _Lit /$/ literal
    /|/ _Var /$/ name
    /|/ parens expr

-- | Parses a list of atoms and folds them with the _App prism.
apps :: SyntaxChar syn Text => syn AST
apps = bifoldl1 _App /$/ S.sepBy1 atom S.spaces1

-- | An expression of our lambda calculus.
--
-- Thanks to 'tuple-morph' we don't have to worry about /* and */ here.
-- Tuples are reassociated and units are removed by the 'morphed'
-- isomorphism (applied in /$~ operator).
expr :: SyntaxChar syn Text => syn AST
expr =  _Abs /$~ S.char '\\'    /*/ S.spaces_
             /*/ name           /*/ S.spaces
             /*/  S.string "->" /*/ S.spaces
             /*/ expr

    /|/ _Let /$~ S.string "let" /*/ S.spaces1
             /*/ name           /*/ S.spaces
             /*/ S.char '='     /*/ S.spaces
             /*/ expr           /*/ S.spaces1
             /*/ S.string "in"  /*/ S.spaces1
             /*/ expr

    /|/ apps

main :: IO ()
main = do
    -- Load the standard input.
    t <- T.getContents

    -- Try to parse it.
    case AP.parseOnly (S.getParser expr <* AP.skipSpace <* AP.endOfInput) t of
      Left err  -> putStrLn err
      Right ast -> do
        -- If parsing succeeded print the AST.
        print ast
        
        -- Try to pretty print it.
        -- (Printing cannot really fail in this example)
        case S.runPrinter expr ast of
          Left err  -> putStrLn err
          Right doc -> putStrLn (P.render doc)

    return ()
