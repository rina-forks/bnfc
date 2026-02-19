{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- Generates a JSON file from a BNF grammar. -}
module BNFC.Backend.Json where

import Prelude hiding ((<>))

import BNFC.Abs (Reg(..))
import BNFC.Backend.Base (mkfile, Backend)
import BNFC.CF
import BNFC.Lexing
import BNFC.Options hiding (Backend)
import BNFC.Utils
import BNFC.PrettyPrint

makeJson :: SharedOptions -> CF -> Backend
makeJson opts cf = do
    mkfile (render $ lowerCase name <> ".json") comment (lexer name cf)
  where name = lang opts

comment :: String -> String
comment = ("// " ++)

lexer :: String -> CF -> Doc
lexer name cf = vcat
    [ "{"
    , indent
        [ doubleQuotes "keywords" <> ":" <+> brackets keywords <> ","
        -- The token is defined using regex
        , doubleQuotes "tokens" <> ":" <+> "["
        , indent (punctuate "," $ map prLexRule (mkLexer cf))
        , "]"
        ]
    , "}"
    ]
  where
    keywords = fsep (punctuate "," (map (doubleQuotes . text) (reservedWords cf)))
    indent = nest 2 . vcat
    prLexRule (reg,ltype) =
        brackets $ hsep $ punctuate "," [
            doubleQuotes (ptext $ pyToken ltype),
            ptext $ escapedDoubleQuotes (prt 0 reg)]
    pyToken LexComment = "Comment"
    pyToken LexSymbols = "Symbols"
    pyToken (LexToken name) = name

escapedDoubleQuotes :: Foldable t => t Char -> [Char]
escapedDoubleQuotes s = "\"" ++ concatMap f s ++ "\""
  where
    f '"' = "\\\""
    f '\\' = "\\\\"
    f x = [x]

escapeRegex :: Char -> String
escapeRegex '\n' = "\\n"
escapeRegex '\t' = "\\t"
escapeRegex c | c `elem` ("$^.[]()|*+?{}\\=<>\\" :: String) = ['\\', c]
escapeRegex c = [c]

characterClassRegex :: Reg -> Maybe [String]
characterClassRegex (RSeqs _)    = Nothing
characterClassRegex (RAlt r1 r2) = liftA2 (++) (characterClassRegex r1) (characterClassRegex r2)
characterClassRegex (RChar c)    = Just [escapeRegex c]
characterClassRegex (RAny)       = Nothing
characterClassRegex (RStar _)    = Nothing
characterClassRegex (RPlus _)    = Nothing
characterClassRegex (ROpt _)     = Nothing
characterClassRegex (RSeq _ _)   = Nothing
characterClassRegex (REps)       = Nothing
characterClassRegex (RAlts cs)   = concat <$> traverse (characterClassRegex . RChar) cs
characterClassRegex (RDigit)     = Just ["\\d"]
characterClassRegex (RUpper)     = Just ["A-Z"]
characterClassRegex (RLower)     = Just ["a-z"]
characterClassRegex (RLetter)    = Just ["a-zA-Z"]
characterClassRegex (RMinus _ _) = Nothing


-- | Convert a Reg to a python regex
-- >>> pyRegex (RSeqs "abc")
-- abc
-- >>> pyRegex (RAlt (RSeqs "::=") (RChar '.'))
-- ::=|\.
-- >>> pyRegex (RChar '=')
-- =
-- >>> pyRegex RAny
-- .
-- >>> pyRegex (RStar RAny)
-- .*
-- >>> pyRegex (RPlus (RSeqs "xxx"))
-- (xxx)+
-- >>> pyRegex (ROpt (RSeqs "abc"))
-- (abc)?
-- >>> pyRegex (RSeq (RSeqs "--") (RSeq (RStar RAny) (RChar '\n')))
-- --.*\n
-- >>> pyRegex (RStar (RSeq (RSeqs "abc") (RChar '*')))
-- (abc\*)*
-- >>> pyRegex REps
-- <BLANKLINE>
-- >>> pyRegex (RAlts "abc[].")
-- [abc\[\]\.]
-- >>> pyRegex RDigit
-- \d
-- >>> pyRegex RLetter
-- [a-zA-Z]
-- >>> pyRegex RUpper
-- [A-Z]
-- >>> pyRegex RLower
-- [a-z]
-- >>> pyRegex (RMinus RAny RDigit)
-- (.)(?<!\d)
-- >>> pyRegex (RSeq (RAlt (RChar 'a') RAny) (RAlt (RChar 'b') (RChar 'c')))
-- (a|.)(b|c)

asPrec :: Int -> Int -> String -> String
asPrec i j s = if j<i then "(?:" ++ s ++ ")" else s

prt :: Int -> Reg -> [Char]
prt i (characterClassRegex -> Just [[c]]) = [c]
prt i (characterClassRegex -> Just [['\\', c]]) = ['\\', c]
prt i (characterClassRegex -> Just classes) = "[" ++ concat classes ++ "]"
prt i (RMinus RAny (characterClassRegex -> Just classes)) = "[^" ++ concat classes ++ "]"
prt i (RSeqs s)       = asPrec i 30 $ concatMap escapeRegex s
prt i (RAlt r1 r2)    = asPrec i 10 $ prt 10 r1 ++ "|" ++ prt 10 r2
prt i (RChar c)       = escapeRegex c
prt i (RAny)          = asPrec i 50 "."
prt i (RStar RAny)    = asPrec i 50 ".*"
prt i (RPlus RAny)    = asPrec i 50 ".+"
prt i (RStar re)      = asPrec i 50 $ prt 51 re ++ "*"
prt i (RPlus re)      = asPrec i 50 $ prt 51 re ++ "+"
prt i (ROpt re)       = asPrec i 50 $ prt 51 re ++ "?"
prt i (RSeq r1 r2)    = asPrec i 30 $ prt 30 r1 ++ prt 30 r2
prt i (REps)          = asPrec i 40 ""
prt i (RMinus r without) = asPrec i 30 $ prt 51 r ++ regexNegative (prt 0 without)

prt i _ = undefined

regexNegative :: [Char] -> [Char]
regexNegative r = "(?<!" ++ r ++ ")"


