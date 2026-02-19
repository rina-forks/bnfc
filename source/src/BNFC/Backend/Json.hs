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
        [ doubleQuotes "keywords" <+> ":" <+> brackets keywords <> ","
        -- The token is defined using regex
        , doubleQuotes "tokens" <+> ":" <+> "["
        , indent (punctuate "," $ map prLexRule (mkLexer cf))
        , "]"
        ]
    , "}"
    ]
  where
    className = camelCase name <> "Lexer"
    keywords = fsep (punctuate "," (map (doubleQuotes . text) (reservedWords cf)))
    indent = nest 2 . vcat
    prLexRule (reg,ltype) =
        brackets $ hsep $ punctuate "," [
            doubleQuotes (ptext $ pyToken ltype),
            escapedDoubleQuotes (regex reg)]
    pyToken LexComment = "Comment"
    pyToken LexSymbols = "Operator"
    pyToken (LexToken name) = name

escapedDoubleQuotes s = ptext $ "\"" ++ concatMap f s ++ "\""
  where
    f '"' = "\\\""
    f '\\' = "\\\\"
    f x = [x]

escapeRegex :: Char -> String
escapeRegex '\n' = "\\n"
escapeRegex '\t' = "\\t"
escapeRegex c | c `elem` ("$^.[]()|*+?{}\\" :: String) = ['\\',c]
escapeRegex c = [c]

characterClassRegex :: Reg -> Maybe [String]
characterClassRegex (RSeqs _)      = Nothing
characterClassRegex (RAlt r1 r2)   = liftA2 (++) (characterClassRegex r1) (characterClassRegex r2)
characterClassRegex (RChar c)      = Just [escapeRegex c]
characterClassRegex (RAny)         = Nothing
characterClassRegex (RStar re)     = Nothing
characterClassRegex (RPlus re)     = Nothing
characterClassRegex (ROpt re)      = Nothing
characterClassRegex (RSeq r1 r2)   = Nothing
characterClassRegex (REps)         = Nothing
characterClassRegex (RAlts cs)     = concat <$> traverse (characterClassRegex . RChar) cs
characterClassRegex (RDigit)       = Just ["\\d"]
characterClassRegex (RUpper)       = Just ["A-Z"]
characterClassRegex (RLower)       = Just ["a-z"]
characterClassRegex (RLetter)      = Just ["a-zA-Z"]
characterClassRegex (RMinus r1 r2) = Nothing


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
regex :: Reg -> String
regex (characterClassRegex -> Just [[c]]) = [c]
regex (characterClassRegex -> Just [['\\', c]]) = ['\\', c]
regex (characterClassRegex -> Just classes) = "[" ++ concat classes ++ "]"
regex (RMinus RAny (characterClassRegex -> Just classes)) = "[^" ++ concat classes ++ "]"
regex (RSeqs s)       = concatMap escapeRegex s
regex (RAlt r1 r2)    = "(?:" ++ regex r1 ++ "|" ++ regex r2 ++ ")"
regex (RChar c)       = escapeRegex c
regex (RAny)          = "."
regex (RStar RAny)    = ".*"
regex (RPlus RAny)    = ".+"
regex (RStar re)      = regex' re ++ "*"
regex (RPlus re)      = regex' re ++ "+"
regex (ROpt re)       = regex' re ++ "?"
regex (RSeq r1 r2)    = regex r1 ++ regex r2
regex (REps)          = ""
regex (RMinus r without) = regex' r ++ regexNegative (regex without)

regex _ = undefined

regex' r = "(?:" ++ regex r ++ ")"
regexNegative r = "(?<!" ++ r ++ ")"


-- TODO: think about precedence
