{-
    BNF Converter: TreeSitter Grammar Generator
    Copyright (C) 2004  Author:  Markus Forsberg, Michael Pellauer,
                                 Bjorn Bringert

    Description   : This module generates the grammar.js input file for
                    tree-sitter.

    Author        : Kangjing Huang (huangkangjing@gmail.com)
    Created       : 23 Nov, 2023

-}

module BNFC.Backend.TreeSitter where

import System.FilePath

import BNFC.Backend.Base
import BNFC.Backend.TreeSitter.CFtoTreeSitter (cfToTreeSitter)
import BNFC.GetCF(fixTokenCats)
import BNFC.Utils(kebabCase_, snakeCase_)
import BNFC.CF
import BNFC.Options hiding (Backend)
import BNFC.PrettyPrint

-- | Entry point: create grammar.js file
makeTreeSitter :: SharedOptions -> CF -> Backend
makeTreeSitter opts cf = do
  mkfile (dir </> "grammar.js") comment $
    render (cfToTreeSitter name wordCat cf)
  where
    name = snakeCase_ (lang opts)
    dir = "tree-sitter-" ++ kebabCase_ (lang opts)
    wordCat = fixTokenCats (tokenNames cf) (strToCat (treeSitterWord opts))

comment :: String -> String
comment = ("// " ++)

-- | TODO: Add Makefile generation for tree-sitter
