{-
    BNF Converter: TreeSitter Grammar Generator
    Copyright (C) 2004  Author:  Markus Forsberg, Michael Pellauer,
                                 Bjorn Bringert

    Description   : This module converts BNFC grammar to the contents of a
                    tree-sitter grammar.js file

    Author        : Kangjing Huang (huangkangjing@gmail.com)
    Created       : 08 Nov, 2023

-}

module BNFC.Backend.TreeSitter.CFtoTreeSitter where

import BNFC.Abs (Reg)
import BNFC.Backend.TreeSitter.RegToJSReg
import BNFC.Backend.TreeSitter.MatchesEmpty(fixPointKnownEmpty, transformEmptyMatches, KnownEmpty, OptSym(..), OptSentForm, isKnownEmpty)
import BNFC.CF
import BNFC.Lexing (mkRegMultilineComment, mkRegSingleLineComment)
import BNFC.PrettyPrint

import Prelude hiding ((<>))

import qualified Data.List as List
import qualified Data.List.NonEmpty as List1

-- | Indent one level of 2 spaces
indent :: Doc -> Doc
indent = nest 2

-- | Create content of grammar.js file
cfToTreeSitter :: String -> Cat -> CF -> Doc
cfToTreeSitter name wordCat cf =
  -- Overall structure of grammar.js
  text "module.exports = grammar({"
    $+$ indent
      ( text "name: '" <> text name <> text "',"
          $+$ extrasSection
          $+$ wordSection
          $+$ rulesSection
      )
    $+$ text "});"
  where
    extrasSection = prExtras cf
    wordSection = prWord wordCat cf
    rulesSection =
      text "rules: {"
        $+$ indent
          ( prRules cf
              $+$ prUsrTokenRules cf
              $+$ prBuiltinTokenRules cf
          )
        $+$ text "},"

-- | Print rules for comments
prExtras :: CF -> Doc
prExtras cf =
  if extraNeeded
    then
      defineSymbol "extras" <> "["
        $+$ indent
          ( -- default rule for white spaces
            text "/\\s/,"
              $+$ mRules
              $+$ sRules
          )
        $+$ text "],"
    else empty
  where
    extraNeeded = length commentMRules + length commentSRules > 0
    (commentMRules, commentSRules) = comments cf
    mRules = vcat' $ map mkOneMRule commentMRules
    sRules = vcat' $ map mkOneSRule commentSRules
    mkOneSRule s = text (printRegJSReg $ mkRegSingleLineComment s) <> ","
    mkOneMRule (s, e) = text (printRegJSReg $ mkRegMultilineComment s e) <> ","

-- | Print word section, this section is needed for tree-sitter
--   to do keyword extraction before any parsing/lexing, see
--   https://tree-sitter.github.io/tree-sitter/creating-parsers#keyword-extraction
--   TODO: currently, we just add every user defined token as well
--   as the predefined Ident token to this list to be safe. Ideally,
--   we should enumerate all defined tokens against all occurrences of
--   keywords. Any tokens patterns that could accept a keyword will go
--   into this list. This will require integration of a regex engine.
prWord :: Cat -> CF -> Doc
prWord wordCat cf =
  if not wordCatValid then
    error "specified tree-sitter word token not found in BNFC grammar"
  else
    if isUsedCat cf wordCat then
      defineSymbol "word"
        <+> formatSent [NonOptional (Left wordCat)] <> ","
    else empty
  where
    wordCatValid = wordCat == TokenCat catIdent || wordCat `elem` allParserCats cf

-- | Print builtin token rules according to their usage
prBuiltinTokenRules :: CF -> Doc
prBuiltinTokenRules cf =
  ifC catInteger integerRule
    $+$ ifC catDouble doubleRule
    $+$ ifC catChar charRule
    $+$ ifC catString stringRule
    $+$ ifC catIdent identRule
  where
    ifC cat d = if isUsedCat cf (TokenCat cat) then d else empty

-- | Predefined builtin token rules
integerRule, doubleRule, charRule, stringRule, identRule :: Doc
integerRule = defineSymbol "token_Integer" <+> text "/\\d+/" <> ","
doubleRule = defineSymbol "token_Double" <+> text "/\\d+\\.\\d+(e-?\\d+)?/" <> ","
charRule =
  defineSymbol "token_Char" <+> text "/'([^'\\\\]|(\\\\[\"'\\\\tnrf]))'/" <> ","
stringRule =
  defineSymbol "token_String" <+> text "/\"([^'\\\\]|(\\\\[\"'\\\\tnrf]))*\"/" <> ","
identRule =
  defineSymbol "token_Ident" <+> text "/[a-zA-Z][a-zA-Z\\d_']*/" <> ","

-- | Prints the rules in the grammar with the entry point first.
--
-- Since Treesitter requires a unique entry point, this will build a "virtual"
-- entry point which dispatches to each of the declared BNFC entry points via
-- a choice list. Additionally, the virtual entry point can be marked optional
-- (and is the only rule which can be).
prRules :: CF -> Doc
prRules cf =
  prOneCat knownEmpty wrapEntry virtEntryCat virtEntryRhsRules
    $+$ vcat' (map (uncurry (prOneCat knownEmpty id)) allGroups)
  where
    wrapEntry =
      if any ((`isKnownEmpty` knownEmpty) . Left) virtEntryRhsCats then
        wrapOptional'
      else
        id

    allGroups = ruleGroupsInternals cf

    virtEntryCat = Cat "BNFCStart"
    virtEntryRhsCats =
      (if hasEntryPoint cf then List1.toList else List1.take 1)
      (allEntryPoints cf)
    virtEntryRhsRules = toVirtRule <$> virtEntryRhsCats

    toVirtRule rhsCat =
      npRule
        (identCat virtEntryCat ++ "_" ++ identCat rhsCat)
        virtEntryCat
        [Left rhsCat]
        Parsable

    knownEmpty = fixPointKnownEmpty allGroups

prUsrTokenRules :: CF -> Doc
prUsrTokenRules cf = vcat' $ map prOneToken tokens
  where
    tokens = tokenPragmas cf

-- | Check if a set of rules contains internal rules
hasInternal :: [Rule] -> Bool
hasInternal = not . all isParsable

-- | Generates one or two tree-sitter rule(s) for one non-terminal from CF.
-- Uses choice function from tree-sitter to combine rules for the non-terminal
-- If the non-terminal has internal rules, an internal version of the non-terminal
-- will be created (prefixed with "_" in tree-sitter), and all internal rules will
-- be sectioned as such.
prOneCat :: KnownEmpty -> (Doc -> Doc) -> NonTerminal -> [Rule] -> Doc
prOneCat knownEmpty wrapRhs nt rules =
  defineSymbol (formatCatName False nt)
    $+$ indentCommaChoice (wrapRhs parRhs)
    $+$
      (if hasInternal
        then defineSymbol (formatCatName True nt) $+$ indentCommaChoice intRhs
        else empty)
  where
    (parsableRules, internalRules) = List.partition isParsable rules
    hasInternal = not (null internalRules)

    indentCommaChoice = indent . appendComma

    internalTokenName = [text $ refName $ formatCatName True nt | hasInternal]
    parRhs = wrapChoice (internalTokenName ++ genRules parsableRules)
    intRhs = wrapChoice (genRules internalRules)

    genRule rule =
      ("//" <+> text (renderOneLine (pretty rule)))
      $+$ (formatRhs . transformEmptyMatches knownEmpty) (rhsRule rule)
    genRules = map genRule

    renderOneLine = renderStyle (style { mode = OneLineMode })

-- | Generate one tree-sitter rule for one defined token
prOneToken :: (TokenCat, Reg) -> Doc
prOneToken (cat, exp) =
  defineSymbol (formatCatName False $ TokenCat cat)
    $+$ indent (text $ printRegJSReg exp) <> ","

-- | Start a defined symbol block in tree-sitter grammar
defineSymbol :: String -> Doc
defineSymbol name = hsep [text name <> ":", text "$", text "=>"]

appendComma :: Doc -> Doc
appendComma = (<> text ",")

commaJoin :: Bool -> [Doc] -> Doc
commaJoin newline =
  foldl comma empty
  where
    commaString = if newline then "," else ", "
    comma a b
      | isEmpty a = b
      | isEmpty b = a
      | otherwise = (if newline then ($+$) else (<>)) (a <> commaString) b

wrapSeq :: [Doc] -> Doc
wrapSeq = wrapOptListFun "seq" False

wrapChoice :: [Doc] -> Doc
wrapChoice = wrapOptListFun "choice" True

wrapOptional :: Doc -> Doc
wrapOptional = wrapFun "optional" False

wrapOptional' :: Doc -> Doc
wrapOptional' = wrapFun "optional" True

-- | Wrap list using tree-sitter fun if the list contains multiple items
-- Returns the only item without wrapping otherwise
wrapOptListFun :: String -> Bool -> [Doc] -> Doc
wrapOptListFun _   _ [x] = x
wrapOptListFun fun _ [ ] = wrapFun fun False empty
wrapOptListFun fun newline list = wrapFun fun newline (commaJoin newline list)

wrapFun :: String -> Bool -> Doc -> Doc
wrapFun fun newline arg = joinOp [text fun <> text "(", indentOp arg, text ")"]
  where
    joinOp = if newline then vcat' else hcat
    indentOp = if newline then indent else id

-- | Helper for referring to non-terminal names in tree-sitter
refName :: String -> String
refName = ("$." ++)

-- | Format right hand side into list of strings
formatRhs :: [OptSentForm] -> Doc
formatRhs = wrapChoice . map formatSent

formatSent :: OptSentForm -> Doc
formatSent = wrapSeq . map fmtOpt
  where
    fmtOpt (Optional x) = wrapOptional (fmt x)
    fmtOpt (NonOptional x) = fmt x

    fmt (Left c) = text $ refName $ formatCatName False c
    fmt (Right term) = quoted term

quoted :: String -> Doc
quoted s = text "\"" <> text s <> text "\""

-- | Format string for cat name, prefix "_" if the name is for internal rules
formatCatName :: Bool -> Cat -> String
formatCatName internal c =
  if internal
    then "_" ++ formatted
    else formatted
  where
    formatted = formatName c
    formatName (Cat name) = name
    formatName (TokenCat name) = "token_" ++ name
    formatName (ListCat c) = "list_" ++ formatName c
    formatName (CoercCat name i) = name ++ show i
