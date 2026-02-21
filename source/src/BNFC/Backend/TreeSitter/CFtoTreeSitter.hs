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
import BNFC.Backend.TreeSitter.MatchesEmpty(fixPointKnownEmpty, transformEmptyMatches, KnownEmpty, OptSym(..), OptSentForm)
import BNFC.CF
import BNFC.Lexing (mkRegMultilineComment, mkRegSingleLineComment)
import BNFC.PrettyPrint

import Prelude hiding ((<>))
import Control.Applicative ((<|>))

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Debug.Trace as Trace

-- | Indent one level of 2 spaces
indent :: Doc -> Doc
indent = nest 2

-- | Create content of grammar.js file
cfToTreeSitter :: String -> CF -> Doc
cfToTreeSitter name cf =
  -- Overall structure of grammar.js
  text "module.exports = grammar({"
    $+$ indent
      ( text "name: '" <> text name <> text "',"
          $+$ extrasSection
          -- TODO: wordSection should point to the identifier name, and should be customisable?
          -- $+$ wordSection
          $+$ rulesSection
      )
    $+$ text "});"
  where
    extrasSection = prExtras cf
    wordSection = prWord cf
    rulesSection =
      text "rules: {"
        $+$ indent
          ( prRules cf
              -- $+$ prWord cf
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
prWord :: CF -> Doc
prWord cf =
  if wordNeeded
    then
      defineSymbol "word"
        $+$ indent
          ( wrapChoice
              ( usrTokensFormatted
                  ++ [text "$.token_Ident" | identUsed]
              )
          )
          <> ","
    else empty
  where
    wordNeeded = identUsed || usrTokens /= []
    identUsed = isUsedCat cf (TokenCat catIdent)
    usrTokens = tokenPragmas cf
    usrTokensFormatted =
      map (text . refName . formatCatName False . TokenCat . fst) $ usrTokens

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

-- | First print the entrypoint rule, tree-sitter always use the
--   first rule as entrypoint and does not support multi-entrypoint.
--   Then print rest of the rules
prRules :: CF -> Doc
prRules cf =
  -- Trace.traceShow (possiblyEmptyCats (ruleGroupsInternals cf) Set.empty) $
  if onlyOneEntry
    then
      -- TODO: the entry token is allowed to be empty. if it can be empty, just choice it with empty or something.
      prOneCat knownEmpty (Trace.traceShow (map (render .pretty) entryRules) entryRules) entryCat
        $+$ vcat' (map (uncurry (prOneCat knownEmpty)) otherRules)
    else error "Tree-sitter only supports one entrypoint"
  where
    --If entrypoint is defined, there must be only one entrypoint
    --If it is not defined, defaults to use the first rule as entrypoint
    onlyOneEntry = not (hasEntryPoint cf) || onlyOneEntryDefined
    onlyOneEntryDefined = length (allEntryPoints cf) == 1

    entryCat = firstEntry cf
    entryRules = rulesForCat' cf entryCat

    otherRules = [(rs, c) | (c, rs) <- ruleGroupsInternals cf, c /= entryCat]

    knownEmpty = fixPointKnownEmpty (ruleGroupsInternals cf)

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
prOneCat :: KnownEmpty -> [Rule] -> NonTerminal -> Doc

prOneCat knownEmpty rules (ListCat cat) | enable =
  defineSymbol (formatCatName False (ListCat cat))
    $+$ (indent . appendComma $
      case (,) <$> singletonOrNilRule <*> consRule of
        -- empty separator/terminator case
        Just ([_], [x, _rec]) -> wrp "repeat1" (fmt [x])

        -- possibly-empty separator list, non-empty separator
        Just ([_], [x, sep, _rec]) -> wrapSeq [fmt [x], wrp "repeat" (fmt [sep, x])]

        -- non-empty terminator list, non-empty terminator
        Just ([_, _], [x, sep, _rec]) -> wrp "repeat1" (fmt [x, sep])
        -- possibly-empty terminator list, non-empty terminator
        Just ([], [x, sep, _rec]) -> wrp "repeat1" (fmt [x, sep])

        _ -> error "treesitter: unexpected singletonRule/consRule combination")
  where
    enable = Maybe.isJust singletonOrNilRule && Maybe.isJust consRule

    nilRule = rhsRule <$> List.find isNilFun rules
    singletonRule = rhsRule <$> List.find isOneFun rules
    consRule = rhsRule <$> List.find isConsFun rules
    singletonOrNilRule = singletonRule <|> nilRule

    fmt = formatSent . map NonOptional
    wrp s = wrapFun s False

prOneCat knownEmpty rules nt =
  defineSymbol (formatCatName False nt)
    $+$ indentChoice parRhs
    $+$
      (if hasInternal
        then defineSymbol (formatCatName True nt) $+$ indentChoice intRhs
        else empty)
  where
    (parsableRules, internalRules) = List.partition isParsable rules
    hasInternal = not $ null internalRules

    indentChoice = indent . appendComma . wrapChoice

    internalTokenName = [text $ refName $ formatCatName True nt | hasInternal]
    parRhs = internalTokenName ++ genChoice parsableRules

    intRhs = genChoice internalRules

    genChoice = map (formatRhs . transformEmptyMatches knownEmpty . rhsRule)


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
    comma a b
      | isEmpty a = b
      | isEmpty b = a
      | otherwise = (if newline then ($+$) else (<>)) (a <> ",") b

wrapSeq :: [Doc] -> Doc
wrapSeq = wrapOptListFun "seq" False

wrapChoice :: [Doc] -> Doc
wrapChoice = wrapOptListFun "choice" True

-- | Wrap list using tree-sitter fun if the list contains multiple items
-- Returns the only item without wrapping otherwise
wrapOptListFun :: String -> Bool -> [Doc] -> Doc
wrapOptListFun _ _ [x] = x
wrapOptListFun fun _ [] = wrapFun fun False empty
wrapOptListFun fun newline list = wrapFun fun newline (commaJoin newline list)

wrapFun :: String -> Bool -> Doc -> Doc
wrapFun fun newline arg = joinOp [text fun <> text "(", indent arg, text ")"]
  where
    joinOp = if newline then vcat' else hcat

-- | Helper for referring to non-terminal names in tree-sitter
refName :: String -> String
refName = ("$." ++)

-- | Format right hand side into list of strings
formatRhs :: [OptSentForm] -> Doc
formatRhs = wrapChoice . map formatSent

formatSent :: OptSentForm -> Doc
formatSent = wrapSeq . map fmtOpt
  where
    fmtOpt (Optional x) = wrapFun "optional" False (fmt x)
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
