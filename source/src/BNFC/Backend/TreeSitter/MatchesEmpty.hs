{-
    BNF Converter: TreeSitter Grammar Generator
    Copyright (C) 2004  Author:  Markus Forsberg, Michael Pellauer,
                                 Bjorn Bringert

    Description   : This module identifies and transforms rules which match
                    the empty string, as required by Treesitter.

    Author        : Kangjing Huang (huangkangjing@gmail.com)
    Created       : 23 Nov, 2023

-}
module BNFC.Backend.TreeSitter.MatchesEmpty where

import BNFC.CF
import Prelude hiding ((<>))

import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Debug.Trace as Trace


-- | Possibly empty non-terminals (Cat) or terminals (String, as token name).
type KnownEmpty = Set.Set (Either Cat String)
data Optional = Optional | NonOptional deriving (Eq, Show)
type OptionalSentForm = [(Optional, Either Cat String)]

-- TODO: if we have a rule like X ::= "" | "A", then we need to go Left []
-- for the first one to indicate that it DOES match empty and indicate that
-- there is nothing remaining if empty is removed.

possiblyEmptyRule :: KnownEmpty -> SentForm -> Either [OptionalSentForm] [SentForm]
possiblyEmptyRule knownEmpty sentence =
  if sentenceMatchesEmpty then
    Left (Maybe.mapMaybe removeEmptyMatch (List.tails sentence))
  else
    Right [sentence]
  where
    sentenceMatchesEmpty = all (`Set.member` knownEmpty) sentence

    removeEmptyMatch (x:xs) = Just ((NonOptional, x) : map (\x -> (Optional, x)) xs)
    removeEmptyMatch [] = Nothing

appendSentForms :: Either [OptionalSentForm] [SentForm] -> Either [OptionalSentForm] [SentForm] -> Either [OptionalSentForm] [SentForm]
appendSentForms (Right x) (Right y) = Right (x ++ y)
appendSentForms x y = Left (toOptional x ++ toOptional y)
  where
    toOptional = either id (map sentToOptionalSent)

sentToOptionalSent :: SentForm -> OptionalSentForm
sentToOptionalSent = map (\x -> (NonOptional, x))

-- | Returns whether the given Cat with the given Rules could match the empty
--   string, given the set of currently-known empty things.
possiblyEmptyCat :: KnownEmpty -> (Cat, [Rule]) -> Either [OptionalSentForm] [SentForm]
possiblyEmptyCat knownEmpty (_, rules) =
  foldr appendSentForms (Right []) $
    map (possiblyEmptyRule knownEmpty . rhsRule) rules

possiblyEmptyCats :: [(Cat, [Rule])] -> KnownEmpty -> KnownEmpty
possiblyEmptyCats cats knownEmpty =
  Set.fromList (map (Left . fst) newEmpties)
    `Set.union` knownEmpty
  where
    newEmpties = filter (Either.isLeft . possiblyEmptyCat knownEmpty) cats

fixPointKnownEmpty :: [(Cat, [Rule])] -> KnownEmpty
fixPointKnownEmpty cats =
  case fixPoint of
    Just knownEmpty -> Trace.traceShowId knownEmpty
    Nothing -> error "impossible due to fix point iteration"
  where
    knownEmptySeq = iterate (possiblyEmptyCats cats) Set.empty

    fixPoint = fst <$> List.find (uncurry (==)) (knownEmptySeq `zip` drop 1 knownEmptySeq)

applyOptionals :: KnownEmpty -> SentForm -> OptionalSentForm
applyOptionals knownEmpty = map apply
  where
    apply x = (if x `Set.member` knownEmpty then Optional else NonOptional, x)

fixSentence :: KnownEmpty -> SentForm -> [OptionalSentForm]
fixSentence knownEmpty =
    either id (map (applyOptionals knownEmpty))
    . possiblyEmptyRule knownEmpty

