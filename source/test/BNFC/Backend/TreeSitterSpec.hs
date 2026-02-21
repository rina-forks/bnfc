module BNFC.Backend.TreeSitterSpec where

import qualified Paths_BNFC
import System.FilePath((</>), (<.>), takeBaseName)

import BNFC.Options
import BNFC.GetCF

import Test.Hspec
import BNFC.Hspec

import BNFC.Backend.TreeSitter -- SUT

calcOptions = defaultOptions { lang = "Calc" }
getCalc = parseCF  calcOptions TargetTreeSitter $
  unlines [ "EAdd. Exp ::= Exp \"+\" Exp1  ;"
          , "ESub. Exp ::= Exp \"-\" Exp1  ;"
          , "EMul. Exp1  ::= Exp1  \"*\" Exp2  ;"
          , "EDiv. Exp1  ::= Exp1  \"/\" Exp2  ;"
          , "EInt. Exp2  ::= Integer ;"
          , "coercions Exp 2 ;" ]

runFileTest basename = do
  let opts = (defaultOptions { lang = basename})

  dataDir <- Paths_BNFC.getDataDir
  let readDataFile x = readFile (dataDir </> "test/BNFC/Backend/TreeSitter" </> x)

  bnfc <- readDataFile (basename <.> "cf")
  expected <- readDataFile (basename <.> "expected.js")

  cf <- parseCF opts TargetTreeSitter bnfc
  let backend = makeTreeSitter opts cf

  backend `shouldGenerateText` ("grammar.js", expected)

makeFileTest filename =
  it ("tree-sitter expect test: " <> filename) $
    runFileTest (takeBaseName filename)

spec = do

  describe "Tree-Sitter backend" $ do
    it "creates the grammar.js file" $ do
      calc <- getCalc
      makeTreeSitter calcOptions calc `shouldGenerate` "grammar.js"

    makeFileTest "basic.cf"
    makeFileTest "list.cf"
    makeFileTest "basic.cf"
