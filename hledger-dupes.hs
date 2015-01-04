module Main where

import Hledger
import Text.Printf (printf)
import System.Environment (getArgs)
import Safe (headDef)
import Data.List
import Data.Function

accountsNames :: Journal -> [(String, AccountName)]
accountsNames j = map leafAndAccountName as
  where leafAndAccountName a = (accountLeafName a, a)
        ps = journalPostings j
        as = nub $ sort $ map paccount ps


dupes :: (Ord k, Eq k) => [(k, v)] -> [(k, [v])]
dupes l = zip dupLeafs dupAccountNames
  where dupLeafs = map (fst . head) d
        dupAccountNames = map (map snd) d
        d = dupes' l
        dupes' = filter ((> 1) . length)
          . groupBy ((==) `on` fst)
          . sortBy (compare `on` fst)

render :: (String, [AccountName]) -> IO ()
render (leafName, accountNameL) = printf "%s as %s\n" leafName (concat $ intersperse ", " accountNameL)

main = do
  args <- getArgs
  deffile <- defaultJournalPath
  let file = headDef deffile args
  j <- readJournalFile Nothing Nothing file >>= either error' return
  mapM_ render $ dupes $ accountsNames j
