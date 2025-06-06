{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lesson2
  ( updateTable,
  )
where

import Control.Exception
import Data.Foldable
import qualified Data.Text.Lazy as LT
import GHC.Wasm.Prim
import Hledger
import Lucid hiding (for_)

foreign import javascript unsafe
  "document.getElementById('table').outerHTML = $1;"
  js_update_table :: JSString -> IO ()

foreign export javascript "updateTable sync"
  updateTable :: IO ()

updateTable :: IO ()
updateTable =
  ( do
      journal <- readJournalFile' "/in"
      js_update_table $ jsStringFromHtml $ mkTable journal
  )
    `catch` ( \(SomeException err) ->
                js_update_table $
                  jsStringFromHtml $
                    p_ [id_ "table"] $
                      toHtml $
                        show err
            )

jsStringFromHtml :: Html a -> JSString
jsStringFromHtml = toJSString . LT.unpack . renderText

mkTable :: Journal -> Html ()
mkTable journal = table_ [id_ "table"] $ do
  caption_ "Ledger entries"
  thead_ $ tr_ $ traverse_ th_ ["Date", "Description", "Account", "Amount"]
  tbody_ $ traverse_ mkTableRow items
  where
    items = entriesReport defreportspec journal

mkTableRow :: EntriesReportItem -> Html ()
mkTableRow Transaction {..} = case tpostings of
  [] -> pure mempty
  (Posting {..}) : ps -> do
    tr_ [class_ "txn-header"] $ do
      td_ $ toHtml $ show tdate
      td_ $ toHtml tdescription
      td_ [class_ "account"] $ toHtml paccount
      td_ $ toHtml $ showMixedAmount pamount
    for_ ps $ \Posting {..} -> tr_ $ do
      td_ mempty
      td_ mempty
      td_ [class_ "account"] $ toHtml paccount
      td_ $ toHtml $ showMixedAmount pamount
