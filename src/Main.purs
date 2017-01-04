module Main where

import Prelude
import Data.Nullable (toMaybe)
import DOM.HTML as Dom
import DOM.HTML.Window as Window
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import DOM.HTML.Types (htmlDocumentToParentNode)
import DOM.Node.Node (setTextContent)
import DOM.Node.ParentNode (querySelector)
import DOM.Node.Types (ParentNode, elementToNode)
import Data.Array (head, index, tail)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replace)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (ignoreCase, noFlags)

data Greeting = Greeting String String

data ChristmasWord
  = EN String
  | DE String
  | FR String

input :: String
input = "Joyeux Noel! Joe"

fromString :: String -> Maybe Greeting
fromString string = do
  case (regex "([^!]+!)\\s([^\\s]+)" noFlags) of
    Left _ -> Nothing
    Right expression -> do
      matches <- match expression string
      m <- tail matches
      first <- index m 0
      second <- index m 1
      Greeting <$> (fixXmasWord <$> first) <*> second

h :: String -> String -> (String -> ChristmasWord) -> Maybe ChristmasWord
h string pattern wrap =
  case regex pattern ignoreCase of
    Left _ -> Nothing
    Right expression -> do
      matches <- match expression string
      wordMatch <- head matches
      wrap <$> wordMatch

getXmasWord :: String -> Maybe ChristmasWord
getXmasWord string =
      h string "Christmas" EN
  <|> h string "Weihnachten" DE
  <|> h string "No[eë]l" FR

fixXmasWord :: String -> String
fixXmasWord string =
  case getXmasWord string of
    Just (EN _) -> replace (Pattern "c") (Replacement "C") string
    Just (DE _) -> replace (Pattern "w") (Replacement "W") string
    Just (FR _) -> replace (Pattern "oe") (Replacement "oë") string
    Nothing -> string

setText :: ParentNode -> String -> String -> forall e. Eff (dom :: DOM | e) Unit
setText container selector text =  do
  queryResult <- toMaybe <$> querySelector selector container
  case queryResult of
    Just node -> setTextContent text $ elementToNode node
    Nothing -> pure unit

render :: Greeting -> forall e. Eff (dom :: DOM | e) Unit
render (Greeting text sender) = do
  window <- Dom.window
  document <- htmlDocumentToParentNode <$> Window.document window
  _ <- setText document "header" text
  setText document "footer" sender

main :: forall e. Eff (console :: CONSOLE, dom :: DOM | e) Unit
main = do
  render greeting
    where
      greeting = case fromString input of
        Just something -> something
        Nothing -> Greeting "x x x" "Someone"
