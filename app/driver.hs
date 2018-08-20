{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Lens
import           Prelude
import qualified Miso         as Mi
import qualified Miso.String  as Mi

data Piece = X | O
  deriving (Show, Eq)

-- | Nothing indicates empty spot
type Board = [[Maybe Piece]]        -- 3 x 3 list

emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Nothing)

type Model = Board

data Action = NullAction
            | Place Piece (Int, Int)

main :: IO ()
main = Mi.startApp Mi.App {..}
  where
    initialAction = NullAction
    model         = emptyBoard
    update        = updateModel
    view          = viewModel
    events        = Mi.defaultEvents
    subs          = []
    mountPoint    = Nothing

updateModel :: Action -> Model -> Mi.Effect Action Model
updateModel = \case
    Place p (x, y) -> pure . set (ix y . ix x) (Just p)
    NullAction     -> pure

-- type Model = [[Maybe Piece]]        -- 3 x 3 list
viewModel :: Model -> Mi.View Action
viewModel board = Mi.div_ [] $
        imap makeRow board
    ++ [Mi.link_ [Mi.rel_ "stylesheet", Mi.href_ "main.css"]]
  where
    makeRow :: Int -> [Maybe Piece] -> Mi.View Action
    makeRow y row = Mi.div_ [] (imap makePiece row)
      where
        makePiece :: Int -> Maybe Piece -> Mi.View Action
        makePiece x p = Mi.div_ [Mi.class_ "piece"]
          [ case p of
              Nothing ->
                Mi.button_ [ Mi.onClick $ Place X (x,y) ] [ Mi.text "??" ]
              Just p  ->
                Mi.span_ [] [Mi.text (Mi.ms (show p))]
          ]

displayPiece :: Maybe Piece -> String
displayPiece Nothing  = "??"
displayPiece (Just X) = "X"
displayPiece (Just O) = "O"



