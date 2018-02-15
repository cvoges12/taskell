module UI.Draw (
    draw,
    chooseCursor
) where

import Events.State (State, Mode(..), InsertMode(..), Pointer, lists, current, mode, size, normalise)
import Brick
import Data.Text (Text, length, pack, concat, append)
import Data.Taskell.List (List, tasks, title)
import Data.Taskell.Task (Task, description)
import Data.Taskell.Text (wrap)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq (mapWithIndex, length)

import IO.Config (LayoutConfig, columnWidth, columnPadding)

import UI.Types (ResourceName(..))
import UI.Theme

colWidth :: LayoutConfig -> Int
colWidth layout = columnWidth layout + columnPadding layout * 2

addCursor :: Int -> Int -> [Text] -> Widget ResourceName -> Widget ResourceName
addCursor li ti d = reportExtent name . showCursor name (Location (h, v))

    where v = Prelude.length d - 1
          h = Data.Text.length $ last d
          name = RNTask (li, ti)

box :: [Text] -> Widget ResourceName
box d = padBottom (Pad 1) . vBox $ txt <$> d

renderTask :: LayoutConfig -> Pointer -> Int -> Int -> Task -> Widget ResourceName
renderTask layout p li ti t =
      (if (li, ti) == p then withAttr taskCurrentAttr . visible else withAttr taskAttr)
    . addCursor li ti d
    $ box d

    where d = wrap (columnWidth layout) $ description t

columnNumber :: Int -> Text -> Text
columnNumber i s = if col >= 1 && col <= 9 then Data.Text.concat [pack (show col), ". ",  s] else s
    where col = i + 1

renderTitle :: LayoutConfig -> Pointer -> Int -> List -> Widget ResourceName
renderTitle layout (p, i) li l = if p /= li || i == 0 then visible title' else title'

    where d = wrap (columnWidth layout) $ columnNumber li (title l)
          attr = if p == li then titleCurrentAttr else titleAttr
          title' = withAttr attr . addCursor li (-1) d $ box d

renderList :: LayoutConfig -> Int -> Pointer -> Int -> List -> Widget ResourceName
renderList layout h p li l = if fst p == li then visible list else list
    where list =
              padLeftRight (columnPadding layout)
            . hLimit (columnWidth layout)
            . viewport (RNList li) Vertical
            . padBottom (Pad h)
            . vBox
            . (renderTitle layout p li l :)
            . toList
            $ renderTask layout p li `Seq.mapWithIndex` tasks l

searchImage :: LayoutConfig -> State -> Int -> Widget ResourceName -> Widget ResourceName
searchImage layout s h i = case mode s of
    Search ent term ->
        let attr = if ent then taskCurrentAttr else taskAttr
        in
            vLimit (h - 3) i <=> (
                  withAttr attr
                . padTop (Pad 1)
                . padLeftRight (columnPadding layout)
                $ txt ("/" `append` term)
            )
    _ -> i

-- draw
draw :: LayoutConfig -> State -> [Widget ResourceName]
draw layout state = [
          searchImage layout state h
        . viewport RNLists Horizontal
        . padRight (Pad $ fst (size state))
        . hLimit (Seq.length ls * colWidth layout)
        . padTop (Pad 1)
        . hBox
        . toList
        $ renderList layout h (current s)  `Seq.mapWithIndex` ls
    ]
    where s = normalise state
          h = snd (size state)
          ls = lists s

-- cursors
cursor :: (Int, Int) -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
cursor c = showCursorNamed (RNTask c)

chooseCursor :: State -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
chooseCursor state = case mode s of
    Insert (CreateList _) -> cursor (fst c, -1)
    Insert EditList -> cursor (fst c, -1)
    Insert CreateTask -> cursor c
    Insert EditTask -> cursor c
    _ -> neverShowCursor s

    where s = normalise state
          c = current s