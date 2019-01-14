{-# LANGUAGE NoImplicitPrelude #-}
module Events.Actions.Normal (event) where

import ClassyPrelude hiding (delete)

import Graphics.Vty.Input.Events
import Data.Char (isDigit)
import Events.State
import Events.State.Types (Stateful)
import Events.State.Modal.Detail (showDetail, editDue)

-- Normal
event :: Event -> Stateful

-- quit
event (EvKey (KChar 'q') _) = quit

-- add/edit
event (EvKey (KChar 's') _) = (startEdit =<<) . store
event (EvKey (KChar 'w') _) = (startEdit =<<) . (clearItem =<<) . store
event (EvKey (KChar 'a') _) = (startCreate =<<) . (newItem =<<) . store
event (EvKey (KChar 'O') _) = (startCreate =<<) . (above  =<<) . store
event (EvKey (KChar 'o') _) = (startCreate =<<) . (below =<<) . store

-- add list
event (EvKey (KChar 'A') _) = (createListStart =<<) . store
event (EvKey (KChar 'S') _) = (editListStart =<<) . store
event (EvKey (KChar 'X') _) = (write =<<) . (deleteCurrentList =<<) . store

-- navigation
event (EvKey (KChar 'e') _) = previous
event (EvKey (KChar 'n') _) = next
event (EvKey (KChar 'h') _) = left
event (EvKey (KChar 'i') _) = right
event (EvKey (KChar 'G') _) = bottom

-- moving items
event (EvKey (KChar 'E') _) = (write =<<) . (up =<<) . store
event (EvKey (KChar 'N') _) = (write =<<) . (down =<<) . store
event (EvKey (KChar 'H') _) = (write =<<) . (bottom =<<) . (left =<<) . (moveLeft =<<) . store
event (EvKey (KChar 'I') _) = (write =<<) . (bottom =<<) . (right =<<) . (moveRight =<<) . store
event (EvKey (KChar ' ') _) = (write =<<) . (moveRight =<<) . store
event (EvKey (KChar 'm') _) = showMoveTo

-- removing items
event (EvKey (KChar 'x') _) = (write =<<) . (delete =<<) . store

-- undo
event (EvKey (KChar 'z') _) = (write =<<) . undo

-- moving lists
event (EvKey (KChar '>') _) = (write =<<) . (listRight =<<) . store
event (EvKey (KChar '<') _) = (write =<<) . (listLeft =<<) . store

-- search
event (EvKey (KChar '/') _) = searchMode

-- help
event (EvKey (KChar '?') _) = showHelp

-- subtasks
event (EvKey KEnter _) = showDetail
event (EvKey (KChar '@') _) = (editDue =<<) . (store =<<) . showDetail

-- selecting lists
event (EvKey (KChar n) _)
    | isDigit n = selectList n
    | otherwise = return

-- fallback
event _ = return
