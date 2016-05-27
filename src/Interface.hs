module Interface where

import Logic
import Graphics.Gloss.Interface.Pure.Game

drawNonogram :: Nonogram -> Picture
drawNonogram = error "not implemented"

handleNonogram :: Event -> Nonogram -> Nonogram
handleNonogram = error "not implemented"

updateNonogram :: Float -> Nonogram -> Nonogram
updateNonogram _ = id
