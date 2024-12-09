{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isAsciiLower" #-}
import Data.List (elemIndex)

-- Определение типов
data Direction = UP | DOWN | LEFT | RIGHT deriving (Show, Read)

newtype World = World [[String]]
  deriving Show

data PlayerCoord = PlayerCoord Int Int
  deriving (Show, Eq)

-- Поиск позиции героя
currentPlayerPosition :: World -> Maybe PlayerCoord
currentPlayerPosition (World world) =
  let rowWithHero = zip [0..] world
  in case [(row, col) | (row, line) <- rowWithHero, Just col <- [elemIndex "H" line]] of
       ((r, c):_) -> Just (PlayerCoord c r)
       []         -> Nothing

-- Перемещение героя
moveHero :: Direction -> World -> World
moveHero dir (World world) =
 case currentPlayerPosition (World world) of
  Nothing -> World world -- Если героя нет, возвращаем мир без изменений
  Just (PlayerCoord x y) ->
    let (newX, newY) = case dir of
          UP    -> (x, max 0 (y - 1))
          DOWN  -> (x, min (length world - 1) (y + 1))
          LEFT  -> (max 0 (x - 1), y)
          RIGHT -> (min (length (head world) - 1) (x + 1), y)
    in placeHero (PlayerCoord newX newY) (clearHero (World world))

-- Очистка старой позиции героя
clearHero :: World -> World
clearHero (World world) =
  World [[if cell == "H" then "#" else cell | cell <- row] | row <- world]

-- Размещение героя
placeHero :: PlayerCoord -> World -> World
placeHero (PlayerCoord x y) (World world) =
  World [if row == y then replaceAt x "H" line else line | (line, row) <- zip world [0..]]
  where
    replaceAt i v xs = take i xs ++ [v] ++ drop (i + 1) xs

-- Вывод мира
printWorld :: World -> IO ()
printWorld (World w) = mapM_ print w

-- Инициализация мира
initWorld :: Int -> Int -> World
initWorld width height =
  let initialWorld = World (replicate height (replicate width "#"))
      centerX = width `div` 2
      centerY = height `div` 2
  in placeHero (PlayerCoord centerX centerY) initialWorld

-- Игровой цикл
gameLoop :: World -> IO ()
gameLoop world = do
  printWorld world
  putStrLn "Enter a command (e.g., UP, DOWN, LEFT, RIGHT or Quit):"
  input <- getLine
  case map toUpper input of
    "UP"    -> gameLoop (moveHero UP world)
    "DOWN"  -> gameLoop (moveHero DOWN world)
    "LEFT"  -> gameLoop (moveHero LEFT world)
    "RIGHT" -> gameLoop (moveHero RIGHT world)
    "QUIT"  -> putStrLn "Game Over!"
    _       -> do
      putStrLn "Invalid command. Try again."
      gameLoop world
  where
    toUpper c
      | 'a' <= c && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise            = c

-- Главная функция
main :: IO ()
main = do
  let world = initWorld 5 5 -- Инициализация мира 5x5
  gameLoop world
