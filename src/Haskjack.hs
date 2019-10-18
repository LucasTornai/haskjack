module Haskjack where

import System.Random
import System.Random.Shuffle

data Card = Num Int | Jack | Queen | King | Ace
    deriving (Show, Eq)

data Move = Hit | Stand
    deriving (Eq)

type Deck = [Card]
type Player = [Card]
type Dealer = Player

type Game = (Deck, Player, Dealer)

main :: IO ()
main = do
  let deck = newDeck
  generator <- getStdGen
  random <- randomList (length newDeck)
  let shuffledDeck = shuffle newDeck random
  let game = newGame shuffledDeck
  (deck', player', dealer') <- playMove game
  showHand "Player hand: " player'
  showHand "Dealer hand: " dealer'
  finishGame (deck', player', dealer')
      where
        -- taken from https://stackoverflow.com/a/30741139
        randomList 0 = return []
        randomList n = do
          r  <- randomRIO (1,6)
          rs <- randomList (n-1)
          return (r:rs) 

newGame :: Deck -> Game
newGame deck = (altDeck, newPlayer, newDealer)
    where
      (deck', player) = doMove deck [] Hit
      (deck'', newPlayer) = doMove deck' player Hit
      (deck''', dealer) = doMove deck [] Hit
      (altDeck, newDealer) = doMove deck' dealer Hit

playerMove :: Game -> Move -> Game
playerMove (deck, player, dealer) move = (deck', player', dealer)
    where
      (deck', player') = doMove deck player move


dealerMove :: Game -> IO Game
dealerMove (deck, player, dealer) = do
  let (deck', dealer') = if handValue dealer < 17 
                        then doMove deck dealer Hit
                        else (deck, dealer)
  return (deck', player, dealer')

playMove :: Game -> IO Game
playMove game = do
  printGameStatus game
  putStrLn "Player move (Hit/Stay)"
  moveString <- getLine
  let move = stringToMove moveString
  if move == Hit
    then do
      let game' = playerMove game move
      (deck', player', dealer') <- dealerMove game'
      if bust player'
        then return (deck', player', dealer')
        else playMove (deck', player', dealer')
    else 
      dealerMove game

newDeck :: Deck
newDeck = concat $ replicate 4 $ map Num [1..10] ++ [Jack, Queen, King, Ace]

doMove :: Deck -> Player -> Move -> (Deck, Player)
doMove deck player Hit   = (drop 1 deck, player ++ take 1 deck)
doMove deck player Stand = (deck, player)

showHand :: String -> Player -> IO ()
showHand str player = putStrLn $ str ++ " " ++ handToString player

handToString :: Player -> String
handToString []     = ""
handToString [x]    = show x
handToString (x:xs) = show x ++ " ; " ++ handToString xs ++ " -> " ++ show (handValue (x:xs))

cardToInt :: Card -> Int
cardToInt (Num a) = a
cardToInt Jack    = 10
cardToInt Queen   = 10
cardToInt King    = 10
cardToInt Ace     = 11

handValue :: [Card] -> Int
handValue [] = 0
handValue [x] = cardToInt x
handValue (x:xs) = removeAceValue (x:xs) (cardToInt x + handValue xs)

removeAceValue :: [Card] -> Int -> Int
removeAceValue hand value
    | value > 21 && snd (getAce hand) = removeAceValue hand (value - 10)
    | otherwise = value

getAce :: [Card] -> ([Card],Bool)
getAce [] = ([], False)
getAce (x:xs)
    | x == Ace = (xs, True)
    | otherwise = getAce xs

printGameStatus :: Game -> IO ()
printGameStatus (deck, player, dealer) = do
  showHand "Player hand: " player
  putStrLn ("Dealer card number: " ++ show (length dealer))

stringToMove :: String -> Move
stringToMove x
    | x == "hit" = Hit
    | x == "Hit" = Hit
    | otherwise  = Stand

finishGame :: Game -> IO ()
finishGame (deck, player, dealer)
    | handValue player == 21                                                                  = putStrLn "Player wins!"
    | handValue player < 21 && (handValue player > handValue dealer || handValue dealer > 21) = putStrLn "Player wins!"
    | otherwise                                                                               = putStrLn "Dealer wins!"

bust :: Player -> Bool
bust player = handValue player > 21