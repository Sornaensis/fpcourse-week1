module Main where

import Cards

import Control.Monad (when, unless)
import Data.Char (toLower)
import Data.Maybe (isJust,listToMaybe)
import System.Random

import Text.Printf

type Money = Double

data Player = Player { 
      name   :: String
    , cash   :: Money
    , wins   :: Int
    , losses :: Int
    , draws  :: Int } deriving (Show,Read,Eq)

data BlackJackGame = BlackJackGame { 
    deck           :: Deck
  , dealerFaceUp   :: [Card]
  , dealerFaceDown :: Maybe Card
  , playerHand     :: [Card]
  , currentBet     :: Money
  , gamePhase      :: GamePhase
  } deriving (Show)

data HandValue = Bust 
                | BlackJack 
                | Value Int deriving (Show,Eq)  

data Action    = Hit  
                | Bet Money
                | Stand
                | Quit
                | NewGame Deck deriving (Eq,Show)

data GamePhase = Started
               | Main 
               | Ended Result deriving (Show,Eq)

data Result = Win Money
            | Loss Money
            | Draw deriving (Show,Eq)

instance Ord HandValue where
  BlackJack `compare` BlackJack = EQ
  BlackJack `compare` _         = GT
  Bust      `compare` Bust      = EQ
  Bust      `compare` _         = LT
  (Value _) `compare` BlackJack = LT
  (Value _) `compare` Bust      = GT
  (Value a) `compare` (Value b) = a `compare` b


-- Empty game state from initial deck
newGame :: Deck -> BlackJackGame
newGame deck = BlackJackGame deck [] Nothing [] 0 Started

startingMoney :: Money
startingMoney = 500

-- Empty player state with default starting money
newPlayer :: String -> Player
newPlayer name = Player name startingMoney 0 0 0

-- Calculate current winnings from starting point
netWinnings :: Player -> Double
netWinnings = (\m -> m - startingMoney) . cash 

-- | Game rules functions

-- In blackjack face cards are worth 10, other cards are worth their index value (1-10)
-- Aces are special but only counted when we consider the whole hand
-- fromEnum 0-whatever + 1
-- | ****** FILL IN FUNCTION *******
cardValue :: Card -> Int
cardValue c@(Card _ face) = undefined


handValue :: [Card] -> HandValue
handValue cards = let aces             = filter isAce cards 
                      oneAce           = length aces == 1
                      otherCards       = filter (not . isAce) cards
                      nonAceScore      = sum . map cardValue $ otherCards
                      oneOther         = length otherCards == 1
                      oneFace          = oneOther && isFaceCard (head otherCards)
                      totalScore       = sum . map cardValue $ cards
                      softAce          = nonAceScore <= (11 - length aces)  
                      adjustedScore    = (if softAce then 10 else 0) + totalScore
                      score            = (if adjustedScore > 21 then Bust else Value adjustedScore)
                  in  if oneFace && oneAce
                        then BlackJack
                        else score

dealerHand :: BlackJackGame -> [Card]
dealerHand game = case dealerFaceDown game of
                    Nothing -> dealerFaceUp game
                    Just c  -> c : dealerFaceUp game 

-- | Game Actions
dealPlayer :: BlackJackGame -> BlackJackGame
dealPlayer game = game { deck = deck', playerHand = hand' } 
          where
          (deck', card) = deal game
          hand'          = case card of
                           Just c -> c : playerHand game
                           _      -> playerHand game
          

dealDealer :: BlackJackGame -> BlackJackGame
dealDealer game = newState 
          where
          (deck', card)  = deal game
          faceDownCard = dealerFaceDown game
          newState | faceDownCard == Nothing = game { deck = deck', dealerFaceDown = card } 
                   | otherwise               = game { deck = deck', dealerFaceUp   = case card of
                                                                                      Just c -> c : dealerFaceUp game
                                                                                      _      -> dealerFaceUp game  }

deal :: BlackJackGame -> (Deck, Maybe Card)
deal game = (deck', card)
          where
          (card, deck') = draw . deck $ game

-- Update the game with a bet
bet :: Money -> BlackJackGame -> BlackJackGame
bet amt game = game { currentBet = bet + amt }
             where
             bet = currentBet game

-- Output hands and player stats
printState :: Player -> BlackJackGame -> IO ()
printState player game = do
        printf "Current Bet: §%.2f\n" (currentBet game)
        putStr "Dealer Hand: "
        putStr . show . dealerFaceUp $ game
        putStrLn ( if isJust $ dealerFaceDown game then " + One Face Down" else "" )
        putStr "Your Hand: "
        print . playerHand $ game
        printf "Money: §%.2f" (cash player)
        printf "  W/L/D: %u/%u/%u\n" (wins player) (losses player) (draws player)

-- Return appropriate action based on current phase
getAction :: GamePhase -> IO Action
getAction Started = do
  putStrLn "Enter your bet: "
  bet <- fmap fst . listToMaybe . reads <$> getLine :: IO (Maybe Money)
  case bet of
    Just b -> return $ Bet b
    _      -> getAction Started
getAction Main = do
  putStrLn "Hit or Stay?"
  res <- map toLower <$> getLine
  return $
    case res of 
      "h"   -> Hit
      "hit" -> Hit
      _     -> Stand
getAction (Ended _) = do
  putStrLn "New Game?"
  res <- map toLower <$> getLine
  case res of 
    "y"   -> mkNewGame
    "yes" -> mkNewGame
    _     -> return Quit
  where
  mkNewGame = do
    rand <- newStdGen
    return . NewGame $ shuffle rand . foldr1 combineDecks $ replicate 4 newDeck

-- | ****** COMPLETE THE FUNCTION *******
play :: Player -> BlackJackGame -> IO ()
play player game = do
    let phase = gamePhase game
    when (phase == Started) (putStrLn "New Game")
    action <- getAction (gamePhase game)
    let game'      = turn action game
        nextPhase  = gamePhase game'
    -- Fill in undefined expressions below
    unless (action == Quit) (
      case nextPhase of
        Started         -> undefined
        Main            -> undefined
        (Ended  result) -> play (updatePlayer result player) game'
      )

-- | ****** FILL IN FUNCTION *******
-- Needs to handle: Getting new cards, Ending play when the player Stands or Busts
turn :: Action -> BlackJackGame -> BlackJackGame
turn act game = undefined

-- | ****** FILL IN FUNCTION *******
updatePlayer :: Result -> Player -> Player
updatePlayer (Win m)   player = player { cash = (cash player) + m, wins = (wins player) + 1 }
updatePlayer (Loss m)  player = undefined
updatePlayer Draw      player = undefined

main :: IO ()
main = do
  -- retrieve the global random number generator
  putStrLn "Enter your name:"
  name <- getLine
  rand <- newStdGen
  let deck    = shuffle rand . foldr1 combineDecks $ replicate 4 newDeck
      game    = newGame deck
      player  = newPlayer name
  play player game

