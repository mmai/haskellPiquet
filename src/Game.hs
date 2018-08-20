{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Game
    ( play
    , Game
    ) where

import Cards
import Combinations
import Shuffle

import Data.List.Split (splitOn)
import Data.Set.Ordered hiding (filter)
import Data.Function
import Data.Bool
import Data.Maybe
import Text.Read (readMaybe)
import Control.Monad.State
import Control.Lens
import Control.Arrow
import Control.Applicative
import Rainbow
import System.Random

data Deal = One | Two | Three | Four | Five | Six deriving (Bounded, Eq, Enum, Show)

data Step = Start 
          | Deal
          | ExchangeElder
          | ExchangeYounger 
          | DeclarePointElder
          | DeclarePointResponse 
          | DeclareSequenceElder
          | DeclareSequenceResponse
          | DeclareSetElder
          | DeclareSetResponse
          | DeclareCarteRougeElder
          | Play1
          | DeclarePointYounger
          | DeclareSequenceYounger
          | DeclareSetYounger
          | DeclareCarteRougeYounger
          | Play 
          deriving (Enum, Show)

data Player = Player { _hand :: Hand
                     , _dealPoints :: Int
                     , _gamePoints :: Int
                     , _points :: Int
                     , _name :: String
                     } 
makeLenses ''Player

instance Show Player where
  show p = (p ^. name) ++ " : "  ++ show (p ^. dealPoints) ++ " : "++ show (p ^. hand)

data DeclarationResponse = Good | NotGood | Equals deriving (Show)

data DeclarationWinner = Elder | Younger | Tie | Nobody deriving (Eq, Show)

data Game = Game { _deck :: Deck
                 , _visible :: Deck
                 , _step :: Step
                 , _player1 :: Player
                 , _player2 :: Player
                 , _elderIsPlayer1 :: Bool
                 , _pointWinner :: DeclarationWinner
                 , _sequenceWinner :: DeclarationWinner
                 , _setWinner :: DeclarationWinner
                 , _dealNum :: Deal
                 }

makeLenses ''Game

instance Show Game where
  show game = "\n--------------------------------"
         ++ "\nStep : " ++ show (game ^. step)
         ++ "\nDeck : " ++ show (game ^. deck)
         ++ "\nElder : " ++ (if game ^. elderIsPlayer1 then "Player1" else "Player2")
         ++ "\nPoint winner : " ++ show (game ^. pointWinner)
         ++ "\nPlayer1 : " ++ show (game ^. player1)
         ++ "\nPlayer2 : " ++ show (game ^. player2)
         ++ "\n-----------------------------\n"

 
play :: IO Game 
play = flip execStateT initialState $
    setElder
 >> replicateM_ 6 playDeal
 >> endGame

setElder :: GameAction
setElder = lift (randomRIO (True, False)) >>= assign elderIsPlayer1

playDeal :: GameAction
playDeal =                  start
         >>                 showDealNum
         >> step %= succ >> deal
         >>                 showGame
         >> step %= succ >> exchangeForElder
         >> step %= succ >> exchangeForYounger
         >> step %= succ >> declareCombinationElder Point
         >>                 showGame
         >>                 dealNum %= nextDealNum
         >>                 elderIsPlayer1 %= not

initialState = Game
  { _deck = sortedDeck
  , _visible = fromList []
  , _step = Start
  , _player1 = initialPlayer & name .~ "Roméo"
  , _player2 = initialPlayer & name .~ "Juliette"
  , _elderIsPlayer1 = True
  , _pointWinner = Nobody
  , _sequenceWinner = Nobody
  , _setWinner = Nobody
  , _dealNum = One
  }
  where initialPlayer = Player { _hand = noCards
                               , _dealPoints = 0
                               , _gamePoints = 0
                               , _points = 0
                               , _name = "undefined"
                               }

type GameAction = StateT Game IO ()

start :: GameAction
start =  step .= Start 
      >> pointWinner .= Nobody
      >> sequenceWinner .= Nobody
      >> setWinner .= Nobody
      >> deck .= sortedDeck
      >> shuffle

shuffle :: GameAction
shuffle = do
  state <- get
  shuffledDeck <- lift $ shuffleIO (state ^. deck)
  deck .= shuffledDeck
-- with arrows :
-- shuffle =  get 
--        >>= lift (liftA2 (fmap . flip (set deck)) id (view deck >>> shuffleIO)) 
--        >>= put

deal :: GameAction
deal = do
  game <- get
  let (hands, stock) = drawHands (game ^. deck) 12 2 
  player1 . hand .= hands !! 0
  player2 . hand .= hands !! 1
  deck .= stock
  step .= succ Deal 

getElderLens :: Game -> Lens' Game Player
getElderLens game = if game ^. elderIsPlayer1 then player1 else player2

getYoungerLens :: Game -> Lens' Game Player
getYoungerLens game = if game ^. elderIsPlayer1 then player2 else player1

exchangeForElder :: GameAction
-- exchangeForElder = get >>= (getElderLens >>> exchangeForPlayer) -- NOK
exchangeForElder = do
  lift $ putStrLn "Elder change cards:"
  game <- get
  exchangeForPlayer $ getElderLens game 
  -- exchangeForPlayer . getElderLens $ game -- NOK

exchangeForYounger :: GameAction
exchangeForYounger = do
  lift $ putStrLn "Younger change cards:"
  game <- get
  exchangeForPlayer $ getYoungerLens game 


-- exchangeForElder :: GameAction
-- exchangeForElder = exchangeForPlayer True
--
-- exchangeForYounger :: GameAction
-- exchangeForYounger = exchangeForPlayer False


-- exchangeForPlayer :: Bool -> GameAction
-- exchangeForPlayer isElder = do
--   game <- get
--   let playerLens 
--         |     isElder &&      game ^. elderIsPlayer1  = player1
--         | not isElder && not (game ^. elderIsPlayer1) = player1
--         | otherwise                                   = player2
exchangeForPlayer :: Lens' Game Player -> GameAction
exchangeForPlayer playerLens = do
  game <- get
  lift getLine >>= (   splitOn ","                                            -- [String]
                   >>> fmap read                                              -- [Int]
                   >>> getCardsAtPos (game ^. playerLens . hand)              -- [Card]
                   >>> fromList                                               -- OSet Card
                   >>> changeCards (game ^. deck) (game ^. playerLens . hand) -- (Hand, Deck)
                   >>> ( ($ game) . (playerLens . hand .~ )) *** (deck .~)    -- (Game, Game -> Game)
                   >>> uncurry (&)                                            -- Game
                   >>> put
                  )

declareCombinationElder :: CombinationType -> GameAction
declareCombinationElder combinationType = do
  game <- get
  let elderLens           = getElderLens game
      youngerLens         = getYoungerLens game
      elderCombinations   = getCombinations combinationType (game ^. elderLens . hand)
      youngerCombinations = getCombinations combinationType (game ^. youngerLens . hand)
  lift $ putStrLn $ "-> Elder declare " ++ show combinationType ++ " : " ++ showDeclarations elderCombinations
  choice <- lift getLine
  let maybeElderCombination = (elderCombinations !!) <$> readMaybe choice
      -- youngerUpperCombinations = maybe youngerCombinations (\elderCombi -> filter ( elderCombi <= ) youngerCombinations) maybeElderCombination
      -- youngerUpperCombinations = maybe youngerCombinations (\elderCombi -> filter ( ( GT /= ) . compareLength elderCombi) youngerCombinations) maybeElderCombination
      responseChoices = getResponseChoices maybeElderCombination youngerCombinations
  lift $ putStrLn $ "[Elder] " ++ show combinationType ++ " : " ++ fromMaybe "Nothing" (showDeclaration <$> maybeElderCombination) 
  lift $ putStrLn $ "-> Younger response " ++ show combinationType ++ " : " ++ show responseChoices
  choiceYounger <- lift getLine
  let maybeChoiceYounger = (responseChoices !!) <$> readMaybe choiceYounger
  declarationWinner <- lift $ getDeclarationWinner responseChoices maybeElderCombination maybeChoiceYounger
  getWinnerCombinationLens combinationType .= declarationWinner
  (getElderLens game . dealPoints) %= if declarationWinner == Elder 
                                         then ( + maybe 0 getCombinationPoints maybeElderCombination )
                                         else id

getResponseChoices :: Maybe Combination -> [Combination] -> [(DeclarationResponse, Combination)]
getResponseChoices Nothing [] = []
-- getResponseChoices Nothing combs = (Equals, Nothing) : ((NotGood, ) <$> combs)
getResponseChoices Nothing combs = ((NotGood, ) <$> combs)
getResponseChoices (Just elderCombination) combs = 
  let ecSize = length elderCombination in
    [ (Good, Nothing) ] 
    ++ bool [] ((Equals, ) <$> (filter (\c -> length c == ecSize ) combs))  
    ++ bool [] ((NotGood, ) <$> (filter (\c -> length c > ecSize ) combs))  

getDeclarationWinner :: [(DeclarationResponse, Combination)] -> Maybe Combination -> Maybe (DeclarationResponse, Combination) -> IO DeclarationWinner
getDeclarationWinner _  Nothing     Nothing                    = return Tie
getDeclarationWinner _  Nothing     (Just (Good, _))           = return Tie
getDeclarationWinner _  (Just cEld) Nothing                    = return Elder
getDeclarationWinner _  (Just cEld) (Just (Good, _))           = return Elder
getDeclarationWinner cs (Just cEld) (Just c@(NotGood, cYoung)) = return $ if c `elem` cs then Younger else Elder
getDeclarationWinner cs (Just cEld) (Just (Tie, cYoung))       =
  if c `notElem` cs
     then return Elder
     else do
       let (winner, response) = case compare cEld cYoung of
                                  EQ -> (Tie, Tie)
                                  LT -> (Younger, NotGood)
                                  GT -> (Elder, Good)
       putStrLn $ "[Elder] " ++ showDeclarationComplete cEld
       putStrLn $ "[Younger] " ++ show response
       return winner

getWinnerCombinationLens :: CombinationType ->  Lens' Game DeclarationWinner
getWinnerCombinationLens Point    = pointWinner
getWinnerCombinationLens Sequence = sequenceWinner
getWinnerCombinationLens Set      = setWinner

nextDealNum :: Deal -> Deal
nextDealNum dealN = if maxBound == dealN then maxBound else succ dealN
-- nextDealNum = liftA2 (`bool` maxBound) succ (maxBound == )

endGame :: GameAction
endGame =  lift (print "---- RESULTS ------")
        >> showGame

showGame :: GameAction
showGame = get >>= ( print >>> lift ) 

showDeck :: GameAction
showDeck = get >>= (view deck >>> show >>> putStrLn >>> lift) 

showDealNum :: GameAction
showDealNum = get >>= (view dealNum >>> show >>> ("--------- " ++ ) >>> putStrLn >>> lift) 

