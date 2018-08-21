{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Game where
-- module Game
--     ( play
--     , Game
--     ) where

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
          | SetPointsPointElder 
          | DeclareSequenceElder
          | DeclareSequenceResponse
          | SetPointsSequenceElder 
          | DeclareSetElder
          | DeclareSetResponse
          | SetPointsSetElder 
          | DeclareCarteRougeElder
          | Play1
          | DeclarePointYounger
          | SetPointsPointYounger 
          | DeclareSequenceYounger
          | SetPointsSequenceYounger 
          | DeclareSetYounger
          | SetPointsSetYounger 
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

data DeclarationResponse = Good | NotGood | Equals deriving (Eq, Show)

data DeclarationWinner = Elder | Younger | Tie | Nobody deriving (Eq, Show)

data Game = Game { _dealNum             :: Deal
                 , _deck                :: Deck
                 , _visible             :: Deck
                 , _step                :: Step
                 , _player1             :: Player
                 , _player2             :: Player
                 , _elderIsPlayer1      :: Bool
                 , _pointWinner         :: DeclarationWinner
                 , _pointCombination    :: Maybe Combination
                 , _sequenceWinner      :: DeclarationWinner
                 , _sequenceCombination :: Maybe Combination
                 , _setWinner           :: DeclarationWinner
                 , _setCombination      :: Maybe Combination
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

-- Lenses accessors
getElderLens :: Game -> Lens' Game Player
getElderLens game = if game ^. elderIsPlayer1 then player1 else player2

getYoungerLens :: Game -> Lens' Game Player
getYoungerLens game = if game ^. elderIsPlayer1 then player2 else player1

getCombinationLens :: CombinationType ->  Lens' Game (Maybe Combination)
getCombinationLens Point    = pointCombination
getCombinationLens Sequence = sequenceCombination
getCombinationLens Set      = setCombination

getWinnerLens :: CombinationType ->  Lens' Game DeclarationWinner
getWinnerLens Point    = pointWinner
getWinnerLens Sequence = sequenceWinner
getWinnerLens Set      = setWinner

------- 

play :: IO Game 
play = flip execStateT initialState $
  lift (randomRIO (True, False)) >>= assign elderIsPlayer1 -- set random elder
 >> replicateM_ 6 playDeal
 >> endGame

playDeal :: GameAction
playDeal =                  start
         >>                 showDealNum
         >> step %= succ >> deal
         >>                 showGame
         >> step %= succ >> changeElderCards
         >> step %= succ >> changeYoungerCards
         >> step %= succ >> declarationElder Point
         >> step %= succ >> declarationElder Sequence
         >> step %= succ >> declarationElder Set
         >>                 showGame
         >>                 dealNum %= nextDealNum
         >>                 elderIsPlayer1 %= not

declarationElder :: CombinationType -> GameAction
declarationElder ct = declareCombinationElder ct 
   >> step %= succ >> declareCombinationResponse ct
   >> step %= succ >> setCombinationElderPoints ct

initialState = Game
  { _dealNum = One
  , _deck = sortedDeck
  , _visible = fromList []
  , _step = Start
  , _player1 = initialPlayer & name .~ "Roméo"
  , _player2 = initialPlayer & name .~ "Juliette"
  , _elderIsPlayer1 = True
  , _pointWinner = Nobody
  , _pointCombination = Nothing
  , _sequenceWinner = Nobody
  , _sequenceCombination = Nothing
  , _setWinner = Nobody
  , _setCombination = Nothing
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

changeElderCards :: GameAction
-- changeElderCards = get >>= (getElderLens >>> changePlayerCards) -- NOK
changeElderCards = do
  lift $ putStrLn "Elder change cards:"
  game <- get
  changePlayerCards $ getElderLens game 
  -- changePlayerCards . getElderLens $ game -- NOK

changeYoungerCards :: GameAction
changeYoungerCards = do
  lift $ putStrLn "Younger change cards:"
  game <- get
  changePlayerCards $ getYoungerLens game 

changePlayerCards :: Lens' Game Player -> GameAction
changePlayerCards playerLens = do
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
      combLens = getCombinationLens combinationType
      elderCombinations   = getCombinations combinationType (game ^. elderLens . hand)
  lift $ putStrLn $ "-> Elder declare " ++ show combinationType ++ " : " ++ showDeclarations elderCombinations
  choice <- lift getLine
  let maybeElderCombination = (elderCombinations !!) <$> readMaybe choice
  lift $ putStrLn $ "[Elder] " ++ show combinationType ++ " : " ++ fromMaybe "Nothing" (showDeclaration <$> maybeElderCombination) 
  combLens .= maybeElderCombination

declareCombinationResponse :: CombinationType -> GameAction
declareCombinationResponse combinationType = do
  game <- get
  let youngerLens           = getYoungerLens game
      maybeElderCombination = game ^. getCombinationLens combinationType
      youngerCombinations   = getCombinations combinationType (game ^. youngerLens . hand)
      responseChoices       = getResponseChoices maybeElderCombination youngerCombinations
  lift $ putStrLn $ "-> Younger response " ++ show combinationType ++ " : " ++ show responseChoices
  choiceYounger <- lift getLine
  let maybeChoiceYounger = (responseChoices !!) <$> readMaybe choiceYounger
  lift $ putStrLn $ "[Younger] " ++ show (fromMaybe Good (fst <$> maybeChoiceYounger))
  (declarationWinner, combination) <- lift $ getDeclarationWinner responseChoices maybeElderCombination maybeChoiceYounger
  getWinnerLens      combinationType .= declarationWinner
  getCombinationLens combinationType .= combination
  (getElderLens game . dealPoints) %= if declarationWinner == Elder 
                                         then ( + maybe 0 getCombinationPoints maybeElderCombination )
                                         else id

setCombinationElderPoints :: CombinationType -> GameAction
setCombinationElderPoints Point           = return ()
setCombinationElderPoints combinationType = do
  game <- get
  if game ^. getWinnerLens combinationType /= Elder
     then return ()
     else do
       let maybeWinComb = game ^. getCombinationLens combinationType
           combinations = getCombinations combinationType (game ^. getElderLens game . hand)
           candidates   = getSmallerCombinations maybeWinComb combinations
       lift $ putStrLn $ "-> Elder other " ++ show combinationType ++ " combinations : " ++ show candidates
       strOthers <- lift getLine
       let others = (candidates !!) . read <$> splitOn "," strOthers
       sequence_ $ (lift . putStrLn . ("[Elder] " ++) . showDeclarationComplete) <$> others -- show combination
       sequence_ $ (getElderLens game . dealPoints %=) . (+) . getCombinationPoints <$> others  -- add points 


getResponseChoices :: Maybe Combination -> [Combination] -> [(DeclarationResponse, Maybe Combination)]
getResponseChoices Nothing [] = []
getResponseChoices Nothing combs = (Equals, Nothing) : ((NotGood, ) . Just <$> combs)
getResponseChoices (Just elderCombination) combs = 
  let ecSize = length (cards elderCombination) in
    [ (Good, Nothing) ] 
    ++ (((Equals, ) . Just) <$> filter (\c -> length (cards c) == ecSize ) combs)
    ++ (((NotGood, ) . Just) <$> filter (\c -> length (cards c) > ecSize ) combs)  

getDeclarationWinner :: [(DeclarationResponse, Maybe Combination)] -> Maybe Combination -> Maybe (DeclarationResponse, Maybe Combination) -> IO (DeclarationWinner, Maybe Combination)
getDeclarationWinner _  Nothing     Nothing                    = return (Tie, Nothing)
getDeclarationWinner _  Nothing     (Just (Good, _))           = return (Tie, Nothing)
getDeclarationWinner _  (Just cEld) Nothing                    = return (Elder, Just cEld)
getDeclarationWinner _  (Just cEld) (Just (Good, _))           = return (Elder, Just cEld)
getDeclarationWinner ds (Just cEld) (Just c@(Equals, Nothing)) = return (Elder, Just cEld)
getDeclarationWinner ds (Just cEld) (Just d@(NotGood, mcYoung)) = return $ if d `elem` ds 
                                                                             then (Younger, mcYoung) 
                                                                             else (Elder, Just cEld)
getDeclarationWinner ds (Just cEld) (Just d@(Equals, Just cYoung)) =
  if d `notElem` ds
     then return (Elder, Just cEld)
     else do
       let (winner, response, combination) = 
             case compare cEld cYoung of
               EQ -> (Tie,     Equals,  Just cEld)
               LT -> (Younger, NotGood, Just cYoung)
               GT -> (Elder,   Good,    Just cEld)
       putStrLn $ "[Elder] " ++ showDeclarationComplete cEld
       putStrLn $ "[Younger] " ++ show response
       return (winner, combination)

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

