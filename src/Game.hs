{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Game where
-- module Game
--     ( play
--     , Game
--     ) where

import PiquetTypes
import Cards
import Combinations
import qualified Shuffle

import Data.List.Split (splitOn)
import Data.Set.Ordered hiding (filter, null)
import Data.Function
import Data.Foldable (toList)
import Data.Aeson hiding ((.=))
import Data.Binary hiding (get)
import Data.Bool
import Data.Maybe
import Text.Read (readMaybe)
import Control.Monad.State
import Control.Lens
import Control.Arrow
import Control.Applicative
import System.Random
import System.IO (hGetLine, hPutStrLn, Handle, stderr, stdout)
import GHC.Generics

import Control.Distributed.Process (SendPortId)

---------------- Additional Lenses accessors & related functions

elder :: Lens' Game Player
elder f g = if g ^. player1 . isElder then player1 f g else player2 f g

younger :: Lens' Game Player
younger f g = if g ^. player1 . isElder then player2 f g else player1 f g

activePlayer :: Lens' Game Player
activePlayer f g = if g ^. isElderToPlay then elder f g else younger f g

inactivePlayer :: Lens' Game Player
inactivePlayer f g = if g ^. isElderToPlay then younger f g else elder f g

getOpponentLens :: Lens' Game Player -> Lens' Game Player
getOpponentLens playerLens f g = if g ^. playerLens . isElder then younger f g else elder f g

getCombinationLens :: CombinationType ->  Lens' Game (Maybe Combination)
getCombinationLens Point    = pointCombination
getCombinationLens Sequence = sequenceCombination
getCombinationLens Set      = setCombination

getCandidateLens :: CombinationType ->  Lens' Player (Maybe Combination)
getCandidateLens Point    = pointCandidate
getCandidateLens Sequence = sequenceCandidate
getCandidateLens Set      = setCandidate

getWinnerLens :: CombinationType ->  Lens' Game DeclarationWinner
getWinnerLens Point    = pointWinner
getWinnerLens Sequence = sequenceWinner
getWinnerLens Set      = setWinner

getPortIdPlayerLens :: SendPortId -> Lens' Game Player
getPortIdPlayerLens portId f g = if g ^. player1SendPortId  == Just portId then player1 f g else player2 f g

isPlayerToPlay ::  Lens' Game Player -> Game -> Bool
isPlayerToPlay playerLens game = game ^. isElderToPlay == game ^. playerLens . isElder 

----------------------------------------------------------------------- 

mkInitialState stdGen = Game
  { _stdGen = newStdGen
  , _dealNum = One
  , _dealMoves = []
  , _deals = []
  , _deck = shuffledDeck
  , _visible = fromList []
  , _step = Start
  , _player1 = initialPlayer & name .~ "Roméo"
  , _player2 = initialPlayer & name .~ "Juliette"
  , _player1SendPortId = Nothing
  , _player2SendPortId = Nothing
  , _isElderToPlay = True
  , _pointWinner = Nobody
  , _pointCombination = Nothing
  , _sequenceWinner = Nobody
  , _sequenceCombination = Nothing
  , _setWinner = Nobody
  , _setCombination = Nothing
  }
  where 
    (newStdGen, shuffledDeck) = Shuffle.shuffle sortedDeck stdGen  
    initialPlayer = Player { _hand = noCards
                               , _isElder = False
                               , _leftUntilCarteRouge = noCards
                               , _cardPlayed = Nothing
                               , _pointCandidate = Nothing
                               , _sequenceCandidate = Nothing
                               , _setCandidate = Nothing
                               , _dealPoints = 0
                               , _dealWons = 0
                               , _gamePoints = 0
                               , _points = 0
                               , _name = "undefined"
                               , _sockHandle = stderr
                               }

chooseElder :: Game -> Game
chooseElder game = 
  game & player1 . isElder .~ elderIsPlayer1
       & player2 . isElder .~ not elderIsPlayer1
       & stdGen .~ newStdGen
  where 
    (elderIsPlayer1, newStdGen) = randomR (False, True) (game ^. stdGen) 

deal :: Game -> Game
deal g = 
  let (hands, stock) = drawHands (g ^. deck) 12 2 
   in g & player1 . hand .~ hands !! 0
        & player1 . leftUntilCarteRouge .~ hands !! 0
        & player1 . pointCandidate      .~ Nothing
        & player1 . sequenceCandidate   .~ Nothing
        & player1 . setCandidate        .~ Nothing
        & player2 . hand                .~ hands !! 1
        & player2 . leftUntilCarteRouge .~ hands !! 1
        & player2 . pointCandidate      .~ Nothing
        & player2 . sequenceCandidate   .~ Nothing
        & player2 . setCandidate        .~ Nothing
        & deck                          .~ stock
        & dealMoves                     .~ []
        & step                          .~ succ Deal


moveByPlayer :: Lens' Game Player -> Game -> PlayerMove -> Move
moveByPlayer playerLens game = bool P2Move P1Move $ game ^. player1 . isElder == game ^. playerLens . isElder

checkCarteBlanche :: Lens' Game Player -> Game -> Either PiquetError Game
checkCarteBlanche playerLens game = 
   let moveCarteBlanche = moveByPlayer playerLens game CarteBlanche
       pastMoves        = fst <$> (game ^. dealMoves)
       piquetError
         | moveCarteBlanche `elem` pastMoves = Just NotYourTurnError 
         | not (isCarteBlanche (game ^. playerLens . hand)) = Just InvalidCombination 
         | otherwise = Nothing
   in if isJust piquetError 
         then Left (fromMaybe UnknownCommand piquetError)
         else Right (addPlayerMove playerLens CarteBlanche game)
  
changePlayerCards :: Hand -> Lens' Game Player -> Game -> Game
changePlayerCards toChange playerLens game = 
  if not (isPlayerToPlay playerLens game && game ^. step <= ExchangeYounger) 
     then game
     else game & deck                             .~ newDeck
               & playerLens . hand                .~ newHand
               & playerLens . leftUntilCarteRouge .~ newHand
               & isElderToPlay                    %~ not
               & step                             %~ succ
               -- & dealMoves                        %~ (moveChange:)
               & addPlayerMove playerLens moveChange
          where pHand = game ^. playerLens . hand 
                (newHand, newDeck) =  changeCards (game ^. deck) pHand toChange
                moveChange = Exchange toChange

declareCombination :: Combination -> Lens' Game Player -> Game -> Either PiquetError Game
declareCombination comb@(Combination ctype _) playerLens game 
  | not (isPlayerToPlay playerLens game)             = Left NotYourTurnError
  | Just ctype /= stepCombinationType (game ^. step) = Left InvalidCombination
  | not (comb `elem` playerCombinations)  = Left InvalidCombination
  | game ^. step `elem` declareElderSteps = declareCombinationElder    comb playerLens game
  | game ^. step `elem` responseSteps     = declareCombinationResponse comb playerLens game
  | game ^. step `elem` setPtsElderSteps  = setPointsCombinationElder  comb playerLens game
  | game ^. step `elem` setPtsYoungSteps  = setPointsCombinationYoung  comb playerLens game
  | otherwise                             = Left $ InvalidForStepError (game ^. step)
  where playerCombinations = (getCombinations ctype (game ^. playerLens . hand))
        declareElderSteps  = [DeclarePointElder, DeclareSequenceElder, DeclareSetElder]
        responseSteps      = [DeclarePointResponse, DeclareSequenceResponse, DeclareSetResponse]
        setPtsElderSteps   = [SetPointsPointElder, SetPointsSetElder, SetPointsSequenceElder]
        setPtsYoungSteps   = [SetPointsPointYounger, SetPointsSetYounger, SetPointsSequenceYounger]

declareCombinationElder :: Combination -> Lens' Game Player -> Game -> Either PiquetError Game
declareCombinationElder comb@(Combination ctype chand) playerLens game 
  | isJust (game ^. playerLens . getCandidateLens ctype) = Left NotYourTurnError
  | otherwise = Right $ game & playerLens . getCandidateLens ctype .~ Just comb
                             & isElderToPlay %~ not
                             & step %~ succ
                             & addPlayerMove playerLens (DeclarationCount ctype (length chand))

declareCombinationResponse :: Combination -> Lens' Game Player -> Game -> Either PiquetError Game
declareCombinationResponse comb@(Combination ctype chand) playerLens game =
  if isJust (game ^. playerLens . getCandidateLens ctype)
     then Left NotYourTurnError
     else Right $ game'' & isElderToPlay %~ not
                         & step %~ succ
       where 
         -- elder combination souldn't be Nothing at this stage
         elderComb = fromMaybe (Combination ctype (fromList [])) (game ^. elder . getCandidateLens ctype)
         compareCombs = compare comb elderComb
         game' = if length chand == length (cards elderComb)
                    then game & addPlayerMove playerLens (PlayerResponse ctype Equals)
                              & addPlayerMove elder (DeclarationUpper ctype (rank $ maximum (cards elderComb)))
                    else game
         game'' = case compareCombs of 
                   EQ -> game' & playerLens . getCandidateLens ctype .~ Nothing
                               & elder             . getCandidateLens ctype .~ Nothing
                               & getWinnerLens ctype .~ Tie
                               & addPlayerMove playerLens (PlayerResponse ctype Equals)
                               & step %~ succ -- tie, we bypass SetPoints step for Elder
                   GT -> game' & playerLens . getCandidateLens ctype .~ Just comb
                               & elder      . getCandidateLens ctype .~ Nothing
                               & getWinnerLens ctype .~ Younger
                               & addPlayerMove playerLens (PlayerResponse ctype NotGood)
                               & step %~ succ -- elder lost, we bypass SetPoints step for Elder
                   LT -> game' & getWinnerLens ctype .~ Elder -- elder win
                               & addPlayerMove playerLens (PlayerResponse ctype Good)
                               & addPlayerMove elder (Declaration elderComb)

setPointsCombinationElder :: Combination -> Lens' Game Player -> Game -> Either PiquetError Game
setPointsCombinationElder comb@(Combination ctype chand) playerLens game
  | length chand == 0 = Right $ game & step %~ succ
  | fmap (compare comb) (game ^. playerLens . getCandidateLens ctype) /= Just LT = Left InvalidCombination
  | otherwise = Right $ game & playerLens . getCandidateLens ctype .~ Just comb
                             & addPlayerMove playerLens (Declaration comb) 

setPointsCombinationYoung :: Combination -> Lens' Game Player -> Game -> Either PiquetError Game
setPointsCombinationYoung comb@(Combination ctype chand) playerLens game
  | length chand == 0 && ctype == Point    = Right $ game & step %~ succ & initPointsYounger Sequence
  | length chand == 0 && ctype == Sequence = Right $ game & step %~ succ & initPointsYounger Set
  | length chand == 0 && ctype == Set      = Right $ game & step %~ succ
  | fmap (compare comb) (game ^. playerLens . getCandidateLens ctype) /= Just LT = Left InvalidCombination
  | otherwise = Right $ game & playerLens . getCandidateLens ctype .~ Just comb
                             & addPlayerMove playerLens (Declaration comb) 

initPointsYounger :: CombinationType -> Game -> Game
initPointsYounger ctype game 
  | isJust mbComb     = game & addPlayerMove younger (Declaration $ fromJust mbComb)
  | ctype == Point    = game & step %~ succ & initPointsYounger Sequence
  | ctype == Sequence = game & step %~ succ & initPointsYounger Set
  | ctype == Set      = game & step %~ succ
  where mbComb = game ^. younger . getCandidateLens ctype

stepCombinationType :: Step -> Maybe CombinationType
stepCombinationType step = 
  case step of
    DeclarePointElder        -> Just Point
    DeclarePointResponse     -> Just Point
    SetPointsPointElder      -> Just Point
    DeclareSequenceElder     -> Just Sequence
    DeclareSequenceResponse  -> Just Sequence
    SetPointsSequenceElder   -> Just Sequence
    DeclareSetElder          -> Just Set
    DeclareSetResponse       -> Just Set
    SetPointsSetElder        -> Just Set
    SetPointsPointYounger    -> Just Point
    SetPointsSequenceYounger -> Just Sequence
    SetPointsSetYounger      -> Just Set
    _                        -> Nothing

playCard :: Card -> Lens' Game Player -> Game -> Either PiquetError Game
playCard card playerLens game
    | not (isPlayerToPlay playerLens game)      = Left NotYourTurnError
    | card `member` (game ^. playerLens . hand) = Left CardNotInHand
    | game ^. step == PlayFirstCard             = Right $ game 
                                                           & playerLens . hand %~ delete card
                                                           & playerLens . cardPlayed .~ Just card
                                                           & addPlayerMove playerLens (PlayFirst card)
                                                           & step %~ succ
                                                           & initPointsYounger Point
    | game ^. step /= PlayCards                 = Left $ InvalidForStepError (game ^. step)
    | otherwise                                 = 
        case game ^. inactivePlayer . cardPlayed of 
          Nothing           -> Right $ game & playerLens . hand %~ delete card
                                            & playerLens . cardPlayed .~ Just card
                                            & addPlayerMove playerLens (PlayFirst card)
          Just opponentCard -> Right $ game & playerLens . hand %~ delete card
                                            & playerLens . cardPlayed .~ Just card
                                            & addPlayerMove playerLens (PlayCard card)
                                            & setWinner opponentCard
                                            & activePlayer   . cardPlayed .~ Nothing
                                            & inactivePlayer . cardPlayed .~ Nothing
                                            & checkLastTrick activePlayerWon
     where 
       wonAgainstCard oppCard = suit card == suit oppCard && rank card > rank oppCard
       activePlayerWon = maybe False wonAgainstCard (game ^. inactivePlayer . cardPlayed)
       setWinner oppCard game = if activePlayerWon 
                                   then game & addPlayerMove playerLens WinAsSecond
                                             & playerLens . dealWons %~ (+1)
                                             & isElderToPlay .~ (game ^. playerLens . isElder) 
                                   else game
                                             & inactivePlayer . dealWons %~ (+1)
                                             & isElderToPlay .~ (game ^. inactivePlayer . isElder) 


checkLastTrick :: Bool -> Game -> Game
checkLastTrick activePlayerWon g 
  | length (g ^. activePlayer . hand) > 0 = g
  | activePlayerWon                          = g & addPlayerMove   activePlayer WinLastTrick & finishCards
  | otherwise                                = g & addPlayerMove inactivePlayer WinLastTrick & finishCards
  where finishCards g = g & checkPlayPoints 
                          & step .~ PlayEnd
                          & checkEndGame

checkEndGame :: Game -> Game
checkEndGame g 
  | g ^. step /= PlayEnd     = g
  | g ^. dealNum == maxBound = g & step .~ End
  | otherwise                = g & nextDeal
                                 & step .~ Deal
                                 & deal
                           
checkPlayPoints :: Game -> Game
checkPlayPoints g 
  | length hand1 /= 0 || length hand2 /= 0 || won1 == won2 = g
  | min won1 won2 == 0 = g & addPlayerMove (if won1 > won2 then player1 else player2) Capot
  | otherwise          = g & addPlayerMove (if won1 > won2 then player1 else player2) WinCards
  --  otherwise          = g & addPlayerMove (if won1 > won2 then player1 else player2) WinCards
  where hand1 = g ^. player1 . hand
        hand2 = g ^. player2 . hand
        won1  = g ^. player1 . dealWons
        won2  = g ^. player2 . dealWons
        -- winnerLens = if won1 > won2 then player1 else player2

                      
nextDeal :: Game -> Game
nextDeal game = game & deals %~ ((game ^. dealNum, game ^. dealMoves ):)
                     & dealNum %~ nextDealNum
                     & (player1 . isElder) %~ not
                     & (player2 . isElder) %~ not

nextDealNum :: Deal -> Deal
nextDealNum dealN = if maxBound == dealN then maxBound else succ dealN

-- checkCarteRouge :: Lens' Game Player -> Maybe Combination -> GameAction
-- checkCarteRouge playerLens maybeCombination = do
--   pSock <- use (playerLens . sockHandle) 
--   leftRouge <- use (playerLens . leftUntilCarteRouge) 
--   -- Check only if Carte Rouge has not already been declared
--   when (size leftRouge > 0) $ do
--     let leftAfterCombination =  maybe leftRouge (leftRouge \\) (cards <$> maybeCombination) 
--     (playerLens . leftUntilCarteRouge) .= leftAfterCombination
--     lift $ hPutStrLn pSock $ "left until Carte rouge :" ++ show leftAfterCombination
--     when (size leftAfterCombination == 0 ) $ do
--       display "[] Carte rouge -> +20"
--       addDealPointsAction playerLens 20

addPlayerMove :: Lens' Game Player -> PlayerMove -> Game -> Game
addPlayerMove playerLens move game = game'' where
  mbPiquetMove = maybePiquetMove playerLens move game  -- is there a pique or repique ?
  game'  = addPlayerMoveSimple playerLens game move  -- add points for the move
  game'' = maybe game' (addPlayerMoveSimple playerLens game') mbPiquetMove -- maybe add points for the pique/repique

addPlayerMoveSimple :: Lens' Game Player -> Game -> PlayerMove -> Game
addPlayerMoveSimple playerLens game pmove = 
  game & playerLens . dealPoints +~ points
       & dealMoves %~ ((move, points):)
  where points = movePoints pmove
        move   = moveByPlayer playerLens game pmove

maybePiquetMove :: Lens' Game Player -> PlayerMove -> Game -> Maybe PlayerMove
maybePiquetMove playerLens move game = maybeMove where
  gameStep       = game ^. step
  pointsBefore   = game ^. (playerLens . dealPoints) 
  opponentPoints = game ^. (getOpponentLens playerLens . dealPoints)
  maybeMove
    | 30 <= pointsBefore                             = Nothing -- not the first time over 30
    | (pointsBefore + movePoints move) < 30          = Nothing -- not over 30
    | move == Capot                                  = Nothing -- capot don't count for piquet
    | (gameStep < PlayCards && opponentPoints <= 1)  = Just Repique
    | (gameStep >= PlayCards && opponentPoints == 0) = Just Pique
    | otherwise                                      = Nothing
