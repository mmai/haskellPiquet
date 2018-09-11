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

getWinnerLens :: CombinationType ->  Lens' Game DeclarationWinner
getWinnerLens Point    = pointWinner
getWinnerLens Sequence = sequenceWinner
getWinnerLens Set      = setWinner

getPortIdPlayerLens :: SendPortId -> Lens' Game Player
getPortIdPlayerLens portId f g = if g ^. player1SendPortId  == Just portId then player1 f g else player2 f g

isPlayerToPlay ::  Lens' Game Player -> Game -> Bool
isPlayerToPlay playerLens game = game ^. isElderToPlay == game ^. playerLens . isElder 

----------------------------------------------------------------------- 

play :: StdGen -> Handle -> Handle -> IO () 
play stdGen p1Handle p2Handle = do
  game <- flip execStateT (mkInitialState stdGen) $ do
    elderIsPlayer1 <- lift (randomRIO (False, True)) 
    assign (player1 . isElder ) elderIsPlayer1
    assign (player2 . isElder ) (not elderIsPlayer1)
    replicateM_ 6 playDeal
    endGame
  return ()

playDeal :: GameAction
playDeal = -- step .= Start >> start
         -- >>                 showDealNum
         -- >> step %= succ >> dealA
         -- >>                 showGame
         -- >> step %= succ >> changeElderCards
         -- >> step %= succ >> changeYoungerCards
         -- >>                 showGame
          step %= succ >> declarationElder Point
         >> step %= succ >> declarationElder Sequence
         >> step %= succ >> declarationElder Set
         >> step %= succ >> playFirstCard -- elder play first card before younger declarations
         >> step %= succ >> setCombinationPoints Younger Point
         >> step %= succ >> setCombinationPoints Younger Sequence
         >> step %= succ >> setCombinationPoints Younger Set
         >> step %= succ >> playCards
         >> step %= succ >> endGame
         >>                 showGame
         >>                 dealNum %= nextDealNum
         >>                 (player1 . isElder) %= not
         >>                 (player2 . isElder) %= not

declarationElder :: CombinationType -> GameAction
declarationElder ct = declareCombinationElder ct 
   >> step %= succ >> declareCombinationResponse ct
   >> step %= succ >> setCombinationPoints Elder ct

mkInitialState stdGen = Game
  { _stdGen = newStdGen
  , _dealNum = One
  , _dealMoves = []
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

type GameAction = StateT Game IO ()

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

declareCombination :: Hand -> Lens' Game Player -> Game -> Either PiquetError Game
declareCombination hand playerLens game = undefined

declareResponse :: DeclarationResponse -> Lens' Game Player -> Game -> Either PiquetError Game
declareResponse Good playerLens game = undefined
declareResponse NotGood playerLens game = undefined
declareResponse Equals playerLens game = undefined

declareCombinationElder :: CombinationType -> GameAction
declareCombinationElder combinationType = do
  game <- get
  elderHand <- use (elder . hand)
  elderSock <- use (elder . sockHandle)
  lift $ hPutStrLn elderSock $ show elderHand
  let   elderCombinations = getCombinations combinationType elderHand
      -- elderCombinations = getCombinations combinationType (game ^. elderLens . hand)
  lift $ hPutStrLn elderSock $ "Declare " ++ show combinationType ++ " : " ++ showDeclarations elderCombinations
  choice <- lift $ hGetLine elderSock
  let maybeElderCombination = (elderCombinations !!) <$> readMaybe choice
  display $ "[Elder] " ++ show combinationType ++ " : " ++ fromMaybe "Nothing" (showDeclaration <$> maybeElderCombination) 
  (getCombinationLens combinationType) .= maybeElderCombination

declareCombinationResponse :: CombinationType -> GameAction
declareCombinationResponse combinationType = do
  game <- get
  youngerSock <- use $ younger . sockHandle
  let maybeElderCombination = game ^. getCombinationLens combinationType
      youngerCombinations   = getCombinations combinationType (game ^. younger . hand)
      responseChoices       = getResponseChoices maybeElderCombination youngerCombinations
  lift $ hPutStrLn youngerSock $ "Response " ++ show combinationType ++ " : " ++ show responseChoices
  choiceYounger <- lift $ hGetLine youngerSock
  let maybeChoiceYounger = (responseChoices !!) <$> readMaybe choiceYounger
  display $ "[Younger] " ++ show (maybe Good fst maybeChoiceYounger)
  (declarationWinner, combination, maybeToDisplay) <- lift $ getDeclarationWinner responseChoices maybeElderCombination maybeChoiceYounger
  maybe (return ()) display maybeToDisplay
  getWinnerLens      combinationType .= declarationWinner
  getCombinationLens combinationType .= combination

checkCarteRouge :: Lens' Game Player -> Maybe Combination -> GameAction
checkCarteRouge playerLens maybeCombination = do
  pSock <- use (playerLens . sockHandle) 
  leftRouge <- use (playerLens . leftUntilCarteRouge) 
  -- Check only if Carte Rouge has not already been declared
  when (size leftRouge > 0) $ do
    let leftAfterCombination =  maybe leftRouge (leftRouge \\) (cards <$> maybeCombination) 
    (playerLens . leftUntilCarteRouge) .= leftAfterCombination
    lift $ hPutStrLn pSock $ "left until Carte rouge :" ++ show leftAfterCombination
    when (size leftAfterCombination == 0 ) $ do
      display "[] Carte rouge -> +20"
      addDealPointsAction playerLens 20

addPlayerMove :: Lens' Game Player -> PlayerMove -> Game -> Game
addPlayerMove playerLens move game = game'' where
  mbPiquetMove = maybePiquetMove playerLens (movePoints move) game  -- is there a pique or repique ?
  game'  = addPlayerMoveSimple playerLens game move  -- add points for the move
  game'' = maybe game' (addPlayerMoveSimple playerLens game') mbPiquetMove -- maybe add points for the pique/repique

addPlayerMoveSimple :: Lens' Game Player -> Game -> PlayerMove -> Game
addPlayerMoveSimple playerLens game pmove = 
  game & playerLens . dealPoints +~ points
       & dealMoves %~ ((move, points):)
  where points = movePoints pmove
        move   = moveByPlayer playerLens game pmove

maybePiquetMove :: Lens' Game Player -> Int -> Game -> Maybe PlayerMove
maybePiquetMove playerLens points game = maybeMove where
  gameStep       = game ^. step
  pointsBefore   = game ^. (playerLens . dealPoints) 
  opponentPoints = game ^. (getOpponentLens playerLens . dealPoints)
  maybeMove
    | (pointsBefore + points) >= 30 || 30 > pointsBefore = Nothing -- not over 30, or not the first time
    | (gameStep < PlayCards && opponentPoints <= 1)      = Just Repique
    | (gameStep >= PlayCards && opponentPoints == 0)     = Just Pique
    | otherwise                                          = Nothing

addDealPointsAction :: Lens' Game Player -> Int -> GameAction
addDealPointsAction playerLens points = do
  game           <- get
  gameStep       <- use step 
  opponentPoints <- use (getOpponentLens playerLens . dealPoints)
  pointsBefore   <- use (playerLens . dealPoints) 
  playerLens . dealPoints %= (+ points)
  when (pointsBefore < 30 && 30 <= (pointsBefore + points)) $  -- first time player go over 30 points
    if gameStep < PlayCards
       then when (opponentPoints <= 1) $ do
             display "[] REPIQUE -> +60" 
             playerLens . dealPoints %= (+ 60 )
       else when (opponentPoints == 0) $ do
             display "[] PIQUE -> +30" 
             playerLens . dealPoints %= (+ 30 )

checkPlayPoints :: GameAction
checkPlayPoints = do
  hand1 <- use $ player1 . hand
  hand2 <- use $ player2 . hand
  when (length hand1 == 0 && length hand2 == 0) $ do -- check if deal is finished
    won1  <- use $ player1 . dealWons
    won2  <- use $ player2 . dealWons
    when (won1 /= won2) $ do -- if there is a play winner
      let winnerLens = if won1 > won2 then player1 else player2
      if (min won1 won2 == 0) 
         then winnerLens . dealPoints %= (+40) -- Capot, do not count for pique
         else 
           addDealPointsAction (if won1 > won2 then player1 else player2) 10
           -- addDealPointsAction winnerLens 10

setCombinationPoints :: DeclarationWinner -> CombinationType -> GameAction
setCombinationPoints decPlayer combinationType = do
  game <- get
  when (game ^. getWinnerLens combinationType == decPlayer) $ do
    -- add points for winning combination
    let winnerLens dc = bool younger elder (dc == Elder)
    maybeWinComb <- use (getCombinationLens combinationType)
    addDealPointsAction (winnerLens decPlayer) ( maybe 0 getCombinationPoints maybeWinComb )
    checkCarteRouge (winnerLens decPlayer) maybeWinComb 
    unless (combinationType == Point) $ do
      -- ask for other smaller combinations of the same type
      let candidates = getSmallerCombinations maybeWinComb . getCombinations combinationType $ game ^. (winnerLens decPlayer) . hand
      when (candidates /= []) $ do
        let winnerSock = (game ^. (winnerLens decPlayer) . sockHandle)
        lift $ hPutStrLn winnerSock $ "-> " ++ show decPlayer ++ " other " ++ show combinationType ++ " combinations : " ++ show candidates
        strOthers <- lift $ liftM (filter (/= '\r')) $ hGetLine winnerSock
        let others = (candidates !!) . read <$> splitOn "," strOthers
        sequence_ $ (display . (("[" ++ show decPlayer ++ "] ") ++) . showDeclarationComplete) <$> others -- show combination
        sequence_ $ (addDealPointsAction (winnerLens decPlayer) . getCombinationPoints) <$> others  -- add points 
        sequence_ $ checkCarteRouge (winnerLens decPlayer) . Just <$> others 

setCombinationElderPoints :: CombinationType -> GameAction
setCombinationElderPoints Point           = return ()
setCombinationElderPoints combinationType = do
  game <- get
  when (game ^. getWinnerLens combinationType == Elder) $ do
    -- add points for winning combination
    maybeWinComb <- use (getCombinationLens combinationType)
    addDealPointsAction elder ( maybe 0 getCombinationPoints maybeWinComb )
    checkCarteRouge elder maybeWinComb 
    -- ask for other smaller combinations of the same type
    let candidates = getSmallerCombinations maybeWinComb . getCombinations combinationType $ game ^. elder . hand
    when (candidates /= []) $ do
      lift $ hPutStrLn (game ^. elder . sockHandle) $ "-> Elder other " ++ show combinationType ++ " combinations : " ++ show candidates
      strOthers <- lift getLine
      let others = (candidates !!) . read <$> splitOn "," strOthers
      sequence_ $ (display . ("[Elder] " ++) . showDeclarationComplete) <$> others -- show combination
      sequence_ $ (addDealPointsAction elder . getCombinationPoints) <$> others  -- add points 
      sequence_ $ checkCarteRouge elder . Just <$> others 

getResponseChoices :: Maybe Combination -> [Combination] -> [(DeclarationResponse, Maybe Combination)]
getResponseChoices Nothing [] = []
getResponseChoices Nothing combs = (Equals, Nothing) : ((NotGood, ) . Just <$> combs)
getResponseChoices (Just elderCombination) combs = 
  let ecSize = length (cards elderCombination) in
    [ (Good, Nothing) ] 
    ++ (((Equals, ) . Just) <$> filter (\c -> length (cards c) == ecSize ) combs)
    ++ (((NotGood, ) . Just) <$> filter (\c -> length (cards c) > ecSize ) combs)  

getDeclarationWinner :: [(DeclarationResponse, Maybe Combination)] -> Maybe Combination -> Maybe (DeclarationResponse, Maybe Combination) -> IO (DeclarationWinner, Maybe Combination, Maybe String)
getDeclarationWinner _  Nothing     Nothing                    = return (Tie, Nothing, Nothing)
getDeclarationWinner _  Nothing     (Just (Good, _))           = return (Tie, Nothing, Nothing)
getDeclarationWinner _  (Just cEld) Nothing                    = return (Elder, Just cEld, Nothing)
getDeclarationWinner _  (Just cEld) (Just (Good, _))           = return (Elder, Just cEld, Nothing)
getDeclarationWinner ds (Just cEld) (Just c@(Equals, Nothing)) = return (Elder, Just cEld, Nothing)
getDeclarationWinner ds Nothing  (Just (NotGood, Just cYoung)) = return (Younger, Just cYoung, Nothing)
getDeclarationWinner ds (Just cEld) (Just d@(NotGood, mcYoung)) = return $ if d `elem` ds 
                                                                             then (Younger, mcYoung, Nothing) 
                                                                             else (Elder, Just cEld, Nothing)
getDeclarationWinner ds (Just cEld) (Just d@(Equals, Just cYoung)) =
  if d `notElem` ds
     then return (Elder, Just cEld, Nothing)
     else do
       let (winner, response, combination) = 
             case compare cEld cYoung of
               EQ -> (Tie,     Equals,  Just cEld)
               LT -> (Younger, NotGood, Just cYoung)
               GT -> (Elder,   Good,    Just cEld)
           toDisplay = "[Elder] " ++ showDeclarationComplete cEld ++ "\n[Younger] " ++ show response
       return (winner, combination, Just toDisplay )

playFirstCard :: GameAction
playFirstCard = use activePlayer >>= lift . chooseCard >>= playCard

playCards :: GameAction
playCards = do
  use activePlayer >>= lift . chooseCard >>= playCard
  use step <&> ( == PlayEnd ) >>= bool playCards checkPlayPoints

chooseCard :: Player -> IO Card
chooseCard player = do
  let pSock = player ^. sockHandle
      cards = toList ( player ^. hand )
  hPutStrLn pSock $ "Choose a card to play : " ++ show cards
  choice <- liftM (filter (/= '\r')) $ hGetLine pSock
  -- let maybeChoice = join $ (cards ^?) <$> ix <$> readMaybe choice
  let maybeChoice = join $ (cards ^?) <$> element <$> readMaybe choice
  case maybeChoice of
    Nothing -> do
      hPutStrLn pSock "Bad choice"
      chooseCard player
    Just card -> return card

playCard :: Card -> GameAction
playCard card = do
  aHand <- use $ activePlayer . hand
  when ( card `member` aHand ) $ do
    game <- get
    pSock <- use $ activePlayer . sockHandle
    display $ "played = " ++ show card
    activePlayer . hand %= delete card
    case (game ^. inactivePlayer . cardPlayed) of
      Nothing -> do -- First to play in the turn
        activePlayer . cardPlayed .= Just card
        addDealPointsAction activePlayer 1
        isElderToPlay %= not
      Just opponentCard -> do -- Second to play in the turn
        let activePlayerWon = (suit card == suit opponentCard && rank card > rank opponentCard)
        when activePlayerWon $ addDealPointsAction activePlayer 1 
        if activePlayerWon then finishTurn activePlayer inactivePlayer else finishTurn inactivePlayer activePlayer 
        when (length (game ^. activePlayer . hand) == 0) $ do -- this is the last turn 
           addDealPointsAction (if activePlayerWon then activePlayer else inactivePlayer) 1 -- an additional point for the winner
           step .= PlayEnd
        activePlayer   . cardPlayed .= Nothing
        inactivePlayer . cardPlayed .= Nothing

finishTurn :: Lens' Game Player -> Lens' Game Player -> GameAction
finishTurn winnerLens looserLens = do
  game <- get
  lift $ hPutStrLn (game ^. winnerLens . sockHandle ) " you won "
  lift $ hPutStrLn (game ^. looserLens . sockHandle ) " you lost "
  winnerLens . dealWons %= (+1)
  isElderToPlay .= (game ^. winnerLens . isElder) 

nextDealNum :: Deal -> Deal
nextDealNum dealN = if maxBound == dealN then maxBound else succ dealN
-- nextDealNum = liftA2 (`bool` maxBound) succ (maxBound == )

display :: String -> GameAction
display message = do
  game <- get
  lift $ hPutStrLn (game ^. player1 . sockHandle) message
  lift $ hPutStrLn (game ^. player2 . sockHandle) message

endGame :: GameAction
endGame =  display "---- RESULTS ------"
        >> showGame

showGame :: GameAction
showGame = get >>= display . show

showDeck :: GameAction
showDeck = get >>= (view deck >>> show >>> putStrLn >>> lift) 

showDealNum :: GameAction
showDealNum = get >>= (view dealNum >>> show >>> ("--------- " ++ ) >>> display) 

