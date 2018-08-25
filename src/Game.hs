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
import Data.Set.Ordered hiding (filter, null)
import Data.Function
import Data.Foldable (toList)
import Data.Bool
import Data.Maybe
import Text.Read (readMaybe)
import Control.Monad.State
import Control.Lens
import Control.Arrow
import Control.Applicative
import System.Random
import System.IO (hGetLine, hPutStrLn, Handle, stderr)

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
          | PlayFirstCard
          | SetPointsPointYounger 
          | SetPointsSequenceYounger 
          | SetPointsSetYounger 
          | PlayCards
          | PlayEnd
          | End
          deriving (Enum, Show, Ord, Eq)

data Player = Player { _hand :: Hand
                     , _isElder :: Bool
                     , _leftUntilCarteRouge :: Hand
                     , _cardPlayed :: Maybe Card
                     , _dealPoints :: Int
                     , _dealWons :: Int
                     , _gamePoints :: Int
                     , _points :: Int
                     , _name :: String
                     , _sockHandle :: Handle
                     } 
makeLenses ''Player

instance Show Player where
  show p = (p ^. name) ++ " : "  ++ show (p ^. dealPoints) ++ " rougeLeft=" ++ (show . size) (p ^. leftUntilCarteRouge) ++ " : "++ show (p ^. hand)

data DeclarationResponse = Good | NotGood | Equals deriving (Eq, Show)

data DeclarationWinner = Elder | Younger | Tie | Nobody deriving (Eq, Show)

data Game = Game { _dealNum             :: Deal
                 , _deck                :: Deck
                 , _visible             :: Deck
                 , _step                :: Step
                 , _player1             :: Player
                 , _player2             :: Player
                 , _isElderToPlay       :: Bool
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
         ++ "\nElder : " ++ (if game ^. player1 . isElder then "Player1" else "Player2")
         ++ "\nPoint winner : " ++ show (game ^. pointWinner)
         ++ "\nPlayer1 : " ++ show (game ^. player1)
         ++ "\nPlayer2 : " ++ show (game ^. player2)
         ++ "\n-----------------------------\n"

-- Additional Lenses accessors
elder :: Lens' Game Player
elder f g = if g ^. player1 . isElder then player1 f g else player2 f g

younger :: Lens' Game Player
younger f g = if g ^. player1 . isElder then player2 f g else player1 f g

getOpponentLens :: Lens' Game Player -> Lens' Game Player
getOpponentLens playerLens f g = if g ^. playerLens . isElder then elder f g else younger f g

getCombinationLens :: CombinationType ->  Lens' Game (Maybe Combination)
getCombinationLens Point    = pointCombination
getCombinationLens Sequence = sequenceCombination
getCombinationLens Set      = setCombination

getWinnerLens :: CombinationType ->  Lens' Game DeclarationWinner
getWinnerLens Point    = pointWinner
getWinnerLens Sequence = sequenceWinner
getWinnerLens Set      = setWinner

------- 

play :: Handle -> Handle -> IO () 
play p1Handle p2Handle = do
  game <- flip execStateT (mkInitialState p1Handle p2Handle) $ do
    elderIsPlayer1 <- lift (randomRIO (True, False)) 
    assign (player1 . isElder ) elderIsPlayer1
    assign (player2 . isElder ) (not elderIsPlayer1)
    replicateM_ 6 playDeal
    endGame
  return ()

playDeal :: GameAction
playDeal = step .= Start >> start
         >>                 showDealNum
         >> step %= succ >> deal
         >>                 showGame
         >> step %= succ >> changeElderCards
         >> step %= succ >> changeYoungerCards
         >>                 showGame
         >> step %= succ >> declarationElder Point
         >> step %= succ >> declarationElder Sequence
         >> step %= succ >> declarationElder Set
         >> step %= succ >> playNextCard -- elder play first card before younger declarations
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

mkInitialState p1Handle p2Handle = Game
  { _dealNum = One
  , _deck = sortedDeck
  , _visible = fromList []
  , _step = Start
  , _player1 = initialPlayer & name .~ "Roméo" & sockHandle .~ p1Handle
  , _player2 = initialPlayer & name .~ "Juliette" & sockHandle .~ p2Handle
  , _isElderToPlay = True
  , _pointWinner = Nobody
  , _pointCombination = Nothing
  , _sequenceWinner = Nobody
  , _sequenceCombination = Nothing
  , _setWinner = Nobody
  , _setCombination = Nothing
  }
  where initialPlayer = Player { _hand = noCards
                               , _isElder = False
                               , _leftUntilCarteRouge = noCards
                               , _cardPlayed = Nothing
                               , _dealPoints = 0
                               , _dealWons = 0
                               , _gamePoints = 0
                               , _points = 0
                               , _name = "undefined"
                               , _sockHandle = stderr
                               }

type GameAction = StateT Game IO ()

start :: GameAction
start =  pointWinner .= Nobody
      >> sequenceWinner .= Nobody
      >> setWinner .= Nobody
      >> deck .= sortedDeck
      >> shuffle

shuffle :: GameAction
shuffle = use deck >>= lift . shuffleIO >>= (deck .=)

deal :: GameAction
deal = do
  game <- get
  let (hands, stock) = drawHands (game ^. deck) 12 2 
  player1 . hand                .= hands !! 0
  player1 . leftUntilCarteRouge .= hands !! 0
  player2 . hand                .= hands !! 1
  player2 . leftUntilCarteRouge .= hands !! 1
  deck                          .= stock

changeElderCards :: GameAction
changeElderCards = changePlayerCards elder

changeYoungerCards :: GameAction
changeYoungerCards = changePlayerCards younger

changePlayerCards :: Lens' Game Player -> GameAction
changePlayerCards playerLens = do
  game <- get
  let pHand = game ^. playerLens . hand 
      pSock = game ^. playerLens . sockHandle
  lift $ hPutStrLn pSock $ (game ^. playerLens . name) ++ ", this is your hand : " ++ show pHand
  when (isCarteBlanche pHand) $ declareCarteBlanche playerLens 
  lift $ hPutStrLn pSock "Change cards : "
  stToChange <- lift (hGetLine pSock) 
  unless (null stToChange) $ do
    let idxToChange        = read <$> splitOn "," stToChange                  
        toChange           = fromList $ getCardsAtPos pHand idxToChange     
        (newHand, newDeck) =  changeCards (game ^. deck) pHand toChange
    deck                             .= newDeck
    playerLens . hand                .= newHand
    playerLens . leftUntilCarteRouge .= newHand
    lift $ hPutStrLn pSock $ "Your new hand : " ++ show newHand

declareCarteBlanche :: Lens' Game Player -> GameAction 
declareCarteBlanche playerLens = do
  game <- get
  let pHand = game ^. playerLens . hand 
      pSock = game ^. playerLens . sockHandle
  lift $ hPutStrLn pSock "Declare carte blanche (y/n) ?"
  resp <- lift (hGetLine pSock)
  when (resp == "y") $ do
    display $ "[] Carte Blanche : " ++ show pHand
    addDealPoints playerLens 10

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
  display $ "[Younger] " ++ show (fromMaybe Good (fst <$> maybeChoiceYounger))
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
      addDealPoints playerLens 20

addDealPoints :: Lens' Game Player -> Int -> GameAction
addDealPoints playerLens points = do
  playerLens . dealPoints %= (+ points)
  game <- get
  gameStep <- use step 
  pointsBefore <- use (playerLens . dealPoints) 
  opponentPoints <- use (getOpponentLens playerLens . dealPoints)
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
           addDealPoints (if won1 > won2 then player1 else player2) 10
           -- addDealPoints winnerLens 10

setCombinationPoints :: DeclarationWinner -> CombinationType -> GameAction
setCombinationPoints decPlayer combinationType = do
  game <- get
  when (game ^. getWinnerLens combinationType == decPlayer) $ do
    -- add points for winning combination
    let winnerLens dc = bool younger elder (dc == Elder)
    maybeWinComb <- use (getCombinationLens combinationType)
    addDealPoints (winnerLens decPlayer) ( maybe 0 getCombinationPoints maybeWinComb )
    checkCarteRouge (winnerLens decPlayer) maybeWinComb 
    unless (combinationType == Point) $ do
      -- ask for other smaller combinations of the same type
      let candidates = getSmallerCombinations maybeWinComb . getCombinations combinationType $ game ^. (winnerLens decPlayer) . hand
      when (candidates /= []) $ do
        lift $ hPutStrLn (game ^. (winnerLens decPlayer) . sockHandle) $ "-> " ++ show decPlayer ++ " other " ++ show combinationType ++ " combinations : " ++ show candidates
        strOthers <- lift getLine
        let others = (candidates !!) . read <$> splitOn "," strOthers
        sequence_ $ (display . (("[" ++ show decPlayer ++ "] ") ++) . showDeclarationComplete) <$> others -- show combination
        sequence_ $ (addDealPoints (winnerLens decPlayer) . getCombinationPoints) <$> others  -- add points 
        sequence_ $ checkCarteRouge (winnerLens decPlayer) . Just <$> others 

setCombinationElderPoints :: CombinationType -> GameAction
setCombinationElderPoints Point           = return ()
setCombinationElderPoints combinationType = do
  game <- get
  when (game ^. getWinnerLens combinationType == Elder) $ do
    -- add points for winning combination
    maybeWinComb <- use (getCombinationLens combinationType)
    addDealPoints elder ( maybe 0 getCombinationPoints maybeWinComb )
    checkCarteRouge elder maybeWinComb 
    -- ask for other smaller combinations of the same type
    let candidates = getSmallerCombinations maybeWinComb . getCombinations combinationType $ game ^. elder . hand
    when (candidates /= []) $ do
      lift $ hPutStrLn (game ^. elder . sockHandle) $ "-> Elder other " ++ show combinationType ++ " combinations : " ++ show candidates
      strOthers <- lift getLine
      let others = (candidates !!) . read <$> splitOn "," strOthers
      sequence_ $ (display . ("[Elder] " ++) . showDeclarationComplete) <$> others -- show combination
      sequence_ $ (addDealPoints elder . getCombinationPoints) <$> others  -- add points 
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

playCards :: GameAction
playCards = do
  playNextCard 
  game <- get
  -- let finished = length (game ^. player1 . hand) == 0 && length (game ^. player2 . hand) == 0 
  if game ^. step == PlayEnd 
     then checkPlayPoints
     else playCards

playNextCard :: GameAction
playNextCard = do
  let nextPlayer isElder = if isElder then elder else younger
  elderTurn <- use isElderToPlay
  pSock <- use (nextPlayer elderTurn . sockHandle)
  cards <- toList <$> use ( nextPlayer elderTurn . hand )
  lift $ hPutStrLn pSock $ "Choose a card to play : " ++ show cards
  choice <- lift $ hGetLine pSock
  let maybeChoice = (cards !!) <$> readMaybe choice
  case maybeChoice of
    Nothing -> do
      lift $ hPutStrLn pSock "Bad choice"
      playNextCard
    Just card -> playCard (nextPlayer elderTurn) card

playCard :: Lens' Game Player -> Card -> GameAction
playCard playerLens card = do
  game <- get
  when (game ^. isElderToPlay == (game ^. playerLens . isElder )) $ do
    playerLens . hand %= (\\ fromList [card])
    played <- use (getOpponentLens playerLens . cardPlayed)
    case played of
      Nothing -> do -- First to play in the turn
        playerLens . cardPlayed .= Just card
        use (playerLens . hand) >>= (show >>> hPutStrLn (game ^. playerLens . sockHandle) >>> lift)
        addDealPoints playerLens 1
        isElderToPlay %= not
      Just opponentCard -> do -- Second to play in the turn
        remaining <- use (playerLens . hand)
        let won = (suit card == suit opponentCard && rank card > rank opponentCard)
            lastTurn = length remaining == 0
        if won 
           then do
             addDealPoints playerLens 1 
             playerLens . dealWons %= (+1)
           else do
             getOpponentLens playerLens . dealWons %= (+1)
             isElderToPlay %= not -- opponent to play next turn
        if lastTurn 
           then do
             addDealPoints (if won then playerLens else getOpponentLens playerLens) 1 
             step .= PlayEnd
           else do
             playerLens . cardPlayed .= Nothing
             getOpponentLens playerLens . cardPlayed .= Nothing

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

