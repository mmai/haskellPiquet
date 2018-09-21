{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.ByteString hiding (putStrLn, pack)
import           Data.ByteString.Lazy (toStrict)
import           Data.Text           (Text, pack, unpack)
import           Data.Text.Encoding  (decodeUtf8)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Data.Set.Ordered
import           Data.Aeson 
import           Data.Aeson.Text
import qualified Network.WebSockets  as WS

import Protocol
import CmdLineParser
import Cards

--------------------------------------------------------------------
-- Base WebSocket helpers

-- receiveJSON :: (FromJSON j, MonadIO m) => WS.Connection -> m (Either String j)
-- receiveJSON = liftIO . fmap eitherDecode . WS.receiveData
--
-- sendJSON :: (ToJSON j, MonadIO m) => WS.Connection -> j -> m ()
-- sendJSON conn = liftIO . WS.sendTextData conn . encode
--------------------------------------------------------------------

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn $ either pack displayState $ eitherDecode msg

    -- Read from stdin and write to WS
    let loop = do
          line <- T.getLine
          unless (T.null line) $
            case makeMsg line of 
              Left errMsg -> putStrLn $ "No comprendo" <> errMsg 
              Right msg   -> WS.sendTextData conn ( encode msg )
          loop

    loop
    WS.sendClose conn ("Bye!" :: Text)

--------------------------------------------------------------------------------

displayState :: GameStateMsg -> Text
displayState (Left e) = pack $ show e
displayState (Right (game, player:_)) = pack infos where
  infos =  "Game State: " <> show (viewStep game)
        <> "\nYou play has " <> playerName player 
        <> " (" <> show (playerPoints player) <> " points)"
        <> if show (viewStep game) == "Start"
              then "\nWaiting for an opponent to connect..."
              else 
                "\nYour hand is " <> show (playerHand player) <>
                if playerIsActive player 
                        then "\nWaiting for your move... "
                        else "\nWaiting for your opponent to play..." 

--------------------------------------------------------------------------------
main :: IO ()
-- main = withSocketsDo $ WS.runClient "echo.websocket.org" 80 "/" app
main = withSocketsDo $ WS.runClient "localhost" 8000 "/" app

-- playFirstCard :: GameAction
-- playFirstCard = use activePlayer >>= lift . chooseCard >>= playCardAction

-- playCards :: GameAction
-- playCards = do
--   use activePlayer >>= lift . chooseCard >>= playCardAction
--   use step <&> ( == PlayEnd ) >>= bool playCards checkPlayPoints

-- chooseCard :: Player -> IO Card
-- chooseCard player = do
--   let pSock = player ^. sockHandle
--       cards = toList ( player ^. hand )
--   hPutStrLn pSock $ "Choose a card to play : " ++ show cards
--   choice <- liftM (filter (/= '\r')) $ hGetLine pSock
--   -- let maybeChoice = join $ (cards ^?) <$> ix <$> readMaybe choice
--   let maybeChoice = join $ (cards ^?) <$> element <$> readMaybe choice
--   case maybeChoice of
--     Nothing -> do
--       hPutStrLn pSock "Bad choice"
--       chooseCard player
--     Just card -> return card

-- playCardAction :: Card -> GameAction
-- playCardAction card = do
--   aHand <- use $ activePlayer . hand
--   when ( card `member` aHand ) $ do
--     game <- get
--     pSock <- use $ activePlayer . sockHandle
--     display $ "played = " ++ show card
--     activePlayer . hand %= delete card
--     case (game ^. inactivePlayer . cardPlayed) of
--       Nothing -> do -- First to play in the turn
--         activePlayer . cardPlayed .= Just card
--         addDealPointsAction activePlayer 1
--         isElderToPlay %= not
--       Just opponentCard -> do -- Second to play in the turn
--         let activePlayerWon = (suit card == suit opponentCard && rank card > rank opponentCard)
--         when activePlayerWon $ addDealPointsAction activePlayer 1 
--         if activePlayerWon then finishTurn activePlayer inactivePlayer else finishTurn inactivePlayer activePlayer 
--         when (length (game ^. activePlayer . hand) == 0) $ do -- this is the last turn 
--            addDealPointsAction (if activePlayerWon then activePlayer else inactivePlayer) 1 -- an additional point for the winner
--            step .= PlayEnd
--         activePlayer   . cardPlayed .= Nothing
--         inactivePlayer . cardPlayed .= Nothing
--
-- finishTurn :: Lens' Game Player -> Lens' Game Player -> GameAction
-- finishTurn winnerLens looserLens = do
--   game <- get
--   lift $ hPutStrLn (game ^. winnerLens . sockHandle ) " you won "
--   lift $ hPutStrLn (game ^. looserLens . sockHandle ) " you lost "
--   winnerLens . dealWons %= (+1)
--   isElderToPlay .= (game ^. winnerLens . isElder) 

-- XXX to use in Client
-- getResponseChoices :: Maybe Combination -> [Combination] -> [(DeclarationResponse, Maybe Combination)]
-- getResponseChoices Nothing [] = []
-- getResponseChoices Nothing combs = (Equals, Nothing) : ((NotGood, ) . Just <$> combs)
-- getResponseChoices (Just elderCombination) combs = 
--   let ecSize = length (cards elderCombination) in
--     [ (Good, Nothing) ] 
--     ++ (((Equals, ) . Just) <$> filter (\c -> length (cards c) == ecSize ) combs)
--     ++ (((NotGood, ) . Just) <$> filter (\c -> length (cards c) > ecSize ) combs)  


-- setCombinationPointsAction :: DeclarationWinner -> CombinationType -> GameAction
-- setCombinationPointsAction decPlayer combinationType = do
--   game <- get
--   when (game ^. getWinnerLens combinationType == decPlayer) $ do
--     -- add points for winning combination
--     let winnerLens dc = bool younger elder (dc == Elder)
--     maybeWinComb <- use (getCombinationLens combinationType)
--     addDealPointsAction (winnerLens decPlayer) ( maybe 0 getCombinationPoints maybeWinComb )
--     checkCarteRouge (winnerLens decPlayer) maybeWinComb 
--     unless (combinationType == Point) $ do
--       -- ask for other smaller combinations of the same type
--       let candidates = getSmallerCombinations maybeWinComb . getCombinations combinationType $ game ^. (winnerLens decPlayer) . hand
--       when (candidates /= []) $ do
--         let winnerSock = (game ^. (winnerLens decPlayer) . sockHandle)
--         lift $ hPutStrLn winnerSock $ "-> " ++ show decPlayer ++ " other " ++ show combinationType ++ " combinations : " ++ show candidates
--         strOthers <- lift $ liftM (filter (/= '\r')) $ hGetLine winnerSock
--         let others = (candidates !!) . read <$> splitOn "," strOthers
--         sequence_ $ (display . (("[" ++ show decPlayer ++ "] ") ++) . showDeclarationComplete) <$> others -- show combination
--         sequence_ $ (addDealPointsAction (winnerLens decPlayer) . getCombinationPoints) <$> others  -- add points 
--         sequence_ $ checkCarteRouge (winnerLens decPlayer) . Just <$> others 

