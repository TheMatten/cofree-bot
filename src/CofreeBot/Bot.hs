module CofreeBot.Bot where

import Control.Arrow qualified as Arrow
import Control.Category qualified as Cat
import Data.Bifunctor
import Data.Profunctor
import Data.Text qualified as T
import Network.Matrix.Client (RoomEvent)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map

data BotAction s o = BotAction { responses :: o, nextState :: s }
  deriving (Functor)

instance Bifunctor BotAction where
  bimap f g (BotAction a b) = BotAction (g a) (f b)

-- | A 'Bot' maps from some input type 'i' and a state 's' to an
-- output type 'o' and a state 's'
newtype Bot m s i o = Bot { runBot :: i -> s -> m (BotAction s o) }

invmapBot :: Functor m => (s -> s') -> (s' -> s) -> Bot m s i o -> Bot m s' i o
invmapBot f g (Bot b) = Bot $ \i s -> (b i (g s)) <&> bimap f id

instance Monad m => Cat.Category (Bot m s) where
  id = Bot $ \a s -> pure $ BotAction a s

  Bot f . Bot g = Bot $ \a s -> do
    BotAction b s' <- g a s
    f b s'

instance Monad m => Arrow.Arrow (Bot m s) where
  arr f = rmap f (Cat.id)
  first = first'

instance Functor f => Profunctor (Bot f s) where
  dimap f g (Bot bot) = Bot $ \a -> fmap (fmap g) . bot (f a)

instance Functor f => Strong (Bot f s) where
  first' (Bot bot) = Bot $ \(a, c) -> fmap (fmap (,c)) . bot a

--instance Functor f => Closed (Bot f s) where
--  closed (Bot bot) = Bot $ \g s -> fmap (fmap _) $ bot _ s

--instance Functor f => Costrong (Bot f s) where
--  unfirst (Bot bot) = Bot $ \a s -> fmap (fmap fst) $ bot _ s

--instance Functor f => Choice (Bot f s) where
--  left' :: Bot f s a b -> Bot f s (Either a c) (Either b c)
--  left' (Bot bot) = Bot $ \case
--    Left a -> \s -> fmap (fmap Left) $ bot a s
--    Right c -> \s -> fmap (fmap Left) $ bot _ s

--instance Functor f => Cochoice (Bot f s) where
--  unleft :: Bot f s (Either a d) (Either b d) -> Bot f s a b
--  unleft (Bot bot) = Bot $ \a s -> fmap (fmap _) $ bot (Left a) s

-- | A 'MatrixBot' maps from 'RoomEvent' to '[RoomEvent]'
type MatrixBot s = Bot IO s RoomEvent [RoomEvent]

-- | A 'SimpleBot' maps from 'Text' to '[Text]'
type SimpleBot s = Bot IO s T.Text [T.Text]

-- | We can use 'dimap' to convert from one 'Bot' type to
-- another. This allows us to construct complex buts from simpler
-- bots. For example we can factor out the particularities of
-- 'Network.Matrix.Client' when constructing a bot.
simpleBotToMatrixBot :: (RoomEvent -> T.Text) -> (T.Text -> RoomEvent) -> SimpleBot s -> MatrixBot s
simpleBotToMatrixBot to from = dimap to (fmap from)

--TODO: For Mapping Simple Bots to Matrix Bots
-- parseRoomEvent :: RoomEvent -> Either ParseError Program
-- parseRoomEvent roomEvent =
--   let t = roomEvent ^. _reContent . _EventRoomMessage . _RoomMessageText . _mtBody
--   in _ t
-- printResponses :: Either CalcError [CalcResp] -> [Event]
-- printResponses = \case
--   Left err ->
--     let msgTxt = (MessageText (T.pack $ show err) TextType Nothing Nothing)
--         event = EventRoomMessage $ RoomMessageText msgTxt
--     in pure $ event
--   Right resps -> resps <&> \(Log expr n) ->
--     let txt = T.pack $ show expr <> " = " <> show n
--         msgTxt = (MessageText txt TextType Nothing Nothing)
--     in EventRoomMessage $ RoomMessageText msgTxt

data SessionState s = SessionState { sessions :: Map.Map Int s }

type SessionBot m s i o = Bot m (SessionState s) (Int, i) o

-- | Lift a 'Bot' into a 'SessionBot'.
mkSessionBot :: (Monoid s, Monad m) => Bot m s i o -> Bot m (SessionState s) (Int, i) (Int, o)
mkSessionBot (Bot bot) = Bot $ \(k, i) states -> do
  let state = findOrCreateSession states k
  BotAction {..} <- bot i state
  pure $ BotAction { responses = (k, responses), nextState = SessionState $ Map.insert k nextState (sessions states) }

findOrCreateSession :: Monoid s => SessionState s -> Int -> s
findOrCreateSession states k =
  case findSession states k of
    Just s -> s
    Nothing -> mempty

findSession :: SessionState s -> Int -> Maybe s
findSession (SessionState states) k = Map.lookup k states
