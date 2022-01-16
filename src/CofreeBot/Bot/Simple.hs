module CofreeBot.Bot.Simple where

import CofreeBot.Bot ( BotAction(..), Bot(..) )
import CofreeBot.Parser
import Data.Foldable ( traverse_ )
import Data.Text ( Text )
import Data.Text qualified as T
import System.IO ( stdout, hFlush )

-- | A 'SimpleBot' maps from 'Text' to '[Text]'. Lifting into a
-- 'SimpleBot' is useful for locally debugging another bot.
type TextBot m s = Bot m s Text [Text]

-- | An evaluator for running 'SimpleBots' in 'IO'
runSimpleBot :: forall s. TextBot IO s -> s -> IO ()
runSimpleBot bot = go
  where
  go :: s -> IO ()
  go state = do
    putStr "<<< "
    hFlush stdout
    input <- getLine
    BotAction {..} <- runBot bot (T.pack input) state
    traverse_ (putStrLn . T.unpack . (">>> " <>)) responses
    go nextState

infixr 0 :-|>
data TextBotCase s where
  (:-|>) :: Parser i -> Bot IO s i [Text] -> TextBotCase s

textBotCase :: [TextBotCase s] -> TextBot IO s
-- textBotCase [] p = Bot $ \i s -> case parse p i of
--   Left msg -> 
-- textBotCase [p :-|> b] = Bot $ \i s -> case parse p i of
--   Left{}   -> pure $ BotAction [] s
--   Right i' -> runBot b i' s
-- textBotCase ((p :-|> b):cs) = Bot $ \i s -> case parse p i of
--   Left{}   -> runBot (textBotCase cs) i s
--   Right i' -> runBot b i' s
textBotCase [] = Bot $ \_ s -> pure $ BotAction [] s
textBotCase (x:xs) = Bot $ \i s -> case parse (go x xs) i of
  Left msg -> pure $ BotAction [T.pack msg] s
  Right b  -> b s
 where
  go (p :-|> b) (c:cs) = try (runBot b <$> p) <|> go c cs
  go (p :-|> b) []     =      runBot b <$> p
