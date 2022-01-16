module CofreeBot.Bot.CoinFlip where

import CofreeBot.Bot
import CofreeBot.Bot.Simple
import CofreeBot.Parser
import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import System.Random 
import Data.Bifunctor (bimap)

coinFlipBot :: Bot IO s i Text
coinFlipBot = Bot $ \_ s -> do
  result <- T.pack . show <$> randomIO @Bool
  pure $ BotAction ("Coin Flip Result: " <> result) s

