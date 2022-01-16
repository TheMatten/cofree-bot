module Main where

import CofreeBot
import CofreeBot.Bot.Calculator.Language
import Data.Profunctor
import Data.Text qualified as T
import Network.Matrix.Client
import Options.Applicative qualified as Opt
import OptionsParser
import System.Environment.XDG.BaseDir ( getUserCacheDir )
{-
*This is a very scrappy rough draft*

TODO:
- [ ] Handle Full RoomEvents
- [ ] Automated Build and Deploy to server
- [ ] Test suite
- [ ] Administrative interface (via private message?)
- [ ] Command to list all sessions
- [ ] Add fixed point of Bot
- [ ] Bot should auto join DMs
- [ ] Debug Mode Flag where bot gets a special session for all behaviors
-}

main :: IO ()
main = do
  command <- Opt.execParser parserInfo
  xdgCache <- getUserCacheDir "cofree-bot"
  let name = "cofree-bot"
      mentionBot = liftSimpleBot $ Bot $ \i s -> if name `T.isInfixOf` i
        then runBot bot i s
        else pure $ BotAction [] s
  case command of
    LoginCmd cred -> do
      session <- login cred
      runMatrixBot session xdgCache mentionBot mempty
    TokenCmd TokenCredentials{..} -> do
      session <- createSession (getMatrixServer matrixServer) matrixToken
      runMatrixBot session xdgCache mentionBot mempty
    ReplCmd -> runSimpleBot bot mempty

bot :: TextBot IO CalcState
bot = textBotCase
  [ "hello"       :-|> helloSimpleBot
  , "flip a coin" :-|> rmap pure coinFlipBot
  , "🎱"          :-|> rmap pure magic8BallBot
  , programP      :-|> rmap printCalcOutput calculatorBot
  ]