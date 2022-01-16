module CofreeBot.Bot.Magic8Ball where

import CofreeBot.Bot
import Data.Text (Text)
import System.Random 

magic8BallBot :: Bot IO s i Text
magic8BallBot = Bot $ \_ s ->
  flip BotAction s . (ballResponses !!) <$>
    randomRIO (0, length ballResponses)

ballResponses :: [Text]
ballResponses = ("*" <>) . (<> ".*") <$>
  [ "It is certain"
  , "It is decidedly so"
  , "Without a doubt"
  , "Yes definitely"
  , "You may rely on it"
  , "As I see it, yes"
  , "Most likely"
  , "Outlook good"
  , "Yes"
  , "Signs point to yes"

  , "Reply hazy, try again"
  , "Ask again later"
  , "Better not tell you now"
  , "Cannot predict now"
  , "Concentrate and ask again"

  , "Don't count on it"
  , "My reply is no"
  , "My sources say no"
  , "Outlook not so good"
  , "Very doubtful"
  ]