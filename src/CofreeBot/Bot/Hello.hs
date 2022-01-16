-- | The Simplest Bot
module CofreeBot.Bot.Hello where

import CofreeBot.Bot
import CofreeBot.Bot.Matrix
import CofreeBot.Bot.Simple

helloSimpleBot :: Applicative m => TextBot m s
helloSimpleBot = pureStatelessBot $ \_ ->
  pure "Are you talking to me, punk?"

helloMatrixBot :: Applicative m => MatrixBot m ()
helloMatrixBot = liftSimpleBot $ helloSimpleBot
