module CofreeBot.Bot.Calculator where

import CofreeBot.Bot
import CofreeBot.Bot.Calculator.Language
import CofreeBot.Utils
import Data.Profunctor
import Data.Text qualified as T
import Data.Functor

type CalculatorOutput = Either CalcError [CalcResp]
type CalculatorBot = Bot IO CalcState Program CalculatorOutput

calculatorBot :: CalculatorBot
calculatorBot = Bot $ \program state ->
  fmap (uncurry BotAction) $ interpretProgram program state

printCalcOutput :: Either CalcError [CalcResp] -> [T.Text]
printCalcOutput = \case
  Left err -> pure $ T.pack $ show err
  Right resps -> resps <&> \case
    Log e n -> T.pack $ show e <> " = " <> show n
