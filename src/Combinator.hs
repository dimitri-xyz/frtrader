module Combinator where

import Reactive.Banana
import Reactive.Banana.Frameworks

-- | Outputs only the first ocurrence of the event.
headE :: MonadMoment m => Event a -> m (Event a)
headE event = do
  notDoneYet <- stepper True (const False <$> event)
  return $ whenE notDoneYet event

-- | Outputs input value only once, upon the first event seen.
once :: MonadMoment m => a -> Event b -> m (Event a)
-- should this be onceE ? What's the naming convention?
once val event = do
  e1 <- headE event
  return (const val <$> e1)
