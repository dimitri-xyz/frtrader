## Book Mirroring Strategy

The current implementation of the book mirroring strategy makes a few assumptions:

1. All requests will eventually succeed - This is a big assumption, but it enables us only to track the requests rather than the actual placements/cancellations at the exchange. This will likely be refined later.
2. Mirroring is done by defining a few (side, price, vol) targets to be placed in the destination (least liquid) exchange.
3. We can keep track of orders per ClientOid rather than by the exchange's actual OrderID. The framework is responsible for matching the two identifiers. This enables strategies to be deterministic (although stateful).
4. All memory is kept in the internal "State" and this state can change due to orderbook events or executions at the exchanges. See the powerpoint presentation for more details.


### Action Life-Cycle

Once requested by the strategy, an Action is considered "open", even if it has not yet been requested to the exchange. Cancellation Actions only ever attempt the cancellation, they do not guarantee it, so they never "fail".


### Rationale

The orderbook mirroring will be defined by a "target definition function" of the source orderbook and current exposure levels. For each orderbook event, we need to produce a corresponding list of target "price-levels" and "volumes" for the receiving book. The targets can be arbitrarily defined and there may be multiple per source orderbook.


### Price-level Matching


The matching between "the orders we currently have open" and "the target" is done by price-level. In other words, for each price, we see if the volume in open actions (i.e. open orders) matches the current volume target.

Once we define a target (side, price, volume) triple, we can just look at the requests we currently have open and adjust our orders to match the new target. We only adjust volumes. Changes in price are seen as the deletion of an old price-level and the creation of a new one.

The disappearance of a target or a change in price may leave orders in a price-level "orphaned". Those orders should be cancelled.

The is no structure to the relationship (i.e. it's a simple relation, not a function) between the target (side, price, volume) triple and the current (side, price-level, volume) pairs we have open through requested orders. Targets may disappear and order may be executed or nothing may happen. Each time we strive to maintain a 1-to-1 correspondence, but this will be challenged (and turned from a bijection into a relation) at every new orderbook event and every new order execution.

Matching is per "price-level", this is additive on targets. In other words, two targets with a volume of 1.0 and 0.5 at the same price-level can be combined into a single target of 1.5. If I perform this first, the matching "per price-level" will be easier. This is because the HashMap data structure can be used to perform set difference or intersection. (This is set difference)

Notice that this is distinct from order placement. I cannot combine two StrategyAdvice placement actions into a single order because each orther would have a distinct client_oid and then I would not be able to only cancel part of an order when given that client_oid in a cancellation request. But this is different from the targets (and the targets are going to be recalculated at each QuoteBook event anyway).

#### Update 2019-01-17

### Exposure Control

Turns out, the pair or strategies copyBook/refill as originally designed is not sufficient. The refill strategy is implementing an immediate state update, which will cause the following bug then described in "Strategy.hs"

> {-
> FIX ME!
> There's a synchronization bug on this strategy "as is". When an order fill is detected. The open order state is immediately updated,
> but we may still have to re-fill the account balances. If, in this interval, another orderbook event is issued, the
> strategy will ask for the amount sold to be put on sale *again* even though we have not yet re-filled it.
> To avoid this, we need to somehow keep track of the balances that are pending refills (and take these into consideration 
> before asking for more placements).
> -}

We need to also keep track of "realized exposure". Once one of our orders is executed in the mirrored book, it disappears from open actions (as it should be), but we still have its exposure until it gets refilled.

Unfortunately, keeping track of exposure on a "per price-level" sense is very complicated, because we need to keep track of which refills have cancelled which exposure. This seems like over engineering! At the same time, we will soon have to deploy strategies (for new tokens) that will have multiple price targets, so it doesn't make sense to completely rip out the current multiple target mechanism. I have decided to go for a compromise:

1. We only keep track of a single global exposure value for now. All targets will share this exposure. This eliminates the need for tracking which refills eliminated whose exposure.
2. One QuoteBook event can fire multiple targets. The targets must all know about it each other to collaboratively control exposure, but multiple targets can be issued.

This greatly simplifies the refill strategy and the new "exposure control" strategy, while keeping the current multiple target dispatch mechanism that we will need very soon (it's the next strategy we will offer).




