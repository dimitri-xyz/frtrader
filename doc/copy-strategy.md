## Book Mirroring Strategy

The current implementation of the book mirroring strategy makes a few assumptions:

1. All requests will eventually succeed - This is a big assumption, but it enables us only to track the requests rather than the actual placements/cancellations at the exchange. This will like be refined later.
2. Mirroring is done per price-level not per-order - This just makes it simpler
3. We can keep track of orders per ClientOid rather than by the exchange's actual OrderID. The framework is responsible for matching the two identifiers.
4. All memory is kept in the internal "State" and this state can change due to orderbook events or executions at the "destination" exchange.


### Action Life-Cycle

Once requested by the strategy, an Action is considered "open", even if it has not yet been requested to the exchange. Cancellation Actions may fail (This may cause cancellation failure events, a new even type, not sure yet).


### Rationale

The orderbook mirroring will be defined "by price region" of the source orderbook. That is, each price region needs to be mirrored. We will take a list of price regions as input. For each price region, we need to produce a corresponding "target price-level" and "target volume" for the receiving book.

A price region can be arbitrarily defined and there may be multiple per source orderbook. To define a price region we need to provide a function that says if the price region exist in the current orderbook and, if so, what are its price and volume targets. In short, a price region is a function with type Event( OrderBook ) -> Maybe (Price Target, Volume Target)

The matching between "the orders we currently have open" and "the target" is done by price-level. In other words, for each price, we see if the volume in open actions (i.e. open orders) matches the current volume target.

Once we define a target (price, volume) pair, we can just look at the requests we currently have open and adjust our orders to match the new target. We only adjust volumes. Changes in price are seen as the deletion of an old price-level and the creation of a new one.

The disappearance of a price region or a change in price may leave orders in a price-level "orphaned". Those orders should be cancelled.

The is no structure to the relationship (i.e. it's a simple relation, not a function) between the target (price, volume) pairs and the current (price-level, volume) pairs we have open through requested orders. Targets may disappear and order may be executed or nothing may happen. Each time we strive to maintain a 1-to-1 correspondence, but this will be challenged (and turned from a bijection into a relation) at every new orderbook event and every new order execution.

### Price-level Matching

Matching is per "price-region", this is additive on targets. In other words, two targets with a volume of 1.0 and 0.5 at the same price-level can be combined into a single target of 1.5. If I perform this first, the matching "per price-level" will be easier. This is because the HashMap data structure can be used to perform a set difference or intersection. (This is set difference)

Notice that this is distinct from order placement. I cannot combine two StrategyAdvice placement actions into a single order because each orther would have a distinct client_oid and then I would not be able to only cancel part of an order when given that client_oid in a cancellation request. But this is different from the targets (and the targets are going to be recalculated at each QuoteBook event anyway).





