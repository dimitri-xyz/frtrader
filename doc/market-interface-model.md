## Strategy/Market Interface Model

A Strategy sees markets through an Event interface where the market outputs new events and an Action interface where it can place and cancel orders (inputs to the market). Markets are typed according to currencies traded in them

The events are:

```
data TradingEv
    = EvPlace
    | EvCancel
    | EvFills
    | EvBook
``` 

The Actions are: 

```
data Action
    = PlaceLimit
    | CancelLimit
```

**Actions are assumed to eventually succeed.** They need not execute immediately, but the exchange will eventually get the request. Hopefully, in a timely manner, but the is no hard guarantee of timeliness (best effort).

Strategies can place and cancel orders by using the corresponding Actions.

All Events and Actions that require or return an identifier for orders placed use a ClientOID rather than an exchange specific OrderID. The ClientOID is an opaque identifier outside the strategy (except for being `Hashable`, `Show` and `Eq`).


### Market Event Description

1. **EvPlace** - Notifies the strategy that an order has been placed in the market. At a minimum, specifies the ClientOID of the Action that succeeded.

2. **EvCancel** - Notifies the strategy that an order has been cancelled. At a minimum, specifies the ClientOID of the cancelled order. No more events for this order will occur. 

3. **EvFills** - Notifies the strategy that an order has been filled by one of more fills. Each fill provides at least: the volume executed, the price, the ClientOID of the order. Each event provides a (possibly empty) list of fills.

4. **EvBook** - Notifies the strategy that the market's orderboook has been updated. Although the updates may be incremental, the event returns the whole QuoteBook each time.

#### Event Sequencing

**EvBook**

`EvBook` provide no synchronization guarantee. These events may happen at any time and are not necessarily sincronized with our own order placements and cancellations. In orders words, an `EvBook` may show an order for which an `EvCancel` has already happened. It may also already show an order for which we have not yet received an `EvPlace`. 

(Ideally, `EvBook` should provide a "filtered" orderbook where all our own orders have been removed, but this is not and may never be impletemented due to exchange API limitations. Note that filtering by itself is *not* sufficient to avoid all the synchrony crazyness these events bring. The order of orderbooks may simply be flipped in time, no amount of filtering can make sense of that...)

**EvPlace, EvCancel and EvFills**

The framework provides the following guarantee of ordering:

`EvPlace` comes before any `EvFills` which can happen multiple times and which, in turn, all come before `EvCancel`.

That is:

1. No `EvFills` or `EvCancel` will happen before the corresponding `EvPlace`
2. The framework guarantees that all `EvFills` events that happened to a cancelled order will be sent to the strategy *before* the final `EvCancel `.

If an order is fully executed `EvCancel` will never happen. It is the strategies' job to keep track of all fills if it needs to know when an order has been fully executed.

Because ClientOIDs are opaque, **the strategy will only receive events corresponding to orders for which the framework received a ClientOID**. The framework receives notifications from the exchange in terms of OrderIDs and it must know which OrderID relates to which ClientOID to be able to generate events for the strategy. This means, that if no corresponding ClientOID is found for a given OerderID, the notification from the exchange is discarded and no event generated.


### Market Action Description

1. **PlaceLimit** - Requests that a limit order be placed on the market. At a minimum, specifies a ClientOID, price and volume.
2. **CancelLimit** - Requests that an order be cancelled. At a minimum, specifies the ClientOID.

#### Action Sequencing

The strategy is free to place actions in any order. However, if the strategy requests cancellation of an order for a ClientOID that the framework doesn't yet know, the results are undefined.

Also, if a strategy repeats the same ClientOID in multiple `PlaceLimit` requests, event reporting may not report all events or generate bogus data.


