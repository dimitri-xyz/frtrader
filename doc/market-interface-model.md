## Strategy/Market Interface Model

A Strategy sees markets through an Event interface where the market outputs new events and an Action interface where it can place and cancel orders (inputs to the market). Markets are typed according to currencies traded in them

The events are:

```
data TradingEv
    = PlaceEv
    | CancelEv
    | DoneEv
    | FillsEv
    | BookEv
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

In fact, the mapping the framework keeps is from `Maybe (ClientOID)` to `OrderID` so that `Nothing` is a valid identifier. The `Nothing` identifier represents any number of orders that have been placed and for which events will be tracked, but for which we do not care about the `ClientOID`.


### Market Event Description

1. **PlaceEv** - Notifies the strategy that an order has been placed in the market. At a minimum, specifies the `Maybe ClientOID` of the Action that succeeded.

2. **CancelEv** - Notifies the strategy that an order has been cancelled. At a minimum, specifies the `Maybe ClientOID` of the cancelled order. No more events for this order will occur. 

3. **DoneEv** - Notifies the strategy that an order has been completely executed and is now closed. At a minimum, specifies the `Maybe ClientOID` of the executed order. No more events for this order will occur. DoneEv and CancelEv events are mutually exclusive and cannot both happen for the same order.

4. **FillsEv** - Notifies the strategy that an order has been filled by one of more fills. Each fill provides at least: the volume executed, the price, the `Maybe ClientOID` and the side of the order that got filled. Each event provides a (possibly empty) list of fills.

5. **BookEv** - Notifies the strategy that the market's orderboook has been updated. Although the updates may be incremental, the event returns the whole QuoteBook each time.

#### Event Sequencing

**BookEv**

`BookEv` provide no synchronization guarantee. These events may happen at any time and are not necessarily sincronized with our own order placements and cancellations. In orders words, an `BookEv` may show an order for which an `CancelEv` has already happened. It may also already show an order for which we have not yet received an `PlaceEv`. 

(Ideally, `BookEv` should provide a "filtered" orderbook where all our own orders have been removed, but this is not and may never be impletemented due to exchange API limitations. Note that filtering by itself is *not* sufficient to avoid all the synchrony crazyness these events bring. The order of orderbooks may simply be flipped in time, no amount of filtering can make sense of that...)

**PlaceEv, CancelEv, DoneEv and FillsEv**

The framework provides the following guarantee of ordering:

`PlaceEv` comes before any `FillsEv` which can happen multiple times and which, in turn, all come before either a `CancelEv` or a `DoneEv`.

That is:

1. No `FillsEv`, `CancelEv` or `DoneEv` will happen before the corresponding `PlaceEv`
2. The framework guarantees that all `FillsEv` events that happened to a cancelled order will be sent to the strategy *before* the final `CancelEv`.
3. Similarly, after all `FillsEv` for a fully executed order are sent, the strategy will *eventually* get a `DoneEv`. (There's not timeliness guaranteed here, the `DoneEv` may be delayed for hours)

If an order is fully executed `CancelEv` will never happen.
If an order is    cancelled    `DoneEv`  will never happen.

There is one exception to guarantee (2) above for orders created with a `Maybe ClientOID` assigned to `Nothing`. Because we can assign multiple orders to this value, it is possible to receive multiple `CancelEv` and later still continue to received `FillsEv` for this `Maybe ClientOID` value, as not all orders assigned this `Maybe ClientOID` may have yet been cancelled.

The corresponding exception to guarantee (3) also applies to orders created with a `Maybe ClientOID` assigned to `Nothing`. The client may receive multiple `DoneEv` and continue to receive `FillsEv` for this `Maybe ClientOID` value.

Because ClientOIDs are opaque, **the strategy will only receive events corresponding to orders for which the framework received a `Maybe ClientOID`**. `Nothing` counts as a valid ClientOID in this context. The framework receives notifications from the exchange in terms of OrderIDs and it must know which OrderID relates to which ClientOID to be able to generate events for the strategy. This means, that if no corresponding `Maybe ClientOID` is found (not even `Nothing`) for a given OrderID, the notification from the exchange is discarded and no event generated.

The framework will discard the mapping between `Maybe ClientOID`s and the exhcanges own `OrderID`s once it dispatches a `CancelEv` or an `DoneEv`. So, all cancellation requests made to those `ClientOID` will de ignored from then on.


**Seemingly Inconsistent Events**

The framework tries to present to trading strategies a view of the world that makes sense, but this only goes as far as the guarantees mentioned above. Otherwise, the strategy needs to do its best to make sense of the information provided by the exchanges. There are many circumstances that will seem weird.

For example: The frameworks makes no assurance that every single change to the orderbook will be shown to the trading strategy or that those will be shown in order. If the strategy places an order, but the order only executes at a much worse price than expected, the framework will not generate orderbook events to try to justify the difference in price for the executed order. The strategy just has to figure out by itself that the market slipped before its order was executed.


### Market Action Description

1. **PlaceLimit** - Requests that a limit order be placed on the market. At a minimum, specifies a `Maybe ClientOID`, side, price and volume.
2. **CancelLimit** - Requests that an order be cancelled. At a minimum, specifies the `ClientOID` (`Nothing` is currently *not* acceptable here).

#### Action Sequencing

The strategy is free to place actions in any order. However, if the strategy requests cancellation of an order for a ClientOID that the framework doesn't yet know, the results are undefined.

A strategy can repeat `Nothing` as a `Maybe ClientOID` as much as it wants, but if a strategy repeats the same ClientOID for a `Just` value in multiple `PlaceLimit` requests, the results are undefined.

(UPDATE 2019-01-29: I decided to add the `DoneEv` because either framework had to keep track of how much was executed in each order and then delete 
orderID <-> ClientOID mappings once the orders were fully executed or it had to be informed the order was done by somebody. If we didn't track this, we would have a memory leak due to keeping track of mappings for fully executed orders forever. So, either the strategy had to keep track of this and tell the framework - which seems backwards - or the framework could do it itself. If the framework is going to keep track of it, then it might as well provide this event, so that the strategy doesn't have to do it also! In short, someone has to do it, so it is best if the framework does it and frees up the strategy. Furthermore, for other exchanges this may be provided without any work.)

