## Mirror Strategy Specification

This document formalizes the market mirroring strategy. The sequential nature of constantly copying orders from one order book to another poses a hard synchronization problem. Lots of pernicious bugs revealed themselves during development of this strategy. The large number of these bugs and their pernicious nature led me to believe that a more formal approach was required. This document has been crafted to address that need.

### Concepts

The market mirroring strategy (MMS) attempts to transfer liquidity from one order book (the source) to another (the destination). It does so by placing orders on the destination market that "mirror" the orders on the source market. 

Once one of its orders is executed, the MMS attempts to "recoup" the items sold by buying it on the source market. There is an implicit assumption that this will usually be possible. In other words, that if we sell some bitcoin, we will be able to buy it back. This is not always the case, this leads us to the concept of exposure. In layman's terms, the strategy's exposure is the volume of assets the strategy has sold or is currently offering the destination market, that it may not be able to recoup in a worst-case scenario.

As defined in the market/strategy interface. The following events may happen:

```
data TradingEv
    = PlaceEv
    | CancelEv
    | DoneEv
    | FillsEv
    | BookEv
``` 

These can happen in either the source or destination markets. `PlaceEv`, `CancelEv`, `DoneEv` and `FillsEv` are only for the strategy's own orders (events for other orders are never seen).

Similarly, the strategy can do the following:

```
data Action
    = PlaceLimit
    | CancelLimit
```

The following are defined at all times:

M    = maximum allowed exposure (assumed constant for the whole execution)
E(t) = current exposure

B(t)      = current orderbook
T( B(t) ) = (side, price, volume) target for current orderbook (for now, assuming only 1 target at any one time)

O(t) = set of open orders in destination exchange at time t 
F(t) = set of executed fills in destination exchange at time t
R(t) = set of open `Action`s at destination exchange at time t

V(O(t)) = overall open volume in orders at the exchange at time t (assuming one-sided operation only)

Open orders are "active" orders. In other words, orders that have been placed, but are neither fully executed or yet cancelled.

### Request Bounds

There is a difference between when a `Action` request is made and when it is actually processed. For example, there is a difference between asking to place an order through `PlaceLimit` and actually having this order placed at the exchange. However, we can bound when the actual `Action` occurs:

- `PlaceLimit`  occurs after the request is created but before (or on) the corresponding `PlaceEv` is received
- `CancelLimit` occurs after the request is created but before (or on) either a corresponding `CancelEv` or `DoneEv` is received. If a `CancelEv` is received, the cancellation succeeded. If a `DoneEv` was received, the cancellation was too late and the order fully executed.

In any case, we can bound the ocurrence of the requested actions by monitoring the corresponding requests and events. We say a `PlaceLimit` request is "open" from the time it is given to the executor (i.e. the time the reactive-banana framework passes it to reactimate) to the time we receive the corresponding closing event (either `CancelEv` or `DoneEv`).

This means that for any open order in O(t) there is a corresponding request in the set of open requests R(t). The R(t) can be seen as a super set of O(t) because it may also include orders that have not yet been placed or that have already executed or cancelled but we don't yet know about it (as well as the cancellation requests themselves).

Diagram 1

The fact that there will always exist an element of R(t) for each element of O(t) means that we can keep an upper bound on the current open volume for each order at the exchange by keeping track of corresponding `FillsEv` and `CancelEv` events. Reducing the current volume in the Request accordingly each time one of these events happens. (`DoneEv`s help but are not necessary here, as all fills will be seen)

Diagram 2

We need to remove the corresponding OpenAction when either a `CancelEv` or a `DoneEv` is received as there will be no further notification.

Diagram 3

The framework will ignore `CancelLimit` requests for an order that has already been cancelled or fully executed (i.e. for which a `CancelEv` or `DoneEv` has been received). 

Because we can upper bound the open volume for all open orders, we can also keep an upper bound on the overall open volume V(O(t)). Let's call this bound V(R(t)). So that V(R(t)) ≥ V(O(t)) for all t.

Summarizing:

- the number of open `PlaceLimit` actions in R(t) is an upper bound on the number of open orders at the exchange
- the volume of open actions V(R(t))              is an upper bound on the volume of open orders at the exchange V(O(t))


### Tracking Cancellation Requests

Cancellation requests (`CancelLimit`) are idempotent. So, we will only track whether one has been placed or not for each known `PlaceLimit` request in R(t).
As noted above, the set of open requests R(t) will include at least one OpenAction for each open order at the exchange. So, we just need an extra bit of information for each `PlaceLimit` request to be able to track whether a corresponding cancellation request has been placed.

Why do we need to track `CancelLimit` requests? To avoid placing multiple, unnecessary requests and overloading the executor.
Here's an example: When we want to cancel an order, we will place a `CancelLimit` request. Soon after placing this request, we should receive a corresponding `CancelEv`, but before we receive that event, other things may happen. If we do not remember that we have already asked for the cancellation, we may mistakenly ask for the cancellation again until we receive the event. Just like we keep track of `PlaceLimit` requests to avoid reissuing them, we should also keep track of `CancelLimit` requests to avoid reissuing them.


### Invariants

#### Limiting the exposure

We have an upper bound on the total volume open at the destination market: V(R(t)). That is part of our exposure because if suddenly all our orders execute and then the market crashes before we can react, we will be short by V(O(t)). So, we can use V(R(t)) as an upper bound on this exposure.

However, our exposure does not disappear once an order executes. If we sell 1 bitcoin and the exchanges then crashes and go offline, even if we have no open orders we are still 1 BTC short. So, fill events only transform one kind of exposure into another. They transform exposure from open orders into exposure from realized sales (or purchases).

To be able to limit our total exposure, we must track our sales (or purchases) that we have not yet, recouped. That is, we must track `FillsEv` events on the destination exchange to track increases in exposure and also on the source exchange to track decreases as we recoup our sales. We will call this difference S(t):

S(t)  = volume executed in   fills   on destination exchange  - volume executed in   fills   on source exchange to recoup sales
Consider:
S'(t) = volume executed in `FillsEv` on destination exchange  - volume executed in `FillsEv` on source exchange to recoup sales

There is a time lag between S(t) and S'(t). See Diagram 4

Diagram 4

On the backend S'(t) over estimates the exposure S(t), but on the front end, because of the lag, it underestimates.

We need to be able to upper bound our total exposure.

Total exposure = exposure from open orders + exposure from executed sales
          E(t) =      V(O(t))              + S(t)

We can upper bound the exposure from open orders with V(R(t)), but we *cannot* upper bound the exposure from executed sales S(t) for all time t using S'(t). This is because we only find out that this exposure has increased after the fact. We are only notified that a sale happened after it has already taken place. So, as we keep track of `FillsEv`s we are underestimating our exposure from sales V(S(t)) in the time interval that begins when a fill happens at the exchange until we are notified of it.

Thankfully, at any time t, how much we underestimate the sales exposure S(t), by using S'(t), will exactly match how much we *overestimate* V(O(t)) (for the same OpenAction) by using V(R(t)). When a sale occurred, the fill volume immediately becomes realized exposure, but it also ceases to be open exposure. So, the two exactly match and we can use the formula above as in:

E'(t) = V(R(t)) + S(t)

to upper bound our total exposure. E'(t) is an upper bound on our current total exposure. It is only a loose bound because it overestimates the exposure:

- at the beginning, when a `PlaceLimit` request is made, but not yet processed by the exchange and
- in the middle, when a `CancelLimit` request is processed but the corresponding `CancelEv` has not yet been issued
- at the end, when a recouping fill has executed, but we have not yet seen the corresponding `FillsEv` event.

Otherwise, this is tight.

To limit our exposure to a maximum of M, we limit the volume of the orders we place with `PlaceLimit` to:

A(t) = volume available(t) = M - E'(t)


#### tracking the market

Each time an `BookEv` happens on the source market, we obtain a list of targets represented by triples (side, price, volume).
For now, we will assume all targets are on the same side of the orderbook. The targets represent the total volume in orders we would like to keep open at the specified (side, price) on the destination market.

The strategy tracks the market by construction, assuming there is enough available volume A(t). Ideally, it should keep track of the current volume in OpenActions per (side, price) pair and:

a) issue new `PlaceLimit` requests to increase the volume in targets where the open volume is low
b) issue new `CancelLimit` and, if necessary, smaller `PlaceLimit` requests to decrease the volume when it's too high

Currently, it only:

a)  issues new `PlaceLimit`  requests to increase the volume in targets where the open volume is low
b') issues new `CancelLimit` to remove all volume (*all the way to zero*) when it's too high.

<<<< WORK IN PROGRESS FROM THIS POINT >>>>

======
If the targets themselves have been limited by taking into account the executed volume S(t), we have the following:

Well,

A(t) = M - E'(t) = M - (V(R(t)) + S(t)) = M - V(R(t)) - S(t) = M - S(t) - V(R(t)) = Q(t) - V(R(t))

where Q(t) is an ajdusted maximum exposure available to targets issued at time t. 
We have to ensure that M ≥ E'(t) for all t. That is, M - E'(t) ≥ 0. This is the same as:

0 ≤ A(t) = Q(t) - V(R(t))
0 ≤ Q(t) - V(R(t))
V(R(t)) ≤ Q(t) 

We must ensure this at all times. So, the mechanism will always have to wait for V(R(t)) to go down (through cancellations, i.e. `CancelEv` or fills `FillsEv`) to then *later* (not at the same time those events happen) request to place new orders. There is no way around it. The mechanism does not know if the targets were limited by the market itself (and there's wiggle room) or by Q(t) (and this has to be followed tightly).

It seems better for the targets, to just be targets and for the exposure control to come in afterwards. That way, the mechanism can use "available volume wiggle room", if available, to both cancel and place orders at the same time. In fact, we can use this in a smart way for multiple targets when the first targets have priority over later ones.

======
The notion of tracking we would like to use is one of *eventual consistency*:

    If no other events happen after a `BookEv` in the source market, the strategy will *eventually* match
    the targets and have those volumes in open orders in the destination market (unless limited by exposure).
    
However, this does not currently hold when the volume decreases.














