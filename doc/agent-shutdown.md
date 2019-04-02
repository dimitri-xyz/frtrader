## Rationale for Agent Shutdown Mechanism

It is clear we need to signal a shutdown to the strategy. The strategy is currently not aware of the need to shut down and, thus, cannot respond to such a request. This is new functionality that we need to implement.

Before tackling this new functionality, let us focus on whether we also need to signal a shutdown to the Connectors through the Strategy/Market interface.

#### Strategy/Market Interface

A strategy does not need to signal to a connector that it will no longer issue any actions, because it can just do that (not issue any actions). Although knowing this in advance would allow the connector to shut down the executor, the producer would have to keep working and dispatching events.

On the other hand, if the strategy will no longer perform any actions and also no longer needs any more event notifications, then we can shut down the whole connector (both executor and producer) and the connector should be notified, so that it can just shut itself down and free up resources. This notification does not have to be done on the Strategy/Market interface, though. There may be another mechanism.

There are two ways we could notify the connector:

1. Through a new notification in the `Action` interface
2. By stopping the executor (current implementation) and making it stop the producer also.

Signalling through the `Action` interface would duplicate current functionality, but make this directly available to the strategy. This would enable a strategy to dynamically stop connectors. However, we don't have a mechanism to dynamically *start* a connector, so this seems like unnecessary functionality. For now, the set of connectors for a strategy is statically determined, we have no plans to change that. So, this seems unnecessarily complicated.

We choose to use (2), so we just need to make one update: *The `stopExecutor` functionality used today, should stop the whole connector, both producer and executor, not just the executor.*

#### Pre/Post conditions for connector shutdown signalling

Any connector shutdown signalling should be sent to a market *after* all the necessary event notifications from that market have been received.

Furthermore, the shutdown signal to a connector will always succeed, i.e. the connector will never fail to gracefully shut down after it has placed all its orders.

**A strategy may still receive a few events _after_ the connector receives the `Shutdown` signal.** This is because those events maybe processed by different threads and already be queued up. The strategy should make sure this is not a problem. 

### New Control Interfaces

##### Connector Control Interface

We already have an implementation of a control interface for the connectors through the definition above: *The `stopExecutor` functionality used today, should stop the whole connector, both producer and executor, not just the executor.*

#### Strategy Control Interface

Let us now define the control interface for the strategies. We need an `Error` Action on this interface. If there is failure from which a strategy cannot recover, it should send this Action to let a human know the situation it found itself in. This message does **not** cause a shutdown. It just signals the problem. 

We also need a new `ShutdownDone` message to let the framework know the strategy has completed its shutdown. The framework should not expect anything else from the strategy after receiving the `ShutdownDone` control action. From the point of view of the strategy this action is fail-proof and will *eventually* succeed.

Here's the definition:

```
data ControlAction
    = ShutdownDone Int
    | Error Int String
```

The extra parameters needed allow the strategy to pass extra information.

There is also a new mechanism to signal a shutdown to the strategy:

```
data ControlEv = ShutdownEv
```

From now on, all strategies that need to be notified of shutdown events will have an extra `ControlEv` input interface and an `ControlAction` output interface.

The strategies will receive a single new event on the `ControlEv` interface: A `ShutdownEv` and they can output two `ControlAction`s: `Error` and `ShutdownDone`.

The framework will signal termination on the `ControlEv` interface. The strategy will send a `ShutdownDone` once it is done and it may issue multiple `Error` messages, as required, before then. The framework itself will timeout the strategy or start shutting down the connectors once it receives the `ShutdownDone` message.

This mechanism may or may not be used to treat errors within a single market or connector, this will be decided later.

One good thing about this separate control channel is that it requires *zero* changes to the connectors as all signalling is to the strategy and we already have a shutdown mechanism for the connectors (it is just an unannounced kill with a 30 second time out).

Another way to look at this extra "control" interface, is that it receives auxiliary information from outside any market. Such an interface would be necessary for any strategy that depends on "off market" events. In this case, we are using it for the shutdown event, but we might consider be using it for other changes, such as an update on the exchange rate or maybe a change in our risk tolerance, etc.

#### Other Errors

Also, what happens if a connectors gets an error such as the 502 Bad gateways I have been getting? Can this also be treated by this control mechanism. Thankfully, I think this decision can be safely postponed.

#### Strategy Shutdown Logic

The logic for treating the output of the strategy's control interface is a little tricky.

First, we want to avoid deadlocks. There maybe multiple *control* threads calling into the strategy. So, we need to insure the strategy does not get blocked when it signals an `Error` or a `ShutdownDone` on its control output.

The immediate way to ensure this is to use the same logic we used for the markets themselves, we just put the signalling (Control) actions in an unbounded queue.

A problem with this is the loose coupling between consumption of this queue and the control signalling. We want things to be tight for the control signaling. It is possible, though, that in the future, the control signalling will including making an HTTP request to send a text message, or some other time consuming process. Thus, it may wise to just re-use the same mechanism we have in place today and just being aware that the control interface is *more important*. This is what I am going to do for now. I will figure out what *more important* means later.


##### Implementation

There is a distinction between the `Action`s in the strategy/market interfaces and the `ControlAction`s in the strategy control interface. There is no `Action` in the strategy/market interface that will cause the thread that is the consumer of the `Action` queue to terminate. On the control interface, a ShutdownDone` message will need to do just that. In other words, we are using "in line signaling" to cause thread termination.

This requires some changes to the current termination logic.











