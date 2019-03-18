## Rationale for Agent Shutdown Mechanism

It is clear we need to signal a shutdown to the strategy. The strategy is currently not aware of the need to shut down and, thus, cannot respond to such a request. This is new functionality that we need to implement.

Before tackling this new functionality, let us focus on whether we also need to signal a shutdown to the connectors through the Strategy/Market interface.

#### Strategy/Market Interface

A strategy does not need to signal to a connector that it will no longer issue any actions, because it can just do that (not issue any actions), no signaling is necessary (this would shut down the executor).

On the other hand, if the strategy not only will no longer perform any actions, but also no longer needs any more event notifications, then (we can shut down both executor and producer and) the connector should be notified, so that it can just shut itself down and free up resources.

There are two ways we could notify the connector:

1. Through a new notification in the `Action` interface
2. By stopping the executor (current implementation)

Signalling through the `Action` interface would duplicate current functionality, but make this directly available to the strategy. This would enable a strategy to dynamically stop a connectors. However, we don't have a mechanism to dynamically *start* a connector, so this seems like unnecessary functionality. For now, the set of connectors for a strategy is statically determined, we have no plans to change that. So, this seems unnecessarily complicated.

We choose to use (2), so we just need to make one update: *The `stopExecutor` functionality used today, should stop the whole connector, both producer and executor, not just the executor.*

#### Pre/Post conditions for connector shutdown signalling

Any shutdown signalling should be sent to a market *after* all the necessary event notifications from that market have been received.

Furthermore, the shut down signal to a connector will always succeed, i.e. the connector will never fail to gracefully shut down after it has placed all its orders.

**A strategy may still receive a few events _after_ the connector receives the `Shutdown` signal.** This is because those events maybe processed by different threads and already be queued up. The strategy should make sure this is not a problem. 

### New Control Interfaces

We already have an implementation of a control interface for the connectors through the definition above: *The `stopExecutor` functionality used today, should stop the whole connector, both producer and executor, not just the executor.*

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

Also, what happens if a connectors gets an error such as the 502 Bad gateways I have been getting? Can this also be treated by this control mechanism. Well, I don't think I have to decide this now.


