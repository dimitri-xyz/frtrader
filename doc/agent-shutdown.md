## Rationale for Agent Shutdown Mechanism

A strategy does not need to signal to a connector that it will no longer issue any actions, because it can just do that (not issue any actions), no signaling is necessary.

On the other hand, if the strategy not only will no longer perform any actions, but also no longer needs any more event notifications, then the connector should be notified, so that it can just shut itself down and free up resources.

So, the messaging on the `Action` interface is about no longer needing event notifications. There is a new `Shutdown` action, this action should be sent to a market *after* all the necessary event notifications from that market have been received.

The `Shutdown` action will always succeed, i.e. the connector will never fail to gracefully shutdown after it has placed all its orders.

There is also the need for a `Error` Action. If there is failure from which the strategy cannot recover, it should send this Action to let a human know the situation it found itself in. This message does **not** cause a shutdown. It just signals the problem. It must be followed by a `Shutdown` message to force the shutdown (again, the `Shutdown` action is fail-proof).

**A strategy may still receive a few events _after_ sending the `Shutdown` action.** This is because those events maybe processed by different threads and already be queued up. The strategy should make sure this is not a problem. 


A problem with this apprach is that `Shutdown` does not differentiate between normal and abnormal termination. Maybe we should take a page from unix's book and return an `Int` as an exit code to make this differentiation.

Should we!!? 

It's not immediately clear if I should have a separate channel to signal termination, not just to the exchanges but also to the whole framework. This is implicit, though. If the strategy signals this to all its markets, it must be the case that it is done and can/should be shutdown. So, again the extra signalling mechanism is not necessary and adds a level of complexity as we have to decide what to do when all markets are shutdown but there is no signalling to the framework itself. Because we want to keep "impossible states unrepresentable". We will *not* have this extra signalling coming out of the strategy.

This reasoning implies that the framework should be able to detect that a strategy has shutdown *all* its markets and kill it. The current way we shutdown is simply by sending a shutdown message to the executors and then waiting for 30 seconds. The strategy is not informed!!

This needs to be more involved. 

We have 2 options:

a) signal a shutdown to the strategy and time out everything: strategy and connectors with a single timeout.

b) signal a shutdown to the strategy, timeout the strategy first and later time out the connectors.

In approach (a), the strategy itself kills the connectors. In approach (b), it doesn't have to. What do we gain from timing out the connectors separately? Again, it adds some complexity, why didn't the strategy kill them itself? How much time do we give them? We do gain some more assurance the connectors will be told to terminate (if the strategy goes crazy), but this does not seem like a good enough reason for the extra complexity.

How do we differentiate a failure due to the connector (e.g. network failure) from a failure in the strategy (e.g. cannot shutdown as it would have to wait more time to buy at the desired price)?

How does the strategy signal an error in the last connector it has, just before shuting down if the connector itself is the problem?

If the `Error` message also cannot fail, then it can use this channel. But really, we are using this error message as an different signalling channel/mechanism.
Maybe startup/shutdown should be handled through a completely different interface!

This is a lot more work but seems to make a lot more sense. In fact, we could send a reconfiguration notification on this interface for the strategy to change its parameters.

===============
Also, what happens if a connectors gets an error such as the 502 Bad gateways I have been getting? Can this also be treated by similar mechanisms.

A connector could signal an error to the strategy directly through the event notification interface. Or, it could do it through the extra Signaling channel that the framework uses. It seems the notification interface itself would make more sense, but this is not clear. In any case, I don't think I have to decide this now.

===============
So, it seems the design spec right now is simple, but requires quite a bit of work:

All strategies will have an extra input/output event interface.
The strategies will receive a single new event on this interface: A `ShutdownEv` and they will output 2 "Actions" which are: `Error` with corresponding message and `ShutdownDone` with an exit code.

The framework will signal termination on this interface. The strategy will signal a `ShutdownDone` once it is done and it may issue multiple `Error` messages as required before then. The framework itself will timeout the strategy or start shutting down the connectors once it receives the `ShutdownDone` message.

This mechanism may or may not be used to treat errors within a single market or connector, this will be decided later.

This is a shit-ton of work, though! Do I really need this extra channel?

Well, the good thing about a separate channel is that it requires *zero* changes to the connectors as all signalling is to the strategy and we already have a shutdown mechanism for the connectors (it is just an unannounced kill with a 30 second time out).

==============
One way to look at this extra "control" interface, is that it receives auxiliary information from outside any market. This interface will be necessary for any strategy that depends on "off market" events. In this case, we are using it for the shutdown event, but we could be using it for other changes, such as an update on the exchange rate or maybe a change in our risk tolerance, etc.

 














