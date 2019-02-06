## Threading Model

At startup, the framework will start each producer and executor in separate threads, but if more than 2 threads are needed, the market's connector should manage that. Care should be taken to kill all threads and free resources in an orderly fashion at shutdown. The shutdown process will allow executors 30 seconds before killing them. They should free any local resources during this time.

The gigantic synchronous callback with multiple entry and exit points represented by the event network can behaves as a critical section as no two distinct calls will execute simultaneously.

All the event network ever does is to place an Action in an unbounded length execution queue. So (unless there is a gigantic looping bug in the strategy) the event network *never blocks indefinitely*. Thus, by giving a connector its own execution/production threads, we ensure that a problem in a connector cannot deadlock the whole trader. It just deadlocks its own execution/production threads limiting the damage to a single market.

Connectors to different markets may also have to keep internal state throughout program execution. The framework doesn't know and doesn't care what this internal state may be, but we are going to pre-establish a convention that allows connectors to establish such shared state.

### Startup

This will be broken up in the future, to allow sharing or changing the same configuration between different connectors, but for now each connector should provide an Initializer. That is, an IO action that outputs 3 other IO actions.

For example:

```
coinbeneInitializer
    :: forall p v. (Coin p, Coin v) 
    -> Handler (TradingEv p v q c)   -- handler to call to fire events
    => IO (Producer p v () (), Executor p v, Terminator)
```

The types are defined as:

```
type Producer p v q c = IO ()
type Executor p v     = Action p v -> IO ()
type Terminator       = IO ()
```

### Shutdown

Currently, there is no way to inform the strategy that we are shutting down. In the future, we should consider adding a `ShutdownEv`. So, we currently can't solve the strategy shutdown problem. We can solve the Connector shutdown problem, though.

If executor and producer use shared resources that are built at startup. It is up to them to control these resources and to collaborate to free them at shutdown.

The way this semi-orderly connector shutdown is handled is that the corresponding `Terminator` action is run on the `Executor` thread and this thread is given 30 seconds before it is killed.





