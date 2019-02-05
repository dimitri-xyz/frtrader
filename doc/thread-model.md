## Threading Model

At startup, the framework will start each producer and executor in separate threads, but if more than 2 threads are needed, the market's connector should manage that. Care should be taken to kill all threads and free resources in an orderly fashion at shutdown. The shutdown process will allow executors 30 seconds before killing them. They should free any local resources during this time.

The gigantic synchronous callback with multiple entry and exit points represented by the event network can behaves as a critical section as no two distinct calls will execute simultaneously.

All the event network ever does is to place an Action in an unbounded length execution queue. So (unless there is a gigantic looping bug in the strategy) the event network *never blocks indefinitely*. Thus, by giving a connector its own execution/production threads, we ensure that a problem in a connector cannot deadlock the whole trader. It just deadlocks its own execution/production threads limiting the damage to a single market.

Connectors to different markets may also have to keep internal state throughout program execution. I don't know what this internal state may be, but we are going to pre-establish a convention for how the trader will call the connectors and in what threads.

### Startup

Both the executor and producer will be given (as a parameter) a shared resource that is built at startup. It is up to them to control this resource and collaborate to free it at shutdown.

The framework will run a given initialization routine to create the shared resource.

The shared resource is presented to both:

```
- executor -> config -> state -> Action -> m
- producer -> config -> state ->
```

It is up to them to put it in TVAR/MVAR or whatever synchronization primitive they need.

### Shutdown

Currently, there is no way to inform the strategy that we are shutting down. In the future, we should consider adding a `ShutdownEv`.


