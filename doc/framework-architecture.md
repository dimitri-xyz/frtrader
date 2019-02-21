## Framework Design Rationale

This strategy-market interface and this framework have be designed with the following goals in mind:

1. Strategies should be totally deterministic (though stateful) to ease testing.
2. Strategies should be independent of any particular exchange, so that they can be put to work on any exchange.


## Working with Multiple Exchanges

Assume we are mirroring the BTC-USD orderbook from Coinbase at the Gemini Exchange. Both markets are BTC-USD, so tagging the types by currency does not uniquely identify the market.

We could also add a (phantom) Exchange tag to the type to track this, that way the BTC-USD Coinbase market would become different from the BTC-USD Gemini market at the type level. But after some thought, this seems to cumbersome and provide little advantage as we cannot put different types on a single list.

For now, we will use a simpler options. We will use tuples to separate the markets. A strategy that operates on 2 markets will output a 2-tuple of `Action`s and receive 2 event streams as input. The more markets the strategy operates on, the wider the tuple.

This has a few advantages:

- It is very simple
- requires no change to the `Action` type
- treats different markets differently independently of the exchange
- allows personalization of strategies per exchange (at the cost of generality)

Phantom type tagging would make a connection in the framework all the way to the exchange at compile time. It seems like a safer option, but it is also cumbersome.

I have decided to implement provides a partial type-level link between the exchange and the executor. The type system will prevent us from trying to buy BTC in the BTC-USD market by placing an order in an ETH-USD markets, but it will not help us if we connect an executor for the same BTC-USD market, but in the wrong exchange. The ordering of executors in the framework will be important in the current implementation.



