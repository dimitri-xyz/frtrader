# FRTrader
A Functional Reactive Bitcoin Trader

Trying to makei it easier for other people to contribute and noticing that my current solution is brittle, I am no longer going to be using *relative-imports* in this repo.

Instead I will put each package in a separate repo and follow the practice of using stack with multpiples "packages" in the stack.yaml file as described here:

https://github.com/commercialhaskell/stack/issues/348#issuecomment-113111788

There are some downsided with this solution. In particular, committing changes in repos such as MarketModel that are low down in the chain is going to be much more time consuming, but I will have to live with that for now. On the positive side, GHC will no longer mangle the names of the input files.
