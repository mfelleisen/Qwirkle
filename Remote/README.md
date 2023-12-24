## Remotes 

The components in this directory support the implementationn of a remote-proxy
protocol for turning a monolithic (game) system into a distributed one:

- `define-dispatcher` supplies a construct for implementing a remote-referee
  for the client side 

- `define-remote` adds `define/remote` to the `class` vocabulary, which turns
  the signature line for a player method into a method that serializes
  arguments, performs TCP-based exchanges, and deserializes the result 

- `remote-testing` provides some basic facilities for testing remote-proxy
  components. 



| file | purpose |
|--------------------- | ------- |
| [define-dispatcher.rkt](define-dispatcher.rkt) | create a remote proxy-context from types of the player's methods | 
| [define-remote.rkt](define-remote.rkt) | ovide | 
| [remote-testing.rkt](remote-testing.rkt) | a facility for testing remote players | 
