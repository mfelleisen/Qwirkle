## Remotes 

this component support the implementationn of a remote-proxy protocol,
which is key to turning a monolithic (game) system into a distributed one:

### Table of Content


See [Modular Programming](https://felleisen.org/matthias/Thoughts/Modular_Programming.html)
for an explanation of how code files are organized in Racket.

| file | purpose |
|--------------------- | ------- |
| [define-dispatcher.rkt](define-dispatcher.rkt) | create a remote proxy-context (remote-ref, remote-manager) from types of the player's methods | 
| [define-remote.rkt](define-remote.rkt) | a library for specifying the methods of remote proxies via "types" (define/remote for classes) | 
| [remote-testing.rkt](remote-testing.rkt) | a facility for testing remote players | 






