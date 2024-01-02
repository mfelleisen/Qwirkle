## Running a Server or Client 

how to run the system from either the server side or the client side (or both) 

### Table of Content


See [Modular Programming](https://felleisen.org/matthias/Thoughts/Modular_Programming.html)
for an explanation of how code files are organized in Racket.

| file | purpose |
|--------------------- | ------- |
| [xserver](xserver) |  main is invoked via the command line as follows: $ ./xserver port-number < server-config-file | 
| [xclients](xclients) | main is invoked via the command line as follows:  $ ./xclient-bonus port-number < client-config-file | 
| [default-server-config.json](default-server-config.json) |  | 
| [sample-client-config.json](sample-client-config.json) |  | 


### Run Server

```
$ ./xserver port-number
```

or

```
$ ./xserver port-number < default-server-config.json
```

Editing the configuration is almost self-evident. 


### Run Client

```
$ ./xclients port-number < sample-client-config.json
```

Editing the configuration is almost self-evident. 
