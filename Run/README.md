## Running a Server or Client 

how to run the system from either the server side or the client side (or both) 

### Files

- `xserver` 
- `default-server-config.json` is a sample configuration for the server, close to what it will use when started without configuration 
- `xclients`
- `sample-client-config.json` is a sample configuration for the client spawn script 

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

| file | purpose |
|--------------------- | ------- |
| [xserver](xserver) |  main is invoked via the command line as follows: $ ./xserver port-number < server-config-file | 
| [xclients](xclients) | main is invoked via the command line as follows:  $ ./xclient-bonus port-number < client-config-file | 
| [default-server-config.json](default-server-config.json) |  | 
| [sample-client-config.json](sample-client-config.json) |  | 
