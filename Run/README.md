## Running a Server or Client 

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
