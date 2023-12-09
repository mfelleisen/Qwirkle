## Integration Tests

This directory satisfies the basic specs of milestone 10 for Sw Dev F'23.

### Files for test fest

- `xtest` is the testing script 
- `xserver` 
- `xclients`
- `xserver-bonus` 
- `xclients-bonus`

Running `xtest` will create the files 

- `n-in.json` so that the test harness finds all test cases 
- `port-starter-file.rktd` in $HOME/Tmp, which records the port last used, 
  just in case something goes wrong with the students' TCP port allocation.

It is safe to delete these files after a run.  

### Test Directories:

- `ForStudents` 
- `Tests` are standard configurations, pretty much like those for `8`
- `Bonus` are client configurations with flaws:
  - players that sign up with bad names
  - players that send broken JSON, plus
  - 2 weird clients that don't sign up properly
  
### Running the required tests

`xtest` should be run as follows: 

```
$ ./xtest pathToTHEIRtests/ pathto_MY_client pathto_MY_server 
```
which will submit their tests to my oracle

```
$ ./xtest Tests/ pathto_THEIR_client pathto_MY_server 
```
which will test their clients with "everything goes well" tests

```
$ ./xtest Tests/ pathto_MY_client pathto_THEIR_server
```
which will test their servers with "everything goes well" tests

```
$ ./xtest Bonus/ pathto_MY_xclients-bonus pathto_THEIR_server
```
which will test their servers with "clients that violate the protocols
in all kinds of ways" 

| file | purpose |
|--------------------- | ------- |
| [get.rkt](get.rkt) | a utility for retrieving JSON from STDIN and check for well-formedness and validity with a predicate | 
| [run-server-client.rkt](run-server-client.rkt) | deal with a port number as a command-line arguments | 
| [run-server-client.rkt~](run-server-client.rkt~) | ovide | 
