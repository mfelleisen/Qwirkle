## Integration Tests

this directory satisfies the basic specs of the integration tests of milestone 10 for Sw Dev F'23.

### Table of Content


| file | purpose |
|--------------------- | ------- |
| [xtest](xtest) | ./xtest Tests/ ./xclients ,/xserver launches a server locally on a port, points n clients there, with tests creates from configs | 
| [xserver](xserver) |  main is invoked via the command line as follows: $ ./xserver 12345 < server-config-file | 
| [xclients](xclients) | main is invoked via the command line as follows: $ ./xclient-bonus 12345 < client-config-file | 
| [get.rkt](get.rkt) | a utility for retrieving JSON from STDIN and check for well-formedness and validity with a predicate | 
| [run-server-client.rkt](run-server-client.rkt) | deal with a port number as a command-line arguments | 
| [xclients-bonus](xclients-bonus) |  main is invoked via the command line as follows:  $ ./xclient-bonus 12345 < client-config-file | 
| [xserver-bonus](xserver-bonus) |  main is invoked via the command line as follows:  $ ./xserver 12345 < server-config-file | 


### MODIFY REF TO RUN

In `Referee/referee.rkt` set (dont-double-check-names #false) to make sure tests are deterministic. 

### Test Directories:

| directory		 | purpose 
| ---------------------- | --------------------------------- | 
| `ForStudents` 	 | examples distributed to students  |
| `Tests` 		 | standard configurations, pretty much like those for milestone 8 |
| `Bonus` 		 | client configurations with flaws  |

The bonus tests check various robustness aspects of the student's server:

- players that sign up with bad names
- players that send broken JSON, plus
- 2 weird clients that don't sign up properly

### Generated Files

Running `xtest` will create the files 

- `n-in.json` so that the test harness finds all test cases 
- `port-starter-file.rktd` in $HOME/Tmp, which records the port last used, 
  just in case something goes wrong with the students' TCP port allocation.

It is safe to delete these files after a run.  
  
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
