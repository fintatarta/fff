# What is this?

This folder contains the source code of an example of implementation of the algorithm of Shamir for secret sharing.

> **Note**: it is almost obvious, but this implementation is not suggested for serious security purposes.  For example, the random number generation is done via the standard library generator which probably is suitable for simulations and other purposes, but it is unlikely that it is cryptographically strong.  Nevertheless, this implementation just wants to be an example of usage of finite fields; therefore, it is OK to use the standard library generator. 

# How do I use it?

The main procedure is `Secret_Sharing.Main` that corresponds to executable `secret_shared-main` (if compiled with GNAT). Command line arguments are case-insensitive. It can be called in three ways

* Help
```bash
secret-shared-main (help|h|?) 
```
Print a help and exit.  The help text is printed also if `secret-shared-main` is called without arguments
* Encode
```bash
secret-shared-main (encode|enc|e) n-points threshold secret
```
Encode a secret where
* `n-points` is the number of points to be generated
* `threshold` the minimum number of points required to recover the secret
* `secret` the secret to be encoded, a 32-bit unsigned integer  

The program will generate `n-points` point that will be printed to the standard output as hexadecimal strings
* Decode
```bash
secret-shared-main (decode|dec|d) point-1  point-2 ... point-N
```
On the command line, after the "verb" `decode`, the program expects a number of points (represented by the hexadecimal strings printed by encode) equal to `threshold`; it recovers the secret and prints it to the standard output
