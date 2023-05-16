# Cryptography Implementation 

The protocol involves the following steps:

1) The protocol uses the elliptic curve EL: y^2 = x^3 + 23x + 11 over Zp, where p = 173 is a prime number.
2) The code generates a point α (not equal to the point at infinity) on the curve EL and prints its value.
3) Alice and Bob agree on the curve EL and the point α.
4) The program prompts for Alice's private key (nA) and Bob's private key (nB) within the range [1, 150].
5) Using nA and nB, Alice and Bob perform a Diffie-Hellman key exchange on the curve EL with the point α to establish a shared secret key SK. The program prints the values of SK.
6) Alice computes a key KA using the SHA-256 hash function and the coordinates of SK (x1, y1).
7) Bob computes a key KB using the SHA-256 hash function and the coordinates of SK (x1, y1).
8) The program prints the values of KA and KB as 32-byte sequences (space-separated).
9) The program asks for Alice's 256-bit message (MA) as input, provided as 32 space-separated bytes in hexadecimal format.
10) Alice encrypts the message MA using the AES-256 bit encryption algorithm in CBC mode of operation with her key KA and an IV of 0. The generated ciphertext CA is printed.
11) Alice generates a Message Authentication Code (MACA) for MA using a specific algorithm involving SHA-256 and the key KA. The MACA is printed as bytes (space-separated).
12) Alice passes the ciphertext CA, MACA, and IV (0) to Bob.
13) Bob decrypts CA using the AES-256 bit decryption algorithm in CBC mode with his key KB and IV 0. The decrypted text MB is displayed.
14) Bob generates a MACB using SHA-256, the key KB, and MB. The MACB is printed as bytes (space-separated).
15) The program displays MB and MACB.
16) If the code is correct, KA = KB, MA = MB, and MACA = MACB for every possible input.

This repository provides an implementation of the protocol that ensures secure key exchange and encryption using elliptic curves and AES-256, along with verification of the expected results for various inputs.
