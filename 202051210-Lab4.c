// Including standard libraries
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>
#include <string.h>

// Global Variables and Matrices Declaration
int p=173, a = 23, b = 11, x, y;
uint8_t W[60][4];
unsigned char key[15][4][4], ciphertext[4][4], plaintext[4][4];

uint32_t H[8] = {
    0x6a09e667, 
    0xbb67ae85, 
    0x3c6ef372, 
    0xa54ff53a, 
    0x510e527f, 
    0x9b05688c, 
    0x1f83d9ab, 
    0x5be0cd19
};
uint32_t K[64] = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
	0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
	0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
	0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
	0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
	0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
	0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
	0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2};
unsigned char s[16][16] = {
    0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
    0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
    0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
    0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
    0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
    0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
    0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
    0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
    0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
    0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
    0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
    0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
    0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
    0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
    0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
    0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16};
unsigned char s_inv[16][16] = {
    0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38, 0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb,
    0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87, 0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb,
    0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d, 0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e,
    0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2, 0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25,
    0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92,
    0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda, 0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84,
    0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a, 0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06,
    0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02, 0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b,
    0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea, 0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73,
    0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85, 0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e,
    0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89, 0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b,
    0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20, 0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4,
    0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31, 0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f,
    0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d, 0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef,
    0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0, 0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61,
    0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26, 0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d};

// Function to generate alpha
void GenerateAlpha(int a, int b){
    int lhs, rhs;
    srand(time(NULL)); 
    do {
        // x = rand() % p;
        x = 1;
        lhs = (x*x*x + a*x + b)%p;
        y = 0;
        while (y<p) {
            rhs = (y*y)%p; 
            if (lhs == rhs) return;
            y++;
        }
    } while(1);
}

// Function to find modular inverse of a under modulo p
int inv(int a) {
    for (int i=1; i<p; i++) if ((a*i)%p == 1) return i;
    return -1;
}

// EC Diffie hellman addition
int* ECAdd(int x1, int y1, int x2, int y2) {
    int m, x3, y3;
    if ((x1 == x2 && y1 == y2) || (x1 == -1 && y1 == -1)) {
        m = ((3*x1*x1 + a)%p) * inv((2*y1)%p);
        m = (m%p + p)%p;
    }
    else if (x1 == x2 && (y1+y2)%p == 0){
        m = 0;
        x3 = -1, y3 = -1;
        int * res = (int *)malloc(2*sizeof(int));
        res[0] = x3, res[1] = y3;
        return res;
    }
    else {
        m = (y2-y1) * inv(x2-x1);
        m = (m%p + p)%p;
    }
    x3 = (m*m - x1 - x2) % p;
    y3 = (m*(x1 - x3) - y1) % p;
    x3 = (x3%p + p)%p;
    y3 = (y3%p + p)%p;
    int * res = (int *)malloc(2*sizeof(int));
    res[0] = x3, res[1] = y3;
    return res;
}

// Right rotate function
uint32_t rotr(uint32_t x, int n){
    return (x >> n) | (x << (32-n));
}

// SHA 256
void sha256(int x, int y){
    uint32_t sha_arr[16];
    sha_arr[0] = x, sha_arr[1] = y, sha_arr[2] = 1 << 31;
    for (int i = 3; i < 15; i++) sha_arr[i] = 0;
    sha_arr[15] = 64;

    // create message schedule
    uint32_t W[64];
    for (int i = 0; i < 16; i++) W[i] = sha_arr[i];
    for (int i = 16; i < 64; i++){
        uint32_t s0 = rotr(W[i-15], 7) ^ rotr(W[i-15], 18) ^ (W[i-15] >> 3);
        uint32_t s1 = rotr(W[i-2], 17) ^ rotr(W[i-2], 19) ^ (W[i-2] >> 10);
        W[i] = W[i-16] + s0 + W[i-7] + s1;
    }

    // initialize hash values
    uint32_t a = H[0], b = H[1], c = H[2], d = H[3], e = H[4], f = H[5], g = H[6], h = H[7];
    for (int i=0; i<64; i++){
        uint32_t S1 = rotr(e, 6) ^ rotr(e, 11) ^ rotr(e, 25);
        uint32_t ch = (e & f) ^ ((~e) & g);
        uint32_t temp1 = h + S1 + ch + K[i] + W[i];
        uint32_t S0 = rotr(a, 2) ^ rotr(a, 13) ^ rotr(a, 22);
        uint32_t maj = (a & b) ^ (a & c) ^ (b & c);
        uint32_t temp2 = S0 + maj;
        h = g, g = f, f = e, e = d + temp1, d = c, c = b, b = a, a = temp1 + temp2;
    }

    // add hash values to H
    H[0] += a, H[1] += b, H[2] += c, H[3] += d, H[4] += e, H[5] += f, H[6] += g, H[7] += h;

    // print hash values in hex
    for (int i = 0; i < 8; i++) printf("%x", H[i]);
}

// SHA 256 without padding (for 512 bit input)
void sha256_no_padding(uint32_t x[8], uint32_t y[8], uint32_t hash[8]){
    uint32_t sha_arr[16];
    for(int i=0;i<8;i++)
    {
        sha_arr[i]=x[i];
        sha_arr[i+8]=y[i];
    }

    // create message schedule
    uint32_t W[64];
    for (int i = 0; i < 16; i++) W[i] = sha_arr[i];
    for (int i = 16; i < 64; i++){
        uint32_t s0 = rotr(W[i-15], 7) ^ rotr(W[i-15], 18) ^ (W[i-15] >> 3);
        uint32_t s1 = rotr(W[i-2], 17) ^ rotr(W[i-2], 19) ^ (W[i-2] >> 10);
        W[i] = W[i-16] + s0 + W[i-7] + s1;
    }

    // initialize hash values
    uint32_t a = hash[0], b = hash[1], c = hash[2], d = hash[3], e = hash[4], f = hash[5], g = hash[6], h = hash[7];
    for (int i=0; i<64; i++){
        uint32_t S1 = rotr(e, 6) ^ rotr(e, 11) ^ rotr(e, 25);
        uint32_t ch = (e & f) ^ ((~e) & g);
        uint32_t temp1 = h + S1 + ch + K[i] + W[i];
        uint32_t S0 = rotr(a, 2) ^ rotr(a, 13) ^ rotr(a, 22);
        uint32_t maj = (a & b) ^ (a & c) ^ (b & c);
        uint32_t temp2 = S0 + maj;
        h = g, g = f, f = e, e = d + temp1, d = c, c = b, b = a, a = temp1 + temp2;
    }

    // add hash values to H
    hash[0] += a, hash[1] += b, hash[2] += c, hash[3] += d, hash[4] += e, hash[5] += f, hash[6] += g, hash[7] += h;

    // print hash values in hex
    // for (int i = 0; i < 8; i++) printf("%x", hash[i]);
}

// AES256 encryption
uint8_t rcon[10][4] = {{0x01, 0x00, 0x00, 0x00}, 
                    {0x02, 0x00, 0x00, 0x00},
                    {0x04, 0x00, 0x00, 0x00},
                    {0x08, 0x00, 0x00, 0x00},
                    {0x10, 0x00, 0x00, 0x00},
                    {0x20, 0x00, 0x00, 0x00},
                    {0x40, 0x00, 0x00, 0x00},
                    {0x80, 0x00, 0x00, 0x00},
                    {0x1b, 0x00, 0x00, 0x00},
                    {0x36, 0x00, 0x00, 0x00}};

// Sbox function
uint8_t sbox(uint8_t a){
    int t1 = a&0xf;
    int t2 = a>>0x4;
    return s[t2][t1];
}

// Inverse Sbox function
uint8_t inv_sbox(uint8_t a){
    int t1 = a&15;
    int t2 = a>>4;
    return s_inv[t2][t1];
}

// Rot Word function
void rotWord(uint8_t temp_arr[4]){
    uint8_t temp = temp_arr[0];
    for(int i=0; i<3; i++){
        temp_arr[i] = temp_arr[i+1];
    }
    temp_arr[3] = temp;
}

// Inverse Rot Word function
void inv_rotWord(uint8_t temp_arr[4]){
    uint8_t temp = temp_arr[3];
    for(int i=3; i>0; i--){
        temp_arr[i] = temp_arr[i-1];
    }
    temp_arr[0] = temp;
}

// Sub bytes function
void subBytes(uint8_t temp_arr[4][4]){
    for(int i=0; i<4; i++){
        for(int j=0; j<4; j++){
            temp_arr[i][j] = sbox(temp_arr[i][j]);
        }
    }
}

// Inverse Sub bytes function
void inv_subBytes(uint8_t temp_arr[4][4]){
    for(int i=0; i<4; i++){
        for(int j=0; j<4; j++){
            temp_arr[i][j] = inv_sbox(temp_arr[i][j]);
        }
    }
}

// Shift Rows function
void shiftRows(uint8_t temp_arr[4][4]){
    uint8_t temp[4][4];
    for(int i=0; i<4; i++){
        for(int j=0; j<4; j++){
            temp[i][j] = temp_arr[i][j];
        }
    }
    for(int i=1; i<4; i++){
        for(int j=0; j<4; j++){
            temp_arr[i][j] = temp[i][(j+i)%4];
        }
    }
}

// Inverse Shift Rows function
void inv_shiftRows(uint8_t temp_arr[4][4]){
    uint8_t temp[4][4];
    for(int i=0; i<4; i++){
        for(int j=0; j<4; j++){
            temp[i][j] = temp_arr[i][j];
        }
    }
    for(int i=1; i<4; i++){
        for(int j=0; j<4; j++){
            temp_arr[i][(j+i)%4] = temp[i][j];
        }
    }
}

// Mul function for mix columns
uint8_t mul(uint8_t a, uint8_t b){
    uint8_t temp = 0;
    for(int i=0; i<8; i++){
        if(b&1) temp ^= a;
        if(a&0x80) a = (a<<1)^0x1b;
        else a <<= 1;
        b >>= 1;
    }
    return temp;
}

// Mix Columns function
void mixColumns(uint8_t temp_arr[4][4]){
    uint8_t temp[4][4];
    for(int i=0; i<4; i++){
        for(int j=0; j<4; j++){
            temp[i][j] = temp_arr[i][j];
        }
    }
    for(int i=0; i<4; i++){
        temp_arr[0][i] = mul(temp[0][i], 0x02) ^ mul(temp[1][i], 0x03) ^ temp[2][i] ^ temp[3][i];
        temp_arr[1][i] = temp[0][i] ^ mul(temp[1][i], 0x02) ^ mul(temp[2][i], 0x03) ^ temp[3][i];
        temp_arr[2][i] = temp[0][i] ^ temp[1][i] ^ mul(temp[2][i], 0x02) ^ mul(temp[3][i], 0x03);
        temp_arr[3][i] = mul(temp[0][i], 0x03) ^ temp[1][i] ^ temp[2][i] ^ mul(temp[3][i], 0x02);
    }
}

// Inverse Mix Columns function
void inv_mixColumns(uint8_t temp_arr[4][4]){
    uint8_t temp[4][4];
    for(int i=0; i<4; i++){
        for(int j=0; j<4; j++){
            temp[i][j] = temp_arr[i][j];
        }
    }
    for(int i=0; i<4; i++){
        temp_arr[0][i] = mul(temp[0][i], 0x0e) ^ mul(temp[1][i], 0x0b) ^ mul(temp[2][i], 0x0d) ^ mul(temp[3][i], 0x09);
        temp_arr[1][i] = mul(temp[0][i], 0x09) ^ mul(temp[1][i], 0x0e) ^ mul(temp[2][i], 0x0b) ^ mul(temp[3][i], 0x0d);
        temp_arr[2][i] = mul(temp[0][i], 0x0d) ^ mul(temp[1][i], 0x09) ^ mul(temp[2][i], 0x0e) ^ mul(temp[3][i], 0x0b);
        temp_arr[3][i] = mul(temp[0][i], 0x0b) ^ mul(temp[1][i], 0x0d) ^ mul(temp[2][i], 0x09) ^ mul(temp[3][i], 0x0e);
    }
}

// Sub Word function
void subWord(uint8_t temp_arr[4]){
    for(int i=0; i<4; i++){
        temp_arr[i] = sbox(temp_arr[i]);
    }
}

// Key Expansion on key1 and key2
void keyExpansion(unsigned char k1[4][4], unsigned char k2[4][4]){
    // W[0] = k1[0][0], W[1] = k1[1][0], W[2] = k1[2][0], W[3] = k1[3][0]
    for(int i=0; i<4; i++){
        for(int j=0; j<4; j++){
            W[i][j] = k1[j][i];
        }
    }
    for(int i=4; i<8; i++){
        for(int j=0; j<4; j++){
            W[i][j] = k2[j][i-4];
        }
    }

    for(int i=8; i<60; i++){
        uint8_t temp[4] = {W[i-1][0], W[i-1][1], W[i-1][2], W[i-1][3]};
        if(i%8 == 0){
            rotWord(temp);
            subWord(temp);
            temp[0] = temp[0]^rcon[(i/8)-1][0];
            temp[1] = temp[1]^rcon[(i/8)-1][1];
            temp[2] = temp[2]^rcon[(i/8)-1][2];
            temp[3] = temp[3]^rcon[(i/8)-1][3];
        }
        else if(i%8 == 4){
            subWord(temp);
        }
        for(int j=0; j<4; j++){
            W[i][j] = W[i-8][j]^temp[j];
        }
    }

    // printing all words
    /*for(int i=0; i<60; i++){
        for(int j=0; j<4; j++){
            printf("%x ", W[i][j]);
        }
        printf("\n");
        if(i%4 == 3){
            printf("\n");
        }
    }*/
}

// Add Round Key function
void addRoundKey(uint8_t temp_arr[4][4], int round){
    for(int i=0; i<4; i++){
        for(int j=0; j<4; j++){
            temp_arr[i][j] = temp_arr[i][j]^W[round*4 + j][i];
        }
    }
}

// AES Encryption function
void aes_encrypt(uint8_t state[4][4]){
    addRoundKey(state, 0);
    for(int i=1; i<14; i++){
        subBytes(state);
        shiftRows(state);
        mixColumns(state);
        addRoundKey(state, i);
    }
    subBytes(state);
    shiftRows(state);
    addRoundKey(state, 14);
}

// AES Decryption function
void aes_decrypt(uint8_t state[4][4]){
    addRoundKey(state, 14);
    for(int i=13; i>0; i--){
        inv_shiftRows(state);
        inv_subBytes(state);
        addRoundKey(state, i);
        inv_mixColumns(state);
    }
    inv_shiftRows(state);
    inv_subBytes(state);
    addRoundKey(state, 0);
}

// Driver function - Main
int main() {
    GenerateAlpha(a, b);
    printf("\nAlpha: (%d, %d)\n", x, y);

    int nA, nB;
    printf("\nEnter nA (Alice's private key): ");
    scanf("%d", &nA);
    printf("Enter nB (Bob's private key): ");
    scanf("%d", &nB);
    nA = nA%150, nB = nB%150;
    printf("\nnA: %d\n", nA);
    printf("nB: %d\n", nB);

    // Alice's public key
    int xA = x, yA = y;
    int* pkA;
    for (int i=1; i<nA; i++) {
        pkA = ECAdd(x, y, xA, yA);
        xA = pkA[0], yA = pkA[1];
    }
    printf("\nAlice's public key: (%d, %d)\n", xA, yA);

    // Bob's public key
    int xB = x, yB = y;
    int* pkB;
    for (int i=1; i<nB; i++) {
        pkB = ECAdd(x, y, xB, yB);
        xB = pkB[0], yB = pkB[1];
    }
    printf("Bob's public key: (%d, %d)\n", xB, yB);

    // Alice's shared key
    int x1A = xA, y1A = yA;
    int* skA;
    for (int i=0; i<(nA*nB)-nA; i++) {
        skA = ECAdd(x, y, x1A, y1A);
        x1A = skA[0], y1A = skA[1];
    }
    printf("\nAlice's shared key: (%d, %d)\n", x1A, y1A);

    // Bob's shared key
    int x1B = xB, y1B = yB;
    int* skB;
    for (int i=0; i<(nB*nA)-nB; i++) {
        skB = ECAdd(x, y, x1B, y1B);
        x1B = skB[0], y1B = skB[1];
    }
    printf("Bob's shared key: (%d, %d)\n", x1B, y1B); 

    // Calling SHA256 on x1A, y1A
    printf("\nKa: ");
    sha256(x1A, y1A);
    uint32_t Ka[8];
    for(int i=0; i<8; i++) Ka[i] = H[i];
    uint32_t Kb[8];
    for(int i=0; i<8; i++) Kb[i] = H[i];

    printf("\nKb: ");
    for(int i=0; i<8; i++) printf("%x", Kb[i]);

    unsigned char plaintext1[4][4], plaintext2[4][4], ciphertext[4][4], key1[4][4], key2[4][4], temp;
    printf("\n\nEnter the plaintext: ");
    for(int i=0; i<4; i++) {
        for(int j=0; j<4; j++) {
            scanf("%x", &temp);
            plaintext1[j][i] = temp;
        }
    }
    for(int i=0; i<4; i++) {
        for(int j=0; j<4; j++) {
            scanf("%x", &temp);
            plaintext2[j][i] = temp;
        }
    }

    uint32_t Ma[8];
    // storing plaintext1 and plaintext2 in Ma
    for(int i=0; i<4; i++) {
        Ma[i] = plaintext1[0][i]<<24 | plaintext1[1][i]<<16 | plaintext1[2][i]<<8 | plaintext1[3][i];
        Ma[i+4] = plaintext2[0][i]<<24 | plaintext2[1][i]<<16 | plaintext2[2][i]<<8 | plaintext2[3][i];
    }
    printf("\nMa: ");
    for(int i=0; i<8; i++) printf("%x ", Ma[i]);

    // Assigning first 128 bits output of SHA256 to key1
    for(int i=0; i<4; i++) {
        key1[0][i] = (H[i]>>24)&0xff;
        key1[1][i] = (H[i]>>16)&0xff;
        key1[2][i] = (H[i]>>8)&0xff;
        key1[3][i] = H[i]&0xff;
    }

    // Assigning last 128 bits output of SHA256 to key2
    for(int i=0; i<4; i++) {
        key2[0][i] = (H[i+4]>>24)&0xff;
        key2[1][i] = (H[i+4]>>16)&0xff;
        key2[2][i] = (H[i+4]>>8)&0xff;
        key2[3][i] = H[i+4]&0xff;
    }

    // calling key expansion
    keyExpansion(key1, key2);

    // calling aes_encrypt on plaintext1
    aes_encrypt(plaintext1);
    // storing output of plaintext1 in ciphertext
    for(int i=0; i<4; i++) {
        for(int j=0; j<4; j++) ciphertext[i][j] = plaintext1[i][j];
    }
    // xor of plaintext1 and plaintext2
    unsigned char xorArr[4][4];
    for(int i=0; i<4; i++){
        for(int j=0; j<4; j++) plaintext2[i][j] = plaintext1[i][j]^plaintext2[i][j];
    }
    // calling aes_encrypt on xorArr
    aes_encrypt(plaintext2);

    // Decryption of plaintext1
    aes_decrypt(plaintext1);
    aes_decrypt(plaintext2);
    // xor of xorArr and ciphertext
    for(int i=0; i<4; i++){
        for(int j=0; j<4; j++) xorArr[i][j] = plaintext2[i][j]^ciphertext[i][j];
    }
    
    uint32_t one[8];
    one[7]=1;
    uint32_t two[8];
    two[7]=2;

    uint32_t rhs_xor_a[8];
    for(int i=0;i<8;i++){
        rhs_xor_a[i]=Ka[i]^two[i];
    }

    uint32_t lhs_xor_a[8];
    for(int i=0;i<8;i++){
        lhs_xor_a[i]=Ka[i]^one[i];
    }
    
    uint32_t tempHash_a[8];
    sha256_no_padding(rhs_xor_a, Ma, tempHash_a);
    uint32_t MACa[8];
    sha256_no_padding(lhs_xor_a, tempHash_a, MACa);

    uint32_t Mb[8];
    for(int i=0; i<4; i++) {
        Mb[i] = plaintext1[0][i]<<24 | plaintext1[1][i]<<16 | plaintext1[2][i]<<8 | plaintext1[3][i];
        Mb[i+4] = xorArr[0][i]<<24 | xorArr[1][i]<<16 | xorArr[2][i]<<8 | xorArr[3][i];
    }
    printf("\nMb: ");
    for(int i=0; i<8; i++) printf("%x ", Mb[i]);

    uint32_t rhs_xor_b[8];
    for(int i=0;i<8;i++){
        rhs_xor_b[i]=Kb[i]^two[i];
    }
    
    uint32_t lhs_xor_b[8];
    for(int i=0;i<8;i++){
        lhs_xor_b[i]=Kb[i]^one[i];
    }
    
    uint32_t tempHash_b[8];
    sha256_no_padding(rhs_xor_b, Mb, tempHash_b);
    uint32_t MACb[8];
    sha256_no_padding(lhs_xor_b, tempHash_b, MACb);

    // printing MACa and MACb
    printf("\nMACa: ");
    for(int i=0; i<8; i++) printf("%x", MACa[i]);
    printf("\nMACb: ");
    for(int i=0; i<8; i++) printf("%x", MACb[i]);   
}