import string

alfabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

def encryptCezar(M, shift):
    alf = alfabet[shift:] + alfabet[:shift]
    dic = {}
    for c in alfabet:
        dic[c] = alf[alfabet.index(c)]

    C = ''
    for c in M :
        C += dic[c]
    return (C)
def decryptCezar(C, shift):
    return encryptCezar(C, -shift)
def BFattackCezar(C):
    for i in range(26):
        print(f"z przesunieciem {i} otrzymano {encryptCezar(C, i)}")
tj = 'CEZARCIPHER'
ct = encryptCezar(tj, 3)

print("tekst janwy", tj)
print("zt", ct)
print("ods", encryptCezar(ct, -3 % 26))
print("cos tam")
print(BFattackCezar(ct))
