Huffman compression implementation in Haskell
=============================================

The following interactive session shows that compression works.
A 2.5 million bytes string is compressed down to 1.8 million bytes:

```haskell
λ> :l huffmanCodes.hs
[1 of 1] Compiling Main             ( huffmanCodes.hs, interpreted )
Ok, one module loaded.
Collecting type info for 1 module(s) ... 
λ> f <- S.readFile "..."
λ> S.length f
2575373
λ> :set +s
λ> e = encodeBS f
(0.02 secs, 0 bytes)
λ> S.length . encodedData $ e
1812385
(5.71 secs, 12,075,504,016 bytes)
```

However, it took 5.76 seconds, and 12 billion bytes were garbage collected along the way.

`zip` compresses the same 2.5 million bytes file from 3.0M to 756K in no time.

Decoding also works, but it's similarly ineffective:

```haskell
λ> S.length . decodeBS $ e
2575684
(4.32 secs, 6,056,021,200 bytes)
λ> f == decodeBS e
True
```
