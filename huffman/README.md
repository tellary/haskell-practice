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
1849068
(5.76 secs, 12,210,660,272 bytes)
```

However, it took 5.76 seconds, and 12 billion bytes were garbage collected along the way.

`zip` compresses the same 2.5 million bytes file from 3.0M to 756K in no time.

Decoding also works, but it's similarly ineffective:

```haskell
λ> S.length . decodeBS $ e
2575373
(4.64 secs, 6,169,753,368 bytes)
λ> f == decodeBS e
True
(4.63 secs, 6,169,754,240 bytes)
```
