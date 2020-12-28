# PLHash

This package defines Hashes which uniquely identify things and self-identify the hashing algorithm used for their own creation.

A Hashable type-class is an interface that describes how types can be Hashed.

By default, `SHA512` is used as the hashing algorithm and hashes are rendered as base58 encoded text.

