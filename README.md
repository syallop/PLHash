# PLHash - experimental

This package defines `Hash`es which:
- Uniquely identify some hashed value within it's type
- Self-describe the algorithm used to perform the hash
- May be shortened against other `Hash`es to produce the 'shortest unambiguous
  hash'.

This is currently used by the [PL](https://github.com/syallop/PL) project for
storing content-addressed programs that can be referenced with wieldy names.

