# ms

Use this package to easily and safely convert various time formats to `DiffTime` at compile time.

## Examples

```haskell
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Time.Clock.Ms (ms)

main :: IO ()
main = do
    print [ms| 2 days |]  -- 172800s
    print [ms| 1d |]      -- 86400s
    print [ms| 10h |]     -- 36000s
    print [ms| 2.5 hrs |] -- 9000s
    print [ms| 2h |]      -- 7200s
    print [ms| 1m |]      -- 60s
    print [ms| 5s |]      -- 5s
    print [ms| 1y |]      -- 31536000s
    print [ms| 100 |]     -- 0.1s
    print [ms| -3 days |] -- -259200s
    print [ms| -1h |]     -- -3600s
    print [ms| -200 |]    -- -0.2s
```
