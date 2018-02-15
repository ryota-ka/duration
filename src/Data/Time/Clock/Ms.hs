{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Time.Clock.Ms (ms) where

import Control.Applicative ((<|>), liftA2)
import Control.Monad (void)
import Data.Bool (bool)
import Data.Char (isAlpha, toLower, toUpper)
import Data.Semigroup ((<>), sconcat)
import Data.Time.Clock (picosecondsToDiffTime)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter, quoteExp))
import Text.Parsec (char, choice, digit, eof, many1, option, optional, runParser, spaces, try)
import Text.Parsec.String (Parser)

ms :: QuasiQuoter
ms = QuasiQuoter { quoteExp = buildExp }

data Time
    = MS Double
    | S Double
    | M Double
    | H Double
    | D Double
    | W Double
    | Y Double
    deriving (Show)

buildExp :: String -> Q Exp
buildExp str = case runParser msP () "" str of
    Left err -> error $ show err
    Right t -> let ps = toPicoseconds t in [e| picosecondsToDiffTime ps |]

toPicoseconds :: Time -> Integer
toPicoseconds (MS x) = round $ x * 10^9
toPicoseconds (S  x) = round $ x * 10^12
toPicoseconds (M  x) = round $ x * 10^12 * 60
toPicoseconds (H  x) = round $ x * 10^12 * 60 * 60
toPicoseconds (D  x) = round $ x * 10^12 * 60 * 60 * 24
toPicoseconds (W  x) = round $ x * 10^12 * 60 * 60 * 24 * 7
toPicoseconds (Y  x) = round $ x * 10^12 * 60 * 60 * 24 * 365

digits :: Int -> Maybe Int
digits n
    | n > 0 = Just . length . show $ n
    | otherwise = Nothing

decimalP :: Parser Double
decimalP = do
    isNegative <- option False $ const True <$> char '-'
    int <- read <$> option "0" (many1 digit)
    frac <- option 0 $ do
        _ <- char '.'
        n <- many1 digit
        pure $ read n

    let int' = fromIntegral int
        frac' = fromIntegral frac

    pure . bool id negate isNegative $ case digits frac of
        Nothing -> int'
        Just n -> int' + (frac' / 10^n)

withSuffix :: String -> Parser Double
withSuffix suffix = do
    optional spaces
    decimal <- decimalP
    optional spaces
    _ <- stringCI suffix
    optional spaces
    eof
    pure decimal

withSuffixes :: [String] -> Parser Double
withSuffixes = choice . map (try . withSuffix)

stringCI :: String -> Parser String
stringCI = foldr (liftA2 (:) . charCI) $ pure ""
  where
    charCI :: Char -> Parser Char
    charCI c
        | isAlpha c = char (toUpper c) <|> char (toLower c)
        | otherwise = char c

millisecondsP, secondsP, minutesP, hoursP, daysP, weeksP, yearsP :: Parser Time
millisecondsP = MS <$> withSuffixes ["", "ms", "msec", "msecs", "millisecond", "milliseconds"]
secondsP      = S  <$> withSuffixes ["s", "sec", "secs", "second", "seconds"]
minutesP      = M  <$> withSuffixes ["m", "min", "mins", "minute", "minutes"]
hoursP        = H  <$> withSuffixes ["h", "hr", "hrs", "hour", "hours"]
daysP         = D  <$> withSuffixes ["d", "day", "days"]
weeksP        = W  <$> withSuffixes ["w", "week", "weeks"]
yearsP        = Y  <$> withSuffixes ["y", "yr", "yrs", "year", "years"]

msP :: Parser Time
msP = choice $ map try parsers
  where
    parsers =
        [ millisecondsP
        , secondsP
        , minutesP
        , hoursP
        , daysP
        , weeksP
        , yearsP
        ]
