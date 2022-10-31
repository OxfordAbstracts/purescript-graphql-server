module GraphQL.Server.DateTime
  ( decodeDate
  , decodeDateTime
  , decodeTime
  , encodeDate
  , encodeDateTime
  , encodeTime
  ) where

import Prelude

import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Array.NonEmpty as NonEmpty
import Data.Bifunctor (lmap)
import Data.CodePoint.Unicode (isHexDigit)
import Data.Date (Date, canonicalDate, day, month, year)
import Data.DateTime (DateTime(..), adjust)
import Data.Either (Either)
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Foldable (class Foldable, foldl)
import Data.Int (toNumber)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (codePointFromChar)
import Data.String.CodeUnits as CodeUnits
import Data.Time (Time(..))
import Data.Time.Duration (Minutes(..))
import Parsing (Parser, fail, runParser)
import Parsing as P
import Parsing.Combinators (optionMaybe, (<|>))
import Parsing.Combinators.Array (many1)
import Parsing.String (char, eof, satisfy)

encodeDateTime :: DateTime -> Json
encodeDateTime = dateTimeString >>> encodeJson

encodeDate :: Date -> Json
encodeDate = dateString >>> encodeJson

encodeTime :: Time -> Json
encodeTime = timeString >>> encodeJson

decodeDateTime :: Json -> Either JsonDecodeError DateTime
decodeDateTime = runJsonParser isoDateTime

decodeDate :: Json -> Either JsonDecodeError Date
decodeDate = runJsonParser isoDate

decodeTime :: Json -> Either JsonDecodeError Time
decodeTime = runJsonParser isoTime

runJsonParser :: forall a. Parser String a -> Json -> Either JsonDecodeError a
runJsonParser p = decodeJson >=> flip runParser p >>> lmap (show >>> TypeMismatch)

dateString :: Date -> String
dateString d =
  showEnum (year d)
    <> "-"
    <> showEnum (month d)
    <> "-"
    <> showEnum (day d)

timeString :: Time -> String
timeString (Time h m s ms) =
  showEnum h
    <> ":"
    <> showEnum m
    <> ":"
    <> showEnum s
    <> "."
    <> showEnum ms

dateTimeString :: DateTime -> String
dateTimeString (DateTime d t) = dateString d <> "T" <> timeString t

showEnum :: forall b. BoundedEnum b => b -> String
showEnum = show <<< fromEnum

isoDateTime :: Parser String DateTime
isoDateTime = do
  date <- isoDate
  charV 'T'
  time <- isoTime
  tzMay <- optionMaybe isoTz
  let
    resWoTz = DateTime date time
  pure $ fromMaybe resWoTz $ tzMay
    >>= \tz ->
      adjust tz resWoTz

isoDate :: Parser String Date
isoDate = do
  year <- enum "year"
  charV '-'
  month <- enum "month"
  charV '-'
  day <- enum "day"
  pure $ canonicalDate year month day

isoTime :: Parser String Time
isoTime = do
  hours <- enum "hours"
  charV ':'
  minutes <- enum "minutes"
  charV ':'
  seconds <- enum "seconds"
  ms <- optionMaybe $ (charV '.' <|> eof) *> enumTruncated 3 "ms"
  pure $ Time hours minutes seconds (fromMaybe bottom ms)

isoTz :: Parser String Minutes
isoTz = do
  sign <- (char '+' <|> char '-')
  hour <- int
  charV ':'
  minute <- int
  let
    tzInt = minute + hour * 60
  pure $ Minutes $ toNumber if sign == '-' then tzInt else -tzInt

charV :: Char -> Parser String Unit
charV = void <<< char

enum :: forall e. BoundedEnum e => String -> Parser String e
enum fail = int >>= (toEnum >>> maybeFail fail)

enumTruncated :: forall e. BoundedEnum e => Int -> String -> Parser String e
enumTruncated max fail = intTruncated max >>= (toEnum >>> maybeFail fail)

int :: Parser String Int
int = many1 (anyDigit) >>= digitsToInt

intTruncated :: Int -> Parser String Int
intTruncated max = many1 anyDigit <#> NonEmpty.take max >>= digitsToInt

digitsToInt :: forall f. Foldable f => f Char -> Parser String Int
digitsToInt =
  foldl (\s c -> s <> CodeUnits.singleton c) ""
    >>> \str -> case Int.fromString str of
      Nothing -> fail $ "Failed to parse Int from: " <> str
      Just i -> pure i

maybeFail :: forall a. String -> Maybe a -> Parser String a
maybeFail str = maybe (P.fail str) pure

anyDigit :: Parser String Char
anyDigit = satisfy (codePointFromChar >>> isHexDigit)
