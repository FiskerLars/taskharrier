import Text.Printf
import Data.Time
import Data.Maybe
import Data.List
import Text.ICalendar
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Default
import qualified Data.Map.Lazy as M


invite = intercalate "\n" ["BEGIN:VCALENDAR",
                           "PRODID:-//Google Inc//Google Calendar 70.9054//EN",
                           "VERSION:2.0",
                           "CALSCALE:GREGORIAN",
                           "METHOD:REQUEST",
                           "BEGIN:VEVENT",
                           "DTSTART:20150902T120000Z",
                           "DTEND:20150902T140000Z",
                           "DTSTAMP:20150820T124425Z",
                           "ORGANIZER;CN=Carsten Bormann:mailto:cabocabo@gmail.com",
                           "UID:C08F26B2-E8D5-4DC5-8625-B21B173F4D08",
                           "ATTENDEE;CUTYPE=INDIVIDUAL;ROLE=REQ-PARTICIPANT;PARTSTAT=NEEDS-ACTION;RSVP=",
                           " TRUE;CN=Lars Fischer;X-NUM-GUESTS=0:mailto:fischer@wiwi.uni-siegen.de",
                           "ATTENDEE;CUTYPE=INDIVIDUAL;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;RSVP=TRUE",
                           " ;CN=Carsten Bormann;X-NUM-GUESTS=0:mailto:cabocabo@gmail.com",
                           "CREATED:20150820T124424Z",
                           "DESCRIPTION:View your event at https://www.google.com/calendar/event?action",
                           " =VIEW&eid=XzhjbzNnaGhpNnAxMzRiYTU3MTIzYWI5azhoMWphYjlvNm9wM2FiYTI2OG9rNGM5b",
                           " jZkMzM4aDFnNzAgZmlzY2hlckB3aXdpLnVuaS1zaWVnZW4uZGU&tok=MTgjY2Fib2NhYm9AZ21h",
                           " aWwuY29tMzQ4ZjEzMDY0N2JiZGY3MDVjNjg2MTIxZDAzOWVjODBiZjQ3NDQxMw&ctz=UTC&hl=e",
                           " n.",
                           "LAST-MODIFIED:20150820T124425Z",
                           "LOCATION:MZH5180",
                           "SEQUENCE:0",
                           "STATUS:CONFIRMED",
                           "SUMMARY:Lars Fischer/cabo",
                           "TRANSP:OPAQUE",
                           "END:VEVENT", 
                           "END:VCALENDAR"]


-- Todo: read from stdin
-- Todo: execute task
-- Todo: gather summary+location as description
-- Todo: collect annotations
main = case parseICalendar (def::DecodingFunctions) "stdin" (B.pack invite ) of
  Right ((cal:_), _) -> putStrLn
                        $ intercalate "\n"
                        $ M.foldr (\a -> (:) (fromMaybe "noEvent" $ veTaskAddStr a ))
                        [] (vcEvents cal) 
  Left err -> putStrLn err



veTaskAddStr:: VEvent -> Maybe String
veTaskAddStr = (>>= return.((++) "task add ").veTaskTime).(>>= return.dtStartDateTimeValue).veDTStart


cet = TimeZone 1 False "CET"
cest = TimeZone 2 True "CEST"

veTaskTime:: DateTime -> String
veTaskTime dt = (++) "rc.datetime:Y-M-DTH:N:S due:"
                $ localDateTimeToYMYHNS
                $ case dt of
                   FloatingDateTime x -> x
                   UTCDateTime x -> utcToLocalTime cest x
                   ZonedDateTime x y -> error $ "zoned time not implemented "
                                        ++ (show $ ZonedDateTime x y)
                where
                  localDateTimeToYMYHNS dt =  intercalate "T"
                                              $ map (\f -> f dt)
                                              [ (showGregorian.localDay)
                                              , (timeToHNS.localTimeOfDay)]
                    where timeToHNS t = intercalate ":" 
                                        $ map (\f -> ((printf "%02d").f) t)
                                        [ todHour 
                                        , todMin 
                                        , (\_ -> 0) ] -- FIXME Pico -> Int ??
