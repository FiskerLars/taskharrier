import Text.Printf
import Data.Time
import Control.Monad
import Data.Maybe
import Data.List
import Text.ICalendar
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Default
import qualified Data.Map.Lazy as M
import qualified Data.Text.Lazy as T
import System.Process (system)
import System.Exit
import System.Environment (getArgs)
import System.IO



{-| Call taskharrier <file.ics> 
|-}
-- Todo: collect annotations
main = getArgs >>= (return.head) >>= readFile >>=
       (\vcal -> case parseICalendar (def::DecodingFunctions) "stdin" (B.pack vcal) of
         Right ((cal:_), _) -> return
                               $ intercalate "\n"
                               $ M.foldr (\a -> (:) (veTaskAddStr a ))
                               [] (vcEvents cal) 
         Left err -> error err
       ) >>=
       askUser >>=
       system >>=
       (\ret -> case ret of
         ExitSuccess     -> getChar >> putStrLn "(press return)"
                            >> getChar -- fixme
                            >>= waiter
                            where
                              waiter a | a == 'x' = return ()
                                       | otherwise = putStrLn " "
         ExitFailure err -> error $ "Failure " ++ (show err)
       )

-- FIXME: cannot read a second time from stdin
askUser:: String -> IO String
askUser s =  putStrLn ("execute\n"++ s ++ "\n(y/N)")
             >> getChar
             >>= (\res -> 
                   case res of
                   'y'       -> return s
                   otherwise -> error "User abort"
                 )

veTaskAddStr:: VEvent -> String
veTaskAddStr e = intercalate " " 
                 $ catMaybes
                 [ Just "task add"
                 , ((veDTStart e >>= (return.dtStartDateTimeValue)) >>= (return.veTaskTime))
                 , Just "--"
                 , (veSummary e >>= (return.(T.unpack).summaryValue))
                 , (veLocation e >>= (return.(T.unpack).locationValue))
                 ]  


cet = TimeZone 60 False "CET"
cest = TimeZone 120 True "CEST"

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
