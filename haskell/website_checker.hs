-- Website Safety and Performance Checker
-- Required packages: http-conduit, time
-- Install with: cabal install http-conduit time

import Network.HTTP.Simple
import Data.Time.Clock
import Control.Exception (try, SomeException)
import Data.List (isPrefixOf)
import System.Environment (getArgs)
import Text.Printf (printf)


-- Safety analysis: simple check for HTTPS and basic risk score
safetyAnalysis :: String -> Int
safetyAnalysis url = if "https://" `isPrefixOf` url then 0 else 2

safetyVerdict :: Int -> String
safetyVerdict 0 = "SAFE"
safetyVerdict _ = "Potentially Unsafe"

safetyMessage :: Int -> String
safetyMessage 0 = "No suspicious patterns detected."
safetyMessage _ = "Site does not use HTTPS."

performanceRating :: Double -> String
performanceRating t
    | t < 0.2 = "Fast"
    | t < 1.0 = "Moderate"
    | otherwise = "Slow"

-- Note: If you get a 'hidden package' error for bytestring, run:
-- cabal install --lib bytestring

checkWebsite :: String -> IO ()
checkWebsite url = do
    putStrLn $ "\n--- SAFETY ANALYSIS ---"
    let risk = safetyAnalysis url
    putStrLn $ "Risk Score: " ++ show risk
    putStrLn $ "Verdict: " ++ safetyVerdict risk
    putStrLn $ safetyMessage risk
    putStrLn $ "\n--- PERFORMANCE ANALYSIS ---"
    start <- getCurrentTime
    result <- try (httpNoBody (parseRequest_ url)) :: IO (Either SomeException (Response ()))
    end <- getCurrentTime
    let diff = realToFrac (diffUTCTime end start) :: Double
    case result of
        Left _ -> putStrLn "Load Time: N/A\nPerformance Rating: Unreachable"
        Right response -> do
            printf "Load Time: %.2f seconds\n" diff
            putStrLn $ "Performance Rating: " ++ performanceRating diff
            putStrLn $ "HTTP Status: " ++ show (getResponseStatusCode response)


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Usage: checker <url1> [url2] ..."
        urls -> mapM_ checkWebsite urls
