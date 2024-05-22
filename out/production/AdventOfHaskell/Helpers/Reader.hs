module Helpers.Reader (fileRead) where

fileRead :: FilePath -> IO String
fileRead filePath = do
  content <- readFile filePath
  return $ content 


