module Main where
    import Data.Map.Strict as Map hiding (map)
    import Data.Char as Char
    import Data.Maybe as Maybe

    import GameDatatypes
    import RoomDeclarations

    -- Helper Methods

    breakCommand :: String -> (String, String)
    breakCommand command = let

                    comlower = (Prelude.map toLower command)

                    cmdName :: String
                    cmdName = (words comlower) !! 0

                    catString :: String -> String -> String
                    catString a b = a ++ " " ++ b

                    in if cmdName == comlower -- easier than installing cabal ,_,
                        then (cmdName, "")
                        else (cmdName, init (Prelude.foldr catString "" (drop 1 (words comlower) )) )


    -- TODO strip articles?? what does this even do?
    stripArticles :: String -> String
    stripArticles command = command

    doList :: [IO()] -> IO()
    doList (x:xs) = x >>= (\a -> doList xs)
    doList [] = return ()

    paragraphPrint :: Int -> String -> IO()
    paragraphPrint lineLen paragraph =
        doList $ map putStrLn $ findLines lineLen paragraph

    findLines :: Int -> String -> [String]
    findLines lineLen corpus
        | corpus == "" = []
        | otherwise =
            let (firstline, rest) = splitFirstLine lineLen corpus
            in firstline : findLines 80 rest

    splitFirstLine :: Int -> String -> (String, String)
    splitFirstLine lineLen corpus =
        case (findBreakPoint lineLen 0 "" corpus) of
            Just (str, remainder) -> (str, remainder)
            Nothing               -> (corpus, "")

    findBreakPoint :: Int -> Int -> String -> String -> Maybe (String, String)
    findBreakPoint lineLen charCt line body
        -- failure and success termination cases
        | charCt >= lineLen = Nothing
        | body == [] = Just (line , body)

        -- trim leading whitespace
        | and [head body == ' ', charCt == 0] =
            findBreakPoint lineLen charCt line (drop 1 body)

        -- reset character count on encountering a \n
        | head body == '\n' = findBreakPoint lineLen 0 nextLine nextBody

        -- If we could break here, check if we can break later. If that fails,
        -- use this breakpoint
        | head body == ' ' = case next of
            Nothing -> Just (line, body)
            Just x  -> Just x

        -- step to the next one
        | otherwise = next
        where next = findBreakPoint lineLen (charCt + 1) nextLine nextBody
              nextLine = line ++ take 1 body
              nextBody = drop 1 body


    formatText = paragraphPrint 80

    -- Game logic

    doroombody :: GameState -> IO ()
    doroombody gamestate = do
        command <- getLine

        let roomVerbs :: Map String (String -> GameState -> VerbResponse)
            roomVerbs = (verbs (room gamestate))
            (cmdName, cmdBody) = breakCommand (stripArticles(command))
            successResponse = (roomVerbs ! cmdName) cmdBody gamestate

        if command /= ""
            then putStr("\n")
            else doroombody gamestate

        if cmdName `elem` (keys roomVerbs)
            then do
                formatText $ message successResponse
                if isNothing (newstate successResponse)
                    then doroombody gamestate
                    else doroom (fromJust (newstate successResponse))
            else do
                formatText $
                    "I don't recognize the verb \"" ++ cmdName ++ "\""
                doroom gamestate


    doroom :: GameState -> IO ()
    doroom gamestate = do
                        formatText (title (room gamestate))
                        formatText ((description (room gamestate)) gamestate)
                        doroombody gamestate

    main :: IO ()
    main = doroom GameState{room=room1, items=[], flags=[]}
