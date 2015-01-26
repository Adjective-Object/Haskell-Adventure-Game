module Main where
    import Data.Map.Strict as Map
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


    stripArticles :: String -> String
    stripArticles command = command

    -- Game logic

    doroombody :: GameState -> IO ()
    doroombody gamestate = do 
            command <- getLine

            if command /= ""
                then putStr("\n")
                else doroombody gamestate
            
            let 
                vrbs :: Map String (String -> GameState -> VerbResponse) 
                vrbs = (verbs (room gamestate))
                
                (cmdName, cmdBody) = breakCommand (stripArticles(command))

                in 
                if cmdName `elem` (keys vrbs) -- backtick makes infix?
                    then let    ret :: VerbResponse
                                ret = (vrbs ! cmdName) cmdBody gamestate
                        in do
                            putStrLn(message ret)
                            if isNothing (newstate ret)
                                then doroombody gamestate 
                                else doroom (fromJust (newstate ret))
                    else do
                            putStrLn ("I don't recognize the verb \"" ++ cmdName ++ "\"")
                            doroom gamestate

    doroom :: GameState -> IO ()
    doroom gamestate = do
                        putStrLn (title (room gamestate))
                        putStrLn ((description (room gamestate)) gamestate)
                        doroombody gamestate
    main :: IO ()
    main = doroom GameState{room=room1, items=[], flags=[]}
