module Main where
    import Data.Map.Strict as Map

    newtype Flag = Flag String
    newtype Item = Item String

    unwrapFlag :: Flag -> String
    unwrapFlag (Flag s) = s
    
    unwrapItem :: Item -> String
    unwrapItem (Item s) = s

    data GameState = GameState{
        items :: [Item],
        flags :: [Flag]
    }

    -- using flags and items is superior to a pure DFM approach, I think.
    data Room = Room {
        title :: String,
        description :: GameState -> String,
        links :: GameState -> Map String (Room,GameState)
        }

    room1 = Room {
        title = "A Damp Dungeon",
        description = (\g
            -> "This is a damp dungeon-y starting room, because this is an adventure game"),
        links = (\g -> fromList [
            ("THIS ROOM", (room1, g))
        ])
    }

    linkNames :: Room -> GameState-> [String]
    linkNames room gamestate = (keys ((links room) gamestate))

    doroombody :: Room -> GameState -> IO ()
    doroombody room gamestate = 
        let lnks = ((links room) gamestate)                            
        in do 
            putStrLn ("Exits are:"
                ++ (Prelude.foldr 
                    (\x y -> y ++ " and " ++ x) 
                    ""
                    (keys lnks)))
            command <- getLine

            if command /= ""
                then putStr("\n")
                else putStr("")

            if command `elem` (keys lnks) -- backtick makes infix?
                then let ret = (lnks ! command)
                    in doroom (fst ret) (snd ret) 
                else do
                    putStrLn ("I don't recognize the command \"" ++ command ++ "\"")
                    doroom room gamestate

    doroom :: Room -> GameState -> IO ()
    doroom room gamestate = do
                                putStrLn (title room)
                                putStrLn ((description room) gamestate)
                                doroombody room gamestate
    main :: IO ()
    main = doroom room1 GameState{items=[], flags=[]}
