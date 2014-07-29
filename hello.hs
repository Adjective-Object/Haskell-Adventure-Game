module Main where
    import Data.Map.Strict as Map

    newtype Flag = Flag String
    newtype Item = Item String

    unwrapFlag :: Flag -> String
    unwrapFlag (Flag s) = s
    
    unwrapItem :: Item -> String
    unwrapItem (Item s) = s

    -- using flags and items is superior to a pure DFM approach, I think.
    data Room = Room {
        title :: String,
        description :: [Item] -> [Flag] -> String,
        links :: [Item] -> [Flag] -> Map String Room
        }

    room1 = Room {
        title = "A Damp Dungeon",
        description = (\i f 
            -> "This is a damp room, because this is an adventure game"),
        links = (\i f -> fromList [
            ("THIS ROOM", room1)
        ])
    }

    linkNames :: Room -> [Item] -> [Flag]-> [String]
    linkNames room items flags = (keys ((links room) items flags))

    evalroom :: Room -> [Item] -> [Flag] -> Room
    evalroom room items flags = let lnks = ((links room) items flags) 
                                in do
                                    putStrLn "ugh"
                                    s <- lnks ! ((keys lnks) !! 0)
                                    

    main = evalroom room1
