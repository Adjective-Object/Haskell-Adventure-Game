module Main where
    import Data.Map.Strict as Map
    import Data.Char as Char
    import Data.Maybe as Maybe

    newtype Flag = Flag String deriving (Eq, Show)
    newtype Item = Item String deriving (Eq, Show)
    
    unwrapFlag :: Flag -> String
    unwrapFlag (Flag s) = s
    
    unwrapItem :: Item -> String
    unwrapItem (Item s) = s

    data GameState = GameState{
        room :: Room,
        items :: [Item],
        flags :: [Flag]
    }

    -- using flags and items is superior to a pure DFM approach, I think.
    data Room = Room {
        title :: String,
        description :: GameState -> String,
        verbs :: Map String (String -> GameState -> Maybe GameState) -- maps VERB strings to change in gamestate
        }

    goRoom :: GameState -> Room -> GameState
    goRoom instate newroom = GameState{
                                    flags = flags instate,
                                    items = items instate,
                                    room = newroom}

    makeGoList :: [(String, Room)] -> Map String (String -> GameState -> Maybe GameState)
    makeGoList largs = let funcs = fromList([
                            (fst pair, 
                                (\ cmdin instate -> 
                                    goRoom instate (snd pair) 
                                )
                            ) | pair <- largs ])
                        in fromList[ ("go", 
                            (\ cmdstr gamestate -> 
                            if cmdstr `elem` keys funcs
                                then Just ((funcs ! cmdstr) cmdstr gamestate)
                                else Nothing
                        )) ]

    mergeVerbList :: [(String, Room)] -> [(String, Room)] -> [(String, Room)]
    mergeVerbList l1 l2 = l1 -- TODO merging verb lists

    room1 = Room {
        title = "A Damp Dungeon"
        ,description = (\g
            -> "This is a damp dungeon-y starting room, \
                \because this is an adventure game, \
                \and that is the sort of thing adventure games do. "
                ++ if (Flag "starting_lever") `elem` (flags g)
                    then "The lever you pulled is still in \
                        \the middle of the floor."
                    else "There is a lever in the middle of the floor, \
                        \beckoning you to pull it."
                    
        )
        ,verbs = makeGoList([
                ("this room", room1 ) -- shorthand that makes links between rooms under the go command
            ])
    }

    doroombody :: GameState -> IO ()
    doroombody gamestate = do 
            command <- getLine

            if command /= ""
                then putStr("\n")
                else putStr("")
            
            let 
                vrbs :: Map String (String -> GameState -> Maybe GameState) 
                vrbs = (verbs (room gamestate))
                
                comlower = (Prelude.map toLower command)

                cmdName :: String
                cmdName = (words comlower) !! 0

                catString :: String -> String -> String
                catString a b = a ++ " " ++ b 

                cmdBody :: String
                cmdBody = (Prelude.foldr catString "" (drop 1 (words command)) )

                in 
                if cmdName `elem` (keys vrbs) -- backtick makes infix?
                    then let ret = (vrbs ! command) cmdBody gamestate
                        in if isNothing ret
                            then do putStr("That went wrong...")
                                    doroom gamestate
                            else doroom (fromJust ret)
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
