module Main where
    import Data.Map.Strict as Map
    import Data.Char as Char
    import Data.Maybe as Maybe

    -- newtype declarations and interfaces

    newtype Flag = Flag String deriving (Eq, Show)
    newtype Item = Item String deriving (Eq, Show)

    -- Data Declaraions

    data VerbResponse = VerbResponse{
        message :: String,
        newstate :: Maybe GameState
    }
    -- using flags and items is superior to a pure DFM approach, I think.
    
    data GameState = GameState{
        room :: Room
        ,items :: [Item]
        ,flags :: [Flag]
    }

    data Room = Room {
        title :: String
        ,description :: GameState -> String
        ,verbs :: Map String (String -> GameState -> VerbResponse) -- maps VERB strings to change in gamestate
    }

    -- verb helpers

    changeRoom :: GameState -> Room -> GameState
    changeRoom g r = GameState{
            room = r,
            items = items g,
            flags = flags g
        } 

    failVerb :: GameState -> String -> VerbResponse
    failVerb state msg = VerbResponse{message=msg, newstate=Nothing} 

    goRoom :: GameState -> Room -> GameState
    goRoom instate newroom = GameState{
                                    flags = flags instate,
                                    items = items instate,
                                    room = newroom}

    makeGoList :: [(String, Room)] -> Map String (String -> GameState -> VerbResponse)
    makeGoList largs = let funcs = fromList([
                            (fst pair, 
                                (\ cmdin instate -> 
                                    goRoom instate (snd pair) 
                                )
                            ) | pair <- largs ])
                        in fromList[ ("go", 
                            (\ cmdstr gamestate -> 
                            if cmdstr `elem` keys funcs
                                then VerbResponse{newstate=Just ((funcs ! cmdstr) cmdstr gamestate), message=""}
                                else failVerb gamestate ("Cannot go to target \"" ++ cmdstr ++"\".")
                            )) ]

    addFlag :: GameState -> Flag -> GameState
    addFlag g f = GameState{
            room = room g,
            items = items g,
            flags = (flags g) ++ [f]
        }

    -- Room declarations

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
        ,verbs = union
            (makeGoList [
                ("this room", room1) -- shorthand that makes custom go command
            ])

            (fromList [
                ("pull", (\ com state -> 
                    if com == "lever"
                        then VerbResponse{newstate=Just (addFlag state (Flag "starting_lever")), message="you pull the lever."}
                        else failVerb state ("cannot pull \"" ++ com ++ "\".")
                    ))
            ])
    }

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
                
                comlower = (Prelude.map toLower command)

                cmdName :: String
                cmdName = (words comlower) !! 0

                catString :: String -> String -> String
                catString a b = a ++ " " ++ b 

                cmdBody :: String
                cmdBody = init (Prelude.foldr catString "" (drop 1 (words comlower) ))

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
