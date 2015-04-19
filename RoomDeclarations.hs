module RoomDeclarations where
    import GameDatatypes
    import Data.Map.Strict as Map
    import Data.Char as Char
    import Data.Maybe as Maybes

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
                                else failVerb gamestate ("Cannot go \"" ++ cmdstr ++"\".")
                            )) ]

    addFlag :: GameState -> Flag -> GameState
    addFlag g f = GameState{
            room = room g,
            items = items g,
            flags = (flags g) ++ [f]
        }

    addVerb :: Map String (String -> GameState -> VerbResponse) -> (String -> GameState -> VerbResponse) -> Map String (String -> GameState -> VerbResponse)
    addVerb list newitems = list --TODO imlement this shiznit

    -- Room declarations

    room1 = Room {
        title = "A Damp Dungeon"
        ,description = (\g
            -> "This is a damp dungeon-y starting room, \
                \because this is an adventure game, \
                \and that is the sort of thing adventure games do. "
                ++ if (Flag "starting_lever") `elem` (flags g)
                    then "The lever you pulled is still in \
                        \the middle of the floor. Also a hidden door opened up to the SOUTH now"
                    else "There is a lever in the middle of the floor, \
                        \beckoning you to pull it."
                    
        )
        ,verbs = (fromList [
                    ("pull", (\ com state -> 
                        if com == "lever"
                            then if not ((Flag "starting_lever") `elem` (flags state))
                                then VerbResponse{newstate=Just (
                                    addFlag state (Flag "starting_lever")), 
                                    message="You pull the lever.\n\
                                    \You hear a grinding sound come from behind you."}
                                else if (Flag "starting_lever_2") `elem` (flags state)
                                    then VerbResponse{newstate=Nothing, 
                                            message="You gave up on that shit, remember?"}
                                    else VerbResponse{newstate=Just(addFlag state (Flag "starting_lever_2")), 
                                            message="You try to pull the lever back, but it seems stuck. You give up on trying to pull the lever."}
                            else failVerb state ("cannot pull \"" ++ com ++ "\".")
                        ))
                ])
    }