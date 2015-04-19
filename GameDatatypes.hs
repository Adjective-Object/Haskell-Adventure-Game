module GameDatatypes where
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
