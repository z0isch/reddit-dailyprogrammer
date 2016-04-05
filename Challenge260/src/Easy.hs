module Easy where
data DoorState = Closed | Open | Closing | Opening | StoppedClosing | StoppedOpening
  deriving (Show, Eq, Read, Ord)

data Command = Click | Complete
  deriving (Show, Eq, Read)

processCommand :: DoorState -> Command  -> DoorState
processCommand Closed Click = Opening
processCommand Open Click = Closing
processCommand Closing Click = StoppedClosing
processCommand Opening Click = StoppedOpening
processCommand StoppedClosing Click  = Opening
processCommand StoppedOpening Click = Closing
processCommand Opening Complete = Open
processCommand Closing Complete = Closed
processCommand s Complete = s

input = [Click,Complete,Click,Click,Click,Click,Click,Complete]
