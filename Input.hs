module Input where

type Time = Float

data Event = Event Time Time
             deriving Show

type Events = [Event]
