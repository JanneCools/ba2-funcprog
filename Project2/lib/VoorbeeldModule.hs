module VoorbeeldModule
  ( hoi -- oplijsting van de publieke functies - als je deze lijst en de haakjes weglaat, wordt alles publiek
  , dag
  ) where

hoi :: String
hoi = "Hoi"

dag :: String
dag = "Niet " ++ nacht

nacht :: String
nacht = "Nacht"


