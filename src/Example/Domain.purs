module Example.Domain where

import Prelude


type User = { id :: String, name :: String, optionId :: String }

type UserOption = { optionId :: String, isTrial :: Boolean }

