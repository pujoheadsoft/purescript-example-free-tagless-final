module Example.TaglessFinal where

import Prelude

import Example.Domain (User, UserOption)

class UserRepository m where
  findUserById :: String -> m User
  findOptionById :: String -> m UserOption

findUserOptionByUserId :: forall m. Monad m => UserRepository m => String -> m UserOption
findUserOptionByUserId userId = do
  user <- findUserById userId
  findOptionById user.optionId


class Presenter m where
  display :: String -> m Unit

displayTrialStatus :: forall m. Monad m => Presenter m => UserOption -> m Unit
displayTrialStatus option = display $ "This User is " <> if option.isTrial then "Trial Account." else "Premium Account."


findUserWithDisplayTrialStatus 
  :: forall m
   . Monad m
   => UserRepository m 
   => Presenter m 
   => String 
   -> m Unit
findUserWithDisplayTrialStatus userId = do
  option <- findUserOptionByUserId userId
  displayTrialStatus option