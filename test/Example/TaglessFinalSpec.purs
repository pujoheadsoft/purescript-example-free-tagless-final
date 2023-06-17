module Test.Example.TaglessFinalSpec where

import Prelude

import Control.Monad.State (StateT, modify_, runStateT)
import Data.Tuple (Tuple, snd)
import Effect.Aff (Aff)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Example.TaglessFinal (class Presenter, class UserRepository, displayTrialStatus, findUserOptionByUserId)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

newtype UserRepositoryAff a = UserRepositoryAff (Aff a)
derive newtype instance functorUserRepositoryAff :: Functor UserRepositoryAff
derive newtype instance applyUserRepositoryAff :: Apply UserRepositoryAff
derive newtype instance applicativeUserRepositoryAff :: Applicative UserRepositoryAff
derive newtype instance bindUserRepositoryAff :: Bind UserRepositoryAff
derive newtype instance monadUserRepositoryAff :: Monad UserRepositoryAff

instance userRepositoryForSpec :: UserRepository UserRepositoryAff where
  findUserById id = do
    assert "userId" id
    (UserRepositoryAff $ pure {id: id, name: "userName", optionId: "optionId"})

  findOptionById id = do
    assert "optionId" id
    UserRepositoryAff $ pure {optionId: id, isTrial: true}

runRepository :: forall a. UserRepositoryAff a -> Aff a
runRepository (UserRepositoryAff a) = a



type DisplayState = {message :: String}
newtype PresenterAff a = PresenterAff (StateT DisplayState Aff a)
derive newtype instance functorPresenterAff :: Functor PresenterAff
derive newtype instance applyPresenterAff :: Apply PresenterAff
derive newtype instance applicativePresenterAff :: Applicative PresenterAff
derive newtype instance bindPresenterAff :: Bind PresenterAff
derive newtype instance monadPresenterAff :: Monad PresenterAff

instance presenterForSpec :: Presenter PresenterAff where
  display message = PresenterAff do
    modify_ \_ -> { message: message }
    pure unit

runPresenter :: forall a. DisplayState -> PresenterAff a -> Aff (Tuple a DisplayState)
runPresenter s (PresenterAff m) = runStateT m s



assert :: forall a m. Monad m => Show a => Eq a => a -> a -> m Unit
assert expected actual =
  if expected /= actual then
    unsafePerformEffect $ throw ("expected: " <> show expected <> ", but was: " <> show actual)
  else
    pure unit

spec :: Spec Unit
spec = do
  describe "Tagless Final Spec" do
    describe "UserRepositryのテスト" do
      it "指定したIDのユーザーのオプションを取得することができる" do
        option <- runRepository $ findUserOptionByUserId "userId"
        option `shouldEqual` {optionId: "optionId", isTrial: true}

    describe "Presenterのテスト" do
      it "トライアルユーザーかどうかを出力する - トライアルユーザーの場合" do
        result <- displayTrialStatus {optionId: "optionId", isTrial: true} 
          # runPresenter {message: ""}
        (snd result).message `shouldEqual` "This User is Trial Account."

      it "トライアルユーザーかどうかを出力する - トライアルユーザでない場合" do
        result <- displayTrialStatus {optionId: "optionId", isTrial: false} 
          # runPresenter {message: ""}
        (snd result).message `shouldEqual` "This User is Premium Account."
