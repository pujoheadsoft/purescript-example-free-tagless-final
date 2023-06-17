module Test.Example.TaglessFinalWithTSpec where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Effect.Aff (Aff)
import Example.Domain (User, UserOption)
import Example.TaglessFinalWithT (class Presenter, class UserRepository, displayTrialStatus, findUserOptionByUserId, findUserWithDisplayTrialStatus)
import Test.PMock (any, fun, mock, mockFun, verify, (:>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type UserRepositoryFunctions = {
  findUserById :: String -> User,
  findOptionById :: String -> UserOption
}

newtype UserRepositoryMockT a = UserRepositoryMockT (ReaderT UserRepositoryFunctions Aff a)
derive newtype instance functorUserRepositoryMockT :: Functor UserRepositoryMockT
derive newtype instance applyUserRepositoryMockT :: Apply UserRepositoryMockT
derive newtype instance applicativeUserRepositoryMockT :: Applicative UserRepositoryMockT
derive newtype instance bindUserRepositoryMockT :: Bind UserRepositoryMockT
derive newtype instance monadUserRepositoryMockT :: Monad UserRepositoryMockT

instance userRepositoryMockT :: UserRepository UserRepositoryMockT where
  findUserById userId = UserRepositoryMockT do
    f <- ask
    pure $ f.findUserById userId
  findOptionById optionId = UserRepositoryMockT do
    f <- ask
    pure $ f.findOptionById optionId

runRepository :: UserRepositoryFunctions -> UserRepositoryMockT ~> Aff
runRepository f (UserRepositoryMockT m) = runReaderT m f

type DisplayFunctions = {
  display :: String -> Unit
}

newtype PresenterMockT a = PresenterMockT (ReaderT DisplayFunctions Aff a)
derive newtype instance functorPresenterMockT :: Functor PresenterMockT
derive newtype instance applyPresenterMockT :: Apply PresenterMockT
derive newtype instance applicativePresenterMockT :: Applicative PresenterMockT
derive newtype instance bindPresenterMockT :: Bind PresenterMockT
derive newtype instance monadPresenterMockT :: Monad PresenterMockT

instance presenterMockT :: Presenter PresenterMockT where
  display message = PresenterMockT do
    f <- ask
    pure $ f.display message

runPresenter :: DisplayFunctions -> PresenterMockT ~> Aff
runPresenter f (PresenterMockT m) = runReaderT m f


type UserRepositoryPresenterFunctions = {
  findUserById :: String -> User,
  findOptionById :: String -> UserOption,
  display :: String -> Unit
}
newtype UserRepositoryPresenterMockT a = UserRepositoryPresenterMockT (ReaderT UserRepositoryPresenterFunctions Aff a)

derive newtype instance functorUserRepositoryPresenterMockT :: Functor UserRepositoryPresenterMockT
derive newtype instance applyUserRepositoryPresenterMockT :: Apply UserRepositoryPresenterMockT
derive newtype instance applicativeUserRepositoryPresenterMockT :: Applicative UserRepositoryPresenterMockT
derive newtype instance bindUserRepositoryPresenterMockT :: Bind UserRepositoryPresenterMockT
derive newtype instance monadUserRepositoryPresenterMockT :: Monad UserRepositoryPresenterMockT

instance userRepositoryUserRepositoryPresenterMockT :: UserRepository UserRepositoryPresenterMockT where
  findUserById userId = UserRepositoryPresenterMockT do
    f <- ask
    pure $ f.findUserById userId
  findOptionById optionId = UserRepositoryPresenterMockT do
    f <- ask
    pure $ f.findOptionById optionId

instance presenterUserRepositoryPresenterMockT :: Presenter UserRepositoryPresenterMockT where
  display message = UserRepositoryPresenterMockT do
    f <- ask
    pure $ f.display message

runComposeFunctions :: UserRepositoryPresenterFunctions -> UserRepositoryPresenterMockT ~> Aff
runComposeFunctions f (UserRepositoryPresenterMockT m) = runReaderT m f

spec :: Spec Unit
spec = do
  describe "Tagless Final With Monad Transformer Spec" do
    describe "UserRepositryのテスト" do
      it "指定したIDのユーザーのオプションを取得することができる" do
        let
          functions = {
            findUserById: mockFun $ "userId" :> {id: "userId", name: "userName", optionId: "optionId"},
            findOptionById: mockFun $ "optionId" :> {optionId: "optionId", isTrial: true}
          }
        option <- findUserOptionByUserId "userId"
                  # runRepository functions 
        option `shouldEqual` {optionId: "optionId", isTrial: true}
    
    describe "Presenterのテスト" do
      it "トライアルユーザーかどうかを出力する - トライアルユーザーの場合" do
        let
          displayMock = mock $ any :> unit
        displayTrialStatus {optionId: "optionId", isTrial: true} 
          # runPresenter { display: fun displayMock } 
        verify displayMock "This User is Trial Account."

      it "トライアルユーザーかどうかを出力する - トライアルユーザでない場合" do
        let
          displayMock = mock $ any :> unit
        displayTrialStatus {optionId: "optionId", isTrial: false}
          # runPresenter { display: fun displayMock } 
        verify displayMock "This User is Premium Account."
    
    describe "UserRepository + Presenterのテスト" do
      it "指定したIDのユーザーが、トライアルユーザーかどうかを出力する - トライアルユーザーの場合" do
        let
          displayMock = mock $ any :> unit
          functions = {
            findUserById: mockFun $ "userId" :> {id: "userId", name: "userName", optionId: "optionId"},
            findOptionById: mockFun $ "optionId" :> {optionId: "optionId", isTrial: true},
            display: fun displayMock
          }
        findUserWithDisplayTrialStatus "userId" 
          # runComposeFunctions functions
        verify displayMock "This User is Trial Account."

      it "指定したIDのユーザーが、トライアルユーザーかどうかを出力する - トライアルユーザでない場合" do
        let
          displayMock = mock $ any :> unit
          functions = {
            findUserById: mockFun $ "userId" :> {id: "userId", name: "userName", optionId: "optionId"},
            findOptionById: mockFun $ "optionId" :> {optionId: "optionId", isTrial: false},
            display: fun displayMock
          }
        findUserWithDisplayTrialStatus "userId" 
          # runComposeFunctions functions
        verify displayMock "This User is Premium Account."
