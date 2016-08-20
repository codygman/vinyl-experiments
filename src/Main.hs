{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, TypeFamilies, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction, GADTs, TypeSynonymInstances, TemplateHaskell, StandaloneDeriving #-}

module Main where

import Data.Vinyl
import Data.Vinyl.Functor
import Control.Applicative
import Control.Lens hiding (Identity)
import Control.Lens.TH
import Data.Char
import Test.DocTest
import Data.Singletons.TH
import Data.Maybe
import Data.List

data Fields = Id | Name | Age | ActivityName deriving Show

type Person = [Id, Name, Age]
type Activity = [Id, ActivityName]

type family ElF (f :: Fields) :: * where
  ElF Id = Int
  ElF Name = String
  ElF Age = Int
  ElF ActivityName = String

newtype Attr f = Attr { _unAttr :: ElF f }
makeLenses ''Attr
genSingletons [ ''Fields ]
instance Show (Attr Id) where show (Attr x) = "id: " ++ show x
instance Show (Attr Name) where show (Attr x) = "name: " ++ show x
instance Show (Attr Age) where show (Attr x) = "age: " ++ show x
instance Show (Attr ActivityName) where show (Attr x) = "activity: " ++ x

(=::) :: sing f -> ElF f -> Attr f
_ =:: x = Attr x

joy = (SId =:: 1)
   :& (SName =:: "Joy")
   :& (SAge =:: 28)
   :& RNil

jon = (SId =:: 0)
   :& (SName =:: "Jon")
   :& (SAge =:: 23)
   :& RNil

karen = (SId =:: 2)
   :& (SName =:: "Karen")
   :& (SAge =:: 15)
   :& RNil


jonFootball = (SId =:: 0)
           :& (SActivityName =:: "football")
           :& RNil

jonDancing = (SId =:: 0)
           :& (SActivityName =:: "dancing")
           :& RNil

joyRacing = (SId =:: 0)
           :& (SActivityName =:: "racing")
           :& RNil


peopleRows = [joy, jon, karen]
activitieRows = [jonFootball, jonDancing, joyRacing]

printActvy :: (ActivityName ∈ fields) => Rec Attr fields -> IO ()
printActvy r = print (r ^. rlens SActivityName)

isInPplIdx :: (Id ∈ fields) => [Int] -> Rec Attr fields -> Bool
isInPplIdx peopleIdx actvyRow =  any (== True) . map (== 1) $ peopleIdx
  where actvyIdInt = actvyRow ^. rlens SId . unAttr

-- joinOnId :: (Id ∈ fields) => [Rec Attr fields] -> [Rec Attr fields] -> Rec Attr fields
joinOnId :: (Id ∈ fields) => [Rec Attr fields] -> [Rec Attr fields] -> IO ()
joinOnId people activities = do
  let peopleIdx =(\r -> r ^. rlens SId . unAttr) <$> people
      filteredActivites = filter (isInPplIdx peopleIdx) activities
  print filteredActivites 

-- main :: IO ()
-- main = do
--   putStrLn "hello world"
