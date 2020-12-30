{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Kubernetes where

import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString as BS
import qualified Data.Set as S
import qualified Data.Text as T
import Text.RawString.QQ (r)

pods :: BS.ByteString
pods =
  [r|
{
  "kind": "List",
  "apiVersion": "v1",
  "items": [
    {
      "kind": "Pod",
      "apiVersion": "v1",
      "metadata": {
        "name": "redis-h315w",
        "creationTimestamp": "2019-03-23T19:42:21Z",
        "labels": {
          "name": "redis",
          "region": "usa"
        }
      },
      "spec": {
        "containers": [
          {
            "name": "redis",
            "image": "redis",
            "ports": [
              {
                "name": "redis",
                "hostPort": 27017,
                "containerPort": 27017,
                "protocol": "TCP"
              }
            ],
            "resources": {
              "requests": {
                "cpu": "100m"
              }
            }
          }
        ]
      }
    },
    {
      "kind": "Pod",
      "apiVersion": "v1",
      "metadata": {
        "name": "web-4c5bj",
        "creationTimestamp": "2019-02-24T20:23:56Z",
        "labels": {
          "name": "web",
          "region": "usa"
        }
      },
      "spec": {
        "containers": [
          {
            "name": "web",
            "image": "server",
            "ports": [
              {
                "name": "http-server",
                "containerPort": 3000,
                "protocol": "TCP"
              }
            ],
            "resources": {
              "requests": {
                "cpu": "100m"
              }
            }
          }
        ]
      }
    }
  ]
}
|]

-- Exercises 15.6

-- 1. Your first task should be mostly straightforward: get the api version which was used to make the call.

a = pods ^? key "apiVersion" . _String

-- 2. Next, count the number of all containers across all pods. You can assume that every element of “items” is a pod.

b = lengthOf (key "items" . values . key "spec" . key "containers") pods

-- Return the “name” (as Text) of all containers which have the same value for their “image” and “name” fields.

c =
  pods
    ^.. key "items"
      . values
      . key "spec"
      . key "containers"
      . values
      . filtered (\v -> v ^? key "name" == v ^? key "image")
      . key "name"
      . _String

-- Collect a list of all “containerPort”s alongside their Pod’s “metadata > name”.

d =
  pods
    ^@.. key "items"
      . values
      . reindexed (^. key "metadata" . key "name" . _String) selfIndex
    <. key "spec"
      . key "containers"
      . values
      . key "ports"
      . values
      . key "containerPort"
      . _Number

-- Uppercase the label values inside each pod’s metadata

e =
  pods
    & key "items"
      . values
      . key "metadata"
      . key "labels"
      . members
      . _String
    %~ T.toUpper

-- Set a resource request of memory: "256M" for every container.

f =
  pods
    & key "items"
      . values
      . key "spec"
      . key "containers"
      . values
      . key "resources"
      . key "requests"
      . _Object
      . at "memory"
    ?~ "256M"

-- Get a Set of all metadata label keys used in the response.

g = S.fromList (pods ^.. key "items" . values . key "metadata" . members . asIndex)

-- Set the hostPort to 8080 on any “port” descriptions where it is unset.

h =
  pods
    & key "items"
      . values
      . key "spec"
      . key "containers"
      . values
      . key "port"
      . values
      . _Object
      . at "hostPort"
      . filteredBy _Nothing
    ?~ _Number # 8080

-- Prepend the region to the name of each container.

i =
  pods
    & key "items"
      . values
      . reindexed (view (key "metadata" . key "labels" . key "region" . _String)) selfIndex
    <. (key "spec" . key "containers" . values . key "name" . _String)
    %@~ (\region name -> region <> "-" <> name)
