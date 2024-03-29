module Entry exposing (..)

import Jwt.Http 
import Html.Attributes exposing (..)
import Http
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Html exposing (..)
import String
import List
import Dict exposing (Dict)

type alias Entry =
    { id: EntryId
    , title: String
    , author: String
    , updated_on: String
    , content: String
    , created_on: String   
    }

type alias EntryId = String

type alias Entries = 
    { next_page: Maybe String
    , entries: Dict String Entry
    , activeEntry: Maybe Entry
    , editable: Bool
    }

type alias TmpEntryList =
    { next_page : Maybe String
    , entries : List Entry
    }

