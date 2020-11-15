module Types exposing (..)

import Browser.Navigation as Nav
import Url
import User as U
import Entry as E
import Browser
import Http
import Entry exposing (Entry)

import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, string)


-- MODEL
type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , user: U.User
  , errorMsg: String
  , entries: E.Entries
  }

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | SetUsername String
  | SetPassword String
  | ClickRegisterUser
  | GotToken (Result Http.Error TokenStrings)
  | GotEntries (Result Http.Error E.TmpEntryList)
  | UpdateEntryContent String
  | UpdateEntryTitle String
  | ClickSaveEntry
  | PatchedEntry (Result Http.Error Entry)

type alias TokenStrings =
    { refreshToken : String
    , accessToken : String
    }

type Route
    = EntryRoute String String
    | EditEntryRoute String String

routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map EditEntryRoute (s "entry" </> s "edit" </> string </> string)
        , map EntryRoute (s "entry" </> string </> string)
        ]