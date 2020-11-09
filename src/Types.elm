module Types exposing (..)

import Browser.Navigation as Nav
import Url
import User as U
import Entry as E
import Browser
import Http



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
  | GotEntries (Result Http.Error E.Entries)

type alias TokenStrings =
    { refreshToken : String
    , accessToken : String
    }
