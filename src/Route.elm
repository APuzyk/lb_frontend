module Route exposing (..)
import Entry exposing (Entry)


type Route
    = Home
    | Entry Entry
    | EditEntry Entry
    | CreateEntry
    | CreateUser
    | Login
    | Insights

parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map 
        , Parser.map Logout (s "logout")
        , Parser.map Settings (s "settings")
        , Parser.map Profile (s "profile" </> Username.urlParser)
        , Parser.map Register (s "register")
        , Parser.map Article (s "article" </> Slug.urlParser)
        , Parser.map NewArticle (s "editor")
        , Parser.map EditArticle (s "editor" </> Slug.urlParser)
        , Parser.map Insights (s "")
        ]




updateUrlAction : Model -> Url.Url -> (Model, Cmd Msg)
updateUrlAction model url =
    let
        oldEntries = model.entries
    in
        case UrlParser.parse T.routeParser url of
            Nothing -> 
                ({model | entries = removeActiveEntry model.entries} , EP.getEntries model)
            Just (T.EntryRoute created_on uuid) -> 
                ( {model | entries = updateActiveEntry created_on uuid model.entries} , Cmd.none )
            Just (T.EditEntryRoute created_on uuid) -> 
                ( {model | entries = updateEditActiveEntry created_on uuid model.entries} , Cmd.none )
            Just T.CreateEntryRoute ->
                ( {model | entries = addEmtpyActiveEntry model.entries }, Cmd.none )
            Just T.CreateUserRoute ->
                let
                    user = model.user
                in
                ( { model | user = {user | accessToken = "reg"}}, Cmd.none)