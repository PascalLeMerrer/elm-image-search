module ImageSearch exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Image exposing (..)


type Msg
    = InputChanged String
    | InputValidated
    | ImagesReceived (Result Http.Error (List Image))
    | ImageSelected Image
    | Unselected


type alias Model =
    { searchTerms : String
    , images : List Image
    , errorMessage : String
    , message : String
    , selectedImage : Maybe Image
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { searchTerms = ""
      , images = []
      , errorMessage = ""
      , message = ""
      , selectedImage = Nothing
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ viewForm model
        , br [] []
        , viewMessage model
        , viewMainContent model
        ]


viewForm : Model -> Html Msg
viewForm model =
    div []
        [ h1 [ class "title" ] [ text "Image Search" ]
        , Html.form [ onSubmit InputValidated ]
            [ input
                [ class "medium input"
                , type_ "text"
                , placeholder "Enter search terms"
                , onInput InputChanged
                , value model.searchTerms
                ]
                []
            ]
        ]


viewMessage : Model -> Html Msg
viewMessage model =
    if model.message /= "" then
        div [ class "notification is-info" ] [ text model.message ]

    else if model.errorMessage == "" then
        br [] []

    else
        div [ class "notification is-warning" ] [ text model.errorMessage ]


viewMainContent : Model -> Html Msg
viewMainContent model =
    case model.selectedImage of
        Nothing ->
            viewImages model.images

        Just image ->
            viewSelectedImage image


viewImages : List Image -> Html Msg
viewImages images =
    div [ class "columns is-multiline" ] (List.map viewImage images)


viewImage : Image -> Html Msg
viewImage image =
    div [ class "column is-one-quarter" ]
        [ img
            [ src image.thumbnailUrl
            , onClick <| ImageSelected image
            ]
            []
        ]


viewSelectedImage : Image -> Html Msg
viewSelectedImage image =
    div []
        [ div [ class "level" ]
            [ span [ onClick Unselected, style "cursor" "pointer" ] [ text "<< Back to search results" ]
            , span [] [ viewCredits image ]
            ]
        , img [ src image.url ] []
        ]


viewCredits : Image -> Html Msg
viewCredits image =
    div [ class "is-pulled-right" ]
        [ span [] [ text "Image by " ]
        , a [ href <| "https://unsplash.com/@" ++ image.username ++ "?utm_source=elm%20tutorial&utm_medium=referral" ] [ text image.author ]
        , span [] [ text " at " ]
        , a [ href "https://unsplash.com/?utm_source=elm%20tutorial&utm_medium=referral" ] [ text "Unsplash" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged value ->
            ( { model | searchTerms = value, message = "", errorMessage = "" }, Cmd.none )

        InputValidated ->
            let
                cmd =
                    Http.get
                        { url =
                            "http://elm-unsplash-orionis-dev.kermit.itn.intraorange/search/photos?query="
                                ++ model.searchTerms
                                ++ "&count=50&autoCorrect=True"
                        , expect = Http.expectJson ImagesReceived imageListDecoder
                        }

                newModel =
                    { model
                        | message = "Recherche en cours..."
                        , errorMessage = ""
                        , images = []
                    }
            in
            ( newModel, cmd )

        ImagesReceived response ->
            case response of
                Ok images ->
                    if List.isEmpty images then
                        ( { model
                            | message = "Aucune image pour cette recherche"
                            , errorMessage = ""
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | images = images
                            , message = ""
                            , errorMessage = ""
                          }
                        , Cmd.none
                        )

                Err httpError ->
                    ( { model | errorMessage = errorToString httpError }, Cmd.none )

        ImageSelected image ->
            ( { model | selectedImage = Just image }, Cmd.none )

        Unselected ->
            ( { model | selectedImage = Nothing }, Cmd.none )


errorToString httpError =
    case httpError of
        Timeout ->
            "Le serveur n'a pas répondu"

        NetworkError ->
            "La connection à Internet semble coupée"

        _ ->
            "Erreur. Essaie encore une fois."


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
