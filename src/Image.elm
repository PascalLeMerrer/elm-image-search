module Image exposing (Image, imageListDecoder)

import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import Json.Decode.Pipeline exposing (requiredAt)


type alias Image =
    { thumbnailUrl : String
    , url : String
    , height : Int
    , width : Int
    , author : String
    , username : String
    }


imageDecoder : Decoder Image
imageDecoder =
    Decode.succeed Image
        |> requiredAt [ "urls", "thumb" ] string
        |> requiredAt [ "urls", "regular" ] string
        |> requiredAt [ "height" ] int
        |> requiredAt [ "width" ] int
        |> requiredAt [ "user", "name" ] string
        |> requiredAt [ "user", "username" ] string


imageListDecoder : Decoder (List Image)
imageListDecoder =
    field "results" (list imageDecoder)
