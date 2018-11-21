module Image exposing (Image, imageListDecoder)

import Json.Decode exposing (..)


type alias Image =
    { url : String
    , thumbnail : String
    }


imageListDecoder : Decoder (List Image)
imageListDecoder =
    field "value" <| list imageDecoder


imageDecoder : Decoder Image
imageDecoder =
    map2 Image
        (field "url" string)
        (field "thumbnail" string)
