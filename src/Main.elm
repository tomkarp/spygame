module Main exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Time


type alias Model =
    { status : Status
    , anzahlSpieler : Int
    , begriffe : List String
    , aktuellerBegriff : Maybe String
    , spion : Int
    , zeit : Int
    , restzeit : Int
    }


type Msg
    = NeueSpielerzahl Int
    | Starten
    | ZeigeKarte Int
    | VerdeckeKarte Int
    | SpionErmittelt Int
    | Tick Time.Posix
    | Reset


type Status
    = Vorbereitung
    | AngezeigteKarte Int
    | VerdeckteKarte Int
    | Countdown


initialModel : Model
initialModel =
    { status = Vorbereitung
    , anzahlSpieler = 5
    , begriffe = [ "Flughafen", "Schule", "Büro", "Kino", "Café" ]
    , aktuellerBegriff = Nothing
    , spion = 1
    , zeit = 180
    , restzeit = 0
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )


viewSpielerinfo : Int -> Html Msg
viewSpielerinfo anzahl =
    div [ class "zahlMitButtons" ]
        [ button [ onClick (NeueSpielerzahl (anzahl - 1)) ] [ text "-" ]
        , span [] [ text (String.fromInt anzahl) ]
        , button [ onClick (NeueSpielerzahl (anzahl + 1)) ] [ text "+" ]
        , text " Spieler"
        ]


view : Model -> Html Msg
view model =
    case model.status of
        Vorbereitung ->
            div []
                (if model.begriffe == [] then
                    [ h1 [] [ text "SpyGame" ]
                    , p [] [ text "Keine Begriffe mehr vorhanden" ]
                    ]

                 else
                    [ h1 [] [ text "SpyGame" ]
                    , viewSpielerinfo model.anzahlSpieler
                    , button [ onClick Starten ] [ text "Los geht's" ]
                    ]
                )

        AngezeigteKarte n ->
            div []
                [ h2 [] [ text ("Spieler " ++ String.fromInt n) ]
                , if n == model.spion then
                    p [ class "begriff" ] [ text "Spion" ]

                  else
                    p [ class "begriff" ]
                        [ model.aktuellerBegriff
                            |> Maybe.withDefault "Ungültiger Begriff"
                            |> text
                        ]
                , button [ onClick (VerdeckeKarte (n + 1)) ] [ text "Weiter" ]
                , p [] [ text "Klicke auf 'Weiter' und gib dann das Gerät weiter" ]
                ]

        VerdeckteKarte n ->
            div []
                [ h2 [] [ text ("Spieler " ++ String.fromInt n) ]
                , p [ class "umdrehhinweis" ] [ text "???" ]
                , button [ onClick (ZeigeKarte n) ] [ text "Zeige Karte" ]
                ]

        Countdown ->
            div []
                [ h2 [] [ text "Zeit läuft ..." ]
                , h2 [] [ text (String.fromInt model.restzeit) ]
                , button [ onClick Reset ] [ text "Neue Runde" ]
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NeueSpielerzahl n ->
            ( { model
                | anzahlSpieler =
                    if n < 3 then
                        3

                    else
                        n
              }
            , Cmd.none
            )

        Starten ->
            ( { model
                | status = AngezeigteKarte 1
                , aktuellerBegriff = model.begriffe |> List.head
                , begriffe = model.begriffe |> List.drop 1
                , restzeit = model.zeit
              }
            , Random.generate SpionErmittelt (Random.int 1 model.anzahlSpieler)
            )

        SpionErmittelt n ->
            ( { model | spion = n }, Cmd.none )

        ZeigeKarte n ->
            ( { model | status = AngezeigteKarte n }
            , Cmd.none
            )

        VerdeckeKarte n ->
            if n <= model.anzahlSpieler then
                ( { model | status = VerdeckteKarte n }, Cmd.none )

            else
                ( { model | status = Countdown }, Cmd.none )

        Tick _ ->
            if model.restzeit > 0 then
                ( { model | restzeit = model.restzeit - 1 }, Cmd.none )

            else
                ( { model | status = Countdown }, Cmd.none )

        Reset ->
            ( { anzahlSpieler = model.anzahlSpieler
              , begriffe = model.begriffe
              , status = Vorbereitung
              , aktuellerBegriff = Nothing
              , spion = 1
              , zeit = model.zeit
              , restzeit = 0
              }
            , Cmd.none
            )


subscriptions model =
    if model.status == Countdown then
        Time.every 1000 Tick

    else
        Sub.none


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
