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
    , kategorie : String
    , begriffe : List String
    , aktuellerBegriff : Maybe String
    , spion : Maybe Int
    , restzeit : Int
    }


type Msg
    = NeueSpielerzahl Int
    | NeueZeit Int
    | Starten
    | ZeigeKarte Int
    | VerdeckeKarte Int
    | SpionErmittelt Int
    | BegriffErmittelt Int
    | Tick Time.Posix
    | Reset


type Status
    = Vorbereitung
    | VerdeckteKarte Int
    | OffeneKarte Int
    | Countdown


initialModel : Model
initialModel =
    { anzahlSpieler = 5
    , kategorie = "Standort"
    , begriffe = [ "Flughafen", "Schule", "Büro", "Kino", "Café", "Restaurant", "Bibliothek", "Park", "Krankenhaus", "Supermarkt", "Einkaufszentrum", "Fitnessstudio", "Schwimmbad", "Theater", "Museum", "Zoo", "Bahnhof", "Tankstelle", "Post", "Friseur", "Apotheke", "Spielplatz", "Stadion", "Kirche", "Tempel", "Moschee", "Kunstgalerie", "Marktplatz", "Strand", "Berg", "See", "Campingplatz", "Bücherei", "Klinik", "Tierheim", "Schloss", "Festplatz", "Botanischer Garten", "Aquarium", "Planetarium", "Hochschule", "Universität", "Messegelände", "Gärtnerei", "Weingut", "Brauerei", "Kochschule", "Fahrradverleih", "Autovermietung", "Reisebüro", "Kunstschule", "Musikschule", "Tanzschule", "Tierschutzverein", "Seniorenheim", "Jugendzentrum", "Schneiderei", "Schreinerei", "Bäckerei", "Metzgerei", "Pferdestall", "Golfplatz", "Tennisplatz", "Skihütte", "Ferienhaus", "Hütte", "Wellness-Oase", "Sauna", "Wildpark", "Abenteuerspielplatz", "Hochseilgarten", "Escape Room", "Kletterhalle", "Laser-Tag-Arena", "Bowlingbahn", "Billardcafé", "Karaokebar", "Disco", "Weihnachtsmarkt", "Flohmarkt", "Kunstmarkt", "Handwerksmarkt" ]
    , status = Vorbereitung
    , aktuellerBegriff = Nothing
    , spion = Nothing
    , restzeit = 180
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )


anzahlSpione : Int -> Int
anzahlSpione spieler =
    --round (toFloat spieler / 3)
    spieler // 3


viewEintrag e =
    li [] [ text e ]


viewEintraege liste =
    div []
        [ ul [] (List.map viewEintrag liste)
        ]


viewSpielerinfo anzahl =
    div []
        [ text "Anzahl der Spieler: "
        , button [ onClick (NeueSpielerzahl (anzahl - 1)) ] [ text "-" ]
        , span [ style "margin" "10px" ] [ text (String.fromInt anzahl) ]
        , button [ onClick (NeueSpielerzahl (anzahl + 1)) ] [ text "+" ]
        , p [] [ text ("Anzahl Spione: " ++ String.fromInt (anzahlSpione anzahl)) ]
        ]


viewZeit minuten =
    div []
        [ text "Zeit (min): "
        , button [ onClick (NeueZeit (minuten * 60 - 60)) ] [ text "-" ]
        , span [ style "margin" "10px" ] [ text (String.fromInt minuten) ]
        , button [ onClick (NeueZeit (minuten * 60 + 60)) ] [ text "+" ]
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
                    , viewZeit (model.restzeit // 60)
                    , p [] [ text ("Kategorie: " ++ model.kategorie) ]
                    , button [ onClick Starten ] [ text "Los geht's" ]
                    ]
                )

        OffeneKarte n ->
            case model.spion of
                Nothing ->
                    p [] [ text "Fehler, bitte neu starten" ]

                Just spionnummer ->
                    div []
                        [ h2 [] [ text ("Spieler Nummer " ++ String.fromInt n) ]
                        , if n == spionnummer then
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
                [ h2 [] [ text ("Spieler Nummer " ++ String.fromInt n) ]
                , p [ class "umdrehhinweis" ] [ text "???" ]
                , button [ onClick (ZeigeKarte n) ] [ text "Zeige Karte" ]
                ]

        Countdown ->
            div []
                [ p [] [ text "Zeit läuft ..." ]
                , p [] [ text (String.fromInt model.restzeit) ]
                , button [ onClick Reset ] [ text "Neue Runde" ]
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NeueSpielerzahl n ->
            let
                neueZahl =
                    if n < 3 then
                        3

                    else
                        n
            in
            ( { model | anzahlSpieler = neueZahl }, Cmd.none )

        NeueZeit n ->
            let
                neueZeit =
                    if n < 60 then
                        60

                    else
                        n
            in
            ( { model | restzeit = neueZeit }, Cmd.none )

        Starten ->
            ( { model
                | status = VerdeckteKarte 1
                , aktuellerBegriff = model.begriffe |> List.head
              }
            , Random.generate SpionErmittelt (Random.int 1 model.anzahlSpieler)
            )

        SpionErmittelt n ->
            ( { model | spion = Just n }
            , Random.generate BegriffErmittelt (Random.int 1 (model.begriffe |> List.length))
            )

        BegriffErmittelt n ->
            let
                neuerBegriff =
                    model.begriffe |> List.drop (n - 1) |> List.head
            in
            ( { model
                | aktuellerBegriff = neuerBegriff
                , begriffe =
                    case neuerBegriff of
                        Nothing ->
                            model.begriffe

                        Just b ->
                            List.filter (\x -> x /= b) model.begriffe
              }
            , Cmd.none
            )

        ZeigeKarte n ->
            ( { model | status = OffeneKarte n }
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
            ( { initialModel
                | anzahlSpieler = model.anzahlSpieler
                , begriffe = model.begriffe
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
