module Main exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


type alias Model =
    { status : Status
    , anzahlSpieler : Int
    , kategorie : String
    , begriffe : List String
    , aktuellerBegriff : Maybe String
    , spion : Maybe Int
    }


type Msg
    = NeueSpielerzahl Int
    | Starten
    | ZeigeKarte Int
    | VerdeckeKarte Int
    | SpionErmittelt Int
    | BegriffErmittelt Int
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


view : Model -> Html Msg
view model =
    case model.status of
        Vorbereitung ->
            div []
                [ h1 [] [ text "SpyGame" ]
                , viewSpielerinfo model.anzahlSpieler
                , p [] [ text ("Kategorie: " ++ model.kategorie) ]
                , button [ onClick Starten ] [ text "Los geht's" ]
                ]

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
                [ p [] [ text "Ihr habt drei Minuten Zeit. Los geht's ..." ]
                , button [ onClick Reset ] [ text "Neustart" ]
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
            ( { model
                | aktuellerBegriff =
                    model.begriffe |> List.drop (n - 1) |> List.head
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

        Reset ->
            ( { initialModel | anzahlSpieler = model.anzahlSpieler }
            , Cmd.none
            )


subscriptions _ =
    Sub.none


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
