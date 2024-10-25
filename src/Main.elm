module Main exposing (..)

import Browser exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Time


type alias Model =
    { status : Status
    , anzahlSpieler : Int
    , anzahlSpione : Int
    , kategorie : String
    , begriffe : List String
    , aktuellerBegriff : Maybe String
    , spione : List Int
    , buerger : List Int
    , zeit : Int
    , restzeit : Int
    }


type Msg
    = NeueSpielerzahl Int
    | NeueSpionzahl Int
    | NeueZeit Int
    | NeueKategorie String
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


kategorien : Dict String (List String)
kategorien =
    Dict.fromList
        [ ( "Standort"
          , [ "Flughafen", "Schule", "Büro", "Kino", "Café", "Restaurant", "Bibliothek", "Park", "Krankenhaus", "Supermarkt", "Einkaufszentrum", "Fitnessstudio", "Schwimmbad", "Theater", "Museum", "Zoo", "Bahnhof", "Tankstelle", "Post", "Friseur", "Apotheke", "Spielplatz", "Stadion", "Kirche", "Tempel", "Moschee", "Kunstgalerie", "Marktplatz", "Strand", "Berg", "See", "Campingplatz", "Bücherei", "Klinik", "Tierheim", "Schloss", "Festplatz", "Botanischer Garten", "Aquarium", "Planetarium", "Hochschule", "Universität", "Messegelände", "Gärtnerei", "Weingut", "Brauerei", "Kochschule", "Fahrradverleih", "Autovermietung", "Reisebüro", "Kunstschule", "Musikschule", "Tanzschule", "Tierschutzverein", "Seniorenheim", "Jugendzentrum", "Schneiderei", "Schreinerei", "Bäckerei", "Metzgerei", "Pferdestall", "Golfplatz", "Tennisplatz", "Skihütte", "Ferienhaus", "Hütte", "Wellness-Oase", "Sauna", "Wildpark", "Abenteuerspielplatz", "Hochseilgarten", "Escape Room", "Kletterhalle", "Laser-Tag-Arena", "Bowlingbahn", "Billardcafé", "Karaokebar", "Disco", "Weihnachtsmarkt", "Flohmarkt", "Kunstmarkt", "Handwerksmarkt" ]
          )
        , ( "Beruf"
          , [ "Lehrer", "Arzt", "Krankenschwester", "Polizist", "Feuerwehrmann", "Koch", "Kellner", "Bäcker", "Metzger", "Frisör", "Maler", "Schreiner", "Elektriker", "Maurer", "Gärtner", "Winzer", "Brauer", "Künstler", "Musiker", "Tänzer", "Schauspieler", "Tierpfleger", "Tierarzt", "Altenpfleger", "Jugendbetreuer", "Schneider", "Buchhalter", "Informatiker", "Biologe", "Kinderarzt", "Burger-Verkäufer", "Entsorgungsspezialist", "Autoverkäufer", "Reiseleiter", "Tanzlehrer", "Steuerberater" ]
          )
        , ( "Hobby"
          , [ "Fußball", "Tennis", "Golf", "Schwimmen", "Laufen", "Radfahren", "Klettern", "Wandern", "Skifahren", "Snowboarden", "Surfen", "Segeln", "Tauchen", "Reiten", "Tanzen", "Musizieren", "Malen", "Zeichnen", "Basteln", "Stricken", "Nähen", "Kochen", "Backen", "Gärtnern", "Fotografieren", "Filmen", "Lesen", "Schreiben", "Theater", "Kino", "Musical", "Konzert", "Oper", "Museum", "Ausstellung", "Flohmarkt", "Kunstmarkt", "Handwerksmarkt", "Weihnachtsmarkt", "Karneval", "Lesen", "Schreiben", "Theater", "Kino", "Musical", "Konzert" ]
          )
        ]


initialModel : Model
initialModel =
    { anzahlSpieler = 5
    , anzahlSpione = maxAnzahlSpione 5
    , kategorie = kategorien |> Dict.keys |> List.head |> Maybe.withDefault "Keine Kategorie"
    , begriffe = kategorien |> Dict.get (kategorien |> Dict.keys |> List.head |> Maybe.withDefault "Keine Kategorie") |> Maybe.withDefault []
    , status = Vorbereitung
    , aktuellerBegriff = Nothing
    , buerger = List.range 1 5
    , spione = []
    , zeit = 180
    , restzeit = 0
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )


maxAnzahlSpione : Int -> Int
maxAnzahlSpione spieler =
    round (toFloat spieler / 3)


viewEintrag e =
    li [] [ text e ]


viewEintraege liste =
    div []
        [ ul [] (List.map viewEintrag liste)
        ]


viewSpielerinfo anzahl =
    div [ class "zahlMitButtons" ]
        [ button [ onClick (NeueSpielerzahl (anzahl - 1)) ] [ text "-" ]
        , span [] [ text (String.fromInt anzahl) ]
        , button [ onClick (NeueSpielerzahl (anzahl + 1)) ] [ text "+" ]
        , text " Spieler"
        ]


viewSpioninfo anzahl =
    div [ class "zahlMitButtons" ]
        [ button [ onClick (NeueSpionzahl (anzahl - 1)) ] [ text "-" ]
        , span [] [ text (String.fromInt anzahl) ]
        , button [ onClick (NeueSpionzahl (anzahl + 1)) ] [ text "+" ]
        , text " Spione"
        ]


viewZeit minuten =
    div [ class "zahlMitButtons" ]
        [ button [ onClick (NeueZeit (minuten * 60 - 60)) ] [ text "-" ]
        , span [] [ text (String.fromInt minuten) ]
        , button [ onClick (NeueZeit (minuten * 60 + 60)) ] [ text "+" ]
        , text " Minuten"
        ]


stringToOption : String -> String -> Html Msg
stringToOption kat s =
    option [ value s, selected (kat == s) ] [ text s ]


viewKategorien : String -> List String -> Html Msg
viewKategorien aktuelleKategorie kategorienliste =
    div []
        [ select [ onInput NeueKategorie ]
            (List.map (stringToOption aktuelleKategorie) kategorienliste)
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
                    , viewSpioninfo model.anzahlSpione
                    , viewZeit (model.zeit // 60)
                    , viewKategorien model.kategorie (kategorien |> Dict.keys)
                    , button [ onClick Starten ] [ text "Los geht's" ]
                    ]
                )

        OffeneKarte n ->
            div []
                [ h2 [] [ text ("Spieler " ++ String.fromInt n) ]
                , if List.member n model.spione then
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
            let
                neueZahl =
                    if n < 3 then
                        3

                    else
                        n
            in
            ( { model
                | anzahlSpieler = neueZahl
                , buerger = List.range 1 neueZahl
                , anzahlSpione =
                    if model.anzahlSpione > maxAnzahlSpione neueZahl then
                        maxAnzahlSpione neueZahl

                    else
                        model.anzahlSpione
              }
            , Cmd.none
            )

        NeueSpionzahl n ->
            let
                neueZahl =
                    if n < 1 then
                        1

                    else if n > maxAnzahlSpione model.anzahlSpieler then
                        maxAnzahlSpione model.anzahlSpieler

                    else
                        n
            in
            ( { model | anzahlSpione = neueZahl }, Cmd.none )

        NeueZeit n ->
            let
                neueZeit =
                    if n < 60 then
                        60

                    else
                        n
            in
            ( { model | zeit = neueZeit }, Cmd.none )

        NeueKategorie k ->
            ( { model
                | kategorie = k
                , begriffe = kategorien |> Dict.get k |> Maybe.withDefault []
              }
            , Cmd.none
            )

        Starten ->
            ( { model
                | status = VerdeckteKarte 1
                , aktuellerBegriff = model.begriffe |> List.head
                , restzeit = model.zeit
              }
            , Random.generate SpionErmittelt (Random.int 1 (List.length model.buerger))
            )

        SpionErmittelt n ->
            let
                neueSpione =
                    model.spione ++ (List.drop (n - 1) model.buerger |> List.take 1)

                neueBuerger =
                    List.filter (\x -> not (List.member x neueSpione)) model.buerger
            in
            ( { model
                | spione = neueSpione
                , buerger = neueBuerger
              }
            , if List.length neueSpione < model.anzahlSpione then
                Random.generate SpionErmittelt (Random.int 1 (neueBuerger |> List.length))

              else
                Random.generate BegriffErmittelt (Random.int 1 (model.begriffe |> List.length))
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
            ( { anzahlSpieler = model.anzahlSpieler
              , begriffe = model.begriffe
              , kategorie = model.kategorie
              , status = Vorbereitung
              , aktuellerBegriff = Nothing
              , buerger = List.range 1 model.anzahlSpieler
              , anzahlSpione = model.anzahlSpione
              , spione = []
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
