port module Increment exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Css exposing (..)
import Csv.Decode as CsvDecode
import File
import File.Select as Select
import Html.Styled exposing (Attribute, Html, b, button, div, input, map, span, styled, text, toUnstyled)
import Html.Styled.Attributes exposing (css, placeholder, step, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Process
import Random
import Task
import Time exposing (Month(..))



-- MODEL


type ModalModel
    = WelcomeModal
    | FieldModal FieldModalModel
    | AnotherFieldModal AnotherFieldModalModel


type alias FieldModalModel =
    { login : String, password : String }


type alias AnotherFieldModalModel =
    { username : String }


type alias Model =
    { value : Int
    , step : Int
    , errorMsg : Maybe String
    , history : List HistoryItem
    , historyCsv : Maybe String
    , modal : Maybe ModalModel
    }


type alias KeyboardPressEvent =
    { key : String, isCtrlPressed : Bool }


type alias HistoryItem =
    { event : String, prevValue : Int, value : Int }


type alias LocalStorageData =
    { value : Int, step : Int, history : List HistoryItem }


init : flags -> ( Model, Cmd msg )
init _ =
    ( Model 0 1 Nothing [] Nothing Nothing, Cmd.none )



-- PORTS


port storeLocalStorage : Decode.Value -> Cmd msg


port onStoreChange : (Decode.Value -> msg) -> Sub msg



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Reset
    | ResetHistory
    | ChangeStepStr String
    | ChangeStepInt Int
    | ResetStep
    | RandomStep
    | ResetError
    | PressKey KeyboardPressEvent
    | HistoryCsvRequested
    | HistoryCsvLoaded File.File
    | HistoryCsvParsed (Result CsvDecode.Error (List HistoryItem))
    | ReceiveLocalStorageValue (Maybe LocalStorageData)
    | ShowModal ModalModel
    | HideModal
    | GotModalMsg ModalMsg


type ModalMsg
    = GotFieldModalMsg FieldModalMsg
    | GotAnotherFieldModalMsg AnotherFieldModalMsg


type FieldModalMsg
    = EnteredLogin String
    | EnteredPassword String


type AnotherFieldModalMsg
    = EnteredUsername String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            updateWithLocalStorage
                ( updateWithHistory "increment" model { model | value = model.value + model.step }
                , Cmd.none
                )

        Decrement ->
            updateWithLocalStorage
                ( updateWithHistory "decrement" model { model | value = model.value - model.step }
                , Cmd.none
                )

        Reset ->
            updateWithLocalStorage ( updateWithHistory "reset" model { model | value = 0 }, Cmd.none )

        ResetHistory ->
            updateWithLocalStorage ( { model | history = [] }, Cmd.none )

        ResetStep ->
            updateWithLocalStorage ( { model | step = 1 }, Cmd.none )

        ResetError ->
            ( { model | errorMsg = Nothing }, Cmd.none )

        RandomStep ->
            ( model, Random.generate ChangeStepInt randomStep )

        ChangeStepInt step ->
            if step <= 100 then
                updateWithLocalStorage ( { model | step = step, errorMsg = Nothing }, Cmd.none )

            else
                updateWithLocalStorage
                    ( { model | step = 100, errorMsg = Just "The step should not be greater than 100." }
                    , delay 1500 ResetError
                    )

        ChangeStepStr newStep ->
            let
                parsedStep : Maybe Int
                parsedStep =
                    parseStepStr newStep
            in
            case parsedStep of
                Maybe.Just step ->
                    update (ChangeStepInt step) model

                Maybe.Nothing ->
                    ( { model | errorMsg = Just "Please use numbers." }, delay 1500 ResetError )

        PressKey { key, isCtrlPressed } ->
            case ( isCtrlPressed, key ) of
                ( True, "ArrowUp" ) ->
                    update Increment model

                ( True, "ArrowDown" ) ->
                    update Decrement model

                _ ->
                    ( model, Cmd.none )

        HistoryCsvRequested ->
            ( model, requestCsv HistoryCsvLoaded )

        HistoryCsvLoaded file ->
            ( model
            , File.toString file
                |> Task.map (CsvDecode.decodeCsv CsvDecode.FieldNamesFromFirstRow historyItemCsvDecoder)
                |> Task.perform HistoryCsvParsed
            )

        HistoryCsvParsed file ->
            case file of
                Ok historyItems ->
                    let
                        headHistoryItem =
                            List.head historyItems
                    in
                    case headHistoryItem of
                        Just historyItem ->
                            updateWithLocalStorage
                                ( { model
                                    | history = historyItems
                                    , value = historyItem.value
                                  }
                                , Cmd.none
                                )

                        Nothing ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ReceiveLocalStorageValue maybeLocalStorageData ->
            case maybeLocalStorageData of
                Just localStorageData ->
                    ( { model
                        | value = localStorageData.value
                        , step = localStorageData.step
                        , history = localStorageData.history
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ShowModal modalModel ->
            ( { model | modal = Just modalModel }, Cmd.none )

        HideModal ->
            ( { model | modal = Nothing }, Cmd.none )

        GotModalMsg modalMsg ->
            case model.modal of
                Just modalModel ->
                    ( { model | modal = Just (modalUpdate modalMsg modalModel) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )



-- TODO: refactor this to a more extendable way


modalUpdate : ModalMsg -> ModalModel -> ModalModel
modalUpdate msg model =
    case ( model, msg ) of
        ( FieldModal modalModel, GotFieldModalMsg modalMsg ) ->
            case modalMsg of
                EnteredLogin login ->
                    FieldModal { modalModel | login = login }

                EnteredPassword password ->
                    FieldModal { modalModel | password = password }

        ( AnotherFieldModal modalModel, GotAnotherFieldModalMsg modalMsg ) ->
            case modalMsg of
                EnteredUsername username ->
                    AnotherFieldModal { modalModel | username = username }

        ( _, _ ) ->
            model


delay : Float -> Msg -> Cmd Msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


randomStep : Random.Generator Int
randomStep =
    Random.int 1 10


parseStepStr : String -> Maybe Int
parseStepStr step =
    case step of
        "" ->
            Just 0

        _ ->
            String.toInt step


requestCsv : (File.File -> Msg) -> Cmd Msg
requestCsv toMsg =
    Select.file [ "text/csv" ] toMsg


historyItemCsvDecoder : CsvDecode.Decoder HistoryItem
historyItemCsvDecoder =
    CsvDecode.into HistoryItem
        |> CsvDecode.pipeline (CsvDecode.field "event" CsvDecode.string)
        |> CsvDecode.pipeline (CsvDecode.field "previous_value" CsvDecode.int)
        |> CsvDecode.pipeline (CsvDecode.field "value" CsvDecode.int)


{-| Returns us an updated model with a history item.
If the value hasn't changed, - we shouldn't add a history item.
-}
updateWithHistory : String -> Model -> Model -> Model
updateWithHistory historyEvent oldModel model =
    if model.value - oldModel.value == 0 then
        oldModel

    else
        { model | history = toHistoryItem historyEvent oldModel model :: model.history }


toHistoryItem : String -> Model -> Model -> HistoryItem
toHistoryItem historyEvent oldModel model =
    HistoryItem historyEvent oldModel.value model.value


historyItemJsonDecoder : Decode.Decoder HistoryItem
historyItemJsonDecoder =
    Decode.map3 HistoryItem
        (Decode.field "event" Decode.string)
        (Decode.field "previous_value" Decode.int)
        (Decode.field "value" Decode.int)


historyItemJsonEncoder : HistoryItem -> Encode.Value
historyItemJsonEncoder historyItem =
    Encode.object
        [ ( "event", Encode.string historyItem.event )
        , ( "previous_value", Encode.int historyItem.prevValue )
        , ( "value", Encode.int historyItem.value )
        ]


storeValueToLocalStorage : Model -> Cmd msg
storeValueToLocalStorage model =
    let
        json =
            Encode.object
                [ ( "value", Encode.int model.value )
                , ( "step", Encode.int model.step )
                , ( "history", Encode.list historyItemJsonEncoder model.history )
                ]
    in
    storeLocalStorage json


updateWithLocalStorage : ( Model, Cmd msg ) -> ( Model, Cmd msg )
updateWithLocalStorage ( model, cmd ) =
    ( model, Cmd.batch [ storeValueToLocalStorage model, cmd ] )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ css
            [ backgroundColor (hex "#222629")
            , color (hex "#FAFAFB")
            , minHeight (vh 100)
            , fontSize (px 16)
            , fontFamilies [ "Verdana", "Arial" ]
            , padding2 (px 20) (px 15)
            ]
        ]
        [ div [ css [ displayFlex, flexDirection column, alignItems center ] ]
            [ styledRow []
                [ div
                    [ css
                        [ fontSize (px 108)
                        ]
                    ]
                    [ text (String.fromInt model.value) ]
                ]
            , styledRow [] (renderActions model)
            , styledRow []
                [ text "Increment step:"
                , styledInput
                    [ value (String.fromInt model.step)
                    , onInput ChangeStepStr
                    , css
                        [ marginLeft (px 5)
                        ]
                    ]
                    []
                , renderMaybeString renderErrorMsg model.errorMsg
                ]
            , styledRow []
                [ div [] [ text "Press 'cmd + arrow up' to increment" ]
                , div [] [ text "Press 'cmd + arrow dowm' to decrement" ]
                ]
            , styledRow
                [ css
                    [ displayFlex
                    , flexDirection column
                    , backgroundColor (hex "#474B4F")
                    , borderRadius (px 5)
                    ]
                ]
                [ div
                    [ css
                        [ textAlign center
                        , fontSize (px 24)
                        , marginBottom (px 5)
                        ]
                    ]
                    [ text "History items" ]
                , div
                    [ css
                        [ width (px 500)
                        , height (px 400)
                        , overflow scroll
                        , padding2 (px 10) (px 15)
                        ]
                    ]
                    (List.map renderHistoryItem model.history)
                ]
            , renderModal model.modal
            ]
        ]


renderActions : Model -> List (Html Msg)
renderActions model =
    [ styledButton [ onClick Increment ] [ text "Add" ]
    , styledButton [ onClick Decrement ] [ text "Remove" ]
    , styledButton [ onClick Reset ] [ text "Reset" ]
    , styledButton [ onClick ResetHistory ] [ text "Reset history" ]
    , styledButton [ onClick (ChangeStepInt model.value) ] [ text "Copy step" ]
    , styledButton [ onClick ResetStep ] [ text "Reset step" ]
    , styledButton [ onClick RandomStep ] [ text "Random step" ]
    , styledButton [ onClick HistoryCsvRequested ] [ text "Upload history" ]
    , styledButton [ onClick (ShowModal WelcomeModal) ] [ text "Show welcome modal" ]
    , styledButton [ onClick (ShowModal (FieldModal (FieldModalModel "" ""))) ] [ text "Show field modal" ]
    , styledButton [ onClick (ShowModal (AnotherFieldModal (AnotherFieldModalModel ""))) ] [ text "Show another field modal" ]
    ]


styledInput : List (Attribute msg) -> List (Html msg) -> Html msg
styledInput =
    styled input
        [ borderRadius (px 5)
        , outline zero
        , border zero
        , padding2 (px 5) (px 10)
        , fontSize (px 24)
        ]


styledRow : List (Attribute msg) -> List (Html msg) -> Html msg
styledRow =
    styled div
        [ padding2 (px 10) (px 0)
        ]


styledButton : List (Attribute msg) -> List (Html msg) -> Html msg
styledButton =
    styled button
        [ padding2 (px 10) (px 15)
        , border zero
        , outline zero
        , cursor pointer
        , borderRadius (px 5)
        , marginRight (px 5)
        , lastChild [ marginRight (px 0) ]
        , backgroundColor (hex "#86C232")
        , color (hex "#FAFAFB")
        ]


renderModal : Maybe ModalModel -> Html Msg
renderModal modal =
    case modal of
        Just modalModel ->
            case modalModel of
                WelcomeModal ->
                    renderWelcomeModal

                FieldModal model ->
                    renderFieldModal model

                AnotherFieldModal model ->
                    renderAnotherFieldModal model

        Nothing ->
            text ""


overlay : List (Attribute Msg) -> List (Html Msg) -> Html Msg
overlay =
    styled div
        [ position absolute
        , top zero
        , bottom zero
        , left zero
        , right zero
        , color (rgb 255 255 255)
        , backgroundColor (rgb 0 0 0)
        , opacity (num 0.8)
        , zIndex (int 1000)
        ]


renderModalWrapper : List (Html Msg) -> Html Msg
renderModalWrapper content =
    div
        [ css
            [ position absolute
            , top zero
            , bottom zero
            , left zero
            , right zero
            , displayFlex
            , alignItems center
            , justifyContent center
            , flexDirection column
            ]
        ]
        [ overlay [] []
        , div
            [ css
                [ zIndex (int 1010)
                , backgroundColor (hex "#474B4F")
                , padding2 (px 15) (px 20)
                , borderRadius (px 5)
                , textAlign center
                ]
            ]
            [ div [ css [ marginBottom (px 20) ] ] [ text "This is modal header" ]
            , div [] content
            , div [ css [ marginTop (px 20) ] ] [ text "This is modal footer" ]
            ]
        ]


renderWelcomeModal : Html Msg
renderWelcomeModal =
    renderModalWrapper
        [ div [] [ text "This is a welcome modal" ]
        , button [ onClick HideModal ] [ text "click here to close" ]
        ]


renderFieldModal : FieldModalModel -> Html Msg
renderFieldModal model =
    renderModalWrapper
        [ div [] [ text "This is a field modal" ]
        , div []
            [ div []
                [ map (\msg -> GotModalMsg (GotFieldModalMsg msg))
                    (input
                        [ value model.login
                        , type_ "text"
                        , placeholder "Type login"
                        , onInput EnteredLogin
                        ]
                        []
                    )
                ]
            , div []
                [ map (\msg -> GotModalMsg (GotFieldModalMsg msg))
                    (input
                        [ value model.password
                        , type_ "password"
                        , placeholder "Type password"
                        , onInput EnteredPassword
                        ]
                        []
                    )
                ]
            ]
        , button [ onClick HideModal ] [ text "click here to close" ]
        ]


renderAnotherFieldModal : AnotherFieldModalModel -> Html Msg
renderAnotherFieldModal model =
    renderModalWrapper
        [ div [] [ text "This is another field modal" ]
        , div []
            [ div []
                [ map (\msg -> GotModalMsg (GotAnotherFieldModalMsg msg))
                    (input
                        [ value model.username
                        , type_ "text"
                        , placeholder "Type username"
                        , onInput EnteredUsername
                        ]
                        []
                    )
                ]
            ]
        , button [ onClick HideModal ] [ text "click here to close" ]
        ]


renderErrorMsg : String -> Html Msg
renderErrorMsg errorMsg =
    div [ css [ textAlign center, color (hex "#ff1c1c") ] ] [ text ("Error:" ++ errorMsg) ]


renderHistoryItem : HistoryItem -> Html Msg
renderHistoryItem item =
    div []
        [ renderLabelValue "Event: " item.event
        , renderLabelValue "Previous value: " (String.fromInt item.prevValue)
        , renderLabelValue "Value: " (String.fromInt item.value)
        ]


renderLabelValue : String -> String -> Html Msg
renderLabelValue label value =
    span []
        [ text label
        , b [] [ text value ]
        , text "; "
        ]


renderMaybeString : (String -> Html msg) -> Maybe String -> Html msg
renderMaybeString elRenderer maybeStr =
    case maybeStr of
        Just str ->
            elRenderer str

        Nothing ->
            text ""



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown (Decode.map PressKey keyDecoder)
        , onStoreChange (\val -> ReceiveLocalStorageValue (decodeLocalStorage val))
        ]


keyDecoder : Decode.Decoder KeyboardPressEvent
keyDecoder =
    Decode.map2 KeyboardPressEvent
        (Decode.field "key" Decode.string)
        mapIsCtrlPressed


{-| We need to handle both mac and windows cases when
the user presses `meta` on mac and `ctrl` on windows
-}
mapIsCtrlPressed : Decode.Decoder Bool
mapIsCtrlPressed =
    Decode.map2 (\isMetaPressed isCtrlPressed -> isMetaPressed || isCtrlPressed)
        (Decode.field "metaKey" Decode.bool)
        (Decode.field "ctrlKey" Decode.bool)


localStorageDecoder : Decode.Decoder LocalStorageData
localStorageDecoder =
    Decode.map3 LocalStorageData
        (Decode.field "value" Decode.int)
        (Decode.field "step" Decode.int)
        (Decode.field "history" (Decode.list historyItemJsonDecoder))


decodeLocalStorage : Encode.Value -> Maybe LocalStorageData
decodeLocalStorage jsonValue =
    Decode.decodeValue localStorageDecoder jsonValue
        |> Result.toMaybe



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
