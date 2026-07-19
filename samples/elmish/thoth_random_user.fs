модуль Thoth.RandomUser

(**
Small application showing how to use:
- Thoth.Json (https://mangelmaxime.github.io/Thoth/json/v3.html)
- Promise and Fetch APIs
*)

відкрити System
відкрити Fable.Core
відкрити Fable.React
відкрити Fable.React.Props
відкрити Elmish
відкрити Elmish.React
відкрити Thoth.Json

// MODEL
тип Gender =
    | Male
    | Female

    статичний член Decoder =
        Decode.string
        |> Decode.andThen (
            функція
            | "male" -> Decode.succeed Male
            | "female" -> Decode.succeed Female
            | invalid -> "`" + invalid + "` isn't a valid value для Gender"
                            |> Decode.fail
        )

тип User =
    { Gender : Gender
      FullName : string
      Email : string
      CellPhone : string
      OfficePhone : string
      Age : int
      Birthday : DateTime
      Picture : string }

    статичний член Decoder =
        // When using Thoth.Json, you are not forced to зробити a 1 to 1
        // mapping between the JSON format and your types
        // For example, у the next decoder we will access deep information
        // and store it at the "root" з our тип
        Decode.object (фун get ->
            // In object decoder, we can execute any F#
            // So для example, we can use temporary variables
            нехай firstname = get.Required.At [ "name"; "first" ] Decode.string
            нехай lastname = get.Required.At [ "name"; "last" ] Decode.string

            { Gender = get.Required.Field "gender" Gender.Decoder
              FullName = firstname + " " + lastname
              Email = get.Required.Field "email" Decode.string
              CellPhone = get.Required.Field "cell" Decode.string
              OfficePhone = get.Required.Field "phone" Decode.string
              Age = get.Required.At [ "dob"; "age" ] Decode.int
              Birthday = get.Required.At [ "dob"; "date" ] Decode.datetime
              Picture = get.Required.At [ "picture"; "large" ] Decode.string }
        )

тип Model =
    /// Loading state
    /// If user is None, тоді it's the initial loading
    | Loading з User option
    /// Loaded state
    | Loaded з User
    /// If last request results у an error
    | Errored

тип Msg =
    | FetchRandomUser
    | FetchResponse з Result<User, string>
    | FetchError з exn

/// At first, we have no user to display
нехай init () = Loading None, Cmd.ofMsg FetchRandomUser

// UPDATE

нехай приватний getRandomUser () = promise {
    // We add a delay з 300ms so the button animation is more visible
    do! Promise.sleep 300
    нехай! response = Fetch.fetch "https://randomuser.me/api/" []
    нехай! responseText = response.text()
    нехай resultDecoder = Decode.field "results" (Decode.index 0 User.Decoder)
    повернути Decode.fromString resultDecoder responseText
}
нехай update (msg:Msg) (model:Model) =
    співстав msg із
    | FetchRandomUser ->
        нехай newModel =
            співстав model із
            // If we have a current user
            // we keep it while waiting the новий user
            | Loaded user ->
                Loading (Some user)
            | _ -> Loading None

        newModel, Cmd.OfPromise.either getRandomUser () FetchResponse FetchError

    // We got a response and decoding succeded
    | FetchResponse (Ok user) ->
        Loaded user, Cmd.none

    // We got a response and decoding failed
    | FetchResponse (Error msg) ->
        JS.console.error msg
        Errored, Cmd.none

    // An error occured, when fetching the новий user
    | FetchError error ->
        JS.console.error error.Message
        Errored, Cmd.none

// VIEW (rendered із React)

нехай інлайн приватний renderInfo iconClass value =
    нехай iconClass = "fa " + iconClass
    div [ ]
        [ span [ Class "icon" ]
            [ i [ Class iconClass ]
                [ ] ]
          str " "
          str value ]

нехай інлайн приватний viewMessage color msg =
    div [ Class ("message " + color) ]
        [ div [ Class "message-body" ]
            [ str msg ] ]

нехай приватний viewLoading =
    viewMessage "is-info" "Waiting the server response..."

нехай приватний viewErrored =
    viewMessage "is-danger" "An error occured, please check the console для more information."

нехай приватний viewUser (user : User) =
    нехай birthday =
        user.Birthday.ToShortDateString()

    div [ Class "card is-avatar" ]
        [ div [ Class "card-image" ]
            [ figure [ Class "image is-128x128" ]
                [ img [ Class "is-rounded"
                        Src user.Picture ] ] ]
          div [ Class "card-content" ]
            [ div [ Class "content has-text-centered" ]
                [ div [ Class "has-text-weight-semibold is-size-5" ]
                    [ str user.FullName ]
                  div [ Class "is-italic" ]
                    [ str birthday ] ]
              div [ Class "content" ]
                [ renderInfo "fa-phone" user.CellPhone
                  renderInfo "fa-phone" user.OfficePhone
                  renderInfo "fa-envelope" user.Email ] ] ]

нехай приватний viewGenerateButton isLoading dispatch =
    нехай buttonClass =
        якщо isLoading тоді
            " is-loading"
        інакше
            ""
        |> (+) "button is-primary "

    div [ Class "has-text-centered" ]
        [ div [ Class buttonClass
                OnClick (фун _ ->
                    dispatch FetchRandomUser
                ) ]
            [ str "Generate a новий user" ] ]

нехай приватний center child =
    div [ Class "columns is-mobile" ]
        [ div [ Class "column is-3" ] [ ]
          div [ Class "column" ] [ child ]
          div [ Class "column is-3" ] [ ] ]

нехай view model dispatch =
    нехай (isLoading, content) =
        співстав model із
        | Loading None ->
            true, viewLoading
        | Loading (Some user) ->
            true, viewUser user
        | Loaded user ->
            false, viewUser user
        | Errored ->
            false, viewErrored

    section [ Class "hero is-fullheight" ]
        [ div [ Class "hero-body" ]
            [ div [ Class "container" ]
                [ center (viewGenerateButton isLoading dispatch)
                  center content ] ] ]

// App
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.run
