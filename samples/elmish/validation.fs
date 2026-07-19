модуль Elmish.Validation

 // Form Validation у Elmish, by Zaid Ajaj

відкрити System
відкрити Fable.Core
відкрити Browser.Types
відкрити Elmish
відкрити Elmish.React
відкрити Fable.React
відкрити Fable.React.Props

тип LoginResult =
    | Success з token:string
    | UsernameDoesNotExist
    | PasswordIncorrect
    | LoginError з errorMsg:string

тип LoginInfo =
    { Username : string
      Password : string }

тип Msg =
    | Login
    | ChangeUsername з string
    | ChangePassword з string
    | LoginSuccess з adminSecureToken: string
    | LoginFailed з error:string
    | UpdateValidationErrors

тип State = {
    LoggingIn: bool
    InputUsername: string
    UsernameValidationErrors: string list
    PasswordValidationErrors: string list
    InputPassword: string
    HasTriedToLogin: bool
    LoginError: string option
}


модуль Http =
    нехай приватний loginAsync (info: LoginInfo) =
        async {
            // simulate server word
            do! Async.Sleep 1500
            повернути LoginResult.Success "my-secure-access-token"
        }

    нехай login (info: LoginInfo) =

        нехай successHandler = функція
            | Success token -> LoginSuccess token
            | UsernameDoesNotExist -> LoginFailed "Username does not exist"
            | PasswordIncorrect -> LoginFailed "The password you entered is incorrect"
            | LoginError error -> LoginFailed error

        Cmd.OfAsync.either loginAsync info
            successHandler
            (фун ex -> LoginFailed "Unknown error occured while logging you у")


нехай init() =
    { InputUsername = ""
      InputPassword = ""
      UsernameValidationErrors =  [ ]
      PasswordValidationErrors =  [ ]
      HasTriedToLogin = false
      LoginError = None
      LoggingIn = false }, Cmd.none


нехай validateInput (state: State) =
  нехай usernameRules =
    [ String.IsNullOrWhiteSpace(state.InputUsername), "Field 'Username' cannot be empty"
      state.InputUsername.Trim().Length < 5, "Field 'Username' must at least have 5 characters" ]
  нехай passwordRules =
    [ String.IsNullOrWhiteSpace(state.InputPassword), "Field 'Password' cannot be empty"
      state.InputPassword.Trim().Length < 5, "Field 'Password' must at least have 5 characters" ]
  нехай usernameValidationErrors =
      usernameRules
      |> List.filter fst
      |> List.map snd
  нехай passwordValidationErrors =
      passwordRules
      |> List.filter fst
      |> List.map snd

  usernameValidationErrors, passwordValidationErrors


нехай update msg (state: State) =
    співстав msg із
    | ChangeUsername name ->
        нехай nextState = { state із InputUsername = name }
        nextState, Cmd.ofMsg UpdateValidationErrors

    | ChangePassword pass ->
        нехай nextState = { state із InputPassword = pass }
        nextState, Cmd.ofMsg UpdateValidationErrors

    | UpdateValidationErrors ->
        нехай usernameErrors, passwordErrors = validateInput state
        нехай nextState =
            { state із UsernameValidationErrors = usernameErrors
                         PasswordValidationErrors = passwordErrors }
        nextState, Cmd.none

    | Login ->
        нехай state = { state із HasTriedToLogin = true }
        нехай usernameErrors, passwordErrors =
           validateInput state
        нехай startLogin =
            List.isEmpty usernameErrors
         && List.isEmpty passwordErrors

        якщо not startLogin тоді state, Cmd.none
        інакше
          нехай nextState = { state із LoggingIn = true }
          нехай credentials = {
              Username = state.InputUsername
              Password = state.InputPassword
          }

          nextState, Http.login credentials

    | LoginSuccess token ->
        нехай nextState = { state із LoggingIn = false }
        nextState, Cmd.none

    | LoginFailed error ->
        нехай nextState =
            { state із
                LoginError = Some error
                LoggingIn = false }

        nextState, Cmd.none

тип InputType = Text | Password

нехай textInput inputLabel initial inputType (onChange: string -> unit) =
  нехай inputType = співстав inputType із
                  | Text -> "input"
                  | Password -> "password"
  div
    [ Class "form-group" ]
    [ input [ Class "form-control form-control-lg"
              Type inputType
              DefaultValue initial
              Placeholder inputLabel
              OnChange (фун e ->
                нехай el = e.target :?> HTMLInputElement
                onChange el.value) ] ]

нехай loginFormStyle =
  Style [ Width "400px"
          MarginTop "70px"
          TextAlign TextAlignOptions.Center ]

нехай cardBlockStyle =
  Style [ Padding "30px"
          TextAlign TextAlignOptions.Left
          BorderRadius 10 ]

нехай errorMessagesIfAny triedLogin = функція
  | [ ] -> None
  | _ when triedLogin = false -> None
  | errors ->
    нехай errorStyle = Style [ Color "crimson"; FontSize 12 ]
    ul [ ]
       [ для error у errors ->
          li [ errorStyle ] [ str error ] ] |> Some

нехай appIcon =
  img [ Src "https://zaid-ajaj.github.io/elmish-login-flow-validation/img/fable_logo.png"
        Style [ Height 80; Width 100 ] ]

нехай render (state: State) dispatch =

    нехай loginBtnContent =
      якщо state.LoggingIn тоді i [ Class "fas fa-circle-notch fa-spin" ] []
      інакше str "Login"

    нехай validationRules =
      [ state.InputUsername.Trim().Length >= 5
        state.InputPassword.Trim().Length >= 5 ]

    нехай canLogin = Seq.forall id validationRules

    нехай btnClass =
      якщо canLogin
      тоді "btn btn-success btn-lg"
      інакше "btn btn-info btn-lg"
    div
      [ Class "container" ; loginFormStyle ]
      [ div
         [ Class "card" ]
         [ div
             [ Class "card-block"; cardBlockStyle ]
             [ div
                [ Style [ TextAlign TextAlignOptions.Center ] ]
                [ appIcon ]
               br []
               textInput "Username" state.InputUsername Text (ChangeUsername >> dispatch)
               ofOption (errorMessagesIfAny state.HasTriedToLogin state.UsernameValidationErrors)
               textInput "Password" state.InputPassword Password (ChangePassword >> dispatch)
               ofOption (errorMessagesIfAny state.HasTriedToLogin state.PasswordValidationErrors)
               div
                [ Style [ TextAlign TextAlignOptions.Center ] ]
                [ button
                    [ Class btnClass
                      OnClick (фун e -> dispatch Login) ]
                    [ loginBtnContent ] ] ] ] ]


Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
