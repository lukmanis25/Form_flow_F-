type User = { Username: string; Password: string; Email: string }
type Preferences = { Language: string; NotificationsEnabled: bool } //for zad2

type ValidatedPreferences = { Language: string; NotificationsEnabled: bool }
type ValidatedUser = { Username: string; HashedPassword: string; Email: string }
type CombinedData = { User: ValidatedUser; Preferences: ValidatedPreferences } //for zad2

type RegisteredUser = { Username: string; RegistrationDate: System.DateTime; Email: string } 

type Error =
    | InvalidData
    | NotSupportedLanguage
    | PasswordTooShort
    | UsernameTaken
    | DatabaseSaveError
    | InvalidEmail


// User validation
let validateUser (user: User) : Result<ValidatedUser, Error> =
    let emailRegex = @"^[^\s@]+@[^\s@]+\.[^\s@]+$"
    if user.Username = "" || user.Password = "" || user.Email = "" then
        Error InvalidData
    elif user.Password.Length < 6 then
        Error PasswordTooShort
    elif not (System.Text.RegularExpressions.Regex.IsMatch(user.Email, emailRegex)) then
        Error InvalidEmail
    else
        let hashedPassword = "HASH_" + user.Password // Simulated hashing
        Ok { Username = user.Username; HashedPassword = hashedPassword; Email = user.Email }

// Validate user preferences
let validatePreferences (preferences: Preferences) : Result<ValidatedPreferences, Error> =
    let supportedLanguages = ["en"; "pl"; "de"]
    if not (List.contains preferences.Language supportedLanguages) then
        Error NotSupportedLanguage
    else
        Ok { Language = preferences.Language; NotificationsEnabled = preferences.NotificationsEnabled }

// Combine validated user data with validated preferences
let combineData (validatedUser: ValidatedUser) (validatedPreferences: ValidatedPreferences) : CombinedData =
    { User = validatedUser; Preferences = validatedPreferences }

// Check if the username is available
let checkUsernameAvailability (user: ValidatedUser) : Result<ValidatedUser, Error> =
    let isUsernameTaken = false // Simulate checking in the database
    if isUsernameTaken then Error UsernameTaken else Ok user

// Register the user
let registerUser (user: ValidatedUser) : Result<RegisteredUser, Error> =
    let registeredUser = { Username = user.Username; RegistrationDate = System.DateTime.Now; Email = user.Email }
    let isSaved = false // Simulate saving in the database
    printfn "User '%s' has been successfully registered." user.Username
    if isSaved then Error DatabaseSaveError else Ok registeredUser

// Register the user
let registerUserWithPreferences (data: CombinedData) : Result<RegisteredUser, Error> =
    let registeredUser = { 
        Username = data.User.Username; 
        RegistrationDate = System.DateTime.Now; 
        Email = data.User.Email;
    }
    let isSaved = false // Simulate saving in the database
    printfn "User '%s' has been successfully registered with preferences." data.User.Username
    if isSaved then Error DatabaseSaveError else Ok registeredUser

// Greet the user
let greetUser (user: RegisteredUser) : RegisteredUser =
    printfn "Welcome, %s! Your account has been successfully registered." user.Username
    user

// Simulate sending an email
let sendEmail (user: RegisteredUser) : RegisteredUser =
    printfn "An email has been sent to %s." user.Email
    user

// let map singleTrackFunction =
//     Result.bind (singleTrackFunction >> Ok)


// let greetUserMapped (user: RegisteredUser) : Result<RegisteredUser, Error> =
//     user |> Ok |> map greetUser

let map singleTrackFunction : 'a -> Result<'b, 'c> =
    fun input -> singleTrackFunction input |> Ok


let registrationProcess (user: User) =
    validateUser user
    |> Result.bind checkUsernameAvailability
    |> Result.bind registerUser
    |> Result.bind (map greetUser)
    |> Result.bind (map sendEmail)


let (>>=) result func =
    match result with
    | Ok value -> func value
    | Error err -> Error err

let registrationProcessWithOperator (user: User) =
    validateUser user
    >>= checkUsernameAvailability
    >>= registerUser
    >>= map greetUser
    >>= map sendEmail



type RegisterBuilder() =
    member this.Bind(x, f) = Result.bind f x
    member this.Return(x) = Ok x

let register = RegisterBuilder()


let registrationProcessWithBuilder (user: User) =
    register {
        let! validated = validateUser user
        let! validatedUsername = checkUsernameAvailability validated
        let! registered = registerUser validatedUsername
        let! registered2 = map greetUser registered
        let! registered3= map sendEmail registered2
        return registered3
    }

let registrationWithPreferences (user: User) (preferences: Preferences) =
    validateUser user
    |> Result.bind (fun validatedUser ->
        validatePreferences preferences
        |> Result.map (combineData validatedUser)
    )
    |> Result.bind registerUserWithPreferences
    |> Result.bind (map greetUser)
    |> Result.bind (map sendEmail)

let registrationWithPreferencesBuilder (user: User) (preferences: Preferences) =
    register {
        let! validatedUser = validateUser user
        let! validatedPreferences = validatePreferences preferences
        let combined = combineData validatedUser validatedPreferences
        let! registered = registerUserWithPreferences combined
        let! registered2 = map greetUser registered
        let! registered3= map sendEmail registered2
        return registered3
    }


//Łukasz Smoliński 184306
//zad1 - zrobione
//zad2 - zrobione
//bonus - nie zrobiony

//WYWOŁANIE PRZEPŁYWÓW
//ZAD1
//OK
let user = { Username = "TestUser"; Password = "Password123"; Email = "testuser@example.com" }

registrationProcess user
registrationProcessWithOperator user
registrationProcessWithBuilder user

//Fail
let userFailed = { Username = ""; Password = "Password123"; Email = "testuser@example.com" }

registrationProcess userFailed
registrationProcessWithOperator userFailed
registrationProcessWithBuilder userFailed

//ZAD2
//OK
let preferences : Preferences = { Language = "en"; NotificationsEnabled = true }

registrationWithPreferences user preferences
registrationWithPreferencesBuilder user preferences

//FAIL
let preferencesFailed : Preferences = { Language = "fr"; NotificationsEnabled = true }

registrationWithPreferences user preferencesFailed
registrationWithPreferencesBuilder user preferencesFailed

registrationWithPreferences userFailed preferences
registrationWithPreferencesBuilder userFailed preferences

registrationWithPreferences userFailed preferencesFailed
registrationWithPreferencesBuilder userFailed preferencesFailed
