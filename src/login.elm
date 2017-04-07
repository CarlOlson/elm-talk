
type Maybe t =
    Nothing | Just t

type alias Login =
    { username : Maybe String
    , password : Maybe String
    }

initLogin : Login
initLogin =
    Login Nothing Nothing

setUsername : Login -> String -> Login
setUsername login username =
    { login |
          username = Just username
    }

setPassword : Login -> String -> Login
setPassword login password =
    { login |
          password = Just password
    }

isValidLogin : Login -> Bool
isValidLogin login =
    case (login.username, login.password) of
        (Just _, Just _) -> True
        _ -> False
