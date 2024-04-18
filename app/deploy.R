rsconnect::setAccountInfo(
  name   = Sys.getenv("SHINY_USER"),
  token  = Sys.getenv("SHINY_TOKEN"),
  secret = Sys.getenv("SHINY_SECRET")
  )

rsconnect::deployApp(
  appDir = "app",
  appName = "epiworldRShiny"
  )