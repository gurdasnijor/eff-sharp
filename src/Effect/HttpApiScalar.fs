namespace Effect

type ScalarThemeId =
    | Alternate
    | Default
    | Moon
    | Purple
    | Solarized
    | BluePlanet
    | Saturn
    | Kepler
    | Mars
    | DeepSpace
    | Laserwave
    | NoneTheme

type ScalarConfig =
    { Theme: ScalarThemeId option
      Layout: string option
      ProxyUrl: string option
      CustomCss: string option
      DarkMode: bool option
      ShowSidebar: bool option
      HideModels: bool option
      HideSearch: bool option
      ShowOperationId: bool option }

[<RequireQualifiedAccess>]
module ScalarConfig =
    let empty =
        { Theme = None
          Layout = None
          ProxyUrl = None
          CustomCss = None
          DarkMode = None
          ShowSidebar = None
          HideModels = None
          HideSearch = None
          ShowOperationId = None }

[<RequireQualifiedAccess>]
module HttpApiScalar =

    let private themeName =
        function
        | Alternate -> "alternate"
        | Default -> "default"
        | Moon -> "moon"
        | Purple -> "purple"
        | Solarized -> "solarized"
        | BluePlanet -> "bluePlanet"
        | Saturn -> "saturn"
        | Kepler -> "kepler"
        | Mars -> "mars"
        | DeepSpace -> "deepSpace"
        | Laserwave -> "laserwave"
        | NoneTheme -> "none"

    let private optionPair key value =
        value |> Option.map (fun value -> key, JString value)

    let private boolPair key value =
        value |> Option.map (fun value -> key, JBool value)

    let private configJson (config: ScalarConfig option) =
        let config = defaultArg config ScalarConfig.empty

        [ config.Theme |> Option.map (fun theme -> "theme", JString(themeName theme))
          optionPair "layout" config.Layout
          optionPair "proxyUrl" config.ProxyUrl
          optionPair "customCss" config.CustomCss
          boolPair "darkMode" config.DarkMode
          boolPair "showSidebar" config.ShowSidebar
          boolPair "hideModels" config.HideModels
          boolPair "hideSearch" config.HideSearch
          boolPair "showOperationId" config.ShowOperationId ]
        |> List.choose id
        |> Map.ofList
        |> JObject
        |> HttpApiDocs.stringify

    let private html (api: HttpApi) (config: ScalarConfig option) =
        let spec = OpenApi.fromApi api |> HttpApiDocs.stringify |> HttpApiDocs.escapeHtml
        let title = HttpApiDocs.escapeHtml api.Identifier
        let config = configJson config

        "<!doctype html><html><head><meta charset=\"utf-8\" />"
        + "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />"
        + "<title>" + title + "</title></head><body>"
        + "<script id=\"api-reference\" type=\"application/json\">" + spec + "</script>"
        + "<script src=\"https://cdn.jsdelivr.net/npm/@scalar/api-reference\"></script>"
        + "<script>Scalar.createApiReference('#api-reference',Object.assign({content:JSON.parse(document.getElementById('api-reference').textContent)},"
        + config + "));</script>"
        + "</body></html>"

    /// Adds a Scalar documentation route for the API. The default path is `/docs`.
    let route (api: HttpApi) (path: string) (config: ScalarConfig option) (router: HttpRouter) : HttpRouter =
        HttpApiDocs.route path (html api config) router

    let routeDefault (api: HttpApi) (router: HttpRouter) : HttpRouter =
        route api "/docs" None router
