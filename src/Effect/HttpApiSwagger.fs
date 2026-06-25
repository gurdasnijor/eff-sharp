namespace Effect

[<RequireQualifiedAccess>]
module HttpApiSwagger =

    let private html (api: HttpApi) =
        let spec = OpenApi.fromApi api |> HttpApiDocs.stringify |> HttpApiDocs.escapeHtml
        let title = HttpApiDocs.escapeHtml api.Identifier

        "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\" />"
        + "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />"
        + "<title>" + title + " Documentation</title>"
        + "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui.css\" />"
        + "</head><body><div id=\"swagger-ui\"></div>"
        + "<script id=\"swagger-spec\" type=\"application/json\">" + spec + "</script>"
        + "<script src=\"https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui-bundle.js\"></script>"
        + "<script>window.onload=function(){SwaggerUIBundle({spec:JSON.parse(document.getElementById('swagger-spec').textContent),dom_id:'#swagger-ui'});};</script>"
        + "</body></html>"

    /// Adds a Swagger UI documentation route for the API. The default path is `/docs`.
    let route (api: HttpApi) (path: string) (router: HttpRouter) : HttpRouter =
        HttpApiDocs.route path (html api) router

    let routeDefault (api: HttpApi) (router: HttpRouter) : HttpRouter =
        route api "/docs" router
