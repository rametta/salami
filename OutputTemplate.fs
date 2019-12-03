module OutputTemplate

open Giraffe.GiraffeViewEngine

let getVal (monthMap: Map<int, float>) (month: int) =
  match monthMap.TryFind month with
  | Some x -> x.ToString()
  | None -> "-"

let renderCat (cat: string, monthMap: Map<int, float>) =
  tr[] [
    yield th [] [str cat]
    yield! Seq.map (fun m -> td [] [str <| getVal monthMap m]) {1..12}
  ]


let renderYear (year: int, data: Map<string, Map<int, float>>) =
  div [_class "container"; _style "margin-top: 1em"] [
    div [_style "overflow-x: auto"] [
      table [_class "table is-bordered is-fullwidth"] [
        thead [] [
          tr [] [
            th [] [str <| year.ToString()]
            th [] [str "Jan"]
            th [] [str "Feb"]
            th [] [str "Mar"]
            th [] [str "Apr"]
            th [] [str "May"]
            th [] [str "Jun"]
            th [] [str "Jul"]
            th [] [str "Aug"]
            th [] [str "Sep"]
            th [] [str "Oct"]
            th [] [str "Nov"]
            th [] [str "Dec"]
          ]
        ]
        tbody [] (data |> Map.toList |> List.map renderCat)
      ]
    ]
  ]

let outputTemplate (data: Map<int, Map<string, Map<int, float>>>) = 
  html [] [
    head [] [
      title [] [ str "💰 Salami Expenses" ]
      meta [_name "viewport"; _content "width=device-width, initial-scale=1"]
      link [_rel "stylesheet"; _href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.4/css/bulma.min.css"]
    ]
    body [_class "section"] (data |> Map.toList |> List.map renderYear)
  ]
  |> renderHtmlDocument