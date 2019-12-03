open System
open System.IO
open System.Globalization
open FSharp.Data

type TransactionType =
  | Debit
  | Credit
  | Other

type Transaction = {
  Date: DateTime
  Transaction: TransactionType
  Name: string
  Memo: string
  Amount: float
  Category: string
}

type TransactionRow = {
  Date: string
  Transaction: string
  Name: string
  Memo: string
  Amount: string
}

type CategoryGroup = {
  Name: string
  Category: string
}

let transacToType: string -> TransactionType =
  fun tran ->
    match tran.ToLower() with
    | "debit" -> Debit
    | "credit" -> Credit
    | _ -> Other

let parseDate: string -> DateTime = 
  fun date ->
      DateTime.ParseExact(date, "M/d/yyyy", CultureInfo.InvariantCulture)

let toCategoryGroup: CsvRow -> CategoryGroup =
  fun row -> {
    Name = row.["name"]
    Category = row.["category"]
  }

let toTransactionRow: CsvRow -> TransactionRow =
  fun row -> {
    Date = row.["Transaction date"]
    Transaction = row.["Transaction"]
    Name = row.["Name"]
    Memo = row.["Memo"]
    Amount = row.["Amount"]
  }

let getCat: CategoryGroup list -> string -> string =
  fun groups name ->
    groups
      |> List.filter (fun g -> name.ToLower().Contains(g.Name.ToLower()))
      |> List.tryHead
      |> fun ocg ->
          match ocg with
          | Some cg -> cg.Category
          | None -> "unknown"

let toTransaction: CategoryGroup list -> TransactionRow -> Transaction =
  fun groups row ->
    {
      Date = parseDate row.Date
      Transaction = transacToType row.Transaction
      Name = row.Name
      Memo = row.Memo
      Amount = float row.Amount
      Category = getCat groups row.Name
    }

let loadCsvRows: string -> seq<CsvRow> =
  fun path ->
    CsvFile.Load(path, hasHeaders = true)
      |> fun f -> f.Rows

let getCategoryGroups: string -> CategoryGroup list =
  fun path ->
    path
      |> loadCsvRows
      |> Seq.map toCategoryGroup
      |> Seq.toList

let groupTrans: Map<int, Map<string, Map<int, float>>> -> Transaction -> Map<int, Map<string, Map<int, float>>> =
  fun yearMap tran ->
    let year = tran.Date.Year
    let month = tran.Date.Month
    let cat = tran.Category
    match yearMap.TryFind year with
    | Some monthMap ->
        match monthMap.TryFind cat with
        | Some catMap ->
            match catMap.TryFind month with
            | Some total -> yearMap.Add(year, monthMap.Add(cat, catMap.Add(month, tran.Amount + total)))
            | None -> yearMap.Add(year, monthMap.Add(cat, catMap.Add(month, tran.Amount)))
        | None -> yearMap.Add(year, monthMap.Add(cat, Map.empty.Add(month, tran.Amount)))
    | None -> yearMap.Add(year, Map.empty.Add(cat, Map.empty.Add(month, tran.Amount)))

[<EntryPoint>]
let main argv =
  let wd = Environment.CurrentDirectory
  let outputPath = Path.Combine(wd, "output.html")
  let categoriesFilePath = Path.Combine(wd, "categories.csv")
  let expensesDir = Path.Combine(wd, "expenses")
  let groups = getCategoryGroups categoriesFilePath

  Directory.GetFiles(expensesDir, "*.CSV")
    |> Array.map loadCsvRows
    |> Seq.concat
    |> Seq.map (toTransactionRow >> toTransaction groups)
    |> Seq.fold groupTrans Map.empty<int, Map<string, Map<int, float>>>
    |> OutputTemplate.outputTemplate
    |> fun html -> File.WriteAllText(outputPath, html)
  0
