namespace LogTypeProvider

module TypeInference =

 open System.Globalization
 open Microsoft.FSharp.Quotations

 let cultureInfo = CultureInfo.InvariantCulture
 let dateTimeStyles = DateTimeStyles.AllowWhiteSpaces ||| DateTimeStyles.RoundtripKind

 let convertDouble arg = System.Double.Parse(arg, NumberStyles.Currency, cultureInfo)
 let convertInteger arg = System.Int32.Parse(arg)
 let convertDateTime arg = System.DateTime.Parse(arg, cultureInfo, dateTimeStyles)

 let toOption = function
     | true, v  -> Some v
     | false, _ -> None

 let (|Date|_|) arg   = System.DateTime.TryParse(arg, cultureInfo, dateTimeStyles) |> toOption
 let (|Int|_|) arg    = System.Int32.TryParse(arg) |> toOption
 let (|Double|_|) arg = System.Double.TryParse(arg, NumberStyles.Currency, cultureInfo) |> toOption
 
 let inferType arg = 
     match arg with
     | Date date   -> typeof<System.DateTime>
     | Int integer -> typeof<int>
     | Double dec  -> typeof<double>
     | _           -> typeof<string>

 let getConversion ``type`` : Expr -> Expr =
   if ``type`` = typeof<string> then (fun arg -> <@@ %%arg @@>)
   elif ``type`` = typeof<int> then (fun arg -> <@@ convertInteger(%%arg) @@>)
   elif ``type`` = typeof<double> then (fun arg -> <@@ convertDouble(%%arg) @@>)
   elif ``type`` = typeof<System.DateTime> then (fun arg -> <@@ convertDateTime(%%arg) @@>)
   else failwith "getConversionQuotation: Unsupported primitive type"

