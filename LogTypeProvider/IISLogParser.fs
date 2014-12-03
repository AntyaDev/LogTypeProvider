namespace LogTypeProvider.IIS
open LogTypeProvider

module IISLogParser = 
   open System
   open System.IO   
   open System.Collections.Generic

   type IISLogSchema = {
      HeaderData: MetaData
   }
   
   let (|Fields|_|) (stream: LogStream) = 
      if stream.Head.StartsWith("#fields", StringComparison.OrdinalIgnoreCase) then Some(stream.Head)
      else None
   
   let (|HeaderData|_|) (stream: LogStream) = 
      if stream.Head.StartsWith("#fields", StringComparison.OrdinalIgnoreCase) then None
      elif stream.Head.StartsWith("#", StringComparison.OrdinalIgnoreCase) then Some(stream.Head)
      else None
   
   let (|Body|_|) (stream: LogStream) = 
      if stream.Head.StartsWith("#", StringComparison.OrdinalIgnoreCase) then None
      else Some(stream.Head)
   
   let initLogStream (content: string) = 
      content.Split([| Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries)
      |> List.ofArray
      |> List.map (fun line -> line.Trim())
   
   let parseValue (line: string) = 
      let nameStart = line.IndexOf('#') + 1
      let nameEnd = line.IndexOf(':')
      if nameStart > 0 && nameEnd > nameStart && nameEnd < line.Length then 
         let name = line.Substring(nameStart, nameEnd - nameStart).Trim()
         let value = line.Substring(nameEnd + 1).Trim()
         Some(name, value)         
      else None

   let parseValueInfo (name, value) =
      let inferedType = TypeInference.inferType(value)
      Some { Name = name; Type = inferedType }
   
   
   let parseIISLogSchema (logStream: LogStream) = 

      let rec loop (logStream: LogStream, metaData: MetaData) = 
         match logStream with
         | HeaderData headers -> parseValue(headers) 
                                 |> Option.bind(parseValueInfo)
                                 |> function
                                    | None -> None 
                                    | Some v -> loop(logStream.Tail, v :: metaData)

         | Fields fields -> loop(logStream.Tail, metaData)
         | Body body -> Some(metaData, body)
         | _ -> None

      match loop (logStream, List.Empty) with
      | None -> None
      | Some(headers, body) -> Some { HeaderData = headers }

      
   let parseHeaderData (logStream: LogStream) =

      let rec loop (logStream: LogStream, headerData: IDictionary<string, string>) = 
         match logStream with
         | HeaderData headers -> parseValue(headers) |> function
                                 | None -> None 
                                 | Some(name, value) -> headerData.Add(name, value)
                                                        loop(logStream.Tail, headerData)
         | _ -> Some(headerData)
               
      loop(logStream, Dictionary<string, string>())