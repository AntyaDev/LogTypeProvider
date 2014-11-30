namespace LogTypeProvider

module internal IISLogReader =

 open System
 open System.IO
 open TypeInference

 type MetaInfo = {
     Name: string
     Value: string
     Type: System.Type
 }

 type IISLogSchema = {
     MetaData: MetaInfo list     
 }

 let (|Fields|_|) (stream: string list) =        
     if stream.Head.StartsWith("#fields", StringComparison.OrdinalIgnoreCase) then Some(stream.Head)         
     else None

 let (|MetaInfo|_|) (stream: string list) =        
     if stream.Head.StartsWith("#fields", StringComparison.OrdinalIgnoreCase) then None
     elif stream.Head.StartsWith("#", StringComparison.OrdinalIgnoreCase) then Some(stream.Head)
     else None

 let (|Body|_|) (stream: string list) =        
     if stream.Head.StartsWith("#", StringComparison.OrdinalIgnoreCase) then None
     else Some(stream.Head)


 let initLogStream (content: string) = 
     content.Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
         |> List.ofArray
         |> List.map(fun line -> line.Trim())
        

 let parseMetaInfo (line: string) = 
     let nameStart = line.IndexOf('#') + 1
     let nameEnd = line.IndexOf(':')
     if nameStart > 0 && nameEnd > nameStart && nameEnd < line.Length then
         let name = line.Substring(nameStart, nameEnd - nameStart).Trim()
         let value = line.Substring(nameEnd + 1).Trim()
         let ``type`` = TypeInference.inferType(value)
         Some { Name = name; Value = value; Type = ``type`` }
     else None 

 let readIISLogSchema (logStream: string list) =
        
     let rec loop (logStream: string list, metaData: MetaInfo list) =
         match logStream with
         | MetaInfo info -> info |> parseMetaInfo
                                 |> function
                                    | None -> None
                                    | Some v -> loop(logStream.Tail, v :: metaData)
        
         | Fields fields -> loop(logStream.Tail, metaData)
         | Body body -> Some (metaData, body)
         | _ -> None

     match loop(logStream, List.Empty) with
     | None -> None
     | Some (mInfo, body) -> Some { MetaData = mInfo }   