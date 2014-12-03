namespace LogTypeProvider.IIS

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection
open System.IO
open LogTypeProvider
open System.Collections.Generic

type IISLogFile(stream: LogStream) =   
   let headers = IISLogParser.parseHeaderData(stream)
   member this.HeaderData = headers.Value

[<AutoOpen>]
module internal IISLogProviderCreation =
 open System
 open LogTypeProvider
 open TypeInference
 open IISLogParser 

 let erasedType<'T> assemblyName rootNamespace typeName = 
     ProvidedTypeDefinition(assemblyName, rootNamespace, typeName, Some(typeof<'T>), HideObjectMethods = true)

 let runtimeType<'T> typeName = 
     ProvidedTypeDefinition(typeName, Some typeof<'T>, HideObjectMethods = true) 

 let createHeaderProperty (vInfo: ValueInfo) =
   let convert = TypeInference.getConversion(vInfo.Type)
   let name = vInfo.Name
   ProvidedProperty(vInfo.Name, vInfo.Type,
                    GetterCode = fun args -> convert(<@@ ((%%(args.[0]):obj) :?> IISLogFile).HeaderData.[name] @@>))

 let createCtor () =   
   ProvidedConstructor(parameters = [ProvidedParameter("content", typeof<string>)],
                       InvokeCode = fun [content] -> <@@ let logStream = IISLogParser.initLogStream((%%content):string)
                                                         IISLogFile(logStream)
                                                     @@>)

 let createHeaderProperties (headerData: MetaData) = 
   headerData |> List.map(createHeaderProperty)

 let ``namespace`` = "LogTypeProvider.IIS"
 let assembly = Assembly.GetExecutingAssembly() 
 
 /// Create an erased type to represent IISLogProvider
 let createIISLogProvider () =    
    let iisLogType = erasedType<obj> assembly ``namespace`` "IISLogProvider"

    let typeParams = [ProvidedStaticParameter("sample", typeof<string>)]    
    
    let initFunction (typeName: string) (parameterValues: obj[]) =            
            match parameterValues with
            | [| :? string as sample |] ->                
               
               let iisLog = erasedType<obj> assembly ``namespace`` typeName         

               IISLogParser.initLogStream(sample) 
                 |> IISLogParser.parseIISLogSchema
                 |> Option.map(fun v -> v.HeaderData |> createHeaderProperties)
                 |> Option.iter(fun prop -> prop |> List.iter(iisLog.AddMember))

               iisLog.AddMember(createCtor())

               iisLog
            | _ -> failwith "unexpected parameter values"

    iisLogType.DefineStaticParameters(typeParams, initFunction)
    iisLogType

[<TypeProvider>]
type IISLogProvider(providerConfig: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()
        
    do this.AddNamespace(``namespace``, [createIISLogProvider()])

[<TypeProviderAssembly>]
do()
