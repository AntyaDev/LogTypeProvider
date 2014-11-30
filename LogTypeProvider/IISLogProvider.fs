namespace LogTypeProvider

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection
open System.IO

[<AutoOpen>]
module internal IISLogTypeCreation =
 open System 
 open TypeInference 
 open IISLogReader

 let erasedType<'T> assemblyName rootNamespace typeName = 
     ProvidedTypeDefinition(assemblyName, rootNamespace, typeName, Some(typeof<'T>), HideObjectMethods = true)

 let runtimeType<'T> typeName = 
     ProvidedTypeDefinition(typeName, Some typeof<'T>, HideObjectMethods = true)

 let property isStatic name ``type`` exprFunc =
     ProvidedProperty(name, ``type``, IsStatic = isStatic, GetterCode = exprFunc)

 let staticProperty name = property true name

 let createMetaInfoProperty (mInfo: MetaInfo) =
    staticProperty mInfo.Name mInfo.Type (TypeInference.getConversionQuotation(mInfo.Value, mInfo.Type))

 let createMetaInfoProperties (mInfo: MetaInfo list) =
    mInfo |> List.map(createMetaInfoProperty)
    


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

                let logStream = IISLogReader.initLogStream(sample)

                IISLogReader.readIISLogSchema(logStream)
                    |> Option.map(fun v -> v.MetaData |> createMetaInfoProperties)
                    |> Option.iter(fun prop -> prop |> List.iter(iisLog.AddMember))

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
