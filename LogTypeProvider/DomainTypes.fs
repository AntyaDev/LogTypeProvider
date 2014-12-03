namespace LogTypeProvider

[<AutoOpen>]
module DomainTypes =

 type LogStream = string list

 type ValueInfo = {
    Name: string
    Type: System.Type
 } 

 type MetaData = ValueInfo list 