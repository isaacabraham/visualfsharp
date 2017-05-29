// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.FSharp.Core

    open System
    open System.Text
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Core.Operators.Checked
    open Microsoft.FSharp.Collections

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module String =

        let inline emptyIfNull str = 
            match str with
            | null -> String.Empty
            | _ -> str

        [<CompiledName("Concat")>]
        let concat sep (strings : seq<string>) =  
            String.Join(sep, strings)

        [<CompiledName("Iterate")>]
        let iter (f : (char -> unit)) (str:string) =
            let str = emptyIfNull str
            for i = 0 to str.Length - 1 do
                f str.[i] 

        [<CompiledName("IterateIndexed")>]
        let iteri f (str:string) =
            let str = emptyIfNull str
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            for i = 0 to str.Length - 1 do
                f.Invoke(i, str.[i]) 

        [<CompiledName("Map")>]
        let map (f: char -> char) (str:string) =
            let str = emptyIfNull str
            let res = StringBuilder str.Length
            str |> iter (fun c -> res.Append(f c) |> ignore)
            res.ToString()

        [<CompiledName("MapIndexed")>]
        let mapi (f: int -> char -> char) (str:string) =
            let str = emptyIfNull str
            let res = StringBuilder str.Length
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            str |> iteri (fun i c -> res.Append(f.Invoke(i, c)) |> ignore)
            res.ToString()

        [<CompiledName("Filter")>]
        let filter (f: char -> bool) (str:string) =
            let str = emptyIfNull str
            let res = StringBuilder str.Length
            str |> iter (fun c -> if f c then res.Append c |> ignore)
            res.ToString()

        [<CompiledName("Collect")>]
        let collect (f: char -> string) (str:string) =
            let str = emptyIfNull str
            let res = StringBuilder str.Length
            str |> iter (fun c -> res.Append(f c) |> ignore)
            res.ToString()

        [<CompiledName("Initialize")>]
        let init (count:int) (initializer: int-> string) =
            if count < 0 then invalidArgInputMustBeNonNegative "count" count
            let res = StringBuilder count
            for i = 0 to count - 1 do 
               res.Append(initializer i) |> ignore
            res.ToString()

        [<CompiledName("Replicate")>]
        let replicate (count:int) (str:string) =
            if count < 0 then invalidArgInputMustBeNonNegative "count" count
            let str = emptyIfNull str
            let res = StringBuilder str.Length
            for i = 0 to count - 1 do 
               res.Append str |> ignore
            res.ToString()

        [<CompiledName("ForAll")>]
        let forall f (str:string) =
            let str = emptyIfNull str
            let rec check i = (i >= str.Length) || (f str.[i] && check (i+1)) 
            check 0

        [<CompiledName("Exists")>]
        let exists f (str:string) =
            let str = emptyIfNull str
            let rec check i = (i < str.Length) && (f str.[i] || check (i+1)) 
            check 0  

        [<CompiledName("Length")>]
        let length (str:string) =
            let str = emptyIfNull str
            str.Length
        
        [<CompiledName("Contains")>]
        let contains (value:string) (str:string) =
            if isNull str then false
            else str.Contains(value)

        [<CompiledName("CompareComparison")>]
        let compareComparison (comparisonType:StringComparison) (strB:string) (strA:string) =
            String.Compare(strA, strB, comparisonType)

        [<CompiledName("Compare")>]
        let compare (strB:string) (strA:string) =
            compareComparison StringComparison.InvariantCultureIgnoreCase strB strA

        [<CompiledName("EndsWithComparison")>]
        let endsWithComparison (comparisonType:StringComparison) (value:string) (str:string) =
            if isNull str then false
            else str.EndsWith(value, comparisonType)

        [<CompiledName("EndsWith")>]
        let endsWith (value:string) (str:string) =
            endsWithComparison StringComparison.InvariantCultureIgnoreCase value str
        
        [<CompiledName("Equals")>]
        let equals (comparisonType:StringComparison) (value:string) (str:string) =
            if isNull str then false
            else str.Equals(value, comparisonType)

        let inline checkIndex (func:_ * StringComparison -> _) comparisonType value str =
            if isNull str then None
            else
                let index = func(value, comparisonType)
                if index = -1 then None
                else Some index
            
        [<CompiledName("IndexOfComparison")>]
        let indexOfComparison (comparisonType:StringComparison) (value:string) (str:string) =
            checkIndex str.IndexOf comparisonType value str

        [<CompiledName("LastIndexOfComparison")>]
        let lastIndexOfComparison (comparisonType:StringComparison) (value:string) (str:string) =
            checkIndex str.LastIndexOf comparisonType value str

        [<CompiledName("IndexOf")>]
        let indexOf (value:string) (str:string) =
            indexOfComparison StringComparison.InvariantCultureIgnoreCase value str

        [<CompiledName("LastIndexOf")>]
        let lastIndexOf (value:string) (str:string) =
            lastIndexOfComparison StringComparison.InvariantCultureIgnoreCase value str