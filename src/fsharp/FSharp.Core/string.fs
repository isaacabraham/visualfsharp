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
        let [<Literal>] DefaultComparison = StringComparison.Ordinal
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
            str.Contains(value)

        [<CompiledName("Compare")>]
        let compare (strB:string) (strA:string) =
            String.Compare(strA, strB, DefaultComparison)

        [<CompiledName("EndsWith")>]
        let endsWith (value:string) (str:string) =
            str.EndsWith(value, DefaultComparison)
        
        [<CompiledName("Equals")>]
        let equals (comparisonType:StringComparison) (value:string) (str:string) =
            str.Equals(value, comparisonType)

        let inline checkIndex func (comparisonType:StringComparison) value =
            let index = func(value, comparisonType)
            if index = -1 then None
            else Some index
            
        [<CompiledName("IndexOf")>]
        let indexOf (value:string) (str:string) =
            checkIndex str.LastIndexOf DefaultComparison value

        [<CompiledName("LastIndexOf")>]
        let lastIndexOf (value:string) (str:string) =
            checkIndex str.LastIndexOf DefaultComparison value

        [<CompiledName("ReplaceChar")>]
        let replaceChar (oldChar:char) (newChar:char) (str:string) =
            str.Replace(oldChar, newChar)

        [<CompiledName("Replace")>]
        let replace (oldValue:string) (newValue:string) (str:string) =
            str.Replace(oldValue, newValue)
        
        [<CompiledName("Split")>]
        let split (separator:string array) (str:string) =
            str.Split(separator, StringSplitOptions.None)

        [<CompiledName("SplitChar")>]
        let splitChar (separator:char array) (str:string) =
            str.Split(separator)

        [<CompiledName("StartsWith")>]
        let startsWith (value:string) (str:string) = 
            str.StartsWith(value, DefaultComparison)

        [<CompiledName("SubstringLength")>]
        let substringLength (startIndex:int) (length: int) (str:string) =
            str.Substring(startIndex, length)

        [<CompiledName("Substring")>]
        let substring (startIndex:int) (str:string) =
            str.Substring(startIndex)
        
        [<CompiledName("ToLower")>]
        let toLower(str:string) =
            str.ToLowerInvariant()
        
        [<CompiledName("ToUpper")>]
        let toUpper(str:string) =
            str.ToUpperInvariant()

        [<CompiledName("Trim")>]
        let trim(str:string) =
            str.Trim()

        [<CompiledName("TrimChars")>]
        let trimChars (trimChars:char []) (str:string) =
            str.Trim(trimChars)

        [<CompiledName("TrimStart")>]
        let trimStart (trimChars:char []) (str:string) =
            str.TrimStart(trimChars)
            
        [<CompiledName("TrimEnd")>]
        let trimEnd(trimChars:char []) (str:string) =
            str.TrimEnd(trimChars)