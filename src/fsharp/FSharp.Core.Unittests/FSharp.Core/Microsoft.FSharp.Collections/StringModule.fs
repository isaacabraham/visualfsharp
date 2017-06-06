// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace FSharp.Core.Unittests.FSharp_Core.Microsoft_FSharp_Collections

open System
open NUnit.Framework
open FsCheck

open FSharp.Core.Unittests.LibraryTestFx

// Various tests for the:
// Microsoft.FSharp.Collections.seq type

(*
[Test Strategy]
Make sure each method works on:
* Few charachter string ("foo")
* Empty string   ("")
* Null string (null)
*)

[<TestFixture>]
type StringModule() =
    let getChars (str:string) =
        seq {
            for c in 0 .. str.Length - 1 do
                yield str.[c]
        } |> Seq.toArray

    let [<Literal>] DefaultComparison = StringComparison.Ordinal
    [<Test>]
    member this.Concat() =
        let e1 = String.concat null ["foo"]
        Assert.AreEqual("foo", e1)
        
        let e2 = String.concat "" []
        Assert.AreEqual("", e2)
        
        let e3 = String.concat "foo" []
        Assert.AreEqual("", e3)        
        
        let e4 = String.concat "" [null]
        Assert.AreEqual("", e4)
        
        let e5 = String.concat "" [""]
        Assert.AreEqual("", e5)
        
        let e6 = String.concat "foo" ["bar"]
        Assert.AreEqual("bar", e6)
        
        let e7 = String.concat "foo" ["bav";"baz"]
        Assert.AreEqual("bavfoobaz", e7)

        let e8 = String.concat "foo" [null;"baz";null;"bar"]
        Assert.AreEqual("foobazfoofoobar", e8)
        
        CheckThrowsArgumentNullException(fun () -> String.concat "foo" null |> ignore)

    [<Test>]
    member this.Iter() =
        let result = ref 0
        do String.iter (fun c -> result := !result + (int c)) "foo"
        Assert.AreEqual(324, !result)

        do result := 0
        do String.iter (fun c -> result := !result + (int c)) null
        Assert.AreEqual(0, !result)

    [<Test>]
    member this.IterI() =
        let result = ref 0
        do String.iteri(fun i c -> result := !result + (i*(int c))) "foo"
        Assert.AreEqual(333, !result)

        result := 0
        do String.iteri(fun i c -> result := !result + (i*(int c))) null
        Assert.AreEqual(0, !result)

    [<Test>]
    member this.Map() =
        let e1 = String.map (fun c -> c) "foo"
        Assert.AreEqual("foo", e1)

        let e2 = String.map (fun c -> c) null 
        Assert.AreEqual("", e2)

    [<Test>]
    member this.MapI() =
        let e1 = String.mapi (fun i c -> char(int c + i)) "foo"
        Assert.AreEqual("fpq", e1)

        let e2 = String.mapi (fun i c -> c) null 
        Assert.AreEqual("", e2)

    [<Test>]
    member this.Filter() =
        let e1 = String.filter (fun c -> true) "foo"
        Assert.AreEqual("foo", e1)

        let e2 = String.filter (fun c -> true) null 
        Assert.AreEqual("", e2)

        let e3 = String.filter (fun c -> c <> 'o') "foo bar"
        Assert.AreEqual("f bar", e3)

        let e4 = String.filter (fun c -> c <> 'o') ""
        Assert.AreEqual("", e4)

    [<Test>]
    member this.Collect() =
        let e1 = String.collect (fun c -> "a"+string c) "foo"
        Assert.AreEqual("afaoao", e1)

        let e2 = String.collect (fun c -> null) "hello"
        Assert.AreEqual("", e2)

        let e3 = String.collect (fun c -> "") null 
        Assert.AreEqual("", e3)

    [<Test>]
    member this.Init() =
        let e1 = String.init 0 (fun i -> "foo")
        Assert.AreEqual("", e1)

        let e2 = String.init 2 (fun i -> "foo"+string(i))
        Assert.AreEqual("foo0foo1", e2)

        let e3 = String.init 2 (fun i -> null)
        Assert.AreEqual("", e3)

        CheckThrowsArgumentException(fun () -> String.init -1 (fun c -> "") |> ignore)

    [<Test>]
    member this.Replicate() = 
        let e1 = String.replicate 0 "foo"
        Assert.AreEqual("", e1)

        let e2 = String.replicate 2 "foo"
        Assert.AreEqual("foofoo", e2)

        let e3 = String.replicate 2 null
        Assert.AreEqual("", e3)

        CheckThrowsArgumentException(fun () -> String.replicate -1 "foo" |> ignore)

    [<Test>]
    member this.Forall() = 
        let e1 = String.forall (fun c -> true) ""
        Assert.AreEqual(true, e1)

        let e2 = String.forall (fun c -> c='o') "foo"
        Assert.AreEqual(false, e2)

        let e3 = String.forall (fun c -> true) "foo"
        Assert.AreEqual(true, e3)

        let e4 = String.forall (fun c -> false) "foo"
        Assert.AreEqual(false, e4)

        let e5 = String.forall (fun c -> true) (String.replicate 1000000 "x")
        Assert.AreEqual(true, e5)

        let e6 = String.forall (fun c -> false) null 
        Assert.AreEqual(true, e6)

    [<Test>]
    member this.Exists() = 
        let e1 = String.exists (fun c -> true) ""
        Assert.AreEqual(false, e1)

        let e2 = String.exists (fun c -> c='o') "foo"
        Assert.AreEqual(true, e2)

        let e3 = String.exists (fun c -> true) "foo"
        Assert.AreEqual(true, e3)

        let e4 = String.exists (fun c -> false) "foo"
        Assert.AreEqual(false, e4)

        let e5 = String.exists (fun c -> false) (String.replicate 1000000 "x")
        Assert.AreEqual(false, e5)

    [<Test>]
    member this.Length() = 
        let e1 = String.length ""
        Assert.AreEqual(0, e1)

        let e2 = String.length "foo"
        Assert.AreEqual(3, e2)

        let e3 = String.length null
        Assert.AreEqual(0, e3)
    
    [<Test>]
    member this.Contains() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) (NonEmptyString value) ->
            str.Contains(value) = String.contains value str

    [<Test>]
    member this.Compare() =
        Check.QuickThrowOnFailure <| fun (strA:string) (strB:string) ->
            String.Compare(strA, strB, DefaultComparison) = String.compare strB strA

    [<Test>]
    member this.EndsWith() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString value) (NonEmptyString str) ->
            str.EndsWith(value) = String.endsWith value str

    [<Test>]
    member this.Equals() =
        Check.QuickThrowOnFailure <|
        fun (comparisonType:StringComparison) (value:string) (NonEmptyString str) ->
            str.Equals(value, comparisonType) = String.equals comparisonType value str

    [<Test>]
    member this.IndexOf() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString value) (NonEmptyString str) ->
            let indexOf = str.IndexOf(value, DefaultComparison)
            let optionResult = String.indexOf value str
            match indexOf, optionResult with
            | -1, None -> true
            | indexOf, Some optionResult when indexOf = optionResult -> true
            | _ -> false

    [<Test>]
    member this.LastIndexOf() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString value) (NonEmptyString str) ->
            let indexOf = str.LastIndexOf(value, DefaultComparison)
            let optionResult = String.lastIndexOf value str
            match indexOf, optionResult with
            | -1, None -> true
            | indexOf, Some optionResult when indexOf = optionResult -> true
            | _ -> false
        
    [<Test>]
    member this.ReplaceChar() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString value) (newChar:char) ->
            (value.Replace(value.[0], newChar) = String.replaceChar value.[0] newChar value)

    [<Test>]
    member this.Replace() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString value) (newValue:string) ->
            let oldValue = String [| value.[0] |]
            (value.Replace(oldValue , newValue) = String.replace oldValue newValue value)

    [<Test>]
    member this.Split() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) ->
            str.Split ([| str.[0] |], StringSplitOptions.None) = String.split [| str.[0] |] str

    [<Test>]
    member this.SplitString() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) ->
            let separator = [| String [| str.[0] |] |]
            str.Split(separator, StringSplitOptions.None) = String.splitString separator str

    [<Test>]
    member this.SplitRemoveEmptyEntries() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) ->
            str.Split ([| str.[0] |], StringSplitOptions.RemoveEmptyEntries) = String.splitRemoveEmptyEntries [| str.[0] |] str

    [<Test>]
    member this.SplitStringRemoveEmptyEntries() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) ->
            let separator = [| String [| str.[0] |] |]
            str.Split(separator, StringSplitOptions.RemoveEmptyEntries) = String.splitStringRemoveEmptyEntries separator str

    [<Test>]
    member this.StartsWith() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) (NonEmptyString value) ->
            str.EndsWith value = String.endsWith value str

    [<Test>]
    member this.SubstringLength() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) (PositiveInt length) ->
            length < str.Length ==> lazy
                str.Substring(0, length) = String.substringLength 0 length str

    [<Test>]
    member this.Substring() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) (PositiveInt length) ->
            length < str.Length ==> lazy
                str.Substring(length) = String.substring length str
       
    [<Test>]
    member this.ToLower() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) ->
            str.ToLower() = String.toLower str
        
    [<Test>]
    member this.ToUpper() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) ->
            str.ToUpper() = String.toUpper str

    [<Test>]
    member this.Trim() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) ->
            str.Trim() = String.trim str
        
    [<Test>]
    member this.TrimChars() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) ->
            let trimChars = [| str.[0] |]
            str.Trim trimChars = String.trimChars trimChars str

    [<Test>]
    member this.TrimStart() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) ->
            let trimChars = [| str.[0] |]
            str.TrimStart trimChars = String.trimStart trimChars str

    [<Test>]
    member this.TrimEnd() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) ->
            let trimChars = [| str.[str.Length - 1] |]
            str.TrimEnd trimChars = String.trimEnd trimChars str

    [<Test>]
    member this.ToList() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) ->
            getChars str |> Array.toList = String.toList str

    [<Test>]
    member this.ToArray() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) ->
            getChars str = String.toArray str

    [<Test>]
    member this.ToSeq() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) ->
            getChars str = (str |> String.toSeq |> Seq.toArray)

    [<Test>]
    member this.OfList() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) ->
            str = (str.ToCharArray() |> Array.toList |> String.ofList)

    [<Test>]
    member this.OfArray() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) ->
            str = (str.ToCharArray() |> String.ofArray)

    [<Test>]
    member this.OfSeq() =
        Check.QuickThrowOnFailure <| fun (NonEmptyString str) ->
            str = (str.ToCharArray() |> Array.toSeq |> String.ofSeq)