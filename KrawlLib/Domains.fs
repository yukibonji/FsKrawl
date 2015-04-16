

//namespace KrawlLib

module KrawlLib.Domains
open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

type Domain = Domain of string
and DomainRulePart = 
    | Wildcard
    | Exception of string
    | Constant of Domain
and DomainRule = 
    {Head: DomainRulePart;
    Tail: Domain list}
    member d.Parts = seq { for p in List.rev d.Tail do yield Constant(p)
                           yield d.Head} |> List.ofSeq

let domainParts (d:DomainRule) = 
    seq {
        for d in List.rev d.Tail do 
            yield Constant(d)
        yield d.Head        
    }

let rules = seq {for line in File.ReadLines(@"C:\Users\Isak\dev\Krawl\effective_tld_names.dat") do
                    let line = line.Trim()
                    if not (String.IsNullOrWhiteSpace(line)) && not (line.StartsWith("//")) then
                        let parts = line.Split('.')
                        let head = match parts.[0] with
                                    | "*" -> Wildcard
                                    | s when s.StartsWith("!") -> Exception(s.Substring(1))
                                    | s -> Constant(Domain(s))
                        let tail = if parts.Length > 1 then
                                        [for p in parts.[1..parts.Length - 1] -> Domain(p)]
                                   else
                                        []
                        yield {Head=head; Tail=tail}}

type RuleTree =
    { Label: string;      
      Children: Map<string, RuleTree>; }

let ruleLabel rule = 
    match rule with 
    | Wildcard -> "*"
    | Exception(s) -> sprintf "!%s" s
    | Constant(Domain(s)) -> s

let rec addRules (m:RuleTree) parts =
    match parts with
    | x::xs ->
        let lbl = ruleLabel x
        let child = if Map.containsKey lbl m.Children then
                        Map.find lbl m.Children
                    else
                        {Label=lbl; Children= Map.empty}

        let newChild = addRules child xs
        {m with Children = Map.add lbl newChild m.Children}
    | _ -> m

let rulesTree = Seq.fold (fun (memo:RuleTree) (r:DomainRule) -> addRules memo (r.Parts))
                        {Label=String.Empty; Children= Map.empty}
                        rules//(Seq.take 20 rules)

type DomainParseResult = 
    | WildcardRule of rule:string * domain:string
    | ExceptionRule of rule:string * domain:string
    | Standard of rule:string * domain:string
    | NoMatch

let rec findRule ruleTree (parts:string array) (idx:int) : DomainParseResult = 
    if idx < parts.Length then
        let part = parts.[idx]
        if Map.containsKey part ruleTree.Children then
            let rt = Map.find part ruleTree.Children
            findRule rt parts (idx + 1)                
        elif Map.containsKey ("!" + part) ruleTree.Children then
            let s = List.map ((Array.get) parts) [0..idx - 1]
                    |> List.rev
                    |> fun ds ->  String.Join(".", ds)
            let rule = sprintf "!%s.%s" part s
            let domain = sprintf "%s.%s" part s
            ExceptionRule(rule=rule, domain=domain)
        elif Map.containsKey "*" ruleTree.Children then
            let sfx = List.map ((Array.get) parts) [0..idx - 1]
                       |> List.rev
                       |> fun ds ->  String.Join(".", ds)
            let ruleStr = sprintf "*.%s" sfx
            let domainRest = [idx + 1; idx] 
                             |> List.filter ((>) (parts.Length))
                             |> List.map ((Array.get) parts)
                             |> fun ds ->  String.Join(".", ds)                                         
            let domain = String.Join(".", [domainRest;sfx])
            WildcardRule(rule = ruleStr, domain= domain)
        else
            let rule = List.map ((Array.get) parts) [0..idx - 1]
                       |> List.rev
                       |> fun ds ->  String.Join(".", ds)
            let domain = sprintf "%s.%s" part rule            
            if idx > 0 then Standard(rule=rule, domain=domain) else NoMatch  
    else
        let rule = List.map ((Array.get) parts) [0..idx-1]
                   |> List.rev
                   |> fun ds ->  String.Join(".", ds)
        Standard(rule=rule, domain=rule)

let parse' (domain:string) =
    let parts = Array.rev (domain.Split('.'))
    findRule rulesTree parts 0

let parse (domain:string) =
    match parse' domain with
    | WildcardRule(_, domain) -> (true, domain)
    | Standard(_, domain) -> (true, domain)
    | ExceptionRule(_, domain) -> (true, domain)
    | NoMatch -> (false, null)
        

        
