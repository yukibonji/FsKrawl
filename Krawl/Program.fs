// Expanded from a small sample in the Expert FSharp book

module Program

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Net
open System.IO
open System.Threading
open System.Text.RegularExpressions
open HtmlAgilityPack
open KrawlLib

let waitingGlobalLock = ref 0
let waitingDomainLock = ref 0
let pendingReqs = ref 0

// Limit concurrent requests globally
type RequestGate(n:int) =
    let semaphore = new Semaphore(initialCount=n, maximumCount=n)

    member x.AsyncAquire(?timeout) = 
        async { incr waitingGlobalLock
                let! ok = Async.AwaitWaitHandle(semaphore, ?millisecondsTimeout=timeout)
                if ok then
                    return 
                        { new System.IDisposable with
                            member x.Dispose() = 
                                decr waitingGlobalLock
                                semaphore.Release() |>  ignore }
                else
                    return! failwith "couldn't aquire a semaphore" }

let globalRequestGate = RequestGate(500)

// Limit concurrent requets per domain
type DomainRequestGate(n:int) =
    let semaphoresByDomain = new ConcurrentDictionary<string, Semaphore>()

    member x.AsyncAquire(domain:string, ?timeout) = 
        let semaphore = semaphoresByDomain.GetOrAdd(domain, fun domain -> new Semaphore(n, n))
        async { incr waitingDomainLock
                let! ok = Async.AwaitWaitHandle(semaphore, ?millisecondsTimeout=timeout)
                if ok then
                    return 
                        { new System.IDisposable with
                            member x.Dispose() = 
                                decr waitingDomainLock
                                semaphore.Release() |>  ignore }
                else
                    return! failwith "couldn't aquire a semaphore" }

let perDomainRequestGate = DomainRequestGate(10)

let inline isNull (x:^a when ^a : not struct) =
    obj.ReferenceEquals (x, Unchecked.defaultof<_>)

let collectLinks (url:string) = 
    async { 
        let! ex, html, respUri = 
            async { 
                use! globalLock = globalRequestGate.AsyncAquire()
                let host = (new Uri(url)).Host
                let ok, domain = Domains.parse host
                let domain = if ok then domain else host
                use! domainLock = perDomainRequestGate.AsyncAquire(domain)                
                let req = WebRequest.Create(url, Timeout=5)
                incr pendingReqs                
                use! response = req.AsyncGetResponse()
                decr pendingReqs

                try
                    use reader = new StreamReader(response.GetResponseStream())
                    return (None, reader.ReadToEnd(), response.ResponseUri) 
                with
                    | ex -> return (Some(ex), String.Empty, null)}

        match ex with
        | None ->
            let doc =  new HtmlDocument()
            doc.LoadHtml(html)        
            let linkNodes = doc.DocumentNode.SelectNodes("//a[@href]")
            if (not (isNull linkNodes)) then
                let links = [for node in linkNodes do
                                let href = node.Attributes.["href"].Value
                                if href <> null then
                                    let ok, uri = Uri.TryCreate(respUri, href)
                                    if ok && not (String.IsNullOrEmpty(uri.Scheme)) && uri.Scheme.StartsWith("http") then
                                        //printfn "Found uri: %s" (uri.ToString())
                                        yield uri]
                        
                do printfn "Thread: %d G: %d, PD: %d, Reqs: %d" Thread.CurrentThread.ManagedThreadId !waitingDomainLock !waitingDomainLock !pendingReqs
                do printfn "finished reading %s, got %d links" url (List.length links)
                return links 
                       |> Seq.distinctBy (fun uri -> uri.Host) 
                       |> Seq.map (fun uri -> uri.ToString()) 
                       |> List.ofSeq
            else
                return []
        | Some(ex) ->
            do printfn "Exception getting %s. Ex: %A" url ex
            return []}

let urlCollector = 
    MailboxProcessor.Start(fun self -> 
        let rec waitForUrl (domainsVisited: Set<string>) = 
            async {
                try
                    let! url = self.Receive()
                    let ok, uri = Uri.TryCreate(url, UriKind.Absolute)
                    if ok then
                        let parseOk, domain = Domains.parse (uri.Host)
                        let domain = if parseOk then domain else uri.Host
                        if not (domainsVisited.Contains(domain)) then
                            printfn "Domain: %s" domain
                            do! (async { let! links = collectLinks url
                                         for link in links do self.Post link }) 
                                    |> Async.StartChild 
                                    |> Async.Ignore
                        return! waitForUrl (domainsVisited.Add(domain))
                    else
                        return! waitForUrl domainsVisited
                with
                    | ex -> 
                        printfn "Ex: %s" ex.Message
                        return! waitForUrl domainsVisited }
                            
        waitForUrl Set.empty)


[<EntryPoint>]
let main argv = 
    let url = "http://news.google.com"
    printfn "Starting off with %s" url
    
    urlCollector.Post url
    System.Console.ReadLine() |> ignore
    printfn "%A" argv
    0 // return an integer exit code
