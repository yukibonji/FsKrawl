namespace KrawlLibTests

open NUnit.Framework
open KrawlLib.Domains

[<TestFixture>]
type DomainsTests() = 
    [<Test>]
    member x.NormalDomain () = 
        let ok, domain = parse "mail.google.com"
        Assert.That(ok, "parse succeeds")
        Assert.AreEqual("google.com", domain)

    [<Test>]
    member x.Strangedomain () = 
        let ok, domain = parse "weh.blah.s3.amazonaws.com"
        Assert.That(ok, "parse succeeds")
        Assert.AreEqual("blah.s3.amazonaws.com", domain)

    [<Test>]
    member x.ExceptionDomain () =
        let res = parse' "foo.www.ck"
        match res with
        | ExceptionRule(rule, domain) -> 
            Assert.AreEqual("!www.ck", rule)
        | _ -> Assert.Fail()

        let ok, domain = parse "foo.www.ck"
        Assert.That(ok, "parse succeeds")
        Assert.AreEqual("www.ck", domain)

    [<Test>]
    member x.WildcardDomain () =
        let res = parse' "sub1.sub2.nom.br"
        match res with
        | WildcardRule(rule, domain) -> 
            Assert.AreEqual("*.nom.br", rule)
            Assert.AreEqual("sub1.sub2.nom.br", domain)
        | _ -> Assert.Fail()

    [<Test>]
    member x.ExceptionBeatsWildcardDomain () =
        let res = parse' "sub1.sub2.sub3.ck"
        match res with
        | WildcardRule(rule, domain) -> 
            Assert.AreEqual("*.ck", rule)
            Assert.AreEqual("sub2.sub3.ck", domain)
        | _ -> Assert.Fail()

        let res = parse' "sub1.www.ck"
        match res with
        | ExceptionRule(rule, domain) -> 
            Assert.AreEqual("!www.ck", rule)
            Assert.AreEqual("www.ck", domain)
        | _ -> Assert.Fail()
        


