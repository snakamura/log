# Posting a large content with curl

When you use curl to post a large content, sometimes a server may return `417` response. This is because curl add `Expect: 100-continue` to a request if it is large enough, and some servers don't support `100` continue response.

To avoid this problem, you can add `Expect` header explicitly. For example:

    curl -H 'Expect:' --data-binary '@test.dat' 'http://example.com/foo'

will make curl not send `Expect` header.
