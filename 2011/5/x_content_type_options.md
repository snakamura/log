# X-Content-Type-Options: nosniff

Because IE determines `Content-Type` not only by the `Content-Type` header but by scanning the contents, it may cause XSS. For example, when a server outputs something like

    <script type="text/javascript">
    // Do something more malicious
    alert(document.cookie);
    </script>

with `Content-Type` of `text/plain`, IE may run the script.

To prevent IE from determining `Content-Type` this way, you should add this header.

    X-Content-Type-Options: nosniff

As of IE8, it no longer scans contents to determine its `Content-Type` with this header.
