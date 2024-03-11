# Binding an event handler to a search box with jQuery Mobile

It's easy to use a search box with jQuery Mobile. You just need to put an input tag.

    <input id="search" type="search">

But when you bind an event handler to this element, you'll find your handler will never be called. For example,

    $('#search').keydown(function(event) {
        // This function will never be called.
    });

This is because that jQuery Mobile removes this element from the DOM tree once and inserts a modified element. So you need to use `on` instead.

    $(document).on('keydown', '#search', function(event) {
        // This function wil be called.
    });
