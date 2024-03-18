# Remove a particular field from a munin graph

Sometimes it's annoying that munin renders unnecessary values on a graph. For example, for apache_processes plugin, it renders "busy servers 80", "idle servers 80" and "free slots 80", but I don't want it to render "free slots 80".

To make it ignore the specified field when it renders a graph, set graph parameter to no. For the example above, use:

    [servername]
    apache_processes.free80.graph no
