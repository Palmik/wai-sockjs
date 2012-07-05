function message(msg)
{
    $('#messagelog').append('</p>'+msg+'</p>');
}


$(document).ready(function()
{
    var ws;

    if ("WebSocket" in window)
    {
        message("WebSockets supported!");
        ws = new WebSocket("ws://0.0.0.0:8080/harr_echo/000/000/websocket");
        
        ws.onopen = function()
        {
            message("WebSocket connection opened.");
        };

        ws.onmessage = function (event)
        {
            message("Received: " + event.data);
        };

        ws.onclose = function()
        {
            message("WebSocket connection closed.");
        };
    }
    else
    {
        message("WebSockets not supported!");
    }

    $("#disconnect-button").click(function()
    {
        if (ws != null)
        {
            ws.close();
        }
    });

    $("#send-button").click(function()
    {
        if (ws != null)
        {
            ws.send($('#send-text').val());
        }
    });
})