local skynet = require "skynet"
local socket = require "socket"
local websocket = require "websocket"
local httpd = require "http.httpd"
local sockethelper = require "http.sockethelper"

local handler = {}
function handler.on_open(ws)
    print(string.format("%d::open", ws.id))
end

function handler.on_message(ws, msg)
    print(string.format("%d receive:%s", ws.id, msg))
    ws:send_text(msg .. " from server")

    skynet.timeout(200, function()
        ws:close()
    end)
end

function handler.on_error(ws, msg)
    print("Error. Client may be force closed.")
end

function handler.on_close(ws, code, reason)
    print(string.format("%d close:%s  %s", ws.id, code, reason))
end

local function handle_socket(id)
    -- limit request body size to 8192 (you can pass nil to unlimit)
    local code, url, method, header, body = httpd.read_request(sockethelper.readfunc(id), 8192)
    if code then
        if url == "/ws" then
            local ws = websocket.new(id, header, handler)
            ws:start()
        end
    end
end

skynet.start(function()
    local address = "0.0.0.0:8001"
    skynet.error("Listening "..address)
    local id = assert(socket.listen(address))
    socket.start(id , function(id, addr)
        socket.start(id)
        pcall(handle_socket, id)
    end)
    skynet.newservice("debug_console", "0.0.0.0", 8000)
end)
