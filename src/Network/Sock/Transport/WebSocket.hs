module Network.Wai.Sock.Transport.WebSocket
( WebSocket
) where

data WebSocket = WebSocket

-- | Goals:
--
--     1. Tie WS connection with Session value (each WS connection should have it's own Session, so WS does not care about SessionIDs, see: http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-61).
--     2. Feed incoming data (from WS' receive) to the Session's incoming buffer.
--     3. Feed data from Session's outgoing buffer to WS' sendSink function.