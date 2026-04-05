import msg.query as query
import msg.handshake as handshake

FrontMsg = query.FrontMsg | handshake.FrontMsg
BackMsg = query.BackMsg | handshake.BackMsg
