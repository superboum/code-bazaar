import msg.query as query
import msg.handshake as handshake

type FrontMsg = query.FrontMsg | handshake.FrontMsg
type BackMsg = query.BackMsg | handshake.BackMsg
