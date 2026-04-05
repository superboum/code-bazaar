from typing import Sequence

import ctrl.markers as mark
import msg.handshake as msg
import msg.gss as gss
import msg.ssl as ssl
import msg.startup_msg as start

MsgAndMarks = tuple[Sequence[msg.BackMsg], mark.HandshakeMarkers]

def handle_gss(req: gss.GSSRequest) -> MsgAndMarks:
    return (
        [gss.GSSResponseNo()], 
        mark.HandshakeMarkers(),
    )

def handle_ssl(req: ssl.SSLRequest) -> MsgAndMarks:
    return (
        [ssl.SSLResponseNo()], 
        mark.HandshakeMarkers(),
    )

def handle_startup_message(req: start.StartupMessage) -> MsgAndMarks:
    return (
        [ msg.AuthenticationOk() ],
        mark.HandshakeMarkers(sent_authentication_ok=True),
    )
