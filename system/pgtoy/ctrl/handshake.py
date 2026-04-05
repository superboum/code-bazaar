from typing import Sequence
import logging

import ctrl.markers as mark
import msg.handshake as msg
import msg.gss as gss
import msg.ssl as ssl
import msg.startup_msg as start

logger = logging.getLogger(__name__)

MsgAndMarks = tuple[Sequence[msg.BackMsg], mark.HandshakeMarkers]


def handle_gss(req: gss.GSSRequest) -> MsgAndMarks:
    logger.debug("Deny GSSAPI session encryption")
    return (
        [gss.GSSResponseNo()],
        mark.HandshakeMarkers(),
    )


def handle_ssl(req: ssl.SSLRequest) -> MsgAndMarks:
    logger.debug("Deny SSL/TLS session encryption")
    return (
        [ssl.SSLResponseNo()],
        mark.HandshakeMarkers(),
    )


def handle_startup_message(req: start.StartupMessage) -> MsgAndMarks:
    logger.info(
        "Handshake done (%s, %s)",
        req.proto,
        req.params[start.Params.USER],
    )

    return (
        [msg.AuthenticationOk()],
        mark.HandshakeMarkers(sent_authentication_ok=True),
    )
