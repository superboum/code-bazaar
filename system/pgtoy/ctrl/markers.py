from dataclasses import dataclass


@dataclass
class HandshakeMarkers:
    sent_authentication_ok: bool = False
