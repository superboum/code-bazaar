import ..ctx.auth as ctx
import ..msg.handshake as msg

@dataclass
class AuthDispath:
    ctx: State

    def register(m: msg.Handshake) -> 
