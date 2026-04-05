from msg.startup_msg import StartupMessage as StartupMessage
from msg.gss import GSSRequest as GSSRequest, GSSResponse as GSSResponse
from msg.ssl import SSLRequest as SSLRequest, SSLResponse as SSLResponse
from msg.authentication_ok import AuthenticationOk as AuthenticationOk

type FrontMsg = StartupMessage | GSSRequest | SSLRequest
type BackMsg = AuthenticationOk | SSLResponse | SSLResponse
