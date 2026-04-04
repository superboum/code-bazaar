from .cmd_id import FrontMsgType

type QFrontMsg = Query | Parse | Bind | Execute | Describe | Close | Flush | Sync
type QBackMsg = ParseComplete | ErrorResponse | PortalSuspended | CommandComplete | EmptyQueryResponse | RowDescription | CloseComplete | ReadyForQuery


    @staticmethod
    async def deserialize(reader: io.Reader) -> Self:
        kind = await Front

    
