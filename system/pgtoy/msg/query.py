
import ctx.query as query


type QFrontMsg = Query | Parse | Bind | Execute | Describe | Close | Flush | Sync
type QBackMsg = ParseComplete | ErrorResponse | PortalSuspended | CommandComplete | EmptyQueryResponse | RowDescription | CloseComplete | ReadyForQuery
