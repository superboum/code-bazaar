import asyncio
import coloredlogs

import server

coloredlogs.install(level="DEBUG")

asyncio.run(server.PgToy().listen())
