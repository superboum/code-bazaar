import logging, asyncio, time

class AsyncPool:
    queue = asyncio.Queue()
    def __init__(self, concurrency=10, name="unamed-queue"):
        self.name = name
        self.workers = [ asyncio.create_task(self.worker(f'{name}-worker-{i}')) for i in range(concurrency) ]

    async def worker(self, name):
        while True:
            fn, args = await self.queue.get()
            try:
                await fn(*args)
            except Exception as e:
                logging.warning(f"task failed with {e}")
                traceback.print_exc()
            self.queue.task_done()
            logging.debug(f"{name} completed a task")

    def put(self, fn, *args):
        self.queue.put_nowait((fn, args))

    async def process(self):
        started_at = time.monotonic()
        await self.queue.join()
        total_slept_for = round(time.monotonic() - started_at, 2)
        logging.info(f"took {total_slept_for} seconds to process {self.name}")

        [ w.cancel() for w in self.workers ]
        await asyncio.gather(*self.workers, return_exceptions=True)
