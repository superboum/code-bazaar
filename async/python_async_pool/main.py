import asyncio, random, logging
from async_pool import AsyncPool

async def slow_io(v):
    sleep = random.randint(1,10)
    await asyncio.sleep(sleep)
    logging.debug(f"{v} slept for {sleep} sec, done.")



async def main():
    pool = AsyncPool(concurrency=3, name="example")
    for task in range(9):
        pool.put(slow_io, task)
    await pool.process()

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    asyncio.run(main())
