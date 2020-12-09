# Youtube Crawling

As many Youtube users, I have the intuition that
the content shown in my "Subscription" page is missing some video
from channels I am following.

To check if I am crazy or if Youtube is not showing everything, I compared:
  - The content of this "Subscription" page
	- The content of the RSS feed of all channels I am following

The script I provide here is very minimal.
You must crawl your subscription page and save the traffic as HAR.
You must also get your subscription list through Google Takeout.
From the two files, my script will crawl the RSS feeds of all your subscribed channel and will check if the video appears in your feed.

In my case, I am subscribing to 864 YT channels.
I crawled my feed for around 1 month (from 2020-12-09 to 2020-11-06).
The following video were missing from my feed :
  - [Cyberpunk 2077 — Russian Celebrity Cast](https://www.youtube.com/watch?v=owglj0VH83U)
	- [Cyberpunk 2077 — Oficjalny Zwiastun Premierowy — V](https://www.youtube.com/watch?v=akHMPv5x3Sw)
	- [Cyberpunk 2077 — Официальный релизный трейлер — Ви](https://www.youtube.com/watch?v=aSrFWinrkeQ)
	- [Cyberpunk 2077 — Trailer de Lançamento Oficial — V](https://www.youtube.com/watch?v=liuFhVXAlZw)
	- [사이버펑크 2077 — 공식 출시 트레일러 — V](https://www.youtube.com/watch?v=jouPRLPCcHU)
	- [赛博朋克2077——发售宣传片——V](https://www.youtube.com/watch?v=eQCL8prX9Cc)
	- [Cyberpunk 2077 — Tráiler de lanzamiento — V](https://www.youtube.com/watch?v=VhM3NRu23Sk)
	- [Cyberpunk 2077 — Offizieller Launch-Trailer — V](https://www.youtube.com/watch?v=OjLlHigo9-8)
	- 
