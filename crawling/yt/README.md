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
  - [Cyberpunk 2077 — Bande-annonce de lancement officielle — V](https://www.youtube.com/watch?v=1DSutjnvTX4)
  - [The Engineer Creating A Virtual World Using Natural Sound - Vice](https://www.youtube.com/watch?v=7JnZPEsIPVQ)
  - [(Audio Described) Look to Speak: Helping accessibility users communicate - Google](https://www.youtube.com/watch?v=7vjNHYQbd5c)
  - [Pierre Davis Is Designing Fashion’s Radical Future - Vice](https://www.youtube.com/watch?v=QfDyc73o2LA)
  - [10 domande che hai sempre voluto fare a... una persona che balbetta - Vice](https://www.youtube.com/watch?v=CGN8K3ICU4k) (This video is unavailable)
  - [Azerbaijan and Armenia at War | Vice Around the World Episode 18](https://www.youtube.com/watch?v=5g78Tqbgh-8) (This video is unavailable)
  - [How Autocomplete works on Google Search - Google](https://www.youtube.com/watch?v=us9tUY_yN7Y)
  - [How the new generation of Latinx voters could change US elections | María Teresa Kumar - Ted](https://www.youtube.com/watch?v=lWaZzww6Tlc)
  - [Modeling of the dispersion in Europe of the air masses from the fires in the Chernobyl region -  Institut de Radioprotection et de Sûreté Nucléaire - IRSN](https://www.youtube.com/watch?v=VlF54zdPil8)
  - [What to Expect from a Biden Presidency | Vice Around the World Episode 17 - Vice](https://www.youtube.com/watch?v=t5TQVqGlk5A) (This video is unavailable)
  - [THE NODEY PROCESS Episode 1 - LEAVING PARIS (eng, french & viet subs) - Nodey](https://www.youtube.com/watch?v=36na4KbUYds)
  - [Why Employment Practices Liability Coverage Matters — Travelers | Epipheo](https://www.youtube.com/watch?v=sioxDglESC0)
  - [A Bow That Tells A Story | Genesis Archery | Epipheo](https://www.youtube.com/watch?v=39nDf36vEjU)
  - [Bridging traditional "onprem" game development with cloud workflows - Google Developers](https://www.youtube.com/watch?v=Tv68pdanovI)
  - [Creating prosocial games - Google Developers](https://www.youtube.com/watch?v=wc6VzgZkc2k)
  - [An introduction to Stadia Games and Entertainment - Google Developers](https://www.youtube.com/watch?v=5vwZDl33Hdc)
  - [Year in the life of a Stadia game developer - Google Developers](https://www.youtube.com/watch?v=8LaYzXDQkqI)
  - [Integrating DEI into the game development process](https://www.youtube.com/watch?v=jx7JjU98NzQ)
  - [How To Choose The Right Insurance Plan — Animated Explainer Video | Epipheo](https://www.youtube.com/watch?v=o661QHdCNF4)

To put this result in perspective, **27 videos over 1574 were not listed in my feed last month**.

It seems that if a channel posts a video that is unavailable, you will not see it in your feed. Additionally, it seems that if you post multiple video in a row, they will not be listed. However, it is not enough to explain the set of non-listed video I aggregated.

**Still, to the question "does Youtube display all videos of channels you follow ? The answer is definitely a NO**
