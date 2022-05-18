//const feed = require('./feed.har.json')
const axios = require('axios').default
const Promise = require("bluebird");
const parseString = require('xml2js').parseString;
const fs = require('fs/promises');


// Run console.log(JSON.stringify(Array.from(document.querySelectorAll("ytd-grid-video-renderer a#video-title[href]").entries()).map(e => e[1].href.match(/watch\?v=(.*)$/)).filter(e => e).map(e => e[1])))
const video_set = require("./video_set.json");
console.log("number of video fetched in the timeline:", video_set.length);

const xml = (cnt) => new Promise((resolve, reject) => parseString(cnt, (err, result) => err ? reject(err) : resolve(result)));

(async () => {
  const fd = await fs.open('abonnements.csv');
  const raw = await fd.readFile({encoding: "utf-8"});
  fd.close()

  const channelRss = raw.split("\n").map(l => l.split(",")[0]).slice(1).filter(i => i != "").map(id => `https://www.youtube.com/feeds/videos.xml?channel_id=${id}`);
  console.log("number of channels to fetch:", channelRss.length)

  const channelContent = await Promise
    .map(channelRss, l => {
	    console.log(`fetching ${l}`)
	    return axios.get(l).catch(e => { console.error(`axios failed on ${l}`); return null })
    }, {concurrency: 1})
    .map(c => c ? xml(c.data) : null)
    .map(v => {
      if (!v) return []
      try {
        return v.feed.entry.map(e => [e.published[0], e['yt:videoId'][0]])
      } catch (error) {
        console.error(v, error);
	return []
      }
    })

  const answer = channelContent
    .flat()
    .sort()
    .map(([date, id]) => [date, id, video_set.includes(id)])

  answer.forEach(([date, id, r]) => console.log(`${date} ${id} ${r ? '✅' : '❌'}`))
  
})()

