//const feed = require('./feed.har.json')
const axios = require('axios').default
const Promise = require("bluebird");
const parseString = require('xml2js').parseString;
const fs = require('fs/promises');


// Run console.log(JSON.stringify(Array.from(document.querySelectorAll("ytd-grid-video-renderer a#video-title[href]").entries()).map(e => e[1].href.match(/([a-zA-Z0-9_-]+)$/)).filter(e => e).map(e => e[1])))
const video_set = require("./video_set.json");
console.log("number of video fetched in the timeline:", video_set.length);

const xml = (cnt) => new Promise((resolve, reject) => parseString(cnt, (err, result) => err ? reject(err) : resolve(result)));

(async () => {
  const fd = await fs.open('abonnements.csv');
  const raw = await fd.readFile({encoding: "utf-8"});
  fd.close()

  const channels = raw.split("\n").slice(1).map(l => l.split(",",3)).filter(([id, url, name]) => id)
  const idToName = channels.reduce((acc, [id, url, name]) => { acc[id] = name ; return acc }, {})
  console.log("number of channels to fetch:", channels.length)

  const channelContent = await Promise
    .map(channels, ([id, url, name]) => {
	    const l = `https://www.youtube.com/feeds/videos.xml?channel_id=${id}`;
	    console.log(`fetching ${l}`)
	    return axios.get(l)
	            .then(c => xml(c.data))
		    .then(v => {
                      try {
                        return v.feed.entry.map(e => [e.published[0], e['yt:videoId'][0], id])
                      } catch (error) {
                        console.error(v, error);
	                return []
                      }
		    })
		    .catch(e => { console.error(`failure on ${id}`); return [] })
    }, {concurrency: 1})

  const answer = channelContent
    .flat()
    .sort()
    .map(([date, id, c]) => [date, id, idToName[c], video_set.includes(id)])

  answer.forEach(([date, id, c, r]) => console.log(`${date} | ${r ? '✅' : '❌'} | ${id} | ${c} | `))
  
})()

