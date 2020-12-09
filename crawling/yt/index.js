const feed = require('./feed.har.json')
const abo = require('./abonnements.json')
const axios = require('axios').default
const Promise = require("bluebird");
const parseString = require('xml2js').parseString;

// 1. Go to about:config and set 'devtools.netmonitor.responseBodyLimit' to at least 5MB to prevent JSON being truncated
// 2. Do a network capture on firefox and save it as feed.har.json in this folder
// 3. Use google takeout, untick everything exception Youtube -> Subscriptions (abonnement in french)

const video_entries_begin = feed
  .log
  .entries
  .filter(http => http.request.url.startsWith('https://www.youtube.com/youtubei/v1/browse'))
  .map(http => http.response.content.text)
  .map(obj => JSON.parse(obj))
  .map(obj => obj.contents.twoColumnBrowseResultsRenderer.tabs[0].tabRenderer.content.sectionListRenderer.contents)[0]
  .map(nobj => nobj.itemSectionRenderer.contents[0].shelfRenderer.content.gridRenderer.items)
  .map(nobj => nobj.map(items => items.gridVideoRenderer))
  .flat()
  .map(video => video.videoId)

const video_entries_next = feed
  .log
  .entries
  .filter(http => http.request.url.startsWith('https://www.youtube.com/browse_ajax'))
  .map(http => http.response.content.text)
  .map(obj => JSON.parse(obj))
  .map(obj => obj[1].response.continuationContents.sectionListContinuation.contents)
  .flat()
  .map(nobj => nobj.itemSectionRenderer.contents[0].shelfRenderer.content.gridRenderer.items)
  .flat()
  .map(video => video.gridVideoRenderer.videoId)

const video_set = [...video_entries_begin, ...video_entries_next].reduce((acc, v) => { acc[v] = true; return acc}, new Object()) 

const xml = (cnt) => new Promise((resolve, reject) => parseString(cnt, (err, result) => err ? reject(err) : resolve(result)));

const channelRss = abo
  .map(obj => obj.snippet.resourceId.channelId)
  .map(id => `https://www.youtube.com/feeds/videos.xml?channel_id=${id}`);

(async () => {
  const channelContent = await Promise
    .map(channelRss, l => {console.log(`fetching ${l}`) ; return axios.get(l)}, {concurrency: 1})
    .map(c => xml(c.data))
    .map(v => {
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
    .map(([date, id]) => [date, id, id in video_set])

  answer.forEach(([date, id, r]) => console.log(`${date} ${id} ${r ? '✅' : '❌'}`))
})()

