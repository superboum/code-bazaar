import { readFile, open } from 'node:fs/promises';

// Queried API endpoint -> https://api.steampowered.com/IStoreService/GetAppList/v1/
const games_appid_list_src = "https://github.com/jsnli/steamappidlist/raw/refs/heads/master/data/games_appid.json"
const games_appid_list_dst = "./games_appid.json"
const games_appdetails_api = "https://store.steampowered.com/api/appdetails?l=english&appids="
const games_appdetails_dst = "./games_appdetails.json"

export function appid() {
  console.error("appid not yet implemented"); 
}

export async function appdetails() {
  const list = JSON.parse(await readFile(games_appid_list_dst));
  console.log(`known games: ${list.length}`);
  // @FIXME: shorten list for test
  list.length = 10

  const out = await open(games_appdetails_dst, 'a');

  await list.reduce(async (promise, appdesc) => {
    await promise 
    const appid = appdesc.appid
    console.log(`fetch ${appid}`);
    const appdetails_req = await fetch(games_appdetails_api + appid);
    const appdetails_json = await appdetails_req.json()
    await out.write(JSON.stringify(appdetails_json)+"\n");
    console.log(`wrote ${appid}`);
  }, Promise.resolve());
  await out?.close();
}
