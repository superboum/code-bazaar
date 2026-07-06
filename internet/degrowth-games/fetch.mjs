import { readFileSync } from 'node:fs';

// Queried API endpoint -> https://api.steampowered.com/IStoreService/GetAppList/v1/
const games_appid_list_src = "https://github.com/jsnli/steamappidlist/raw/refs/heads/master/data/games_appid.json"
const games_appid_list_dst = "./games_appid.json"
const games_appdetails_api = "https://store.steampowered.com/api/appdetails?l=english&appids="

export function appid() {
  console.error("appid not yet implemented"); 
}

export function appdetails() {
  const list = JSON.parse(readFileSync(games_appid_list_dst));
  /*
  const candidate_ref = list[0];
  const appdetails_req = await fetch(games_appdetails_api + candidate_ref.appid)
  const appdetails_json = await appdetails_req.json()
  */
  console.error("appdetails not yet implemented")
}
