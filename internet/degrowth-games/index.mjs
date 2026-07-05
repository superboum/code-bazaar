import sqlite from 'node:sqlite';
import { readFileSync } from 'node:fs';

const games_appid_list_src = "https://github.com/jsnli/steamappidlist/raw/refs/heads/master/data/games_appid.json"
// Queried API endpoint -> https://api.steampowered.com/IStoreService/GetAppList/v1/
const games_appid_list_dst = "./games_appid.json"

const games_appdetails_api = "https://store.steampowered.com/api/appdetails?l=english&appids="

const database = new sqlite.DatabaseSync('steam.db');

const list = JSON.parse(readFileSync(games_appid_list_dst));

const candidate_ref = list[0];

class Requirement {
  constructor(platform, min, recommended) {
    this.platform = platform
    this.min = min
    this.recommended = recommended
  }
}

class Game {
  constructor() {

  }

  static from_appdetails(appdetails_data) {
    return Game(
      appdetails_data.steam_appid,
      appdetails_data.name,
      appdetails_data.detailed_description,
      appdetails_data.pc_requirements.minimum || null,
      appdetails_data.pc_requirements.recommended || null,
      appdetails_data.mac_requirements.minimum || null,
      appdetails_data.mac_requirements.recommended || null,
      appdetails_data.linux_requirements.minimum || null,
      appdetails_data.linux_requirements.recommended || null,
    );
  }
}

/*const appdetails_req = await fetch(games_appdetails_api + candidate_ref.appid)
const appdetails_json = await appdetails_req.json()*/
const appdetails_json = JSON.parse(readFileSync("cs.json"));
const appdetails_data = appdetails_json[candidate_ref.appid].data
console.log(appdetails_data);
