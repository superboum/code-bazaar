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

  static win_from_appdetails(appdetails_data) {
    if (!appdetails_data.platforms.windows) {
      return null
    }
    return new Requirement(
      "windows",
      appdetails_data.pc_requirements.minimum || null,
      appdetails_data.pc_requirements.recommended || null,
    );
  }

  static mac_from_appdetails(appdetails_data) {
    if (!appdetails_data.platforms.mac) {
      return null
    }
    return new Requirement(
      "mac",
      appdetails_data.mac_requirements.minimum || null,
      appdetails_data.mac_requirements.recommended || null,
    );
  }

  static linux_from_appdetails(appdetails_data) {
    if (!appdetails_data.platforms.linux) {
      return null
    }
    return new Requirement(
      "mac",
      appdetails_data.linux_requirements.minimum || null,
      appdetails_data.linux_requirements.recommended || null,
    );
  }
}

class Game {
  constructor(appid, name, description, requirements, arts, tags) {
    this.appid = appid
    this.name = name
    this.description = description
    this.requirements = requirements
    this.arts = arts
    this.tags = tags
  }

  static from_appdetails(appdetails_data) {
    return new Game(
      appdetails_data.steam_appid,
      appdetails_data.name,
      appdetails_data.detailed_description,
      [ 
        Requirement.win_from_appdetails(appdetails_data), 
	Requirement.mac_from_appdetails(appdetails_data), 
	Requirement.linux_from_appdetails(appdetails_data), 
      ],
    );
  }
}

/*const appdetails_req = await fetch(games_appdetails_api + candidate_ref.appid)
const appdetails_json = await appdetails_req.json()*/
const appdetails_json = JSON.parse(readFileSync("cs.json"));
const appdetails_data = appdetails_json[candidate_ref.appid].data
console.log(Game.from_appdetails(appdetails_data));
