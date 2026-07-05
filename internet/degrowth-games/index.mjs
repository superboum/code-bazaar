import sqlite from 'node:sqlite';
import { readFileSync } from 'node:fs';

import { Game } from "./models/game.mjs";
import { init, Manager } from "./database.mjs";

const games_appid_list_src = "https://github.com/jsnli/steamappidlist/raw/refs/heads/master/data/games_appid.json"
// Queried API endpoint -> https://api.steampowered.com/IStoreService/GetAppList/v1/
const games_appid_list_dst = "./games_appid.json"

const games_appdetails_api = "https://store.steampowered.com/api/appdetails?l=english&appids="

const database = new sqlite.DatabaseSync('steam.db');

const list = JSON.parse(readFileSync(games_appid_list_dst));

const candidate_ref = list[0];


init();
const manager = new Manager();

/*const appdetails_req = await fetch(games_appdetails_api + candidate_ref.appid)
const appdetails_json = await appdetails_req.json()*/
const appdetails_json = JSON.parse(readFileSync("cs.json"));
const appdetails_data = appdetails_json[candidate_ref.appid].data
const game = Game.from_appdetails(appdetails_data);
console.log(game);
game.persist(manager);
console.log("done");
