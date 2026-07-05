import sqlite from 'node:sqlite';
import { readFileSync } from 'node:fs';

const games_appid_list_src = "https://github.com/jsnli/steamappidlist/raw/refs/heads/master/data/games_appid.json"
// Queried API endpoint -> https://api.steampowered.com/IStoreService/GetAppList/v1/
const games_appid_list_dst = "./games_appid.json"


const database = new sqlite.DatabaseSync('steam.db');

const list = JSON.parse(readFileSync(games_appid_list_dst));
console.log(list[0]);
