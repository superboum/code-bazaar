import { readFileSync } from 'node:fs';

import { type IQuery } from "./irepository.ts"

import * as db from "./database.ts"
import { Game } from "./models/game.ts";

export const init = db.init
export const Query = db.Query

export function inject_appdetails() {
  //@FIXME read all appdetails one by one
  const manager = new Query();
  const all_appdetails = [ JSON.parse(readFileSync("cs.json", { encoding: 'utf8' })) ]
  all_appdetails.forEach(appdetails => {
    const appid = Object.keys(appdetails)[0];
    const appdetails_data = appdetails[appid].data
    const game = Game.from_appdetails(appdetails_data);
    game.persist(manager);
  })
  console.log("inject appdetails done")
}
