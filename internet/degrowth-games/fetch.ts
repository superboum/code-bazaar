import { readFile, open } from 'node:fs/promises';
import { setTimeout } from 'node:timers/promises';
import { argv } from 'node:process';

// Queried API endpoint -> https://api.steampowered.com/IStoreService/GetAppList/v1/
const games_appid_list_src = "https://github.com/jsnli/steamappidlist/raw/refs/heads/master/data/games_appid.json"
const games_appid_list_dst = "./games_appid.json"
const games_appdetails_api = "https://store.steampowered.com/api/appdetails?l=english&appids="
const games_appdetails_dst = "./games_appdetails.json"

export function appid() {
  console.error("appid not yet implemented"); 
}

export async function appdetails() {
  const list = JSON.parse(await readFile(games_appid_list_dst, { encoding: 'utf8' }));
  console.log(`known games: ${list.length}`);

  if (argv.length > 3) {
    const past_crawls = argv.slice(3);
    console.log(`Reading past crawls: ${past_crawls}`);
    await past_crawls.reduce(async (promise, crawl_path) => {
      await promise;
      const past_fd = await open(crawl_path, 'r');
      for await (const line of past_fd.readLines()) {
        const many_appdetails = JSON.parse(line);
	if (many_appdetails == null) continue;
	Object.keys(many_appdetails)
	.map(obj_key => parseInt(obj_key))
        // We expect the appid to be at the beginning of the list
	// We find its index
	.map(appid => list.findIndex((elem: any) => elem.appid === appid))
	.forEach(appid_idx => {
          if (appid_idx < 0) {
	    //appid_idx is -1 if not found
            return
	  }
	  // We add a fetched key to remember to skip it.
	  list[appid_idx].fetched = true;
	})
      }
      await past_fd.close();
    }, Promise.resolve());
  }

  const out = await open(games_appdetails_dst, 'a');

  await list.reduce(async (promise: Promise<null>, appdesc: any) => {
    // At most 200 req per 5 min
    // ie. one request every 1.5 sec.
    await promise 
    const appid = appdesc.appid
    if (appdesc.fetched) {
      console.log(`skipping ${appid}, already fetched.`);
      return
    }
    console.log(`fetch ${appid}`);
    const appdetails_req = await fetch(games_appdetails_api + appid);
    const appdetails_json = await appdetails_req.json()
    await out.write(JSON.stringify(appdetails_json)+"\n");
    await setTimeout(1500);
    console.log(`wrote ${appid} & slept 1.5 sec.`);
  }, Promise.resolve());
  await out?.close();
}
