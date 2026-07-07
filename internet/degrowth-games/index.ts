import { argv } from 'node:process';

import * as repo from "./repository.ts";
import * as fetch from "./fetch.ts";

async function main() {
  const subcmd = (name: string) => argv.length >= 3 && argv[2] == name;

  if (subcmd('fetch:appid')) return fetch.appid();
  if (subcmd('fetch:appdetails')) return await fetch.appdetails();
  if (subcmd('repo:init')) return repo.init();
  if (subcmd('repo:inject:appdetails')) return repo.inject_appdetails();

  console.error(`
A supported subcommand is required.

fetch:appid               fetch appid list
fetch:appdetails          for each appid, fetch its appdetails
repo:init                 initialize the repository
repo:inject:appdetails    parse appdetails and inject them in the repo
    `);
}


await main();
