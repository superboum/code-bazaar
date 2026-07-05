import sqlite from 'node:sqlite';

const database = new sqlite.DatabaseSync('steam.db');


export class Manager {
  constructor() {
    this.games_upsert = database.prepare(`
      INSERT INTO games(appid, last_appdetails_update, name, description, release_date, recommendations)
      VALUES(?, ?, ?, ?, ?, ?) ON CONFLICT DO UPDATE SET 
        last_appdetails_update=excluded.last_appdetails_update,
        name=excluded.name,
        description=excluded.description,
        release_date=excluded.release_date,
        recommendations=excluded.recommendations
      ;
    `).run; 

    this.requirements_upsert = database.prepare(`
      INSERT INTO requirements(appid, platform, kind, content)
      VALUES(?, ?, ?, ?) ON CONFLICT DO UPDATE SET
        content=excluded.content
      ;
    `).run;

    this.images_upsert = database.prepare(`
      INSERT INTO images(appid, kind, url)
      VALUES(?, ?, ?) ON CONFLICT DO NOTHING;
    `).run;
  }
}

export function init() {
  database.exec(`
    CREATE TABLE IF NOT EXISTS games(
      appid INTEGER,
      last_appdetails_update INTEGER,
      name TEXT,
      description TEXT,
      release_date INTEGER,
      recommendations INTEGER
    ) STRICT;
    CREATE UNIQUE INDEX IF NOT EXISTS idx_game_appid ON games(appid);

    CREATE TABLE IF NOT EXISTS requirements(
      appid INTEGER,
      platform TEXT,
      kind TEXT,
      content TEXT,
      FOREIGN KEY(appid) REFERENCES games(appid)
    ) STRICT;
    CREATE UNIQUE INDEX IF NOT EXISTS idx_requirements_appid_platform_kind ON requirements(appid, platform, kind);

    CREATE TABLE IF NOT EXISTS images(
      appid INTEGER,
      kind TEXT,
      url TEXT,
      FOREIGN KEY(appid) REFERENCES games(appid)
    ) STRICT;
    CREATE INDEX IF NOT EXISTS idx_images_appid_kind ON images(appid, kind);
    CREATE UNIQUE INDEX IF NOT EXISTS idx_images_unique_url ON images(appid, kind, url);

    CREATE TABLE IF NOT EXISTS tags(
      kind TEXT,
      steam_id INTEGER,
      name TEXT
    ) STRICT;
    CREATE UNIQUE INDEX IF NOT EXISTS idx_tags_steam_ref ON tags(kind, steam_id);

    CREATE TABLE IF NOT EXISTS games_tags(
      kind INTEGER,
      steam_id INTEGER,
      appid INTEGER,
      FOREIGN KEY(appid) REFERENCES games(appid),
      FOREIGN KEY(kind) REFERENCES tags(kind),
      FOREIGN KEY(steam_id) REFERENCES tags(steam_id)
    ) STRICT;
    CREATE INDEX IF NOT EXISTS idx_game_to_tags ON games_tags(appid);
    CREATE INDEX IF NOT EXISTS idx_tags_to_game ON games_tags(kind, steam_id);
    CREATE UNIQUE INDEX IF NOT EXISTS idx_game_tag_uniqueness ON games_tags(appid, kind, steam_id);
  `);
}
