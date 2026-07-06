import sqlite from 'node:sqlite';

const database = new sqlite.DatabaseSync('steam.db');


export class Query {
  constructor() {
    this._games_upsert = database.prepare(`
      INSERT INTO games(appid, last_appdetails_update, name, description, release_date, recommendations)
      VALUES(?, ?, ?, ?, ?, ?) ON CONFLICT DO UPDATE SET 
        last_appdetails_update=excluded.last_appdetails_update,
        name=excluded.name,
        description=excluded.description,
        release_date=excluded.release_date,
        recommendations=excluded.recommendations
      ;
    `); 

    this._requirements_upsert = database.prepare(`
      INSERT INTO requirements(appid, platform, kind, content)
      VALUES(?, ?, ?, ?) ON CONFLICT DO UPDATE SET
        content=excluded.content
      ;
    `);

    this._arts_upsert = database.prepare(`
      INSERT INTO arts(appid, kind, url)
      VALUES(?, ?, ?) ON CONFLICT DO NOTHING;
    `);

    this._tags_upsert = database.prepare(`
      INSERT INTO tags(kind, steam_id, name)
      VALUES(?, ?, ?) ON CONFLICT DO UPDATE SET
        name=excluded.name
      ;
    `);

    this._games_xref_tags_upsert = database.prepare(`
      INSERT INTO games_xref_tags(kind, steam_id, appid)
      VALUES(?, ?, ?) ON CONFLICT DO NOTHING;
    `);
  }

  games_upsert(appid, last_update, name, description, release_date, recommendations) {
    return this._games_upsert.run(
      appid,
      last_update,
      name,
      description,
      release_date,
      recommendations,
    )
  }

  requirements_upsert(appid, platform, kind, content) {
    return this._requirements_upsert.run(
      appid,
      platform,
      kind,
      content,
    )
  }

  arts_upsert(appid, kind, url) {
    return this._arts_upsert.run(
      appid,
      kind,
      url,
    )
  }

  tags_upsert(kind, steam_id, name) {
    return this._tags_upsert.run(
      kind,
      steam_id,
      name,
    )
  }

  games_xref_tags_upsert(kind, steam_id, appid) {
    return this._games_xref_tags_upsert.run(
      kind,
      steam_id,
      appid,
    )
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

    CREATE TABLE IF NOT EXISTS arts(
      appid INTEGER,
      kind TEXT,
      url TEXT,
      FOREIGN KEY(appid) REFERENCES games(appid)
    ) STRICT;
    CREATE INDEX IF NOT EXISTS idx_images_appid_kind ON arts(appid, kind);
    CREATE UNIQUE INDEX IF NOT EXISTS idx_images_unique_url ON arts(appid, kind, url);

    CREATE TABLE IF NOT EXISTS tags(
      kind TEXT,
      steam_id INTEGER,
      name TEXT
    ) STRICT;
    CREATE UNIQUE INDEX IF NOT EXISTS idx_tags_steam_ref ON tags(kind, steam_id);

    CREATE TABLE IF NOT EXISTS games_xref_tags(
      kind TEXT,
      steam_id INTEGER,
      appid INTEGER,
      FOREIGN KEY(appid) REFERENCES games(appid),
      FOREIGN KEY(kind, steam_id) REFERENCES tags(kind, steam_id)
    ) STRICT;
    CREATE INDEX IF NOT EXISTS idx_games_xref_tags_forward ON games_xref_tags(appid);
    CREATE INDEX IF NOT EXISTS idx_games_xref_tags_backward ON games_xref_tags(kind, steam_id);
    CREATE UNIQUE INDEX IF NOT EXISTS idx_games_xref_tags_uniqueness ON games_xref_tags(appid, kind, steam_id);
  `);
}
