import sqlite from 'node:sqlite';

const database = new sqlite.DatabaseSync('steam.db');

database.exec(`
  CREATE TABLE IF NOT EXISTS games(
    appid INTEGER,
    last_appdetails_update INTEGER,
    name TEXT,
    description TEXT,
    win_min_requirements TEXT,
    win_reco_requirements TEXT,
    mac_min_requirements TEXT,
    mac_reco_requirements TEXT,
    lin_min_requirements TEXT,
    lin_reco_requirements TEXT,
    release_date INTEGER,
    recommendations INTEGER
  ) STRICT;
  CREATE UNIQUE INDEX IF NOT EXISTS idx_game_appid ON games(appid);

  CREATE TABLE IF NOT EXISTS images(
    appid INTEGER,
    kind TEXT,
    url TEXT,
    FOREIGN KEY(appid) REFERENCES games(appid)
  ) STRICT;
  CREATE INDEX IF NOT EXISTS idx_images_appid_kind ON images(appid, kind);

  CREATE TABLE IF NOT EXISTS tags(
    id INTEGER PRIMARY KEY,
    kind TEXT,
    steam_id INTEGER,
    name TEXT
  ) STRICT;
  CREATE UNIQUE INDEX IF NOT EXISTS idx_tags_steam_ref ON tags(kind, steam_id);

  CREATE TABLE IF NOT EXISTS games_tags(
    tag_id INTEGER,
    appid INTEGER,
    FOREIGN KEY(appid) REFERENCES games(appid),
    FOREIGN KEY(tag_id) REFERENCES tags(id)
  ) STRICT;
  CREATE INDEX IF NOT EXISTS idx_game_to_tags ON games_tags(appid);
  CREATE INDEX IF NOT EXISTS idx_tags_to_game ON games_tags(tag_id);
  CREATE UNIQUE INDEX IF NOT EXISTS idx_game_tag_uniqueness ON games_tags(appid, tag_id);
`);
