export class Tags {
  constructor(appid, kind, steam_id, name) {
    this.appid = appid
    this.kind = kind 
    this.steam_id = steam_id 
    this.name = name 
  }

  persist(manager) {
    manager.tags_upsert(
      this.kind,
      this.steam_id,
      this.name,
    );

    manager.games_xref_tags_upsert(
      this.kind,
      this.steam_id,
      this.appid,
    );
  }

  static category_from_appdetails(appdetails_data, category) {
    return new Tags(
      appdetails_data.steam_appid,
      "category",
      category.id,
      category.description,
    );
  }

  static genre_from_appdetails(appdetails_data, genre) {
    return new Tags(
      appdetails_data.steam_appid,
      "genre",
      genre.id,
      genre.description,
    );
  }
}
