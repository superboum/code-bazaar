import { type IQuery } from "../irepository.ts" 

export class Tags {
  appid: number
  kind: string
  steam_id: number
  name: string

  constructor(appid: number, kind: string, steam_id: number, name: string) {
    this.appid = appid
    this.kind = kind 
    this.steam_id = steam_id 
    this.name = name 
  }

  persist(manager: IQuery) {
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

  static category_from_appdetails(appdetails_data: any, category: any) {
    return new Tags(
      appdetails_data.steam_appid,
      "category",
      category.id,
      category.description,
    );
  }

  static genre_from_appdetails(appdetails_data: any, genre: any) {
    return new Tags(
      appdetails_data.steam_appid,
      "genre",
      genre.id,
      genre.description,
    );
  }
}
