export interface IQuery {
  games_upsert(
    appid: number, 
    last_update: number, 
    name: string, 
    description: string, 
    release_date: number, 
    recommendations: number
  ): void;


  requirements_upsert(
    appid: number, 
    platform: string, 
    kind: string, 
    content: string
  ): void;

  arts_upsert(
    appid: number, 
    kind: string, 
    url: string
  ): void;

  tags_upsert(
    kind: string, 
    steam_id: number, 
    name: string
  ): void;

  games_xref_tags_upsert(
    kind: string, 
    steam_id: number, 
    appid: number
  ): void;
}
