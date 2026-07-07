import { type IQuery } from "../irepository.ts" 

export class Arts {
  appid: number
  kind: string
  url: string

  constructor(appid: number, kind: string, url: string) {
    this.appid = appid
    this.kind = kind
    this.url = url
  }

  persist(manager: IQuery) {
    manager.arts_upsert(
      this.appid,
      this.kind,
      this.url,
    )
  }

  static header_from_appdetails(appdetails_data: any) {
    return new Arts(
      appdetails_data.steam_appid,
      "header",
      appdetails_data.header_image,
    )
  }

  static background_from_appdetails(appdetails_data: any) {
    return new Arts(
      appdetails_data.steam_appid,
      "background",
      appdetails_data.background,
    )
  }

  static screenshot_from_appdetails_fragment(appdetails_data: any, fragment: any) {
    return new Arts(
      appdetails_data.steam_appid,
      "screenshot",
      fragment.path_full,
    )
  }
}
