export class Arts {
  constructor(appid, kind, url) {
    this.appid = appid
    this.kind = kind
    this.url = url
  }

  persist(manager) {
    manager.arts_upsert(
      this.appid,
      this.kind,
      this.url,
    )
  }

  static header_from_appdetails(appdetails_data) {
    return new Arts(
      appdetails_data.steam_appid,
      "header",
      appdetails_data.header_image,
    )
  }

  static background_from_appdetails(appdetails_data) {
    return new Arts(
      appdetails_data.steam_appid,
      "background",
      appdetails_data.background,
    )
  }

  static screenshot_from_appdetails_fragment(appdetails_data, fragment) {
    return new Arts(
      appdetails_data.steam_appid,
      "screenshot",
      fragment.path_full,
    )
  }
}
