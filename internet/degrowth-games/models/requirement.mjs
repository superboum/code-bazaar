export class Requirement {
  constructor(platform, min, recommended) {
    this.platform = platform
    this.min = min
    this.recommended = recommended
  }

  static win_from_appdetails(appdetails_data) {
    if (!appdetails_data.platforms.windows) {
      return null
    }
    return new Requirement(
      "windows",
      appdetails_data.pc_requirements.minimum || null,
      appdetails_data.pc_requirements.recommended || null,
    );
  }

  static mac_from_appdetails(appdetails_data) {
    if (!appdetails_data.platforms.mac) {
      return null
    }
    return new Requirement(
      "mac",
      appdetails_data.mac_requirements.minimum || null,
      appdetails_data.mac_requirements.recommended || null,
    );
  }

  static linux_from_appdetails(appdetails_data) {
    if (!appdetails_data.platforms.linux) {
      return null
    }
    return new Requirement(
      "mac",
      appdetails_data.linux_requirements.minimum || null,
      appdetails_data.linux_requirements.recommended || null,
    );
  }
}
