import { type IQuery } from "../irepository.ts" 

export type MaybeRequirement = Requirement | null;

export class Requirement {
  appid: number
  platform: string
  min: string
  recommended: string

  constructor(appid: number, platform: string, min: string, recommended: string) {
    this.appid = appid
    this.platform = platform
    this.min = min
    this.recommended = recommended
  }

  persist(manager: IQuery) {
    if (this.min) {
      manager.requirements_upsert(
        this.appid,
        this.platform,
        "minimum",
        this.min
      )
    }

    if (this.recommended) {
      manager.requirements_upsert(
	this.appid,
        this.platform,
        "recommended",
	this.recommended,
      )
    }
  }

  static win_from_appdetails(appdetails_data: any): MaybeRequirement {
    if (!appdetails_data.platforms.windows) {
      return null
    }
    return new Requirement(
      appdetails_data.steam_appid,
      "windows",
      appdetails_data.pc_requirements.minimum || null,
      appdetails_data.pc_requirements.recommended || null,
    );
  }

  static mac_from_appdetails(appdetails_data: any): MaybeRequirement {
    if (!appdetails_data.platforms.mac) {
      return null
    }
    return new Requirement(
      appdetails_data.steam_appid,
      "mac",
      appdetails_data.mac_requirements.minimum || null,
      appdetails_data.mac_requirements.recommended || null,
    );
  }

  static linux_from_appdetails(appdetails_data: any): MaybeRequirement {
    if (!appdetails_data.platforms.linux) {
      return null
    }
    return new Requirement(
      appdetails_data.steam_appid,
      "linux",
      appdetails_data.linux_requirements.minimum || null,
      appdetails_data.linux_requirements.recommended || null,
    );
  }
}
