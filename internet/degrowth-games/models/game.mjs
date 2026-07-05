import { Requirement } from "./requirement.mjs";

export class Game {
  constructor(appid, name, description, requirements, arts, tags) {
    this.appid = appid
    this.name = name
    this.description = description
    this.requirements = requirements
    this.arts = arts
    this.tags = tags
  }

  static from_appdetails(appdetails_data) {
    return new Game(
      appdetails_data.steam_appid,
      appdetails_data.name,
      appdetails_data.detailed_description,
      [ 
        Requirement.win_from_appdetails(appdetails_data), 
	Requirement.mac_from_appdetails(appdetails_data), 
	Requirement.linux_from_appdetails(appdetails_data), 
      ],
    );
  }
}
