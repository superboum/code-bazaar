import { Requirement } from "./requirement.mjs";

export class Game {
  constructor(appid, name, description, recommendations, requirements, arts, tags) {
    this.appid = appid
    this.name = name
    this.description = description
    this.recommendations = recommendations
    this.requirements = requirements

    this.arts = arts
    this.tags = tags
  }

  persist(manager) {
    manager.games_upsert(
      this.appid,
      null, // last appdetails update (@now)
      this.name,
      this.description,
      null, // release_date
      this.recommendations
    );

    // @FIXME: save requirements, tags, arts.
  }

  static from_appdetails(appdetails_data) {
    return new Game(
      appdetails_data.steam_appid,
      appdetails_data.name,
      appdetails_data.detailed_description,
      appdetails_data.recommendations.total,
      [ 
        Requirement.win_from_appdetails(appdetails_data), 
	Requirement.mac_from_appdetails(appdetails_data), 
	Requirement.linux_from_appdetails(appdetails_data), 
      ],
      [
	// arts
      ],
      [
        // tags
      ],
    );
  }
}
