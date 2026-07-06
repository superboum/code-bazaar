import { Requirement } from "./requirement.mjs";
import { Arts } from "./arts.mjs";

export class Game {
  constructor(appid, name, description, release_date, recommendations, requirements, arts, tags) {
    this.appid = appid
    this.name = name
    this.description = description
    this.release_date = release_date
    this.recommendations = recommendations

    this.requirements = requirements || []
    this.arts = arts || []
    this.tags = tags || []
  }

  persist(manager) {
    manager.games_upsert(
      this.appid,
      Date.now(),
      this.name,
      this.description,
      this.release_date,
      this.recommendations
    );

    this.requirements
      .filter(r => r != null)
      .forEach(r => r.persist(manager));

    this.arts
      .filter(a => a != null)
      .forEach(a => a.persist(manager));

    // @FIXME: save tags.
  }

  static from_appdetails(appdetails_data) {
    return new Game(
      appdetails_data.steam_appid,
      appdetails_data.name,
      appdetails_data.detailed_description,
      Date.parse(appdetails_data.release_date.date),
      appdetails_data.recommendations.total,
      [ 
        Requirement.win_from_appdetails(appdetails_data), 
	Requirement.mac_from_appdetails(appdetails_data), 
	Requirement.linux_from_appdetails(appdetails_data), 
      ],
      [
	Arts.header_from_appdetails(appdetails_data),
	Arts.background_from_appdetails(appdetails_data),
	...appdetails_data.screenshots.map(screenshot => 
          Arts.screenshot_from_appdetails_fragment(
	    appdetails_data, 
	    screenshot
	  )
	)
      ],
      [
        // tags
      ],
    );
  }
}
