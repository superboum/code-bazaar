import { type IQuery } from "../irepository.ts" 
import { Requirement, type MaybeRequirement } from "./requirement.ts";
import { Arts } from "./arts.ts";
import { Tags } from "./tags.ts";


export class Game {
  appid: number
  name: string
  description: string
  release_date: number
  recommendations: number
  requirements: MaybeRequirement[]
  arts: Arts[]
  tags: Tags[]

  constructor(appid: number, name: string, description: string, release_date: number, recommendations: number, requirements: MaybeRequirement[], arts: Arts[], tags: Tags[]) {
    this.appid = appid
    this.name = name
    this.description = description
    this.release_date = release_date
    this.recommendations = recommendations

    this.requirements = requirements || []
    this.arts = arts || []
    this.tags = tags || []
  }

  persist(manager: IQuery) {
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

    this.tags
      .filter(t => t != null)
      .forEach(t => t.persist(manager));
  }

  static from_appdetails(appdetails_data: any) {
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
	...appdetails_data.screenshots.map((screenshot: any) => 
          Arts.screenshot_from_appdetails_fragment(
	    appdetails_data, 
	    screenshot
	  )
	)
      ],
      [
	...appdetails_data.genres.map((genre: any) =>
	  Tags.genre_from_appdetails(
	    appdetails_data,
	    genre,
	  ),
	),
	...appdetails_data.categories.map((category: any) =>
	  Tags.category_from_appdetails(
	    appdetails_data,
	    category,
	  ),
	),
      ],
    );
  }
}
