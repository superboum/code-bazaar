CatAnimation = {
  -- global parameter
  spritesheet = nil,
  sprite_size = 32,

  current_time = 0,
  current_frame = 1,
  current_anim = nil,
  orientation = 1, -- 1 = right, -1 = left

  walk_speed = 100,
  run_speed = 200,

  pos_x = 32,
  pos_y = 400,

  -- quad is computed later
  spritemap = {
    idle_face = { row = 0, len = 4, dur=1, quads = {} },
    idle_side = { row = 1, len = 4, dur=1, quads = {} },
    meow = { row = 2, len = 4, dur=1, quads = {} },
    clean = { row = 3, len = 4, dur=1, quads = {} },
    walk = { row = 4, len = 8, dur=1, quads = {} },
    run = { row = 5, len = 8, dur=1, quads = {} },
    sleep = { row = 6, len = 4, dur=1, quads = {} },
    unknown = { row = 7, len = 7, dur=1, quads = {} },
    jump = { row = 8, len = 7, dur=1, quads = {} },
    angry = { row = 9, len = 8, dur=1, quads = {} },
  },
}
function CatAnimation:load()
    spr_sz = CatAnimation.sprite_size
    CatAnimation.spritesheet = love.graphics.newImage("cat.png")

    for anim_name, anim_data in pairs(CatAnimation.spritemap) do
        anim_idx = 1
	while anim_idx <= anim_data.len do
	    anim_data.quads[anim_idx] = love.graphics.newQuad(
		    (anim_idx-1)*spr_sz, -- col (x), here frame selection for anim
		    anim_data.row*spr_sz, -- row (y), here anim selection
		    spr_sz, -- sprite width
		    spr_sz, -- sprite height
		    CatAnimation.spritesheet:getDimensions())
	    anim_idx = anim_idx + 1
	end
    end

    CatAnimation.current_anim = CatAnimation.spritemap.idle_face
end

function CatAnimation.tick(dt)
    CatAnimation.current_time = (CatAnimation.current_time + dt) % CatAnimation.current_anim.dur
    CatAnimation.current_frame = math.floor(CatAnimation.current_time / CatAnimation.current_anim.dur * #CatAnimation.current_anim.quads) + 1
end

function CatAnimation.walk(dt)
    if CatAnimation.current_anim ~= CatAnimation.spritemap.walk then
	CatAnimation.current_anim = CatAnimation.spritemap.walk
	CatAnimation.current_time = 0
	CatAnimation.current_frame = 1
    end

    if dt < 0 then
	CatAnimation.orientation = -1
    else
	CatAnimation.orientation = 1
    end

    CatAnimation.pos_x = CatAnimation.pos_x + dt * CatAnimation.walk_speed
end

function CatAnimation.run(dt)
    if CatAnimation.current_anim ~= CatAnimation.spritemap.run then
	CatAnimation.current_anim = CatAnimation.spritemap.run
	CatAnimation.current_time = 0
	CatAnimation.current_frame = 1
    end

    if dt < 0 then
	CatAnimation.orientation = -1
    else
	CatAnimation.orientation = 1
    end

    CatAnimation.pos_x = CatAnimation.pos_x + dt * CatAnimation.run_speed
end

function CatAnimation.rest()
    if CatAnimation.current_anim ~= CatAnimation.spritemap.idle_face then
	CatAnimation.current_anim = CatAnimation.spritemap.idle_face
	CatAnimation.current_time = 0
	CatAnimation.current_frame = 1
    end
end

function CatAnimation.sleep()
    if CatAnimation.current_anim ~= CatAnimation.spritemap.sleep then
	CatAnimation.current_anim = CatAnimation.spritemap.sleep
	CatAnimation.current_time = 0
	CatAnimation.current_frame = 1
    end
end

function CatAnimation.jump()
    if CatAnimation.current_anim ~= CatAnimation.spritemap.jump then
	CatAnimation.current_anim = CatAnimation.spritemap.jump
	CatAnimation.current_time = 0
	CatAnimation.current_frame = 1
    end
end

function CatAnimation.draw()
    love.graphics.draw(
	CatAnimation.spritesheet, 
	CatAnimation.current_anim.quads[CatAnimation.current_frame], 
	CatAnimation.pos_x,
	CatAnimation.pos_y, 
	0, 
	CatAnimation.orientation * 4,
	4,
	CatAnimation.sprite_size/2,
	CatAnimation.sprite_size/2
    )
end

function love.load()
    love.graphics.setDefaultFilter("nearest", "nearest", 1)
    love.graphics.setBackgroundColor(1, 1, 1, 0)
    CatAnimation.load()
end

function love.update(dt)
    CatAnimation.tick(dt)

    if love.keyboard.isDown("right") then
	if love.keyboard.isDown("lshift") then
	  CatAnimation.run(dt)
	else
	  CatAnimation.walk(dt)
	end
    elseif love.keyboard.isDown("left") then
	if love.keyboard.isDown("lshift") then
	  CatAnimation.run(-dt)
	else
	  CatAnimation.walk(-dt)
	end
    elseif love.keyboard.isDown("down") then
	CatAnimation.sleep()
    elseif love.keyboard.isDown("up") then
	CatAnimation.jump()
    else
	CatAnimation.rest()
    end
end

function love.draw()
    CatAnimation.draw()
    love.graphics.print("Hello World", 400, 300)
end
