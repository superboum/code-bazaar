function love.load()
    x = 100 
    cat = love.graphics.newImage("cat.png")
    love.graphics.setBackgroundColor(1, 1, 1, 0)
end

function love.update()
    if love.keyboard.isDown("right") then
    	x = x + 5
    end
    if love.keyboard.isDown("left") then
    	x = x - 5
    end
end

function love.draw()
    --love.graphics.rectangle("fill", x, 200, 50, 80)
    love.graphics.draw(cat, x, 100)
    love.graphics.print("Hello World", 400, 300)
end
