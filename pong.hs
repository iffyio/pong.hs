import Haste
import Haste.Graphics.Canvas
import Data.IORef

-- Type declarations
data GameState = GameState{
	ballPos :: Point, -- position of ball
	ballSpeed :: Point, -- how far will ball move in a single update
	paddlePos:: Double, -- start position of paddle on x axis
	score  :: Int, 
        canvasElement :: Maybe Elem
}

data Paddle = Top | Bottom

-- Constants ?
width, height,ballRadius, paddleWidth, paddleHeight :: Double
width = 500 -- width of canvas
height = 600 -- height of canvas
ballRadius = 5 --radius of ball
paddleHeight = 5 -- height of paddle
paddleWidth = 150 -- width of paddle
halfWidth = width / 2
halfHeight = height / 2
--Dimensions for start and restart button
btnX1 = halfWidth - 50
btnY1 = halfHeight - 25 
btnX2 = halfWidth + 60
btnY2 = halfHeight + 25
scoreLabel :: String
scoreLabel = "Score: "
defaultSpeed = (8,10)

initialState :: GameState
initialState = GameState{
 	ballPos = (20, 20),
 	ballSpeed = defaultSpeed,
	paddlePos = (width / 2) - 75,
	score = 0,
        canvasElement = Nothing
 }

-- Render game state on canvas
renderState :: Bool -> Canvas -> GameState ->  IO ()
renderState gameover canvas state = render canvas $ do
  gamePicture state
  if gameover
     then gameOver $ show (score state)
     else return () 
  where
 	x1 = paddlePos state
 	x2 = x1 + paddleWidth

-- blue color
blue :: Picture () -> Picture ()
blue = color (RGB 130 205 185)
-- Draw a ball
ball :: Point -> Picture ()
ball pt = color (RGB 243 114 89) $ do
	fill $ circle pt ballRadius

-- Draw a paddle
paddle :: Rect -> Picture ()
paddle (Rect x1 y1 x2 y2) = blue $ do
	fill $ rect (x1, y1) (x2, y2)

-- Draw a button with label
drawButton :: String -> Picture ()
drawButton note = blue $ do
	stroke $ rect (btnX1, btnY1) (btnX2, btnY2)
        font "20px italic Monospace" $ text ((btnX1 + btnX2) / 2 - 35 ,(btnY1 + btnY2) / 2 + 5) note

-- Draw text 
drawText :: Point -> String -> Picture ()
drawText point s = blue $ do 
	text point s

-- Draw game state on screen / paddles, ball, score 
gamePicture :: GameState -> Picture ()
gamePicture state = do
  ball $ ballPos state
  let x1 = paddlePos state
      x2 = x1 + paddleWidth
  paddle $ Rect x1 0 x2 paddleHeight
  paddle $ Rect x1 (height - paddleHeight) x2 height
  font "20px italic Monospace" $ drawText (30,50) $ scoreLabel ++ show (score state) 

-- Create new canvas to draw on
mkCanvas :: Double -> Double -> IO Elem
mkCanvas width height = do
	canvas <- newElem "canvas"
	setProp canvas "width" (show width)
	setProp canvas "height" (show height)
	setStyle canvas "display" "block"
	setStyle canvas "border" "1px solid #524F52"
	setStyle canvas "margin" "0px auto 0 auto"
	setStyle canvas "backgroundColor" "#524F52"
	return canvas



-- move the ball / change ball coordinates
moveBall :: GameState -> GameState
moveBall state = state {ballPos = (x + vx, y + vy)}
  where
  	(x, y)   = ballPos state
  	(vx, vy) = ballSpeed state

-- Display game and Update screen
animate :: Canvas -> IORef GameState -> IO ()
animate canvas stateRef = do
	state <- readIORef stateRef
	let (x, y) = ballPos state
            (Just canvasElem) = canvasElement state
	renderState False canvas state
	case gameEnded state of
		Nothing -> do 
			atomicWriteIORef stateRef $ update state 
			setTimeout 30 $ animate canvas stateRef
		Just Top  -> do
                        let  x' = paddlePos state
                        restartGame canvasElem canvas state {ballPos = (x' + paddleWidth / 2, 12)}
		Just Bottom -> do
                        let x' = paddlePos state
                        restartGame canvasElem canvas state {ballPos = (x' + paddleWidth / 2, height - 12)}
 where
   update = paddleHit . moveBall . detectCollision  
   
-- Redraw canvas and restart game
restartGame :: Elem -> Canvas -> GameState -> IO ()
restartGame canvasElem canvas state = do 
        let (x',y') = ballPos state
            (vx,vy) = ballSpeed state
	setTimeout 30 $ renderState True canvas  $ state {ballPos = (x', y')}
        _ <- onEvent canvasElem OnClick $ \btn (x,y) -> btnEvent btn (x,y) canvasElem state {ballSpeed = (vx, -vy), score = 0}
        return ()
  

-- move the paddles 
movePaddles :: (Int, Int) -> IORef GameState -> IO ()
movePaddles (mouseX, mouseY) stateRef = do
	atomicModifyIORef stateRef (\state -> ((state {paddlePos = (fromIntegral mouseX) - (paddleWidth / 2)}), ()))

-- change ball direction if ball hits paddle
paddleHit :: GameState -> GameState
paddleHit state = 
	if and [bx' >= px, bx'' <= pl, (by >= height-ph) || (by <= ph)] 
	then increaseSpeed state {ballSpeed = (vx, -vy), score = score state + 1}
	else state		
 where
	(bx,by) = ballPos state
	bx' = bx + ballRadius
	bx'' = bx - ballRadius
	(vx,vy) = ballSpeed state
	px = paddlePos state
	ph = paddleHeight
	pl = px + paddleWidth

-- Change ball direction if ball hits walls
detectCollision :: GameState -> GameState
detectCollision state
	| (x + ballRadius) >= width = state {ballPos = (width - ballRadius,y), ballSpeed = (-vx, vy)}
	| (x + ballRadius) <= 0 = state {ballPos = (ballRadius, y), ballSpeed = (-vx, vy)}
	| otherwise = state
 where
 	(x, y) = ballPos state
 	(vx,vy) = ballSpeed state

-- increment ball speed
increaseSpeed :: GameState -> GameState
increaseSpeed state = if  score state `mod` 4 == 0 && (abs vx < 15)
                      then let
                             vx' = if vx < 0 then -1 else 1
                             vy' = if vy < 0 then -2 else 2
                           in  state {ballSpeed = (vx+vx', vy+vy') }
                      else state     
 where
   (vx,vy) = ballSpeed state
   
-- Check if ball is out / Has a paddle missed ?
gameEnded :: GameState -> Maybe Paddle
gameEnded state
	| y >= height && (x < px || x > px + paddleWidth) = Just Bottom
	| y <= 0 && (x < px || x > px + paddleWidth) = Just Top
	| otherwise = Nothing
 where
   (x,y) = ballPos state
   px = paddlePos state
   
-- Game over sequence
gameOver :: String -> Picture ()
gameOver score = do
    drawButton "Restart"
    color(RGB 255 255 255) $ do
      let fnt = font "25px italic Monospace" 
      fnt $ text (btnX1, btnY1 - 30) "Game Over"
      fnt $ text (btnX1 - 50, btnY2 + 30) $ "Your total score was " ++ score

-- start animation
startGame state = do
  canvasElem <- mkCanvas width height
  addChild canvasElem documentBody
  Just canvas <- getCanvas canvasElem
  stateRef <- newIORef $ state {canvasElement = Just  canvasElem, ballSpeed = defaultSpeed}
  onEvent canvasElem OnMouseMove $ \mousePos -> do
		movePaddles mousePos stateRef
  animate canvas stateRef

-- handle click event on button
btnEvent :: Int -> (Int, Int) -> Elem -> GameState -> IO ()
btnEvent btn (x,y) canvasElem state | btn == 0 =
  let x' = fromIntegral x
      y' = fromIntegral y
  in
  if and [btn == 0, x' >= btnX1, x' <= btnX2, y' >= btnY1, y' <= btnY2 ]
  then do
    removeChild canvasElem documentBody
    startGame state
  else  
    return ()
    | otherwise = return ()

-- main
main :: IO Bool
main = do
	canvasElem <- mkCanvas width height
	addChild canvasElem documentBody
	Just canvas <- getCanvas canvasElem
        render canvas $ do 
          drawButton "Start"
          gamePicture initialState
        onEvent canvasElem OnClick $ \btn (x,y) -> btnEvent btn (x,y) canvasElem initialState
 	

  



	
