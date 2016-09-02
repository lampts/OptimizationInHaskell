module Main where

import System.Time
import Text.Printf
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Control.Concurrent
import Data.IORef
import Random
import Text.Printf
import List
import Array
import Maybe

-- global values
ncolor = 8
rcity = 20
rcity1 = 10
maxcities = 10
maxants = 10
alpha = 1.0
beta = 2.0
rho = 0.5
init_pheromon = 0.1
q = 100
maxround = 100000

normal = 300
fast =  50

data Ant = Ant { 
	current :: Int,
	next :: Int,
	tabu :: [Int],
	path :: [Int],
	len :: Double	
	}
	

-- :: --

main = do
	initGUI
	Just xml <- xmlNew "aco.glade"
	window   <- xmlGetWidget xml castToWindow "window"
	onDestroy window mainQuit
	
	-- global variables
	cities <- newIORef ([] :: [(Int,Int)])
	pheromons <- newIORef (array ((0,0),(maxcities - 1,maxcities - 1)) [if i == j then ((i,j),0) else ((i,j),init_pheromon)| i <- [0..maxcities - 1], j <- [0..maxcities - 1]])
	distances <- newIORef (array ((0,0),(maxcities - 1,maxcities - 1)) [((i,j),0)| i <- [0..maxcities - 1], j <- [0..maxcities - 1]])
	ants <- newIORef ([] :: [Ant])
	round1s <- newIORef (0 :: Int)
	best_tour <- newIORef ([] :: [Int])
	best_ever_tour <- newIORef ([] :: [Int])
	best_tour_len <- newIORef (10000 :: Double) 
	best_ever_tour_len <- newIORef (10000 :: Double)
	offset <- newIORef ((0,0) :: (Int,Int))
	movingCity <- newIORef ((-1) :: Int)
	canStop <- newIORef True
	isRunning <- newIORef False
	speed <- newIORef normal
	idProcess <- newIORef (-1)
	
	writeIORef cities [(50,50),(100,70),(80,130),(120,30),(50,60),(210,70),(180,230),(120,95),(35,53),(130,75)]
	
	canvas <- xmlGetWidget xml castToDrawingArea "canvas"
	(w, h) <- drawingAreaGetSize canvas
	drawable <- drawingAreaGetDrawWindow canvas
	
	let mkGCWithColour r g b = gcNewWithValues drawable commonGCValues { foreground = Color (r*255) (g*255) (b*255), background = Color (62025) (62025) (62025)}
	redGC <- mkGCWithColour 255 0 0
	greenGC <- mkGCWithColour 0  255 0
	blueGC <- mkGCWithColour 0 0 255
	blackGC <- mkGCWithColour 0 0 0
	yellowGC <- mkGCWithColour 255 255 0
	viloletGC <- mkGCWithColour 255  0 255
	navyGC <- mkGCWithColour 0 255 255
	whiteGC <- mkGCWithColour 255 255 255
	
	-- updateLabel
	bestTour <- xmlGetWidget xml castToLabel "best_tour_label"
	bestEverTour <- xmlGetWidget xml castToLabel "best_ever_tour_label"
	bestTourLen  <- xmlGetWidget xml castToLabel "best_tour_length_label"
	bestEverTourLen  <- xmlGetWidget xml castToLabel "best_ever_tour_length_label"
	roundLabel <- xmlGetWidget xml castToLabel "rounds_label"
	
	speedbox <- xmlGetWidget xml castToCheckButton "checkbutton"
	set speedbox [toggleButtonActive := False]
	
	infobox <- xmlGetWidget xml castToCheckButton "info_checkbutton"
	set infobox [toggleButtonActive := False]
	info <- newIORef False
	
	mainBox <- xmlGetWidget xml castToVBox "vbox1"
	smb <- widgetGetSizeRequest mainBox
	sizeMainBox <- newIORef smb
	vBox <- vBoxNew True 5
	scrolledWindow <- scrolledWindowNew Nothing Nothing
	adj <- adjustmentNew 30 30 150 10 10 500
	viewPort <- viewportNew adj adj
	let st = "The ant-cycle algorithm uses alpha = 1.0,beta = 2.0, rho = 0.5, an initial tau (pheromone) on each link of 0.1, and a Q of 100. Once the ACO has been started by pressing 'Start', then after each full cycle the links (routes between cities) are displayed coloured by their relative pheromone strength, from the lowest, white (invisible) through black,blue,cyan,green,yellow, violet, to the most attractive, red. The nodes (cities) can be clicked and dragged to new positions within the original problem area, either before or while the ACO runs."
	infotext <- labelNew (Just st)
	labelSetLineWrap infotext True
	widgetSetSizeRequest infotext 500 100
	widgetSetSizeRequest vBox 550 110
		
	containerAdd viewPort infotext
	containerAdd scrolledWindow viewPort
	containerAdd vBox scrolledWindow
	
	let updateStatsLabels = do
		r <- readIORef round1s
		bt <-readIORef best_tour
		bet <-readIORef best_ever_tour
		btl <- readIORef best_tour_len
		betl <- readIORef best_ever_tour_len
		update1 roundLabel (show r)
		update2 bestTour (show bt)
		update bestEverTour (show bet)
		update2 bestTourLen (formatNumber btl)
		update bestEverTourLen (formatNumber betl)
	
	-- core of algorithm
	genCities cities (rcity, w-rcity) (rcity, h - rcity)
	getDistance cities distances
	genAnts ants
	
	let getBestTourLen best_tour_len best_ever_tour_len = do
		bt <- readIORef best_tour
		bet <- readIORef best_ever_tour
		bt' <- getTourLen bt distances
		bet' <- getTourLen bet distances
		writeIORef best_ever_tour_len bet'
		writeIORef best_tour_len bt'
		
	let run  = do
		simulateACO ants distances pheromons
		updateTrails pheromons ants
		restartAnts ants best_tour
		bt <- readIORef best_tour
		bl <- getTourLen bt distances
		writeIORef  best_tour_len bl
		bet <- readIORef best_ever_tour
		updatebestEver best_tour best_ever_tour distances
		getBestTourLen best_tour_len best_ever_tour_len
		
	let doStep = do
		r <- readIORef round1s
		run
		writeIORef round1s (r+1)
		return (r+1 < maxround )
		
	let doSteps steps = doSteps' steps
				where doSteps' n 	| n == 0 = return True
									| otherwise = do 
												continue <- doStep
												if continue then doSteps' (n-1)
													else return False
	
	let drawMap drawable cities = do
			drawCities canvas drawable redGC cities
			drawRoads drawable blueGC cities
			
	canvas `onExpose` (\_ -> do 
			-- drawCities drawable redGC cities
			-- drawRoads drawable blueGC cities
			showMap canvas drawable cities pheromons
			-- drawBestEverTour drawable blackGC best_ever_tour cities
			updateStatsLabels
			return True)
	
	onButtonPress canvas $ \Button { eventButton = button,eventX = x', eventY = y' } -> (do
		case button of
			LeftButton  ->	do
								let (x,y) = (floor x',floor y')
								i <- getIndexCity cities (x,y)
								if i == Nothing then return ()
									else do
										let id = fromJust i
										cts <- readIORef cities
										let ct = cts !! id
										let ofs = (x - fst ct, y - snd ct)
										writeIORef offset ofs
										writeIORef movingCity id 
								return True
			_            -> return True)
	onButtonRelease canvas $ \Button { eventButton = button, eventX = x', eventY = y' } -> ( do
		case button of
			LeftButton -> do
						(ox,oy) <- readIORef offset
						id <- readIORef movingCity
						if id < 0 then return ()
							else do
								let (x,y) = (floor x',floor y')
								let (cx,cy) = (x - ox, y - oy)
								let (w',h') = (w - 2*rcity1,h - 2*rcity1)
								reg <- regionRectangle $ Rectangle (rcity1) (rcity1) w' h'
								hit <- regionPointIn reg (cx ,cy)
								if not hit then return ()
									else do
								updatePositonCities id cities (cx,cy)
								getDistance cities distances
								genPheromone pheromons
								genAnts ants
						writeIORef movingCity (-1)
						return True
			_          -> return True)
	
	onMotionNotify canvas False $ \Motion {eventX = x', eventY = y'} -> do
		id <- readIORef movingCity
		let (x,y) = (floor x',floor y')
		if id < 0 then return ()
			else do
				ofs <- readIORef offset
				let (cx,cy) = (x - fst ofs, y - snd ofs)
				let (w',h') = (w - 2*rcity1,h - 2*rcity1)
				reg <- regionRectangle $ Rectangle (rcity1) (rcity1) w' h'
				hit <- regionPointIn reg (cx ,cy)
				if not hit then return ()
					else do
				updatePositonCities id cities (cx,cy)
				drawWindowClearArea drawable 0 0 w h
				-- drawMap drawable cities
				showMap canvas drawable cities pheromons
				-- drawBestEverTour drawable blackGC best_ever_tour cities
		return True
	
	-- events process
	start  <- xmlGetWidget xml castToButton "start_button"
	pause  <- xmlGetWidget xml castToButton "pause_button"
	reset  <- xmlGetWidget xml castToButton "reset_button"
	
	reset `onClicked` do
		drawWindowClearArea drawable 0 0 w h
		genCities cities (rcity, w-rcity) (rcity, h - rcity)
		getDistance cities distances
		genAnts ants
		genPheromone pheromons
		writeIORef canStop True
		-- writeIORef isRunning False
		writeIORef round1s 0
		writeIORef best_tour  []
		writeIORef best_ever_tour []
		writeIORef best_tour_len 10000
		writeIORef best_ever_tour_len 10000
		writeIORef offset (0,0)
		writeIORef movingCity (-1)
		updateStatsLabels
		showMap canvas drawable cities pheromons
		-- drawBestEverTour drawable blackGC best_ever_tour cities
		
	pause `onClicked` do
		canstop <- readIORef canStop
		iR <- readIORef isRunning
		if not iR || (not canstop) then return ()
			else do
				writeIORef canStop False
				r <- readIORef round1s
				if r <= maxround then writeIORef isRunning False
					else return ()
	
	let timeoutAddFull1 act prior speed = do
		sp <- readIORef speed
		idhandler <- timeoutAddFull act prior sp
		return idhandler
	
	start `onClicked` do
		st <- readIORef isRunning
		if st then return ()
			else do
		writeIORef isRunning True
		writeIORef canStop True
		id <- timeoutAddFull1 (do 
			continue <- doSteps 1
			iok <- readIORef canStop
			updateStatsLabels
			showMap canvas drawable cities pheromons
			-- drawBestEverTour drawable blackGC best_ever_tour cities
			return (continue && iok)) 
			priorityDefaultIdle speed
		writeIORef idProcess id
		return ()
	
	onToggled speedbox (do 
						s <- readIORef speed
						if s == normal then writeIORef speed fast else writeIORef speed normal
						id <- readIORef idProcess
						idleRemove id
						timeoutRemove id
						st <- readIORef isRunning
						if not st then return ()
							else do
								writeIORef isRunning True
								writeIORef canStop True
								id <- timeoutAddFull1 (do 
										continue <- doSteps 1
										iok <- readIORef canStop
										updateStatsLabels
										showMap canvas drawable cities pheromons
										-- drawBestEverTour drawable blackGC best_ever_tour cities
										return (continue && iok)) 
										priorityDefaultIdle speed
								writeIORef idProcess id
						widgetShowAll speedbox
						)
	
	infobox `onToggled` do
			i <- readIORef info
			if not i  then do
							boxPackEndDefaults mainBox vBox
							widgetShowAll mainBox
							writeIORef info True
				else do
					writeIORef info False	
					containerRemove mainBox vBox
					widgetHideAll  vBox
					(x,y) <- readIORef sizeMainBox
					widgetSetSizeRequest mainBox x y
					widgetShowAll mainBox

	widgetShowAll window
	mainGUI
-- --------------------------------------------------------------------

addHBoxToVBox :: Int -> VBox -> IO()
addHBoxToVBox w vBox  = do 
	hBox <- hBoxNew True w
	boxPackEndDefaults vBox hBox

simulateACO :: IORef [Ant] -> IORef (Array (Int,Int) Double) -> IORef (Array (Int,Int) Double) -> IO ()
simulateACO ants dists pheromone = do
    r <- simulateAnts ants dists pheromone
    if r == 0 then return ()
        else simulateACO ants dists pheromone

-- :: --


simulateAnts :: IORef [Ant] -> IORef (Array (Int,Int) Double) -> IORef (Array (Int,Int) Double) -> IO Int
simulateAnts ants dists pheromones = do
    moving <- newIORef 0
    -- sequence_ [simulateAnt i moving ants dists pheromones | i <- [0..maxants - 1]]
    doFromTo 0 (maxants -1) (\i -> simulateAnt i moving ants dists pheromones )
    mv <- readIORef moving
    return mv

-- :: --

simulateAnt :: Int -> IORef Int -> IORef [Ant] -> IORef (Array (Int,Int) Double) -> IORef (Array (Int,Int) Double) -> IO ()
simulateAnt i moving ants dists pheromones = do
    ans <- readIORef ants
    dts <- readIORef dists
    len <- newIORef 0 
    let ant = ans !! i
        path = getPath ant
        tabu = getTabu ant
        tlen = getLen ant
        ccity = getCCity ant
    if length path < maxcities then do
              ncity <- selectNextCity i ants dists pheromones
              let ntabu = tabu ++ [ncity]
                  npath = path ++ [ncity]
                  ntlen = tlen + dts ! (ccity,ncity)
              writeIORef len ntlen
              ln <- readIORef len
              if maxcities == length npath  then do
                    let nntlen = ntlen + dts ! ((npath !! 0),ncity)
                    writeIORef len nntlen
                else return ()  
              let ant' = (Ant ncity ncity ntabu npath ln)
                  xs = [if k == i then ant' else ans !! k | k <- [0.. maxants -1]]
              writeIORef ants xs
              m <- readIORef moving
              writeIORef moving (m+1)    
        else return ()
        
-- :: --        
getCCity :: Ant -> Int
getCCity (Ant ccity _ _ _ _)   = ccity 

getPath :: Ant -> [Int]
getPath (Ant _ _ _ path _) = path

getTabu :: Ant -> [Int]
getTabu (Ant _ _ tabu _ _) = tabu

getLen :: Ant -> Double
getLen (Ant _ _ _ _ len) = len
-- :: --

selectNextCity :: Int -> IORef [Ant] -> IORef (Array (Int,Int) Double) -> IORef (Array (Int,Int) Double) -> IO Int
selectNextCity i ants dist pheromones = do
	ans <- readIORef ants
	-- ps <- readIORef pheromones
	let an = ans !! i
	let ccity = getCCity an
	let tabu = getTabu an
	from <- newIORef 0
	to <- newIORef 0
	denom <- newIORef (0 :: Double)
	writeIORef from ccity
	sequence_ [if elem k tabu then return ()
					else do
						den <- readIORef denom ;
						apc <- antProduct ccity k dist pheromones;
						let dn = den + apc;
						writeIORef denom dn
				|k <- [0 .. maxcities - 1]]
	d <- readIORef denom
	pcs <- sequence [do
						p' <- antProduct ccity i dist pheromones
						let p = p' / d;
						return (i,p)
					|i <- ([0..maxcities -1] \\ tabu)]
	let ls = (head pcs) : (myzip (tail pcs) ls)
	r <- randomRIO (0,1)
	let t = getTo r ls
	if t /= -1 then return t
		else do
			let vs = head (reverse ls);ind = fst vs
			return ind     

-- :: --

getTo :: Double -> [(Int,Double)] -> Int
getTo r [] = -1
getTo r ((i,p):xs) = if r < p then i
                        else getTo r xs
-- :: --                        

myzip :: [(Int,Double)] -> [(Int,Double)] -> [(Int,Double)]
myzip [] [] = []
myzip [] _ = []
myzip _ [] = []
myzip ((i,x):xs) ((j,y):ys) = (i,x+y) : myzip xs ys

-- :: --

antProduct :: Int -> Int -> IORef (Array (Int,Int) Double) -> IORef (Array (Int,Int) Double) -> IO Double
antProduct ccity k ds ps = do
	ps1 <- readIORef ps
	ds1 <- readIORef ds
	let p = ps1 ! (ccity,k);d = 1.0 / ds1 ! (ccity,k);x = p ** alpha;y = d ** beta
	return (x * y)

-- :: --

updateTrails :: IORef (Array (Int,Int) Double) -> IORef [Ant] -> IO ()
updateTrails pheromone ants = do
    ps <- readIORef pheromone
    as <- readIORef ants
    let ps' = array ((0,0),(maxcities - 1,maxcities - 1)) [((from,to),ps ! (from,to) * (1-rho)) | from <- [0..maxcities -1], to <- [0.. maxcities -1]]
    writeIORef pheromone ps'
    sequence_ [updateTrail (as !! i) pheromone | i <- [0..maxants - 1]]
    ps'' <- readIORef pheromone
    let ps''' = array ((0,0),(maxcities - 1,maxcities - 1)) [((from,to),ps'' ! (from,to) * rho)|from <- [0..maxcities -1], to <- [0.. maxcities -1]]
    writeIORef pheromone ps'''

-- :: --    

updateTrail :: Ant -> IORef (Array (Int,Int) Double) -> IO ()
updateTrail an pheromone = do
	
	phs <- readIORef pheromone
	let ps = getPath an
	let len = getLen an
	let qs = (tail ps) ++ [head ps]
	let cs = zip ps qs
	let cs' = [(c2,c1) | (c1,c2) <- cs]
	let ks = cs ++ cs'
	let ps' = array ((0,0),(maxcities - 1,maxcities - 1)) [if (elem (from,to) ks) then ((from,to),phs ! (from,to) + dq) else ((from,to),phs ! (from,to))|from <- [0..maxcities -1], to <- [0.. maxcities -1],let dq = q / len]
	writeIORef pheromone ps'
	return ()

-- :: --

restartAnts :: IORef [Ant] -> IORef [Int] -> IO ()
restartAnts ants best = do
    getBest ants best
    genAnts ants

-- :: --

getBest :: IORef [Ant] -> IORef [Int] -> IO ()
getBest ants best = do
    bs <- readIORef best
    as <- readIORef ants
    let xs = [(tlen,ps)| i <- [0..maxants -1],let ps = getPath (as !! i),
                let tlen = getLen (as !! i)]
    let bst = snd (ffmin xs)
    writeIORef best bst

-- :: --

fmin :: (Double,[Int])-> (Double,[Int]) -> (Double,[Int])    
fmin (x,xs) (y,ys) = if x > y then (y,ys) else (x,xs)

ffmin xs = foldr fmin a xs
          where a = head xs

-- :: --

getTourLen :: [Int] -> IORef (Array (Int,Int) Double) -> IO Double
getTourLen bst dist = do
	dst <- readIORef dist
	let xs = zip bst (tail bst ++ [head bst])
	let sm = sum [dst ! (x,y) | (x,y) <- xs]
	return sm   

-- :: --

updatebestEver :: IORef [Int] -> IORef [Int]-> IORef (Array (Int,Int) Double) -> IO ()
updatebestEver besttour bestEvertour dist = do
    bt <- readIORef besttour
    bet <- readIORef bestEvertour
    bl <- getTourLen bt dist
    bel <- getTourLen bet dist
    if null bet then writeIORef bestEvertour bt
		else if bl < bel then writeIORef bestEvertour bt
				else return ()
-- :: --
genAnts :: IORef [Ant] -> IO ()
genAnts ants = do
    let as = [genAnt i | i <- [0..maxants - 1]]
    writeIORef ants as

genAnt :: Int -> Ant
genAnt i = let ccity = i `mod` maxcities
               ncity = -1
               tabu = [ccity]
               path = [ccity]
               tourlen = 0.0
           in (Ant ccity ncity tabu path tourlen)

genPheromone pheromones = writeIORef pheromones (array ((0,0),(maxcities - 1,maxcities - 1)) [if i == j then ((i,j),0) else ((i,j),init_pheromon)| i <- [0..maxcities - 1], j <- [0..maxcities - 1]])

getDistance :: IORef [(Int,Int)]  -> IORef (Array (Int,Int) Double)-> IO ()
getDistance cities distance = do
	cts <- readIORef cities
	let ds = (array ((0,0),(maxcities - 1,maxcities - 1)) [if i == j then ((i,j), 0.0)  else ((i,j),(dist i j cts)) | i <- [0..maxcities - 1], j <- [0..maxcities - 1]])
	writeIORef distance ds
	return ()

dist x y cits = let 
                        cx = cits !! x
                        cy = cits !! y
                        dx = abs (fst cx - fst cy)
                        dy = abs (snd cx - snd cy)
                    in sqrt (fromIntegral(dx*dx + dy*dy))
-- draw city
-- circle with radium = 5
genCities :: IORef [(Int,Int)] -> (Int,Int) -> (Int,Int) -> IO ()
genCities cities (w1,w2) (h1,h2)= do
    g <- randomIO
    let xs = randomRs (w1,w2) (mkStdGen g)
        ys = randomRs (h1,h2) (mkStdGen (g*3))
        ps = zip xs ys
        cts = take maxcities (nub ps)
    writeIORef cities cts

commonGCValues = newGCValues {
      fill = Solid,
      function = Copy,
      lineWidth = 1,
      lineStyle = LineSolid,
      capStyle = CapButt,
      joinStyle = JoinMiter
    }
updatePositonCities :: Int -> IORef [(Int,Int)] -> (Int,Int) -> IO ()
updatePositonCities id cities (cx,cy) = do
	cts <- readIORef cities
	let cts' = [if i == id then (cx,cy) else cts !! i | i <- [0.. length cts -1]]
	writeIORef cities cts'

inCircle :: (Int,Int) -> (Int,Int) -> Bool
inCircle (cx,cy) (x,y) = let d = sqrt (fromIntegral ((cx-x)^2 + (cy-y)^2 ))
							 ;r = fromIntegral (div rcity 2)
						 in d < r

getIndexCity :: IORef [(Int,Int)] -> (Int,Int) -> IO (Maybe Int)
getIndexCity pss (x,y) = do
	ps <- readIORef pss
	let res = findIndex (inCircle (x,y)) ps
	return res

toCenter (x,y) = (x + div rcity 2 , y + div rcity 2)
toSquare (x,y)  = (x - div rcity 2 , y - div rcity 2)
getCombinator xs = [(xi,xj) | xi <- xs, xj <- tail1 (dropWhile (/= xi) xs)]
tail1 x | x ==[] = []
		| otherwise = tail x


drawCity drawable gc pos = do
	let (x,y) = pos
	let (x',y') = toSquare pos
	drawArc drawable gc True x' y' rcity rcity (135*64) (360*64)

drawCities canvas drawable gc cities = do
	ps <- readIORef cities
	doFromTo 0 (length ps -1 ) (\i -> do
		drawCity drawable gc (ps !! i)
		num <- canvas `widgetCreateLayout` (show i)
		-- layoutSetAttributes num [AttrWeight 20 30 WeightBold,AttrSize 15 20 1024]
		drawLayoutWithColors drawable gc (fst (ps !! i) - 5) (snd (ps !! i) - 10 ) num (Just (Color 0 0 0)) Nothing)		

drawRoad drawable gc (p1,p2) = drawLine drawable gc p1 p2

drawRoads drawable gc cities = do
	ps <- readIORef cities
	let pss = getCombinator ps
	mapM_ (\x -> drawRoad drawable gc x) pss
drawBestEverTour drawable gc bestTour cities = do
	bt <- readIORef bestTour
	cts <- readIORef cities
	let bt1 = tail bt ++ [head bt]
	let cs = zip bt bt1
	let ps = [(cts !! x, cts !! y) | (x,y) <- cs]
	mapM_ (\x -> drawRoad drawable gc x) ps
	
showMap canvas drawable cities pheromones= do
	(w, h) <- drawingAreaGetSize canvas
	drawWindowClearArea drawable 0 0 w h
	cs <- readIORef cities
	ps <- readIORef pheromones
	let css = getCombinator cs
	let is = getCombinator [0.. maxcities - 1]
	let phes = [ ps ! (i,j) | (i,j) <- is]
	let mx = maximum phes
	let mi = minimum phes
	let dm = (mx - mi) / ncolor
	let ds = [mi + i*dm | i <- [1..8]]
	let getColor x ds = if (findIndex (>= x) ds) == Nothing then (length ds -1) else fromJust (findIndex (>= x) ds)
	let colors = map (\x -> getColor x ds) phes
	let citiesNcolors = zip css colors
	let mkGCWithColour r g b = gcNewWithValues drawable commonGCValues { foreground = Color (r*255) (g*255) (b*255), background = Color (62025) (62025) (62025) }
	citiesGC <- mkGCWithColour 255 0 0
	mapM_  (\(x,c) -> drawCitiesNcolorRoad drawable x c ) citiesNcolors
	drawCities canvas drawable citiesGC cities

drawCitiesNcolorRoad drawable (c1,c2) color = do
	let mkGCWithColour r g b = gcNewWithValues drawable commonGCValues { foreground = Color (r*255) (g*255) (b*255), background = Color (62025) (62025) (62025) }
	-- citiesGC <- mkGCWithColour 255 0 0
	-- drawCity drawable citiesGC c1
	-- drawCity drawable citiesGC c2
	case color of
		0 -> return ()
				
		1 -> do
				roadGC <- mkGCWithColour 10 20 10
				drawRoad drawable roadGC (c1,c2)
		2 -> do
				roadGC <- mkGCWithColour 0 0 255  
				drawRoad drawable roadGC (c1,c2)
		3 -> do
				roadGC <- mkGCWithColour 0 255 255
				drawRoad drawable roadGC (c1,c2)
		4 -> do
				roadGC <- mkGCWithColour 0 255 0
				drawRoad drawable roadGC (c1,c2)
		5 -> do
				roadGC <- mkGCWithColour 255 255 0
				drawRoad drawable roadGC (c1,c2)
		6 -> do
				roadGC <- mkGCWithColour 255 0 255
				drawRoad drawable roadGC (c1,c2)
		7 -> do
				roadGC <- mkGCWithColour 255  0 0 
				drawRoad drawable roadGC (c1,c2)
		_ -> return ()
doFromTo :: Int -> Int -> (Int -> IO ()) -> IO ()
doFromTo from to action =  let loop n | n > to = return ()
			           | otherwise = do
					action n
					loop (n+1)
		            in loop from
		            
formatNumber x = (printf "%.2f" x) :: String

updateLabelValue label atts value = labelSetMarkup label markup
    where 
      markup  = "<span " ++ xmlatts ++ ">" ++ value ++ "</span>"
      join c  = concat . intersperse c
      xmlatts = join " " [t++"=\""++v++"\"" | (t,v) <- atts]

update x y = updateLabelValue x atts y
update1 x y = updateLabelValue x att1s y
update2 x y = updateLabelValue x att2s y
atts   = [("font_family", "verdana"),("color","red")]
att1s   = [("font_family", "verdana"),("color","blue"),("size","30")]
att2s   = [("font_family", "verdana"),("color","black")]