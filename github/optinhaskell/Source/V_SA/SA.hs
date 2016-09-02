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

-- task
doms :: (Double,Double)
doms = (0,100)

f :: Double -> Double
f x = sin x + sin (x/2) + cos (x/3)

normal = 50
slow = 300
-- :: --

main = do
	initGUI
	Just xml <- xmlNew "sa.glade"
	
	window   <- xmlGetWidget xml castToWindow "window"
	onDestroy window mainQuit
	-- parameters
	epsilon <- newIORef (10000 :: Double)
	initial_tem <- newIORef (30 :: Double)
	final_tem <- newIORef (0.5 :: Double)
	alpha <- newIORef 0.99
	steps_per_change <- newIORef (1 :: Int)
	speed <- newIORef normal
	idProcess <- newIORef (-1)
	
	state <- newIORef 0
	current <- newIORef (0 :: Double)
	working <- newIORef (0 :: Double)
	best <- newIORef (0 :: Double)
	
	tem <- newIORef (0 :: Double)
	copySolution tem initial_tem
	
	-- drawable
	canvas <- xmlGetWidget xml castToDrawingArea "canvas"
	(w, h) <- drawingAreaGetSize canvas
	drawable <- drawingAreaGetDrawWindow canvas
	let commonGCValues = newGCValues {
      fill = Solid,
      function = Copy,
      lineWidth = 1,
      lineStyle = LineSolid,
      capStyle = CapButt,
      joinStyle = JoinMiter
    }
	let mkGCWithColour r g b = gcNewWithValues drawable commonGCValues { foreground = Color (r*255) (g*255) (b*255) }
	redGC <- mkGCWithColour 255 0 0
	blackGC <- mkGCWithColour 0 0 0
	bestSolutionGC <- mkGCWithColour 255 0 255
	currentSolutionGC <- mkGCWithColour 0 0 255
	workingSolutionGC <- mkGCWithColour 0 0 0 
	-- boldGC <- gcNewWithValues drawable commonGCValues { foreground = Color 0 0 0}		
	-- gcSetValues boldGC $ newGCValues {lineWidth = 10}
	let dw = floor (0.08 * fromIntegral w)
	let	dh = floor (0.3 * fromIntegral h)
	let w' = w - 2 * dw
	let h' = floor (0.5 * fromIntegral h)
	let delta = (snd doms - fst doms) / fromIntegral w'
	let ys = [y | i<- [0..w'], let x = delta * fromIntegral i, let y = f x]
	let ymin = minimum ys
	let ymax = maximum ys
	let dh' = floor (0.05 * fromIntegral h')
	let dy = fromIntegral (h' - 2*dh') / (ymax - ymin)
	let pys = [h - (dh + dh' + floor ((y - ymin)* dy)) | y<- ys ]
	let pxs = [dw + i| i <- [0..w']  ]
	let points = zip pxs pys
	let converter (x,y) = do
		let x' = dw + floor (x/delta);y'= h - (dh + dh' + floor ((y - ymin)* dy))
		return (x',y')
	let reconverter (px,py) = do
		let x = fst doms + fromIntegral (px - dw) * delta
		let y = ymin + fromIntegral (h - dh - dh' - py) / dy
		return (x,y)
			
	funtext <- canvas `widgetCreateLayout` "f(x) = sin (x) + sin (x/2) + cos (x/3)"
	zerotext <- canvas `widgetCreateLayout` 	"0"
	hundredtext <- canvas `widgetCreateLayout` "100"
	twentyfivetext <- canvas `widgetCreateLayout` "25"
	fiftytext <- canvas `widgetCreateLayout` "50"
	seventyfivetext <- canvas `widgetCreateLayout` "75"
	ymintext <- canvas `widgetCreateLayout` (formatNumber ymin)
	ymaxtext <- canvas `widgetCreateLayout` (formatNumber ymax)
		
	
	let drawFrame = do
		-- oxy
		drawLine drawable blackGC (dw - 6, h - dh + 2) (w-dw,h - dh + 2)
		drawLine drawable blackGC (dw - 6,h - h' - dh) (dw - 6 ,h - dh + 2)
		
		drawLayoutWithColors drawable blackGC (w `div` 2 -100) (h - dh + 40) funtext (Just (Color 0 0 0)) Nothing
		
		drawLayoutWithColors drawable blackGC (dw) (h - dh + 2) zerotext (Just (Color 0 0 0)) Nothing
		drawLine drawable blackGC (dw,h-dh) (dw,h-dh+3) 
		
		drawLayoutWithColors drawable blackGC (dw + (w' `div` 4)) (h - dh + 2) twentyfivetext (Just (Color 0 0 0)) Nothing
		drawLine drawable blackGC (dw + (w' `div` 4),h-dh) (dw + (w' `div` 4),h-dh+3) 
		
		drawLayoutWithColors drawable blackGC (dw + (w' `div` 2)) (h - dh + 2) fiftytext (Just (Color 0 0 0)) Nothing
		drawLine drawable blackGC (dw + (w' `div` 2),h-dh) (dw + (w' `div` 2),h-dh+3)
		
		drawLayoutWithColors drawable blackGC (dw + floor (fromIntegral w' * 0.75)) (h - dh + 2) seventyfivetext (Just (Color 0 0 0)) Nothing	
		drawLine drawable blackGC (dw + floor (fromIntegral w' * 0.75),h-dh) (dw + floor (fromIntegral w' * 0.75),h-dh+3)
		
		drawLayoutWithColors drawable blackGC (w-dw) (h - dh + 2) hundredtext (Just (Color 0 0 0)) Nothing
		drawLine drawable blackGC (w-dw,h-dh) (w-dw,h-dh+3)
		
		drawLayoutWithColors drawable blackGC (dw - 38) (h - dh - dh' - 5) ymintext (Just (Color 0 0 0)) Nothing
		drawLayoutWithColors drawable blackGC (dw - 38) (h - dh - h' + dh'- 5) ymaxtext (Just (Color 0 0 0)) Nothing
		drawLine drawable blackGC (dw - 7, h - dh - dh') (dw - 5, h - dh - dh')
		drawLine drawable blackGC (dw - 7, h - dh - h' + dh') (dw - 5, h - dh - h' + dh')
		
	let drawFunction = do
		drawLines drawable redGC points
	
	let initializeSolution current doms = do
		ep <- readIORef epsilon
		let dx = (snd doms - fst doms) / ep
		r <- randomRIO (0:: Int,round ep)
		let x = fst doms + fromIntegral r * dx
		writeIORef current x
	
	let run t current working best doms = do
		useNew <- newIORef (0 :: Int)
		initializeSolution working doms
		w <- getValue working
		c <- getValue current
		b <- getValue best
		if c < w then writeIORef useNew 1
			else do
				r <- randomIO
				let delta = c - w;calc = exp (- delta / t)
				if calc > r
					then writeIORef useNew 1
					else return ()
		u <- readIORef useNew
		if u == 1
			then do
					writeIORef useNew 0
					copySolution current working
					if w > b then copySolution best working
						else return ()
			else  return () -- copySolution working current
	
	initializeSolution current doms
	copySolution working current
	copySolution best current
	
	tem_label <- xmlGetWidget xml castToLabel "tem_label"
	best_solution_label <- xmlGetWidget xml castToLabel "best_solution_label"
	current_solution_label <- xmlGetWidget xml castToLabel "current_solution_label"
	working_solution_label <- xmlGetWidget xml castToLabel "working_solution_label"
	best_value_label <- xmlGetWidget xml castToLabel "best_value_label"
	current_value_label <- xmlGetWidget xml castToLabel "current_value_label"
	working_value_label <- xmlGetWidget xml castToLabel "working_value_label"
	
	eps <- xmlGetWidget xml castToEntry "epsilon_entry"
	intem <- xmlGetWidget xml castToEntry "init_tem_entry"
	fintem <- xmlGetWidget xml castToEntry "fin_tem_entry"
	alf <- xmlGetWidget xml castToEntry "alpha_entry"
	stp <- xmlGetWidget xml castToEntry "step_entry"
	set eps [entryText := "10000"]
	set intem [entryText := "30"]
	set fintem [entryText := "0.5"]
	set stp [entryText := "1"]
	set alf [entryText := "0.99"]
	
	speedbox <- xmlGetWidget xml castToCheckButton "speed_checkbutton"
	set speedbox [toggleButtonActive := False]
	
	
	
	let updateStatsLabels = do
		t <- readIORef tem
		bs <-readIORef best
		cs <- readIORef current
		ws <- readIORef working
		bv <- getValue best
		cv <- getValue current
		wv <- getValue working
		labelSetText tem_label (formatNumber t)
		-- labelSetText best_solution_label (formatNumber bs)
		update best_solution_label (formatNumber bs)
		-- labelSetText current_solution_label (formatNumber cs)
		update1 current_solution_label (formatNumber cs)
		labelSetText working_solution_label (formatNumber ws)
		-- labelSetText best_value_label (formatNumber bv)
		update best_value_label (formatNumber bv)
		-- labelSetText current_value_label (formatNumber cv)
		update1 current_value_label (formatNumber cv)
		labelSetText working_value_label (formatNumber wv)
	
	let doStep = do
		t <- readIORef tem
		spc <- readIORef steps_per_change
		alf <- readIORef alpha
		ftem <- readIORef final_tem 
		doFromTo 1 spc (\i -> run t current working best doms)
		writeIORef tem (t*alf)
		return (t*alf > ftem)
  
	let doSteps steps = doSteps' steps
				where doSteps' n 	| n == 0 = return True
									| otherwise = do 
												continue <- doStep
												if continue then doSteps' (n-1)
													else return False
													
	let drawSolution = do
		xb <- readIORef best
		xc <- readIORef current
		xw <- readIORef working
		let yb = f xb; yc = f xc; yw = f xw
		(xbest,ybest) <- converter (xb,yb)
		(xcurrent,ycurrent) <- converter (xc,yc)
		(xworking,yworking) <- converter (xw,yw)
		drawRectangle drawable bestSolutionGC True xbest ybest 7 7
		drawRectangle drawable currentSolutionGC True xcurrent ycurrent 6 6
		drawRectangle drawable workingSolutionGC True xworking yworking 5 5
		drawLine drawable bestSolutionGC (xbest,h - dh) (xbest,h-dh-h')
		drawLine drawable currentSolutionGC (xcurrent,h - dh) (xcurrent,h-dh-h')
		drawLine drawable workingSolutionGC (xworking,h - dh) (xworking,h-dh-h')
	let drawAlls = do
		drawWindowClearArea  drawable dw (h - dh - h') (w'+5) h'
		-- drawFrame
		drawFunction
		drawSolution
		
	widgetSetDoubleBuffered canvas False
	widgetSetRedrawOnAllocate canvas False
	canvas `onExpose` (\Expose { eventRegion = region } -> do
	rects <- regionGetRectangles region
	st <- readIORef state
	drawFrame
	if st == 0 then drawFunction
		else drawAlls
	return True)
	
	canStop <- newIORef True
	
	pause  <- xmlGetWidget xml castToButton "pause_button"
	pause `onClicked` do
		canstop <- readIORef canStop
		st <- readIORef state
		if (st == 0) || (not canstop) then return ()
			else do
				writeIORef canStop False
				t <- readIORef tem
				ft <- readIORef final_tem
				if t > ft then writeIORef state 0
					else return ()
				
	
	let timeoutAddFull1 act prior speed = do
		sp <- readIORef speed
		idhandler <- timeoutAddFull act prior sp
		return idhandler
		
	run  <- xmlGetWidget xml castToButton "run_button"
	run `onClicked` do
		st <- readIORef state
		if st /= 0 then return ()
			else do
		writeIORef state 1
		writeIORef canStop True
		id <- timeoutAddFull1 (do 
			continue <- doSteps 2
			iok <- readIORef canStop
			updateStatsLabels
			drawAlls
			return (continue && iok)) 
			priorityDefaultIdle speed
		writeIORef idProcess id
		return ()
	
	onToggled speedbox (do 
						-- cur <- toggleButtonGetActive speedbox
						-- toggleButtonSetActive speedbox (not cur)
						s <- readIORef speed
						if s == normal then writeIORef speed slow else writeIORef speed normal
						id <- readIORef idProcess
						idleRemove id
						timeoutRemove id
						st <- readIORef state
						if st == 0 then return ()
							else do
								writeIORef state 1
								writeIORef canStop True
								id <- timeoutAddFull1 (do 
										continue <- doSteps 2
										iok <- readIORef canStop
										updateStatsLabels
										drawAlls
										return (continue && iok)) 
										priorityDefaultIdle speed
								writeIORef idProcess id
						widgetShowAll speedbox
						)
	set  <- xmlGetWidget xml castToButton "set_button"
	set `onClicked` do
		e <- get eps entryText
		it <- get intem entryText
		ft <- get fintem entryText
		af <- get alf entryText
		st <- get stp entryText
		let xs = [e,it,ft,af,st]
		if or [null x | x <- xs] then do
								writeIORef epsilon 10000
								writeIORef initial_tem 30
								writeIORef final_tem 0.5
								writeIORef alpha 0.99
								writeIORef steps_per_change 1
			else do
				let st' = read st :: Int
				let	af' = read af :: Double
				let	ft' = read ft :: Double
				let	it' = read it :: Double
				let	e' = read e :: Double
				writeIORef epsilon e'
				writeIORef initial_tem it'
				writeIORef final_tem ft'
				writeIORef alpha af'
				writeIORef steps_per_change st'
		writeIORef canStop True
		tm <- readIORef tem
		ftem <- readIORef final_tem
		if tm <= ftem then writeIORef state 0
			else return ()
		item <- readIORef initial_tem
		writeIORef tem item
		initializeSolution current doms
		copySolution working current
		copySolution best current
	
	let updatePosition canvas (x',y') = do
		drawWindowClearArea drawable dw 50 100 15
		xtext <- canvas `widgetCreateLayout` ("X: " ++ formatNumber x')
		ytext <- canvas `widgetCreateLayout` ("Y: " ++ formatNumber y')
		drawLayoutWithColors drawable redGC (dw) (50) xtext (Just (Color 0 0 0)) Nothing
		drawLayoutWithColors drawable redGC (dw + 50) (50) ytext (Just (Color 0 0 0)) Nothing
		
	onMotionNotify canvas False $ \Motion {eventX = x, eventY = y} -> do
			-- st <- readIORef state
			-- if st /= 0 then return False
			--	else do
			reg <- regionRectangle $ Rectangle dw (h - dh - h' + dh'-1) w' (h'-2*dh'+ 1)
			hit <- regionPointIn reg (floor x, floor y)
			if hit then do
					(x',y') <- reconverter (floor x, floor y)
					updatePosition canvas (x',y')
					return True
				else return True
					
	widgetShowAll window
	mainGUI
	
-- :: --

getValue :: IORef Double -> IO Double
getValue solution = do
	s <- readIORef solution
	let res = f s
	return res

copySolution :: IORef Double -> IORef Double -> IO ()
copySolution working current = do
    c <- readIORef current
    writeIORef working c
    
formatNumber x = (printf "%.2f" x) :: String
formatNumber1 x = (printf "%.1f" x) :: String

updateLabelValue label atts value = labelSetMarkup label markup
    where 
      markup  = "<span " ++ xmlatts ++ ">" ++ value ++ "</span>"
      join c  = concat . intersperse c
      xmlatts = join " " [t++"=\""++v++"\"" | (t,v) <- atts]

update x y = updateLabelValue x atts y
update1 x y = updateLabelValue x att1s y
atts   = [("font_family", "verdana"),("color","red")]
att1s   = [("font_family", "verdana"),("color","blue")]      

doFromTo :: Int -> Int -> (Int -> IO ()) -> IO ()
doFromTo from to action =  let loop n | n > to = return ()
			           | otherwise = do
					action n
					loop (n+1)
		            in loop from
		            
