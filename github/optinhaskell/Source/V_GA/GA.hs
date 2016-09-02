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
import Maybe

-- task
doms :: (Double,Double)
doms = (0,100)

f :: Double -> Double
f x = sin x + sin (x/2) + cos (x/3) + exp (-x^2/4)

data State = Start | Elitism | Crossover | NewPop | Sorted
instance Eq State

pc = 0.8
pm = 0.03
popLevel = 1000
mEpsilon = 2^32 -1
popSize = 16
chromLen = 32

-- :: --

main = do
	initGUI
	Just xml <- xmlNew "ga.glade"
	
	-- global variables
	presentPop <- newIORef []
	newPop <- newIORef []
	parents <- newIORef []
	newoffspring <- newIORef []
	mutatedoffspring <- newIORef []
	state <- newIORef Start
	crossoverpoint <- newIORef (-1)
	crossovercase <- newIORef 0
	level <- newIORef 1
	isRunning <- newIORef 0
	sizeNewPop <- newIORef 0
	mutbits <- newIORef ([],[]) 
	selectLines <- newIORef []
	solutionPositions <- newIORef []
	bestPosition <- newIORef 0
	canStop <- newIORef False
	let dh = 5
	let	dw = 5
	window   <- xmlGetWidget xml castToWindow "window"
	onDestroy window mainQuit

	canvas <- xmlGetWidget xml castToDrawingArea "canvas"
	(w, h) <- drawingAreaGetSize canvas
	drawable <- drawingAreaGetDrawWindow canvas
	
	let working_height = floor (0.65 * fromIntegral h)
	let	plot_height = floor (0.33 * fromIntegral h)
	let	working_width = (floor . fromIntegral) (w - 2*dw)
	let	plot_width = working_width
	let	working_position = (dw,dh)
	let	plot_position = (dw,2*dh + working_height)
	let label_height = floor (10 * fromIntegral dh)
	let label_width = working_width
	let connect_height = floor (15 * fromIntegral dh)
	let connect_width = working_width
	let crossarea_height = 240
	let crossarea_width = working_width
	let	label_position = (dw+2,2*dh)
	let crossarea_position = (dw+2,3*dh + label_height)
	let connect_position = (dw+2,3*dh + label_height + crossarea_height + 2*dh)
	let present_pop_position = (dw + 70,snd crossarea_position + dh)
	let new_pop_position = (w - dw - 240,snd crossarea_position + dh)
	let parent_position = (fst present_pop_position + 220, snd new_pop_position)
	let new_offspring_position = (fst parent_position + 90, snd new_pop_position)
	let mutated_offspring_position = (fst new_offspring_position + 90, snd new_pop_position)
	let dbit = 2
	let rbit = 5
	
	-- draw function 
	let w' = plot_width - 2 * dw
	let h' = plot_height - 2 * dh
	let delta = (snd doms - fst doms) / fromIntegral w'
	let ys = [y | i<- [0..w'], let x = fst doms + delta * fromIntegral i, let y = f x]
	let ymin = minimum ys
	let ymax = maximum ys
	let dy = fromIntegral h'/ (ymax - ymin)
	let pys = [snd plot_position + h' + dh - floor ((y - ymin)* dy) | y<- ys ]
	let pxs = [2*dw + i| i <- [0..w']  ]
	let points = zip pxs pys
	let converterX x =  2*dw + floor ((x - fst doms)/delta)
	
	let commonGCValues = newGCValues{
		fill = Solid,
		function = Copy,
		lineWidth = 1,
		lineStyle = LineSolid,
		capStyle = CapButt,
		joinStyle = JoinMiter}
	let lineGCValues = newGCValues {
		fill = Solid,
		function = Copy,
		lineWidth = 2,
		lineStyle = LineSolid,
		capStyle = CapButt,
		joinStyle = JoinMiter}
	let mkGCWithColour r g b = gcNewWithValues drawable commonGCValues { foreground = Color (r*255) (g*255) (b*255),background = Color 65025 65025 65025 }
	redGC <- mkGCWithColour 255 0 0
	blackGC <- mkGCWithColour 0 0 0
	blueGC <- mkGCWithColour 0 0 255
	greenGC <- mkGCWithColour 0 255 0
	let mkLineGCWithColour r g b = gcNewWithValues drawable lineGCValues { foreground = Color (r*255) (g*255) (b*255),background = Color 65025 65025 65025 }
	selectLineGC <- mkLineGCWithColour 0 0 0
	crossoverLineGC <- mkLineGCWithColour 0 255 0
	
	let getNewChromPos i = (fst new_pop_position + i * (rbit + dbit + 4), snd new_pop_position)
	let drawNewEmptyChrom i = do
		drawRec drawable blackGC False (getNewChromPos i) (rbit+2) 226
	let drawNewEmptyPop = mapM_ drawNewEmptyChrom [0..15]
	let showNewEmptyPop = do
		drawWindowClearArea drawable (fst new_pop_position) (snd new_pop_position) 180 226
		drawNewEmptyPop
	let drawBit i bit_position = do
		if i == 0 then drawRectangle drawable blueGC True (fst bit_position) (snd bit_position) rbit rbit
			else drawRectangle drawable redGC True (fst bit_position) (snd bit_position) rbit rbit
		return ()
	
	let getBitPos i pos = (fst pos + 2, snd pos + dbit + i*(rbit + dbit))
	
	let drawChrom pos chr = doFromTo 0 31 (\i -> do
		drawRec drawable blackGC False pos (rbit+4) 226
		drawBit (chr !! i) (getBitPos i pos))
	
	let getPresentChromPos i = (fst present_pop_position + i * (rbit + dbit + 4), snd present_pop_position)
	
	let replaceChromToNewPop pos chrom = do
		drawWindowClearArea drawable (fst pos) (snd pos) (rbit + 4) 226
		drawChrom pos chrom
		
	let drawPresentPop chrs = doFromTo 0 (popSize - 1) (\i -> do
		drawChrom (getPresentChromPos i) (chrs !! i))
		
	let drawNewPop chrs = doFromTo 0 (length chrs - 1) (\i -> do
		replaceChromToNewPop (getNewChromPos i) (chrs !! i))
	
	let showNewPop pop = do
		p <- readIORef pop
		if null p then showNewEmptyPop
			else do
				drawWindowClearArea drawable (fst new_pop_position) (snd new_pop_position) 180 226
				drawNewEmptyPop
				drawNewPop p
		
	let showPresentPop pop = do
		p <- readIORef pop
		drawWindowClearArea drawable (fst present_pop_position) (snd present_pop_position) 180 226
		drawPresentPop p
		
	let drawParents ps = do
		[dad,mum] <- readIORef ps
		drawChrom (fst parent_position,snd new_pop_position) dad
		drawChrom (fst parent_position + 20,snd new_pop_position) mum
	let showParents ps = do
		prs <- readIORef ps
		if null prs then return ()
			else do
				let [x,y] = prs
				drawWindowClearArea drawable  (fst parent_position) (snd new_pop_position) 30 226
				drawParents ps
	
	let drawNew (c1,c2) = do
		drawChrom (fst new_offspring_position,snd new_pop_position) c1
		drawChrom (fst new_offspring_position + 20,snd new_pop_position) c2
	
	let showNew newoffspring = do
		nos <- readIORef newoffspring
		if null nos then return ()
			else do
				let [x,y] = nos
				drawWindowClearArea drawable  (fst new_offspring_position) (snd new_pop_position) 30 226
				drawNew (x,y)
				
	let drawMutBit i pos =  do
		let ps = getBitPos i pos
		drawRectangle drawable blackGC True (fst ps) (snd ps) rbit rbit
	
	let drawMutated (child1,child2) (mut1,mut2) = do
		mapM_ (\i -> drawMutBit i (fst mutated_offspring_position - 4 - rbit,snd new_pop_position )) mut1
		mapM_ (\i -> drawMutBit i (fst mutated_offspring_position + 16 - rbit,snd new_pop_position)) mut2
		drawChrom (fst mutated_offspring_position,snd new_pop_position) child1
		drawChrom (fst mutated_offspring_position + 20,snd new_pop_position) child2
	
	let showMutated mutatedoffspring mutbits = do
		mos <- readIORef mutatedoffspring
		(mut1,mut2) <- readIORef mutbits
		if null mos then return ()
			else do
				let [x,y] = mos
				drawWindowClearArea drawable  (fst mutated_offspring_position) (snd new_pop_position) 30 226
				drawMutated (x,y) (mut1,mut2)
		
	
	let drawSelectLines (d,m) = do
		let (xd',yd') = getPresentChromPos d;(xm',ym') = getPresentChromPos m; (xd,yd) = (xd' +1,yd'); (xm,ym) = (xm' +2,ym')
		drawLine drawable selectLineGC (xd,yd-2) (xd+rbit+dbit,yd-2)
		drawLine drawable selectLineGC (xm,ym-2) (xm+rbit+dbit,ym-2)
		drawLine drawable selectLineGC (xd,yd +230) (xd+rbit+dbit,yd +230)
		drawLine drawable selectLineGC (xm,ym +230) (xm+rbit+dbit,ym +230)
	
	let showSelectLines  selectLines = do
		sl <- readIORef selectLines
		if null sl then return ()
			else do
				let [x,y] = sl
				drawSelectLines (x,y)
				
	let drawCrossoverLine i = do
		let pos = getBitPos i parent_position; (xc,yc) = (fst pos -2,snd pos - dbit)
		drawLine drawable crossoverLineGC (xc,yc) (xc+210,yc)
	
	let showCrossoverLine cpoint = do
		cp <- readIORef cpoint
		if cp >= 0 then drawCrossoverLine cp
			else return ()
		
	let drawNavigator dx (x1,y1) (x2,y2) = do
		drawLines drawable greenGC [(x1,y1),(x1,y1-dx),(x2,y1 -dx),(x2,y2)]
	
	let drawConnectedLines pos (x1,x2)= do
		let p1 = (fst pos + 4, snd pos + 230);p2 = (fst pos + 24, snd pos + 230)
		drawNavigator dh (x1, dh + working_height - 2 ) p1
		drawNavigator (2*dh) (x2, dh + working_height - 2) p2
	
	let showConnectedLines = do
		cr <- readIORef crossovercase
		case cr of
			1 -> do
					ps <- readIORef parents
					if null ps then return ()
						else do	
								let [px,py] = ps;x1 = converterX (decode doms px);x2 = converterX (decode doms py)
								drawConnectedLines parent_position (x1,x2)
			2 -> do
					ps <- readIORef newoffspring
					if null ps then return ()
						else do	
								let [px,py] = ps;x1 = converterX (decode doms px); x2 = converterX (decode doms py)
								drawConnectedLines new_offspring_position (x1,x2)
			3 -> do
					ps <- readIORef mutatedoffspring
					if null ps then return ()
						else do	
								let [px,py] = ps;x1 = converterX (decode doms px); x2 = converterX (decode doms py)
								drawConnectedLines mutated_offspring_position (x1,x2)
			_ -> return ()
	
	let drawFunction = do
		drawLines drawable  blueGC points
	
	let drawSolution x = do
		let x' = x ; y' = snd plot_position + 2
		drawLine drawable greenGC (x',y') (x', y' + plot_height - 4)
	
	let drawBestSolution x = do 
		let x' = x ; y' = snd plot_position + 2
		drawLine drawable redGC (x',y') (x', y' + plot_height - 4)
			
	
	let getSolutionPosition = do
		presPop <- readIORef presentPop
		let vs = eval presPop;xs = [converterX (decode doms p) | p <- presPop]
		writeIORef solutionPositions xs
		let m = maximum vs;i = fromJust (findIndex (== m) vs)
		writeIORef bestPosition (xs !! i)
	
	let showSolutions = do
		sps <- readIORef solutionPositions
		bst <- readIORef bestPosition
		mapM_ drawSolution sps
		drawBestSolution bst
		
	let drawLabels canvas = do
		drawWindowClearArea drawable (fst label_position) (snd label_position) (label_width-2) label_height
		presentpoptext <- canvas `widgetCreateLayout` "Present population"
		parentstext <- canvas `widgetCreateLayout` "Parents"
		newoffspringtext <- canvas `widgetCreateLayout` "New"
		mutatedoffspringtext <- canvas `widgetCreateLayout` "Mutated"
		newpoptext <- canvas `widgetCreateLayout` "New population"
		drawLayoutWithColors drawable blackGC (fst present_pop_position) (fst label_position + 10) presentpoptext (Just (Color 0 0 0)) Nothing
		drawLayoutWithColors drawable blackGC ( fst parent_position ) (fst label_position + 10) parentstext (Just (Color 0 0 0)) Nothing
		drawLayoutWithColors drawable blackGC ( fst new_offspring_position ) (fst label_position + 10) newoffspringtext (Just (Color 0 0 0)) Nothing
		drawLayoutWithColors drawable blackGC ( fst mutated_offspring_position ) (fst label_position + 10) mutatedoffspringtext (Just (Color 0 0 0)) Nothing
		drawLayoutWithColors drawable blackGC ( fst new_pop_position ) (fst label_position + 10) newpoptext (Just (Color 0 0 0)) Nothing	
	
	let showInfoWarning = do
		st <- readIORef state
		l <- readIORef level
		case st of
			Start -> do
					if l <= 1 then return ()
						else do
							newpophasbeensortedtext <- canvas `widgetCreateLayout` "has been sorted"
							drawLayoutWithColors drawable blackGC (fst present_pop_position) (fst label_position + 30) newpophasbeensortedtext (Just (Color 65025 0 0)) Nothing
			Elitism -> do
					elitismtext <- canvas `widgetCreateLayout` "E l i t i s m"
					justcopytext <- canvas `widgetCreateLayout` "The best chromosomes are just copied"
					drawLayoutWithColors drawable blackGC ( fst new_offspring_position - 20 ) (fst label_position + 150) elitismtext (Just (Color 65205 0 0)) Nothing
					drawLayoutWithColors drawable blackGC ( fst parent_position ) (fst label_position + 170) justcopytext (Just (Color 65205 0 0)) Nothing
			Crossover -> do
						cc <- readIORef crossovercase
						case cc of
							0 -> return ()
							1 -> do
								cp <- readIORef crossoverpoint
								if cp >= 0 then do
										crosspointtext <- canvas `widgetCreateLayout` "Crossover point"
										cp <- readIORef crossoverpoint
										let pos = getBitPos cp parent_position; (xc,yc) = (fst pos + 35 ,snd pos - 20 )
										drawLayoutWithColors drawable blackGC  xc yc crosspointtext (Just (Color 0 0 0)) Nothing
									else do
										nocrosstext <- canvas `widgetCreateLayout` "No crossover. Just copy parents"
										drawLayoutWithColors drawable blackGC ( fst new_offspring_position - 20 ) (fst label_position + 150) nocrosstext (Just (Color 65025 0 0)) Nothing
							2 -> return ()
							3 -> return ()
								
			Sorted -> do
						newpophasbeencopiedtext <- canvas `widgetCreateLayout` "has copied"
						drawLayoutWithColors drawable blackGC (fst present_pop_position) (fst label_position + 30) newpophasbeencopiedtext (Just (Color 65025 0 0)) Nothing
			_ -> return ()				
	
	canvas `onExpose` (\Expose { eventRegion = region } -> do
	rects <- regionGetRectangles region
	drawRec drawable blackGC False working_position working_width working_height
	drawRec drawable blackGC False plot_position plot_width plot_height
	-- drawRec drawable blackGC False label_position (label_width -4) label_height
	-- drawRec drawable blackGC False crossarea_position (crossarea_width -4) crossarea_height
	-- drawRec drawable blackGC False present_pop_position (170) (crossarea_height - 2 * dh)
	-- drawRec drawable blackGC False new_pop_position (170) (crossarea_height - 2 * dh)
	drawLabels canvas
	-- drawNewEmptyPop
	-- drawPresentPop testpresentpop
	-- drawParents parents
	-- drawNew news
	-- drawMutated (testchrom,testchrom) ([],[4,7])
	-- drawSelectLines (1,2)
	-- drawCrossoverLine 10
	-- drawConnectedLines mutated_offspring_position (converterX 1.48,converterX 80)
	-- replaceChromToNewPop (getNewChromPos 0) testchrom
	showPresentPop presentPop
	showNewPop newPop
	showParents parents
	showNew newoffspring
	showMutated mutatedoffspring mutbits
	showCrossoverLine crossoverpoint
	showSelectLines  selectLines
	showConnectedLines
	showInfoWarning
	drawFunction
	showSolutions
	return True)
	
	let clearSelectLines = drawWindowClearArea drawable (fst present_pop_position-3) (snd present_pop_position - 3) 180 2
	let onestep = do
		st <- readIORef state
		case st of 
			Start -> do
						writeIORef state Elitism
						drawWindowClearArea drawable (fst present_pop_position - 1) (fst label_position + 29) 100 18 
						drawWindowClearArea drawable (fst parent_position ) (snd parent_position - 2 ) 225 228
						elitismtext <- canvas `widgetCreateLayout` "E l i t i s m"
						justcopytext <- canvas `widgetCreateLayout` "The best chromosomes are just copied"
						drawLayoutWithColors drawable blackGC ( fst new_offspring_position - 20 ) (fst label_position + 150) elitismtext (Just (Color 65205 0 0)) Nothing
						drawLayoutWithColors drawable blackGC ( fst parent_position ) (fst label_position + 170) justcopytext (Just (Color 65205 0 0)) Nothing
						present <- readIORef presentPop
						addChromToPop (present !! 0) newPop
						modifyIORef sizeNewPop (\x -> x+1)
						replaceChromToNewPop (getNewChromPos 0) (present !! 0)
			Elitism -> do
						writeIORef state Crossover
						present <- readIORef presentPop
						addChromToPop (present !! 1) newPop
						modifyIORef sizeNewPop (\x -> x+1)
						replaceChromToNewPop (getNewChromPos 1) (present !! 1)
			Crossover -> do
						cc <- readIORef crossovercase
						case cc of
							0 -> do
								drawWindowClearArea drawable (fst parent_position - 2 ) (snd parent_position - 2 ) 225 228
								writeIORef crossovercase 1
								[x,y] <- selectParents presentPop
								writeIORef selectLines [x,y]
								presPop <- readIORef presentPop
								let px = presPop !! x ;py = presPop !! y
								ptem <- sortPop [px,py]
								writeIORef parents ptem
								let x1 = converterX (decode doms px); x2 = converterX (decode doms py)
								drawParents parents
								drawWindowClearArea drawable (dw +1 )  (snd present_pop_position + 229) (working_width - 2) (dh + working_height - 231 - snd present_pop_position)
								drawSelectLines (x,y)
								drawConnectedLines parent_position (x1,x2)
								ok <- getCrossover pc
								if not ok then do
												nocrosstext <- canvas `widgetCreateLayout` "No crossover. Just copy parents"
												drawLayoutWithColors drawable blackGC ( fst new_offspring_position - 20 ) (fst label_position + 150) nocrosstext (Just (Color 65025 0 0)) Nothing
												return () -- show only copy to new offspring
									else do
										crosspoint <- getCrossoverPoint chromLen
										writeIORef crossoverpoint crosspoint
										drawCrossoverLine crosspoint
										crosspointtext <- canvas `widgetCreateLayout` "Crossover point"
										cp <- readIORef crossoverpoint
										let pos = getBitPos cp parent_position; (xc,yc) = (fst pos + 35 ,snd pos - 20 )
										drawLayoutWithColors drawable blackGC  xc yc crosspointtext (Just (Color 0 0 0)) Nothing
							1 -> do
								writeIORef crossovercase 2
								clearSelectLines
								cp <- readIORef crossoverpoint
								let pos = getBitPos cp parent_position; (xc,yc) = (fst pos + 29 ,snd pos - 19 )
								drawWindowClearArea drawable xc yc 100 15
								if cp >= 0 then do 
													nos <- crossOver cp parents
													writeIORef newoffspring nos
									else do
											drawWindowClearArea drawable ( fst new_offspring_position - 25 ) (fst label_position + 148) 190 25  
											copyPopFromTo parents newoffspring
								[nx,ny] <- readIORef newoffspring
								drawNew (nx,ny)
								drawWindowClearArea drawable (dw +1 )  (snd present_pop_position + 229) (working_width - 2) (dh + working_height - 231 - snd present_pop_position) 
								let x1 = converterX (decode doms nx); x2 = converterX (decode doms ny)
								drawConnectedLines new_offspring_position (x1,x2)
								writeIORef selectLines []
							2 -> do
								writeIORef crossovercase 3
								(mb1,mb2) <- mutatedFromTo newoffspring mutatedoffspring
								[mx,my] <- readIORef mutatedoffspring
								writeIORef mutbits (mb1,mb2)
								drawMutated (mx,my) (mb1,mb2)
								drawWindowClearArea drawable (dw +1 )  (snd present_pop_position + 229) (working_width - 2) (dh + working_height - 231 - snd present_pop_position) 
								let x1 = converterX (decode doms mx); x2 = converterX (decode doms my)
								drawConnectedLines mutated_offspring_position (x1,x2)
								
							3 -> do
								index <- readIORef sizeNewPop
								[m1,m2] <- readIORef mutatedoffspring
								replaceChromToNewPop (getNewChromPos index) m1
								replaceChromToNewPop (getNewChromPos (index + 1)) m2
								drawWindowClearArea drawable (fst parent_position ) (snd parent_position - 2 ) 210 228
								drawWindowClearArea drawable (dw +1 )  (snd present_pop_position + 230) (working_width - 2) (dh + working_height - 231 - snd present_pop_position) 
								addChromToPop m1 newPop
								addChromToPop m2 newPop
								writeIORef sizeNewPop (index + 2)
								if index >=14 then writeIORef state NewPop
									else return ()
								writeIORef crossovercase 0
								writeIORef crossoverpoint (-1)
								writeIORef parents []
								writeIORef newoffspring []
								writeIORef mutatedoffspring []
								writeIORef selectLines []
								writeIORef mutbits ([],[])
								writeIORef selectLines []
							_ -> return ()
			NewPop -> do
						copyPopFromTo newPop presentPop
						showPresentPop presentPop
						showNewEmptyPop
						newpophasbeencopiedtext <- canvas `widgetCreateLayout` "has copied"
						drawLayoutWithColors drawable blackGC (fst present_pop_position) (fst label_position + 30) newpophasbeencopiedtext (Just (Color 65025 0 0)) Nothing
						writeIORef state Sorted				
						writeIORef sizeNewPop 0
						writeIORef newPop []
			Sorted -> do
						writeIORef state Start
						p <- readIORef presentPop
						p' <- sortPop p
						writeIORef presentPop p'
						modifyIORef level (\x -> x +1)
						showPresentPop presentPop
						drawWindowClearArea drawable (fst plot_position + 2) (snd plot_position + 2) (plot_width - 4) (plot_height - 4)
						getSolutionPosition
						drawWindowClearArea drawable (fst present_pop_position - 1) (fst label_position + 29) 100 18
						newpophasbeensortedtext <- canvas `widgetCreateLayout` "has been sorted"
						drawLayoutWithColors drawable blackGC (fst present_pop_position) (fst label_position + 30) newpophasbeensortedtext (Just (Color 65025 0 0)) Nothing
						drawFunction
						showSolutions
			
			
	-- init presentPop
	initPopulation presentPop
	getSolutionPosition
	
	reset  <- xmlGetWidget xml castToButton "reset_button"
	reset `onClicked` do
		writeIORef state Start
		writeIORef newPop []
		writeIORef parents []
		writeIORef newoffspring []
		writeIORef mutatedoffspring []
		writeIORef state Start
		writeIORef crossoverpoint (-1)
		writeIORef crossovercase 0
		writeIORef level 1
		writeIORef isRunning 0
		writeIORef sizeNewPop 0
		writeIORef mutbits ([],[])
		writeIORef selectLines []
		writeIORef canStop False
		initPopulation presentPop
		getSolutionPosition
		drawWindowClearArea drawable (fst crossarea_position) (snd crossarea_position) (crossarea_width -4) (dh + working_height - 1 - snd crossarea_position)
		showPresentPop presentPop
		showNewEmptyPop
		drawWindowClearArea drawable (fst plot_position + 2) (snd plot_position + 2) (plot_width - 4) (plot_height - 4)
		getSolutionPosition
		drawFunction
		showSolutions
		showInfoWarning
		
	step  <- xmlGetWidget xml castToButton "step_button"
	step `onClicked` do
		st <- readIORef isRunning
		if st == 1 then return ()
			else onestep
	stop  <- xmlGetWidget xml castToButton "stop_button"
	stop `onClicked` do
		canstop <- readIORef canStop
		r <- readIORef isRunning
		if (r == 0) || (not canstop) then return ()
			else do
				writeIORef canStop False
				l <- readIORef level
				if l < popLevel then writeIORef isRunning 0
					else return ()
	let doSteps = do
		onestep
		l <- readIORef level
		return (l < popLevel)
		
	start  <- xmlGetWidget xml castToButton "start_button"
	start `onClicked` do
		canStart <- readIORef isRunning
		if canStart /= 0 then return ()
			else do
		writeIORef  isRunning 1
		writeIORef canStop True
		timeoutAddFull (do 
			continue <- doSteps
			iok <- readIORef canStop
			return (continue && iok)) 
			priorityDefaultIdle 500
		return ()
				
		
	widgetShowAll window
	mainGUI
	
-- :: --
	
selectParents :: IORef [[Int]] -> IO [Int]
selectParents presentPop = do
	present <- readIORef presentPop
	let vs = eval present;vs' = (linear . possitive) vs;vs'' = rulet vs'
	r1 <- randomRIO (0,1)
	r2 <- randomRIO (0,1)
	let p1 = findIndex (>r1) vs'';p2 = findIndex (>r2) vs'';len = length vs''-1
	case p1 of
		Nothing -> do case p2 of
							Nothing -> return [len , len]
							_ -> return [len, fromJust p2]
		_ -> do case p2 of
					Nothing -> return [fromJust p1,len]
					_ -> return [fromJust p1, fromJust p2]

linear :: [Double] -> [Double]
linear fs = let s = sum fs
                favg = s / fromIntegral (length fs)
            in [f + favg | f <- fs]
rulet :: [Double] -> [Double]
rulet fs = let s = sum fs
               fs' = [f/s | f <- fs] 
           in rul fs' 

rul :: [Double] -> [Double]
rul (x:xs) = ls
            where ls = x : zipWith (+) ls xs

possitive :: [Double] -> [Double]
possitive fs = let 
                    m = minimum fs
               in [f- m | f <- fs]  

getCrossover :: Double -> IO Bool
getCrossover pc = do
	r <- randomRIO (0,1)
	if r < pc then return True
		else return False


getCrossoverPoint :: Int -> IO Int
getCrossoverPoint clen = do
	r <- randomRIO (0::Int, clen - 1 :: Int)
	return r

crossOver :: Int -> IORef [[Int]] -> IO [[Int]]
crossOver pos parents = do
	[p1,p2] <- readIORef parents
	let p1x = take pos p1;p1y = drop pos p1;p2x = take pos p2;p2y = drop pos p2;c1 = p2x ++ p1y;c2 = p1x ++ p2y
	return [c1,c2]

mutatedFromTo :: IORef [[Int]] -> IORef [[Int]] -> IO ([Int],[Int])
mutatedFromTo newoffspring mutatedoffspring = do
	[nx,ny] <- readIORef newoffspring
	bx <- getMutatedBits nx
	by <- getMutatedBits ny
	mx <- reBits nx bx
	my <- reBits ny by
	writeIORef mutatedoffspring [mx,my]
	return (bx,by)

getMutatedBits :: [Int] -> IO [Int]
getMutatedBits ns = do
	let l = length ns
	g <- randomIO
	let rs =  randomRs (0,1) (mkStdGen g );rs' = (take l . nub)  rs
	let bs = findIndices (< pm) rs'
	return bs
	
reBits :: [Int] -> [Int] -> IO [Int]
reBits cs poss = sequence [if (elem i poss)  then return (1 - (cs !! i)) else return (cs !! i) | i <- [0.. (length cs -1)]]

addChromToPop :: [Int] -> IORef [[Int]] -> IO ()
addChromToPop chrom pop = modifyIORef pop (\ps -> ps ++ [chrom])
	
copyPopFromTo :: IORef [[Int]] -> IORef [[Int]] -> IO ()
copyPopFromTo from to = do
	f <- readIORef from
	writeIORef to f

initPopulation :: IORef [[Int]] -> IO ()
initPopulation pop = do
	p <- genPop popSize chromLen
	p' <- sortPop p
	writeIORef pop p'


genPop :: Int -> Int -> IO [[Int]]
genPop s l = sequence [genChrom l | i<- [1..s]]

-- generate gen with given length
-- maybe like that
genChrom :: Int -> IO [Int]
genChrom l =  do      
                  g  <- getStdRandom (randomR (0,maxBound))   
		  let 
                   rs = randomRs (0,1) (mkStdGen g)                    
                   xs = take l rs
		  return xs

sortByFits :: [([Int],Double)] -> [([Int],Double)]
sortByFits [] = []
sortByFits ((c,v):cs) = let lf = [(cm,x) | (cm,x) <- cs, x > v]
                            rt = [(cm,y) | (cm,y) <- cs, y <= v]
                           in sortByFits lf ++ [(c,v)] ++ sortByFits rt
                           
sortPop :: [[Int]] -> IO [[Int]]
sortPop pops = do
	let vs = eval pops; zs = zip pops vs
	let ps = sortByFits zs; pop = [c | (c,v) <- ps]
	return pop 
	
eval :: [[Int]] -> [Double]
eval pop = [eva c | c <- pop]

eva :: [Int] -> Double
eva c = let 
            s = decode doms c
        in f s 

decode :: (Double,Double) -> [Int] -> Double
decode (a,b) c = a + fromIntegral (demical c) * (b-a) / mEpsilon
                
demical :: [Int] -> Integer
demical cs = dem cs 0

dem :: [Int] -> Integer -> Integer
dem [] res = res
dem (x:xs) res = dem xs (res*2 + fromIntegral x)	


drawRec drawable blackGC False plot_position plot_width plot_height = do
	let x = fst plot_position; y = snd plot_position
	drawRectangle drawable blackGC False x y plot_width plot_height
     
formatNumber x = (printf "%.2f" x) :: String

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
		            
