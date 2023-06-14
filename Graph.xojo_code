#tag Class
Protected Class Graph
Inherits Canvas
	#tag Event
		Sub Close()
		  // This event handler makes sure that everything gets closed down
		  // when the control closes. (I found that if one does not set
		  // the pointers to defined objects to nil before quitting, the 
		  // objects pile up in memory when one does trial runs while
		  // debugging.)
		  
		  GXP = Nil
		  GX = Nil
		  Close        // Call the user close method for any additions...
		  
		End Sub
	#tag EndEvent

	#tag Event
		Sub Open()
		  // This event handler sets up default values for important properties
		  // so that all methods have something meaningful to work with.
		  // Post a ScrollQuery event to get pointers to the graph's
		  // scroll bars (if there are any).
		  
		  SBV = ScrollQueryV
		  SBH = ScrollQueryH
		  
		  // Initialize the operational size of the frame to correspond to the
		  // size of the control if we have no scroll bars, or the maximum
		  // possible dimensions (as supplied by the user responding to
		  // HeightQuery and WidthQuery events) if we do. If the user fails
		  // to supply frame size information, the size of the frame will be
		  // set to the size of the control.
		  
		  if SBV <> nil then
		    FHeight = HeightQuery
		    if FHeight = 0 then
		      FHeight = Height
		    end if
		  else
		    FHeight = Height
		  end if
		  
		  if SBH <> nil then
		    FWidth = WidthQuery
		    if FWidth = 0 then
		      FWidth = Width
		    end if
		  else
		    FWidth = Width
		  end if
		  
		  // Initialize the graph picture. If we cannot open the picture, we cannot
		  // go on, and so must quit. (The problem is likely to be insufficient
		  // memory allocated to the application.)
		  
		  GXP = New Picture(FWidth, FHeight)
		  if GXP <> nil then
		    GX = GXP.Graphics   // Initialize the target for all drawing
		    if GX <> nil then
		      // Adjust the scrollbars to reflect any difference between the
		      // size of the control and the size of the graph.
		      
		      ScPixH = 0
		      ScPixV = 0
		      zSBVAdjust
		      zSBHAdjust
		      
		      // Initialize various constants
		      
		      InvLog10 = 1/log(10)
		      CBlack = Color.RGB(0,0,0)
		      CWhite = Color.RGB(255,255,255)
		      CGrid = Color.RGB(180,180,180)
		      
		      // Initialize other properties
		      
		      RErr = false
		      Printing = false
		      HasGrid = false
		      NoGraph = true
		      PrevCurvH = 0
		      PrevCurvV = 0
		      GHeight = 0
		      GLeft = 0
		      GTop = 0
		      GWidth = 0
		      
		      // Set the default font to plain 12-point System, and the
		      // default color to black.
		      
		      SetFontInfo("System", 12, false, false)
		      SetColor(CBlack)
		      SetTickFontInfo("System", 9)
		      
		      // Initialize the axes.
		      
		      SetXColor(CBlack)
		      SetYColor(CBlack)
		      XLabel = ""
		      XLabelH = 0
		      XLabelV = 0
		      YLabel = ""
		      YLabelH = 0
		      YLabelV = 0
		      XFlags = 0
		      YFlags = 0
		      SetXAdjust(true)
		      SetYAdjust(true)
		      SetXMajTix(true)
		      SetYMajTix(true)
		      SetXMinTix(true)
		      SetYMinTix(true)
		      SetXTixPix(7)
		      SetYTixPix(7)
		      SetXZeroLn(false)
		      SetYZeroLn(false)
		      XMax = 0
		      XMin = 0
		      YMax = 0
		      YMin = 0
		      XTickN = 0
		      YTickN = 0
		      XTPower = 0
		      YTPower = 0
		      XVToPix = 0
		      YVToPix = 0
		      
		      // Set the title.
		      
		      Title = "Graph of " + MainWindow.PMGraphMain.RowValueAt(MainWindow.PMGraphMain.SelectedRowIndex) + " vs time"
		      
		      // Clear the frame area.
		      
		      ClearFrame
		      
		      // Then call the user's Open command for further instructions
		      // or overrides to the previous initializations.
		      
		      Open
		      
		      // Make sure that screen reflects any changes made.
		      
		      me.Invalidate
		      
		    end if      // GX initialization test
		  end if        // GXP initialization test
		End Sub
	#tag EndEvent

	#tag Event
		Sub Paint(g As Graphics, areas() As REALbasic.Rect)
		  
		  // This handles paint requests by simply redrawing the stored picture.
		  
		  g.DrawPicture GXP, 0, 0, Width, Height, ScPixH, ScPixV, Width, Height
		  
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub ClearFrame()
		  //changing
		  // This method clears everything in the frame area to the color in CWhite.
		  // Note that the existing foreground color is preserved and restored.
		  
		  Var oldColor as color
		  oldColor = GX.DrawingColor
		  SetColor(CWhite)
		  GX.FillRectangle( 0, 0, GXP.Width, GXP.Height )
		  SetColor(oldColor)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CurveStart(x as double, y as double)
		  
		  // This method starts creation of a "curve" by converting the
		  // supplied real values to coordinates and saving them for
		  // future reference to the "CurveTo" method.
		  
		  PrevCurvH = XCoord(x)
		  PrevCurvV = YCoord(y)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CurveTo(x as double, y as double)
		  
		  // This method is meant for quickly drawing curves. 
		  // This is a very simple and fast method meant
		  // to draw short line segments between points to construct a
		  // plotted "curve". It does not draw the segment if either end
		  // lies outside the inner graph rectangle, but does not do more
		  // complicated error checking or truncation.
		  // Call CurveStart to initialize the curve, and then CurveTo
		  // to draw a segment from the previous point to the current point.
		  
		  Var XNow, YNow as integer
		  
		  XNow = XCoord(x)
		  YNow = YCoord(y)
		  
		  
		  GX.DrawLine PrevCurvH, PrevCurvV, XNow, YNow
		  
		  PrevCurvH = XNow
		  PrevCurvV = YNow
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DefineGraph(x1 as double, x2 as double, y1 as double, y2 as double)
		  
		  // This method allows for the initialization and drawing of a normal
		  // graph in one easy step. The method assumes that you have done all
		  // that you want to do to set up all the axes. This method defines
		  // the graph rectangle according to the axes' properties (leaving
		  // appropriate space for the title and axis and tick labels, initializes
		  // the axes (defining the conversion between axis values and pixels),
		  // draws all the labels and then draws the axes. This method
		  // returns a GraphError if the graph turns out to be too small.
		  
		  Var oldColor as color  // storage for current color
		  
		  // Calculate the size of the graph rectangle
		  
		  zDefineGRect(x1, x2, y1, y2)
		  
		  // Draw everything and update the screen.
		  
		  ClearFrame
		  oldColor = GX.DrawingColor
		  SetColor(CBlack)
		  GX.DrawRectangle(GLeft, GTop, GWidth+1, GHeight+1)
		  zDrawLabels
		  zDrawXAxis
		  zDrawYAxis
		  SetColor(oldColor)
		  me.Invalidate
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DefSqGraph(x1 as double, x2 as double, y1 as double, y2 as double)
		  
		  // This method allows for the initialization and drawing of a normal
		  // graph in one easy step. The method assumes that you have done all
		  // that you want to do to set up all the axes. This method defines
		  // the graph rectangle according to the axes' properties (leaving
		  // appropriate space for the title and axis and tick labels, initializes
		  // the axes (defining the conversion between axis values and pixels),
		  // draws all the labels and then draws the axes. This method
		  // returns a GraphError if the graph turns out to be too small.
		  
		  // This method is just like DefineGraph except that it does a further
		  // adjustment of the axis limits (if necessary) to make the value/pixel
		  // ratio the same for the horizontal and vertical axes. This is
		  // important for certain types of graphs. The parameters specify the axis
		  // limits for the axis with the smallest pixel length.
		  
		  Var a, CMax, CMin as double
		  Var newLength, adj as integer
		  Var oldColor as color
		  
		  // Calculate the size of the graph rectangle
		  
		  zDefineGRect(x1, x2, y1, y2)
		  
		  // The axis which has the larger VToPix has more pixels per unit value
		  // than the other. This means that we can (1) shrink that axis, or (2)
		  // increase its range, or both, to get the same pixel/value ratio.
		  // (Note that VToPix is misleadingly named in this context: it is the
		  // conversion factor to convert values to pixels, so it actually
		  // corresponds to the ratio pixels/val.)
		  
		  if XVToPix > YVToPix then     // x axis needs adjusting
		    
		    // We first adjust both limits outward symmetrically so that (if
		    // no further adjustments take place) makes XVToPix = YVToPix.
		    // The trick is that if we want the range to increase by a factor F, and
		    // and we want CMax and CMin to define the limits of the new range,
		    // then we want CMax - CMin = a*(XMax - XMin) but we also
		    // want CMax + CMin = XMax + XMin to keep the graph centered.
		    // Solving this for CMax and CMin yields
		    
		    a = XVToPix/YVToPix
		    CMax = 0.5*(a*(XMax - XMin) + XMax + XMin)
		    CMin = 0.5*(XMax + XMin - a*(XMax - XMin))
		    zDefineXAxis(CMin, CMax)
		    
		    // If the zDefineXAxis method is also adjusting the limits, we will also
		    // need to adjust the size of the graph. After the last adjustment,
		    // the graph limits may be too large (meaning XVToPix will be
		    // too small), so we back off the limits in steps of 6% of the 
		    // range until we again find that XVToPix is too large. CMax and
		    // CMin store the "current" guesses for the appropriate XMax and
		    // XMin quantities. 
		    
		    if BitwiseAnd(XFlags,32) <> 0 then
		      CMax = XMax
		      CMin = XMin
		      do until XVToPix >= YVToPix
		        CMax = CMax - 0.03*(CMax - CMin)
		        CMin = CMin + 0.03*(CMax - CMin)
		        zDefineXAxis(CMin, CMax)
		      loop
		    end if
		    
		    // Now compute the new (smaller) length of the axis required to
		    // make XVToPix equal to YVToPix and set the graph to that size.
		    // Note that we directly set the XVToPix property so that
		    // zDefineXAxis does not have the opportunity to change anything further.
		    
		    newLength = YVToPix*(XMax-XMin)
		    adj = Round(0.5*(GWidth - newLength))
		    GLeft = GLeft+adj
		    GWidth = newLength
		    XVToPix = YVToPix
		    
		  else  // it is the y axis that needs adjusting, so do everything
		    // above but with the x and y axes reversed.
		    
		    a = XVToPix/YVToPix
		    CMax = 0.5*(a*(YMax - YMin) + YMax + YMin)
		    CMin = 0.5*(YMax + YMin - a*(YMax - YMin))
		    zDefineYAxis(CMin, CMax)
		    
		    if BitwiseAnd(YFlags,32) <> 0 then
		      CMax = YMax
		      CMin = YMin
		      do until YVToPix >= XVToPix
		        CMax = CMax - 0.03*(CMax - CMin)
		        CMin = CMin + 0.03*(CMax - CMin)
		        zDefineYAxis(CMin, CMax)
		      loop
		    end if
		    
		    newLength = XVToPix*(YMax-YMin)
		    adj = Round(0.5*(GHeight - newLength))
		    GTop = GTop+adj
		    GHeight = newLength
		    YVToPix = XVToPix
		    
		  end if
		  
		  // Draw everything and update the screen.
		  
		  ClearFrame
		  oldColor = GX.DrawingColor
		  SetColor(CBlack)
		  GX.DrawRectangle(GLeft, GTop, GWidth, GHeight)
		  zDrawLabels
		  zDrawXAxis
		  zDrawYAxis
		  SetColor(oldColor)
		  me.Invalidate
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawLine(x1 as double, y1 as double, x2 as double, y2 as double)
		  
		  // This draws a simple line from the point specified by the first pair of
		  // real values to the point by the second pair of real values.
		  // Errors accumulate in RErr. The line would be incorrect if specified
		  // points lie outside the graph, so it is not drawn.
		  
		  Var h1, h2, v1, v2 as integer
		  
		  h1 = XCoord(x1)
		  h2 = XCoord(x2)
		  v1 = YCoord(y1)
		  v2 = YCoord(y2)
		  
		  GX.DrawLine h1, v1, h2, v2
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawPoint(XVal as double, YVal as double)
		  
		  // This method draws a circle at the point coordinates supplied.
		  // If the point lies outside the graphics range, it is not drawn, and
		  // the error flag is set and an error message is returned.
		  
		  Var x, y as integer
		  x = XCoord(XVal)
		  y = YCoord(YVal)
		  
		  GX.FillOval x-2, y-2, 5, 5
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawSString(theSS as string, x as integer, y as integer)
		  
		  // This provides a version of the DrawString command that
		  // does not require a justification flag. The displayed string
		  // will be left-justified at x, and y specifies the vertical
		  // position of the first line's baseline.
		  
		  DrawSString(theSS, x, y, 0)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawSString(theSS as string, x as integer, y as integer, just as integer)
		  
		  // This method draws a "styled" string, which uses a special code
		  // to indicate superscripts, subscripts, font changes, style
		  // changes, line breaks, and so on. The code is as follows:
		  //
		  //    ^ : turn on superscripting (next ^ or space turns off)
		  //    _ : turn on subscripting (next _ or space turns off)
		  //    $ : switch to symbol font (next $ or space restores prev font)
		  //    ! : toggle boldface
		  //    \ : toggle italics
		  //    | : start new line (also see below)
		  //    ~ : 1-point space
		  //    @ : begin font name (second @ ends name and executes change)
		  //    # : begin font size (second # ends size and executes change)
		  //    & : the next character should not be interpreted as a command
		  //        (in case one needs to display one of the command characters)
		  //
		  //   This method will utilize a data block placed at the beginning of
		  // the string by the SetSString method. This block begins with a period
		  // and consists of a set of 8 or 16-bit binary numbers (the 16-bit
		  // numbers are assembled from the Ascii values of two successive
		  // characters). The numbers specify:
		  //
		  //    1st: number of lines (8 bits)
		  //    2nd: line height (8 bits)
		  //    3rd: maximum line width (16 bits)
		  //    4th (and following): width of each line (16 bits)
		  //
		  // Don't set up this information directly yourself: send the SString
		  // through SetSString to set up the block in the correct format.
		  //
		  //   If this block is not present, the method will draw the string
		  // anyway (indeed, it will draw a plain string as DrawString would),
		  // but the line height will be taken to be the value of GX.TextHeight
		  // when the method is called, the h,v coordinates will specify the
		  // leftmost point on the baseline of the first line, and all subsequent
		  // lines will begin at the same horizontal position. The justification
		  // flag will be ignored in this case.
		  //
		  //   The justification flag, which is an integer between 1 and 9,
		  // specifies one of nine possible points on an imaginary rectangle
		  // that encloses the displayed string. The points are arranged in
		  // analogy to the positions of the corresponding digits on a
		  // a numerical entry pad: 1 corresponds to the lower left corner,
		  // 5 to the center, and so on. The string will be drawn so that
		  // the specified point is at the position x,y. If this flag not a
		  // digit between 1 and 9 or is not present, the string will be drawn
		  // so that x,y corresponds to the leftmost point on the baseline of
		  // the first line and the string will be left-justified.
		  
		  //   In all other respects, this method behaves like DrawString.
		  // Note that it also restores whatever font or style settings you had
		  // established before calling the method.
		  
		  dim i, j, k, n, shift, oldSize, currx, curry, cSize as integer
		  dim achr, asup, asub, asym, aspc, atbf, aita as integer
		  dim anwl, afnm, afsz, ainc, assp as integer
		  dim lineH, nLines, ssWidth, justh, lw(0) as integer
		  dim oldFont, cFont, prev as string
		  dim symb, fnmode, fsmode as boolean
		  dim cBold, cItal, oldBface, oldIface as boolean
		  
		  if len(theSS) = 0 then
		    return
		  end if
		  if GX <> nil then
		    
		    // If we get here, we are set to actually do some drawing.
		    // Save old font info for later restoration.
		    
		    oldFont = GX.TextFont
		    oldSize = GX.TextSize
		    oldBface = GX.Bold
		    oldIface = GX.Italic
		    
		    // Set up current font information and default size information
		    
		    cFont = oldFont
		    cSize = oldSize
		    cBold = oldBface
		    cItal = oldIface
		    lineH = GX.TextHeight
		    nLines = 0         // signifies that we don't know how many lines
		    lw(0) = 0
		    
		    // Since string comparisons are slow, set up some Ascii constants
		    // for the special symbols to speed the main loop.
		    
		    asup = Asc("^")   // superscript indicator
		    asub = Asc("_")   // subscript indicator
		    asym = Asc("$")   // symbol font indicator
		    aspc = Asc(" ")   // space
		    assp = Asc("~")   // 1-point space indicator
		    atbf = Asc("!")   // toggle boldface indicator
		    aita = Asc("\")   // toggle italics indicator
		    anwl = Asc("|")   // begin new line indicator
		    afnm = Asc("@")   // begin/close font name indicator
		    afsz = Asc("#")   // begin/close font size indicator
		    ainc = Asc("&")   // ignore next character indicator
		    
		    // Initialize some other basic variables
		    
		    n = Len(theSS)    // length of text 
		    symb = false      // symbol mode flag
		    fnmode = false    // name mode flag
		    fsmode = false    // size mode flag
		    shift = 0         // indicates baseline shift for super/subscripts
		    i = 1             // index of the current character
		    j = 1             // index of the character following last command
		    k = 0             // index to the line array
		    currx = x         // default starting positions
		    curry = y
		    justh = 0         // default horizontal justification
		    
		    // Look for the initial data block and decode it if found
		    
		    if Mid(theSS,1,1) = "." then
		      nLines = Asc(Mid(theSS,2,1))
		      lineH = Asc(Mid(theSS,3,1))
		      ssWidth = Asc(Mid(theSS,4,1))*256 + Asc(Mid(theSS,5,1))
		      if nLines < 1 or nLines*2 + 5 > n then
		        lineH = GX.TextHeight  // data block is screwed up:
		        ssWidth = 0            //   restore original data and
		        nLines = 0             //   ignore the data block
		      else
		        redim lw(nLines-1)
		        for i = 0 to nLines-1
		          j = 6 + 2*i
		          lw(i) = Asc(Mid(theSS,j,1))*256 + Asc(Mid(theSS,j+1,1))
		        next
		        i = 6 + 2*nLines      // point to the first character
		        j = i                 //   beyond the datablock
		      end if
		    end if
		    
		    // Set up starting positions according to the justification
		    
		    if just = 1 then
		      curry = y - lineH*(nLines-1)
		    elseif just = 2 then
		      currx = x - Round(lw(0)/2)
		      curry = y - lineH*(nLines-1)
		      justh = 2
		    elseif just = 3 then
		      currx = x - lw(0)
		      curry = y - lineH*(nLines-1)
		      justh = 3
		    elseif just = 4 then
		      curry = y - Round(lineH*(nLines-2)/2) - 2
		    elseif just = 5 then
		      currx = x - Round(lw(0)/2)
		      curry = y - Round(lineH*(nLines-2)/2) - 2
		      justh = 2
		    elseif just = 6 then
		      currx = x - lw(0)
		      curry = y - Round(lineH*(nLines-2)/2) - 2
		      justh = 3
		    elseif just = 7 then
		      curry = y + lineH - 2
		    elseif just = 8 then
		      currx = x - Round(lw(0)/2)
		      curry = y + lineH - 2
		      justh = 2
		    elseif just = 9 then
		      currx = x - lw(0)
		      curry = y + lineH - 2
		      justh = 3
		    end if
		    
		    // This loop scans the string character by character, looking for
		    // the command characters. When it finds a command character,
		    // the first thing that is generally done is to draw out the characters
		    // following the previous command character. Thus almost every case
		    // begins with the sequence:
		    //
		    //  if i > j then
		    //    prev = Mid(theSS, j, i-j)
		    //    GX.DrawString prev, currx, curry
		    //    currx = currx + GX.StringWidth(prev)
		    //  end if
		    //
		    // This says that if the currently-detected command character
		    // (at index i) is not the first character after the previous
		    // command (which is at index j) then we draw the string between
		    // command characters, starting at the current x and y position.
		    // We then update the current x-position according to the
		    // width of the string just drawn.
		    //    Otherwise, the code handling each case simply updates the
		    // variables controlling how the string is to be displayed, and then
		    // sets i and j to point to the character following the command.
		    // (In the case of the font name and font size commands, the
		    // code directly skips to the end of the name or size without
		    // going through the main loop.)
		    
		    do until i > n
		      achr = Asc(Mid(theSS, i, 1))
		      
		      // Handle superscript command:
		      
		      if achr = asup then
		        if i > j then               // draw previous characters
		          prev = Mid(theSS, j, i-j)
		          GX.DrawString prev, currx, curry
		          currx = currx + GX.StringWidth(prev)
		        end if
		        if shift = 0 then           // if no shift currently active
		          GX.TextSize = Round(cSize*0.72)    // reduce font size
		          shift = Round(cSize*0.3)  //   set an upward shift
		          curry = curry - shift     //   update current y to reflect this
		          if cItal then             // if italics, we need a bit of a
		            currx = currx + Round(cSize*0.2) // horizontal shift too
		          end if
		        else                        // otherwise...
		          curry = curry + shift     //   undo previous shift
		          shift = 0                 //   clear shift flag
		          GX.TextSize = cSize       //   restore previous font size
		        end if
		        i = i+1                     // point to the next character
		        j = i                       // start new segment
		        
		        // Handle the subscript command:
		        
		      elseif achr = asub then
		        if i > j then               // draw any previous characters
		          prev = Mid(theSS, j, i-j)
		          GX.DrawString prev, currx, curry
		          currx = currx + GX.StringWidth(prev)
		        end if
		        if shift = 0 then           // if no shift currently active
		          GX.TextSize = Round(cSize*0.72)   // reduce font size
		          shift = -Round(cSize*0.3) //   set a downward shift
		          curry = curry - shift     //   update current y to reflect this
		          if cItal then             // if italics, we need a bit of
		            currx = currx - Round(cSize*0.2)  // horizontal shift as well
		          end if
		        else                        // if we already are shifted
		          curry = curry + shift     //   undo the previous shift
		          shift = 0                 //   clear shift flag
		          GX.TextSize = cSize       //   restore previous font size
		        end if
		        i = i+1                     // point to the next character 
		        j = i                       // start new segment
		        
		        // Handle space (which turns off some commands):
		        
		      elseif achr = aspc and (shift <> 0 or symb) then
		        if i > j then               // draw any previous characters
		          prev = Mid(theSS, j, i-j)
		          GX.DrawString prev, currx, curry
		          currx = currx + GX.StringWidth(prev)
		        end if
		        if shift <> 0 then          // if we have a nonzero shift
		          curry = curry + shift     //   undo the previous shift
		          shift = 0                 //   clear the shift flag
		          GX.TextSize = cSize       //   restore the previous font size
		        end if
		        if symb then                // if we are in symbol mode
		          symb = false              //   clear the symbol flag
		          GX.TextFont = cFont       //   restore the previous font
		        end if
		        j = i                       // the space starts new segment
		        i = i+1                     // point to the next character
		        
		        // Handle a small space:
		        
		      elseif achr = assp then
		        if i > j then               // draw any previous characters
		          prev = Mid(theSS, j, i-j)
		          GX.DrawString prev, currx, curry
		          currx = currx + GX.StringWidth(prev)
		        end if
		        currx = currx + 1
		        i = i+1                     // point to the next character 
		        j = i                       // start new segment
		        
		        // Handle the symbol font command:
		        
		      elseif achr = asym then
		        if i > j then               // draw any previous characters
		          prev = Mid(theSS, j, i-j)
		          GX.DrawString prev, currx, curry
		          currx = currx + GX.StringWidth(prev)
		        end if
		        if symb then                // if the symbol flag is setÉ
		          GX.TextFont = cFont       //   restore the previous font
		          symb = false              //   and clear the symbol flag
		        else                        // if it is not set,
		          symb = true               //   set it to true
		          GX.TextFont = "Symbol"    //   and set the font to "Symbol"
		        end if
		        i = i+1                     // point to the next character
		        j = i                       // start a new segment
		        
		        // Handle the boldface command:
		        
		      elseif achr = atbf then
		        if i > j then               // draw any previous characters
		          prev = Mid(theSS, j, i-j)
		          GX.DrawString prev, currx, curry
		          currx = currx + GX.StringWidth(prev)
		        end if
		        cBold = not cBold           // toggle the bold flag
		        GX.Bold = cBold             // set the output to agree
		        i = i+1                     // point to the next character
		        j = i                       // start a new segment
		        
		        // Handle the italics command:
		        
		      elseif achr = aita then
		        if i > j then               // draw any previous characters
		          prev = Mid(theSS, j, i-j)
		          GX.DrawString prev, currx, curry
		          currx = currx + GX.StringWidth(prev)
		        end if
		        cItal = not cItal          // toggle the italics flag
		        GX.Italic = cItal          // set the target to agree
		        i = i+1                    // point to the next character
		        j = i                      // start a new segment
		        
		        // Handle a new line command:
		        
		      elseif achr = anwl then
		        if i > j then              // draw any previous characters
		          prev = Mid(theSS, j, i-j)
		          GX.DrawString prev, currx, curry
		        end if
		        curry = curry + lineH      // move the y-position down
		        k = k + 1                  // update line index
		        if justh = 2 then          // define initial x-position 
		          currx = x - Round(lw(k)/2)
		        elseif justh = 3 then
		          currx = x - lw(k)
		        else
		          currx = x
		        end if
		        i = i+1                    // point to the next character
		        j = i                      // start a new segment
		        
		        // Handle the font name command:
		        
		      elseif achr = afnm then
		        if i > j then              // draw any previous characters
		          prev = Mid(theSS, j, i-j)
		          GX.DrawString prev, currx, curry
		          currx = currx + GX.StringWidth(prev)
		        end if
		        i = i+1                    // point to the next character
		        j = i                      // start a new segment
		        fnmode = true              // set name mode flag
		        do until i > n or not fnmode   // scan to end of the name
		          achr = Asc(Mid(theSS, i, 1))
		          if achr = afnm then          // when we find the name end
		            fnmode = false             // clear the name mode flag
		            cFont = Mid(theSS, j, i-j)  // record the new font name
		            GX.TextFont = cFont    // and set the target to agree
		          end if
		          i = i+1                  // point to the next character
		        loop                       // end of name-scan loop
		        j = i                      // start a new segment
		        
		        //  Handle the font size command:
		        
		      elseif achr = afsz then  
		        if i > j then              // draw any previous characters
		          prev = Mid(theSS, j, i-j)
		          GX.DrawString prev, currx, curry
		          currx = currx + GX.StringWidth(prev)
		        end if
		        i = i+1                    // point to the next character
		        j = i                      // start a new segment
		        fsmode = true              // set the size mode flag
		        do until i > n or not fsmode   // scan to end of the size
		          achr = Asc(Mid(theSS, i, 1))
		          if achr = afsz then          // when we find the size end
		            fsmode = false             // clear the size mode flag
		            cSize = Round(Val(Mid(theSS, j, i-j)))   // record the size
		            GX.TextSize = cSize    // and set the target to agree
		          end if
		          i = i+1                  // point to the next character
		        loop                       // end of size-scan loop
		        j = i                      // start a new segment
		        
		        // Handle ignore next character command:
		        
		      elseif achr = ainc then
		        if i > j then              // draw any previous characters
		          prev = Mid(theSS, j, i-j)
		          GX.DrawString prev, currx, curry
		          currx = currx + GX.StringWidth(prev)
		        end if
		        j = i+1                   // start a new segment past the "&"
		        i = i+2                   // skip over the "&" and what follows
		        
		      else         // if the character was not a command,
		        i = i+1    //   simply point to the next
		      end if
		    loop           // end of main scanning loop
		    
		    // Draw any characters left over
		    
		    if n >= j and not fnmode and not fsmode then
		      prev = Mid(theSS, j, n-j+1)
		      GX.DrawString prev, currx, curry
		    end if
		    
		    // Restore the graphics target to its original font state
		    
		    GX.TextFont = oldFont
		    GX.TextSize = oldSize
		    GX.Bold = oldBface
		    GX.Italic = oldIface
		    
		  end if  // end non-nil GX test
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawXUBar(x as double, y as double, up as double, um as double)
		  
		  Var xcp, ycp, xm, xp as integer
		  Var mBad, pBad, OldRErr as boolean
		  
		  // This method attaches a horizontal error bar to the point x, y
		  // that spans an uncertainty range of up on the positive side and
		  // um on the negative side (under normal conditions, um = up)
		  // This method does not draw the point itself. It returns an error
		  // if the bar is outside the graph rectangle or needs to be truncated.
		  
		  oldRErr = RErr
		  mBad = false
		  pBad = false
		  
		  xcp = XCoord(x)  // get the coordinates of the center point
		  RErr = false     // ignore range error if x is out of range
		  ycp = YCoord(y)  // but if y is out of range, there is no bar
		  
		  if not RErr then
		    
		    // In this part, now that we know we have a possible error bar,
		    // we will check that the negative end of the bar is in range.
		    // If it is not, we will set the appropriate error flags;
		    // otherwise, we will draw the little mark at the bar's end.
		    
		    xm = XCoord(x-um)
		    if RErr then
		      mBad = true
		    else
		      GX.DrawLine xm, ycp+3, xm, ycp-3
		    end if
		    
		    // Now we do the same thing for the positive end of the bar.
		    
		    RErr = false
		    xp = XCoord(x+up)
		    if RErr then
		      pBad = true
		    else
		      GX.DrawLine xp, ycp+3, xp, ycp-3
		    end if
		    
		    // If both ends have been truncated and the bar does not span
		    // the entire graphics range, then there is no bar. Otherwise,
		    // we can draw the bar itself.
		    
		    if not (mBad and pBad and (xp = xm)) then
		      GX.DrawLine xm, ycp, xp, ycp
		    end if
		  end if  // range check of center point
		  
		  RErr = oldRErr or RErr or mBad or pBad  // set the range error flag
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawYUBar(x as double, y as double, up as double, um as double)
		  
		  Var xcp, ycp, ym, yp as integer
		  Var oldRErr, mBad, pBad as boolean
		  
		  // This method attaches a vertical error bar to the point x, y
		  // that spans an uncertainty range of up on the positive side and
		  // um on the negative side (under normal conditions, um = up)
		  // This method does not draw the point itself. It returns an error
		  // if the bar is outside the graph rectangle or needs to be truncated.
		  
		  oldRErr = RErr
		  mBad = false
		  pBad = false
		  
		  ycp = YCoord(y) // get the coordinates of the center point
		  RErr = false      // ignore error if y is out of range
		  xcp = XCoord(x) // but if x is out of range, there is no bar
		  
		  if not RErr then
		    
		    // In this part, now that we know we have a valid error bar,
		    // we will check that the negative end of the bar is in range.
		    // If it is not, we will set the appropriate error flags;
		    // otherwise, we will draw the little mark at the bar's end.
		    
		    ym = YCoord(y-um)
		    if RErr then
		      mBad = true
		    else
		      GX.DrawLine xcp+3, ym, xcp-3, ym
		    end if
		    
		    // Now we do the same thing for the positive end of the bar.
		    
		    RErr = false
		    yp = YCoord(y+up)
		    if RErr then
		      pBad = true
		    else
		      GX.DrawLine xcp+3, yp, xcp-3, yp
		    end if
		    
		    // If both ends have been truncated and the bar does not span
		    // the entire graphics range, then there is no bar. Otherwise,
		    // we can draw the bar itself.
		    
		    if not (mBad and pBad and (yp = ym)) then
		      GX.DrawLine xcp, ym, xcp, yp
		    end if
		  end if
		  
		  RErr = oldRErr or RErr or mBad or pBad   // set the range error flag
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub GetContent()
		  
		  // This method simply posts a "DrawContent" event so that the
		  // user's program can supply the content to a graph.
		  
		  DrawContent
		  me.Invalidate
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetSSHeight(theSS as string) As Integer
		  
		  // This method returns the height of the styled string specified.
		  // If there is no initial data block for this string, the method
		  // returns 0.
		  
		  if theSS.Middle(1,1) = "." then
		    return Asc(theSS.Middle(2,1))*Asc(theSS.Middle(3,1))
		  else
		    return 20
		  end if
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetSString(theSS as string) As String
		  
		  // This function strips the data block (if any) from the beginning
		  // of a styled string and returns just the basic styled string.
		  
		  Var nLines, i, n as integer
		  // This gets enough
		  
		  if theSS.Middle(1,1) <> "." then
		    return theSS
		    
		    // This part gets enough information to determine the length
		    // of the data block, and then returns the part after the block.
		    
		  else
		    nLines = Asc(theSS.Middle(2,1))
		    if nLines < 1 or nLines*2 + 6 > theSS.Length then
		      return theSS
		    else
		      i = 6 + 2*nLines
		      n = theSS.Length - i
		      return theSS.Middle(i,n)
		    end if
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetSSWidth(theSS as string) As Integer
		  
		  // This method returns the width of the styled string specified.
		  // If there is no initial data block for this string, the method
		  // returns 0.
		  
		  if theSS.Middle(1,1) = "." then
		    return Asc(theSS.Middle(4,1))*256 + Asc(theSS.Middle(5,1))
		  else
		    return 20
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub HandleHScroll(val as integer)
		  
		  // This method executes a horizontal scroll. Call this method when the
		  // value of the horizontal scroll bar changes.
		  
		  ScPixH = val
		  me.Invalidate
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub HandleResize()
		  
		  // Call this method from the Resized event handler of the window
		  // containing the graph object. This correctly handles any resizing
		  // of the control. If there is no horizontal scroll bar, the width of
		  // the graph frame is set equal to the control's width. If there is no
		  // vertical scroll bar, the height of the graph frame is set equal to
		  // the control's height. In either case, the graph is redrawn to reflect
		  // a possible change in size. If there is either a horizontal or vertical
		  // scroll bar, it is adjusted to reflect the possible change in the size
		  // of the control relative to the graph. If both are active, the screen
		  // is simply updated (instead of the graph being redrawn) because in this
		  // case, the frame size of the graph has not been changed. Make sure that
		  // the Resized method of the window containing the graph calls this method.
		  
		  if SBH <> nil then
		    zSBHAdjust
		  else
		    FWidth = Width
		  end if
		  if SBV <> nil then
		    zSBVAdjust
		  else
		    FHeight = Height
		  end if
		  if SBH = nil or SBV = nil then
		    
		    // If either dimension has no scroll-bar, then we have to reinitialize
		    // the basic graphics variables
		    
		    GXP = nil
		    GXP = New Picture(FWidth, FHeight)
		    if GXP = nil then
		      // optional -- do somthing here
		    else
		      GX = nil
		      GX = GXP.Graphics
		      if GX = nil then
		        // optional -- do something here
		      else
		        ReDrawAll
		      end if
		    end if
		  end if
		  me.Invalidate
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub HandleVScroll(val as integer)
		  
		  // This method executes a horizontal scroll. Call this method when the
		  // value of the horizontal scroll bar changes.
		  
		  ScPixV = val
		  me.Invalidate
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub PrintGraph(page as Graphics)
		  
		  // This method prints out the current graph by redrawing it to a printing
		  // page. Note that if you define any default color or font info in the
		  // Open routine for your graph, you should duplicate it here so that the
		  // printed graph looks like the graph on the screen. If you are not using
		  // any page setup information, simply pass nil as the parameter.
		  
		  Var oldGX as Graphics
		  if page <> nil then
		    oldGX = GX
		    GX = page
		    SetFontInfo("System", 12, false, false)
		    SetColor(CBlack)
		    Printing = true
		    ReDrawAll
		    Printing = false
		    GX = oldGX
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ReDrawAll()
		  
		  // This method redraws the entire graph from scratch. Call this
		  // routine after something that causes a change in the size of
		  // the graph frame (such as a window resize event), when you redefine
		  // the graph limits (but want to display the same data), if you redefine
		  // the size of labels but want to redisplay the same data, and so on.
		  // Note that this method initiates a DrawContent event, which allows the
		  // user program to redraw any data that should be displayed in the
		  // window. This method does *not* need to be called when the graph
		  // needs refreshing but no changes have been made: that task is auto-
		  // matically handled by the Paint event handler.
		  
		  if GWidth <> 0 then // won't do anything if graph is not defined
		    DefineGraph(XMin, XMax, YMin, YMax)
		    GetContent
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetColor(theColor as color)
		  
		  // This method sets the DrawingColor of the target graphics object
		  // to the specified color.
		  
		  GX.DrawingColor = theColor
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetFontInfo(fName as string, fSize as integer)
		  
		  // This method handily sets up font information for the graph.
		  // Note that if you pass a null name or zero size, the corresponding
		  // picture properties will remain what they were before. This version of
		  // the method does not affect the style information.
		  
		  if fName <> "" then
		    GX.FontName = fName
		  end if
		  if fSize <> 0 then
		    GX.FontSize = fSize
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetFontInfo(fName as string, fSize as integer, bFlag as boolean, iFlag as boolean)
		  
		  // This method handily sets up font information for the graph as a whole.
		  // Note that if you pass a null name or zero size, the corresponding
		  // picture properties will remain what they were before.
		  
		  if fName <> "" then
		    GX.FontName = fName
		  end if
		  if FSize <> 0 then
		    GX.FontSize = fSize
		  end if
		  GX.Bold = bFlag
		  GX.Italic = iFlag
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetGrid(flag as boolean)
		  
		  // This method sets the grid flag.
		  
		  hasGrid = flag
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function SetSSLineH(theSS as string, theLineH as integer) As String
		  
		  // This method sets the line height entry in the data block
		  // at the front of the styled string theSS to the specified
		  // value and returns the modified string. (If theSS doesn't
		  // have a data block, it is given one before it is returned.
		  
		  Var temp as string
		  Var n as integer
		  
		  temp = theSS
		  if theSS.Middle(1,1) <> "." then  // if it does't have a data block
		    temp = SetSString(temp)      // give it one
		  end if
		  n = temp.Length - 3              // number of characters after line height
		  return temp.Middle(1,2) + Chr(BitwiseAnd(theLineH,255)) + temp.Middle(4,n)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function SetSString(theSS as string) As String
		  
		  // This method sets up a the "styled" string, which uses a special code
		  // to indicate superscripts, subscripts, font changes, style
		  // changes, line breaks, and so on. The code is as follows:
		  //
		  //    ^ : turn on superscripting (next ^ or space turns off)
		  //    _ : turn on subscripting (next _ or space turns off)
		  //    $ : switch to symbol font (next $ or space restores prev font)
		  //    ! : toggle boldface
		  //    \ : toggle italics
		  //    | : start new line (also see below)
		  //    ~ : 1-point space
		  //    @ : begin font name (second @ ends name and executes change)
		  //    # : begin font size (second # ends size and executes change)
		  //    & : the next character should not be interpreted as a command
		  //        (in case one needs to display one of the command characters)
		  //
		  //   The purpose of this method is to compute some size information about
		  // the string and place it in a data block at the beginning of the string.
		  // This data block begins with a period and consists of a sequence of
		  // of 8 or 16-bit binary numbers stored as 8-bit characters in the
		  // string. The numbers specify:
		  //
		  //    1st: number of lines (8 bits)
		  //    2nd: line height (8 bits)
		  //    3rd: maximum line width (16 bits)
		  //    4th (and following): width of each line (16 bits)
		  //
		  // Don't ever try set up this information directly yourself: use this
		  // method instead to ensure that it is set up correctly. The method
		  // returns the complete string with the front-end data block.
		  // 
		  // The point of this data block is to make it easy to determine the
		  // rectangle that the styled string will fill when it is drawn,
		  // to make it possible to justify the string in various ways, and
		  // to establish a fixed and appropriate line height for the string.
		  // DrawSString will operate with out this data block, but it is
		  // helpful to have it for a number of reasons.
		  
		  Var i, j, k, n, shift, oldSize, currx, cSize as integer
		  Var achr, asup, asub, asym, aspc, atbf, aita as integer
		  Var anwl, afnm, afsz, ainc, assp as integer
		  Var lineH, nLines, ssWidth, lw(0) as integer
		  Var oldFont, cFont, prev, dblock as string
		  Var symb, fnmode, fsmode, fchange as boolean
		  Var cBold, cItal, oldBface, oldIface as boolean
		  
		  if theSS.Length = 0 then  // A null string needs no data block.
		    return ""
		  end if
		  if GX <> nil then
		    
		    // Save old font info for later restoration.
		    
		    oldFont = GX.FontName
		    oldSize = GX.FontSize
		    oldBface = GX.Bold
		    oldIface = GX.Italic
		    
		    // Set up current font information and default size information
		    
		    cFont = oldFont
		    cSize = oldSize
		    cBold = oldBface
		    cItal = oldIface
		    ssWidth = 0
		    lineH = 0
		    fchange = true
		    
		    // Since string comparisons are slow, set up some Ascii constants
		    // for the special symbols to speed the main loop.
		    
		    asup = Asc("^")   // superscript indicator
		    asub = Asc("_")   // subscript indicator
		    asym = Asc("$")   // symbol font indicator
		    aspc = Asc(" ")   // space
		    assp = Asc("~")   // 1-point space indicator
		    atbf = Asc("!")   // toggle boldface indicator
		    aita = Asc("\")   // toggle italics indicator
		    anwl = Asc("|")   // begin new line indicator
		    afnm = Asc("@")   // begin/close font name indicator
		    afsz = Asc("#")   // begin/close font size indicator
		    ainc = Asc("&")   // ignore next character indicator
		    
		    // Initialize some other basic variables
		    
		    n = theSS.Length    // length of text 
		    symb = false      // symbol mode flag
		    fnmode = false    // name mode flag
		    fsmode = false    // size mode flag
		    shift = 0         // indicates baseline shift for super/subscripts
		    i = 1             // index of the current character
		    j = 1             // index of the character following last command
		    k = 0             // index to the line array
		    currx = 0         // current horizontal position
		    
		    // This loop scans the string character by character, looking for
		    // the command characters. When it finds a command character,
		    // the first thing that is generally done is to measure the characters
		    // following the previous command character. Thus almost every case
		    // begins with the sequence:
		    //
		    //  if i > j then
		    //    prev = theSS.Middle(j, i-j)
		    //    currx = currx + GX.TextWidth(prev)
		    //  end if
		    //
		    // This says that if the currently-detected command character
		    // (at index i) is not the first character after the previous
		    // command (which is at index j) then we measure the string's
		    // width between the two command characters and then update
		    // the current x-position appropriately.
		    //    Otherwise, the code handling each case simply updates the
		    // variables controlling how the string is to be displayed, and then
		    // sets i and j to point to the character following the command.
		    // (In the case of the font name and font size commands, the
		    // code directly skips to the end of the name or size without
		    // going through the main loop.)
		    
		    do until i > n
		      achr = Asc(theSS.Middle( i, 1))
		      
		      // Handle superscript command:
		      
		      if achr = asup then
		        if i > j then               // measure previous characters
		          prev = theSS.Middle( j, i-j)
		          currx = currx + GX.TextWidth(prev)
		        end if
		        if shift = 0 then           // if no shift currently active
		          GX.FontSize = Round(cSize*0.72)    // reduce font size
		          shift = Round(cSize*0.3)  //   set an upward shift
		          if cItal then             // if italics, we need a bit of
		            currx = currx + Round(cSize*0.2)  // horizontal shift as well
		          end if
		        else                        // otherwise...
		          shift = 0                 //   clear shift flag
		          GX.FontSize = cSize       //   restore previous font size
		        end if
		        i = i+1                     // point to the next character
		        j = i                       // start new segment
		        
		        // Handle the subscript command:
		        
		      elseif achr = asub then
		        if i > j then               // measure previous characters
		          prev = theSS.Middle( j, i-j)
		          currx = currx + GX.TextWidth(prev)
		        end if
		        if shift = 0 then           // if no shift currently active
		          GX.FontSize = Round(cSize*0.72)   // reduce font size
		          shift = -Round(cSize*0.3) //   set a downward shift
		          if cItal then             // if italics, we need a bit of
		            currx = currx - Round(cSize*0.2)  // horizontal shift as well
		          end if
		        else                        // if we already are shifted
		          shift = 0                 //   clear shift flag
		          GX.FontSize = cSize       //   restore previous font size
		        end if
		        i = i+1                     // point to the next character 
		        j = i                       // start new segment
		        
		        // Handle space (which turns off some commands):
		        
		      elseif achr = aspc and (shift <> 0 or symb) then
		        if i > j then               // measure previous characters
		          prev = theSS.Middle( j, i-j)
		          currx = currx + GX.TextWidth(prev)
		        end if
		        if shift <> 0 then          // if we have a nonzero shift
		          shift = 0                 //   clear the shift flag
		          GX.FontSize = cSize       //   restore the previous font size
		        end if
		        if symb then                // if we are in symbol mode
		          symb = false              //   clear the symbol flag
		          GX.FontName = cFont       //   restore the previous font
		        end if
		        j = i                       // the space starts new segment
		        i = i+1                     // point to the next character
		        
		        // Handle a small space:
		        
		      elseif achr = assp then
		        if i > j then               // measure previous characters
		          prev = theSS.Middle( j, i-j)
		          currx = currx + GX.TextWidth(prev)
		        end if
		        currx = currx + 1
		        i = i+1                     // point to the next character 
		        j = i                       // start new segment
		        
		        // Handle the symbol font command:
		        
		      elseif achr = asym then
		        if i > j then               // measure previous characters
		          prev = theSS.Middle( j, i-j)
		          currx = currx + GX.TextWidth(prev)
		        end if
		        if symb then                // if the symbol flag is setÉ
		          GX.FontName = cFont       //   restore the previous font
		          symb = false              //   and clear the symbol flag
		        else                        // if it is not set,
		          symb = true               //   set it to true
		          GX.FontName = "Symbol"    //   and set the font to "Symbol"
		          fchange = true            // flag a font change
		        end if
		        i = i+1                     // point to the next character
		        j = i                       // start a new segment
		        
		        // Handle the boldface command:
		        
		      elseif achr = atbf then
		        if i > j then               // measure previous characters
		          prev = theSS.Middle( j, i-j)
		          currx = currx + GX.TextWidth(prev)
		        end if
		        cBold = not cBold           // toggle the bold flag
		        GX.Bold = cBold             // set the output to agree
		        i = i+1                     // point to the next character
		        j = i                       // start a new segment
		        
		        // Handle the italics command:
		        
		      elseif achr = aita then
		        if i > j then               // measure previous characters
		          prev = theSS.Middle( j, i-j)
		          currx = currx + GX.TextWidth(prev)
		        end if
		        cItal = not cItal          // toggle the italics flag
		        GX.Italic = cItal          // set the target to agree
		        i = i+1                    // point to the next character
		        j = i                      // start a new segment
		        
		        // Handle a new line command:
		        
		      elseif achr = anwl then
		        if i > j then              // measure previous characters
		          prev = theSS.Middle( j, i-j)
		          currx = currx + GX.TextWidth(prev)
		        end if
		        lw(k) = currx              // save the width of last line 
		        ssWidth = max(ssWidth, currx)  // update maximum width
		        currx = 0                  // reset the x-position
		        lw.Add(0)                // lengthen the line width array
		        k = k + 1                  // update line index
		        i = i+1                    // point to the next character
		        j = i                      // start a new segment
		        
		        // Handle the font name command:
		        
		      elseif achr = afnm then
		        if i > j then              // measure previous characters
		          prev = theSS.Middle( j, i-j)
		          currx = currx + GX.TextWidth(prev)
		        end if
		        i = i+1                    // point to the next character
		        j = i                      // start a new segment
		        fnmode = true              // set name mode flag
		        do until i > n or not fnmode   // scan to end of the name
		          achr = Asc(theSS.Middle( i, 1))
		          if achr = afnm then          // when we find the name end
		            fnmode = false             // clear the name mode flag
		            cFont = theSS.Middle( j, i-j)  // record the new font name
		            GX.FontName = cFont    // and set the target to agree
		            fchange = true         // flag a font change
		          end if
		          i = i+1                  // point to the next character
		        loop                       // end of name-scan loop
		        j = i                      // start a new segment
		        
		        //  Handle the font size command:
		        
		      elseif achr = afsz then  
		        if i > j then              // measure previous characters
		          prev = theSS.Middle( j, i-j)
		          currx = currx + GX.TextWidth(prev)
		        end if
		        i = i+1                    // point to the next character
		        j = i                      // start a new segment
		        fsmode = true              // set the size mode flag
		        do until i > n or not fsmode   // scan to end of the size
		          achr = Asc(theSS.Middle( i, 1))
		          if achr = afsz then          // when we find the size end
		            fsmode = false             // clear the size mode flag
		            cSize = Round(Val(theSS.Middle(j, i-j)))   // record the size
		            GX.FontSize = cSize    // and set the target to agree
		            fchange = true         // flag a font change
		          end if
		          i = i+1                  // point to the next character
		        loop                       // end of size-scan loop
		        j = i                      // start a new segment
		        
		        // Handle ignore next character command:
		        
		      elseif achr = ainc then
		        if i > j then              // measure previous characters
		          prev = theSS.Middle(j, i-j)
		          currx = currx + GX.TextWidth(prev)
		        end if
		        j = i+1                   // start a new segment past the "&"
		        i = i+2                   // skip over the "&" and what follows
		        
		      else         // if the character was not a command,
		        i = i+1    //   simply point to the next
		        if fchange then  // also update lineH info if necessary
		          lineH = max(lineH, GX.TextHeight)
		          fchange = false
		        end if
		      end if
		    loop           // end of main scanning loop
		    
		    // measure any characters left over
		    
		    if n >= j and not fnmode and not fsmode then
		      prev = theSS.Middle(j, n-j+1)
		      currx = currx + GX.TextWidth(prev)
		    end if
		    
		    // set the linewidth of the last line
		    
		    lw(k) = currx
		    ssWidth = max(ssWidth,currx)
		    
		    // set up data block
		    
		    nLines = k + 1
		    dblock = "." + Chr(BitwiseAnd(nLines,255))
		    dblock = dblock + Chr(BitwiseAnd(lineH,255))
		    dblock = dblock + Chr(BitwiseAnd(Floor(ssWidth/256),255))
		    dblock = dblock + Chr(BitwiseAnd(ssWidth,255))
		    for k = 0 to nLines-1
		      dblock = dblock + Chr(BitwiseAnd(Floor(lw(k)/256),255))
		      dblock = dblock + Chr(BitwiseAnd(lw(k),255))
		    next
		    
		    // Restore the graphics target to its original font state
		    
		    GX.FontName = oldFont
		    GX.FontSize = oldSize
		    GX.Bold = oldBface
		    GX.Italic = oldIface
		    
		    // Return the processed string. Note that we also lock in
		    // the current font and size information so that later
		    // drawing will be consistent with the font information
		    // assumed here.
		    
		    cFont = GX.FontName
		    cSize = GX.FontSize
		    return dblock + "@" + cFont + "@#" + cSize.ToString + "#" + theSS
		    
		  end if  // end non-nil GX test
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetTickFontInfo(fName as string, fSize as integer)
		  
		  // This method handily sets up font information for the axis ticks.
		  // Note that if you pass a null name or zero size, the corresponding
		  // tick properties will remain what they were before.
		  
		  if fName <> "" then
		    TickFont = fName
		  end if
		  if fSize <> 0 then
		    TickFSize = fSize
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetTitle(theTitle as string)
		  
		  // This method defines the SString for the title.
		  
		  Title = SetSString(theTitle)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetXAdjust(flag as boolean)
		  
		  // This method sets the adjustment flag for the x axis. If this flag
		  // is true (which is the default), then the axis limits will be adjusted
		  // when the graph is next defined so that the axis limits correspond to
		  // major tick values.
		  
		  // Note that the flags stored in the XFlags property are:
		  //   MinTPix          =  first 5 binary digits
		  //   Adjust Flag      =  32s place
		  //   Draw Major Ticks =  64s place
		  //   Draw Minor Ticks = 128s place
		  //   Draw a line at 0 = 256s place
		  
		  XFlags = BitwiseOr(XFlags,32)
		  if not flag then
		    XFlags = BitwiseXOr(XFlags,32)
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetXColor(theColor as color)
		  
		  // This method sets the color of the graph's x-axis
		  
		  XColor = theColor
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetXLabel(theLabel as string)
		  
		  // This method defines the label for the x axis.
		  // The string is initialized as center-justified in both axes.
		  // The font and size is set to the current font and size in GX.
		  
		  XLabel = SetSString(theLabel)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetXMajTix(flag as boolean)
		  
		  // This method sets the major ticks flag for the x axis. If this flag
		  // is true (which is the default), then the major ticks will be drawn
		  // for the axis whenever it is redrawn.
		  
		  // Note that the flags stored in the XFlags property are:
		  //   MinTPix          =  first 5 binary digits
		  //   Adjust Flag      =  32s place
		  //   Draw Major Ticks =  64s place
		  //   Draw Minor Ticks = 128s place
		  //   Draw a line at 0 = 256s place
		  
		  XFlags = BitwiseOr(XFlags,64)
		  if not flag then
		    XFlags = BitwiseXOr(XFlags,64)
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetXMinTix(flag as boolean)
		  
		  // This method sets the minor ticks flag for the x axis. If this flag
		  // is true (which is the default), then the minor ticks will be drawn
		  // for the axis whenever it is redrawn.
		  
		  // Note that the flags stored in the XFlags property are:
		  //   MinTPix          =  first 5 binary digits
		  //   Adjust Flag      =  32s place
		  //   Draw Major Ticks =  64s place
		  //   Draw Minor Ticks = 128s place
		  //   Draw a line at 0 = 256s place
		  
		  XFlags = BitwiseOr(XFlags,128)
		  if not flag then
		    XFlags = BitwiseXOr(XFlags,128)
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetXTixPix(nPix as integer)
		  
		  // This method sets the minimum number of pixels per minor tick for
		  // the x axis. The default is 7. Values are constrained to be within
		  // the range 3 to 31.
		  
		  // Note that the flags stored in the XFlags property are:
		  //   MinTPix          =  first 5 binary digits
		  //   Adjust Flag      =  32s place
		  //   Draw Major Ticks =  64s place
		  //   Draw Minor Ticks = 128s place
		  //   Draw a line at 0 = 256s place
		  
		  XFlags = BitwiseAnd(XFlags,32+64+128+256)
		  if nPix < 3 then
		    XFlags = XFlags + 3
		  elseif nPix > 31 then
		    XFlags = XFlags + 31
		  else
		    XFlags = XFlags + nPix
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetXZeroLn(flag as boolean)
		  
		  // This method sets the line-at-zero flag for the x axis. If this flag
		  // is true (which is not the default), then a black vertical line will
		  // be drawn across the graph at the horizontal position corresponding
		  // to x = 0 when the axis is drawn.
		  
		  // Note that the flags stored in the XFlags property are:
		  //   MinTPix          =  first 5 binary digits
		  //   Adjust Flag      =  32s place
		  //   Draw Major Ticks =  64s place
		  //   Draw Minor Ticks = 128s place
		  //   Draw a line at 0 = 256s place
		  
		  XFlags = BitwiseOr(XFlags,256)
		  if not flag then
		    XFlags = BitwiseXOr(XFlags,256)
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetYAdjust(flag as boolean)
		  
		  // This method sets the adjustment flag for the y axis. If this flag
		  // is true (which is the default), then the axis limits will be adjusted
		  // when the graph is next defined so that the axis limits correspond to
		  // major tick values.
		  
		  // Note that the flags stored in the YFlags property are:
		  //   MinTPix          =  first 5 binary digits
		  //   Adjust Flag      =  32s place
		  //   Draw Major Ticks =  64s place
		  //   Draw Minor Ticks = 128s place
		  //   Draw a line at 0 = 256s place
		  
		  YFlags = BitwiseOr(YFlags,32)
		  if not flag then
		    YFlags = BitwiseXor(YFlags,32)
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetYColor(theColor as color)
		  
		  // This method sets the color of the graph's y-axis
		  // to the specified color.
		  
		  YColor = theColor
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetYLabel(theLabel as string)
		  
		  // This method defines the label for the y axis.
		  // The string is initialized as center-justified in both axes.
		  // The font and size is set to the current font and size in GX.
		  
		  YLabel = SetSString(theLabel)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetYMajTix(flag as boolean)
		  
		  // This method sets the major ticks flag for the y axis. If this flag
		  // is true (which is the default), then the major ticks will be drawn
		  // for the axis whenever it is redrawn.
		  
		  // Note that the flags stored in the YFlags property are:
		  //   MinTPix          =  first 5 binary digits
		  //   Adjust Flag      =  32s place
		  //   Draw Major Ticks =  64s place
		  //   Draw Minor Ticks = 128s place
		  //   Draw a line at 0 = 256s place
		  
		  YFlags = BitwiseOr(YFlags,64)
		  if not flag then
		    YFlags = BitwiseXOr(YFlags,64)
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetYMinTix(flag as boolean)
		  
		  // This method sets the minor ticks flag for the y axis. If this flag
		  // is true (which is the default), then the minor ticks will be drawn
		  // for the axis whenever it is redrawn.
		  
		  // Note that the flags stored in the YFlags property are:
		  //   MinTPix          =  first 5 binary digits
		  //   Adjust Flag      =  32s place
		  //   Draw Major Ticks =  64s place
		  //   Draw Minor Ticks = 128s place
		  //   Draw a line at 0 = 256s place
		  
		  YFlags = BitwiseOr(YFlags,128)
		  if not flag then
		    YFlags = BitwiseXOr(YFlags,128)
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetYTixPix(nPix as integer)
		  
		  // This method sets the minimum number of pixels per minor tick for
		  // the y axis. The default is 7. Values are constrained to be within
		  // the range 3 to 31.
		  
		  // Note that the flags stored in the YFlags property are:
		  //   MinTPix          =  first 5 binary digits
		  //   Adjust Flag      =  32s place
		  //   Draw Major Ticks =  64s place
		  //   Draw Minor Ticks = 128s place
		  //   Draw a line at 0 = 256s place
		  
		  YFlags = BitwiseAnd(YFlags,32+64+128+256)
		  if nPix < 3 then
		    YFlags = YFlags + 3
		  elseif nPix > 31 then
		    YFlags = YFlags + 31
		  else
		    YFlags = YFlags + nPix
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetYZeroLn(flag as boolean)
		  
		  // This method sets the line-at-zero flag for the y axis. If this flag
		  // is true (which is not the default), then a black horizontal line will
		  // be drawn across the graph at the vertical position corresponding
		  // to y = 0 when the axis is drawn.
		  
		  // Note that the flags stored in the YFlags property are:
		  //   MinTPix          =  first 5 binary digits
		  //   Adjust Flag      =  32s place
		  //   Draw Major Ticks =  64s place
		  //   Draw Minor Ticks = 128s place
		  //   Draw a line at 0 = 256s place
		  
		  YFlags = BitwiseOr(YFlags,256)
		  if not flag then
		    YFlags = BitwiseXOr(YFlags,256)
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function XCoord(val as double) As Integer
		  // This function converts a double-precision value to a
		  // pixel coordinate. Note that it returns the pixel coordinate of
		  // the nearest edge of the axis if the value is out of range (just
		  // to return something meaningful). Note also that it will NOT erase
		  // the range error flag RErr if it is already true. This allows on
		  // to check the validity of a whole series of conversions.
		  
		  if GWidth <> 0 then  // this will do nothing if we have no graph
		    
		    if val < XMin - Abs(XMin)*0.00001 then
		      Return GLeft
		      Var e As New GraphException
		      e.RangeError
		    elseif val > XMax + Abs(XMax)*0.00001 then
		      Return GLeft + GWidth
		      Var e As New GraphException
		      e.RangeError
		    else
		      Return Round((val-XMin)*XVToPix) + GLeft
		    end if
		    
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function XToVal(coord as integer) As Double
		  
		  // This method converts the supplied pixel coordinate within
		  // the graph rectangle to the corresponding numerical value
		  // as it would be read from the axis scale. It returns zero
		  // and sets a GraphError if the graph is not defined, and
		  // returns a RErr if the value is out of range.
		  
		  Var val as double
		  
		  if GWidth <> 0  then // This method will do nothing if there is no graph
		    val = (coord - GLeft)/XVToPix + XMin
		    if val > XMin - Abs(XMin)*0.00001 and val < XMax + Abs(XMin)*0.00001 then
		      Return val
		    else
		      return 0
		      Var e As GraphException
		      Raise e
		    end if
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function YCoord(val as double) As Integer
		  
		  // This function converts a double-precision value to a
		  // pixel coordinate. Note that it returns the pixel coordinate of
		  // the nearest edge of the axis if the value is out of range (just
		  // to return something meaningful). Note also that it will NOT erase
		  // the range error flag RErr if it is already true. This allows on
		  // to check the validity of a whole series of conversions.
		  
		  if GHeight <> 0 then // This method will do nothing if there is no graph
		    if val < YMin - Abs(YMin)*0.00001 then
		      Return GTop + GHeight
		      Var e As New GraphException
		      e.RangeError
		    elseif val > YMax + Abs(YMax)*0.00001 then
		      Return GTop
		      Var e As New GraphException
		      e.RangeError
		    else
		      Return GTop + GHeight - Round((val-YMin)*YVToPix)
		    end if
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function YToVal(coord as integer) As Double
		  
		  // This method converts the supplied pixel coordinate within
		  // the graph rectangle to the corresponding numerical value
		  // as it would be read from the axis scale. It returns zero
		  // and sets a UErr if the axis is not completely defined, and
		  // returns a RErr if the value is out of range.
		  
		  Var val as double
		  
		  if GHeight = 0  then  // This method will do nothing if the graph is not defined
		    val = (GTop + GHeight - coord)/YVToPix + YMin
		    if val > YMin - Abs(YMin)*0.00001 and val < YMax + Abs(YMax)*0.00001 then
		      Return val
		    else
		      return 0
		      Var e As New GraphException
		      e.RangeError
		    end if
		  end if
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub zDefineGRect(x1 as double, x2 as double, y1 as double, y2 as double)
		  
		  // This method does the hard work of deciding how big the graphics
		  // rectangle is going to be. It examines the title, labels and tick
		  // marks and decides how much space needs to be reserved inside the
		  // frame for these items. It also defines the x and y axis.
		  
		  Var lwidth as integer  // width needed on left side for labels
		  Var rwidth as integer  // width needed on right side
		  Var tspace as integer  // space at top needed for title 
		  Var bspace as integer  // space needed at bottom for labels
		  Var hbuffer, vbuffer, vtick as integer  // see below
		  
		  // These constants specify the minimum white space between
		  // horizontal and vertical elements. 
		  
		  hbuffer = 10
		  vbuffer = 10
		  
		  // This constant specifies the vertical space to reserve for
		  // a tick label on a horizontal axis. The value here is appropriate
		  // for a tick font size of 9: you may need to change it if you
		  // change that font size
		  
		  vtick = 14
		  
		  // Reserve the space needed at the top. This part reserves
		  // white space of height vbuffer above and below the title.
		  
		  tspace = GetSSHeight(Title)
		  tspace = tspace + 2*vbuffer
		  
		  // Reserve the space needed at the bottom.
		  
		  bspace = GetSSHeight(XLabel)
		  if bspace > 0 then                  // reserve vbuffer space below
		    bspace = bspace + vbuffer         //   any nonnull label
		  end if
		  if BitwiseAnd(XFlags,192) > 0 then  // if we are showing any ticks
		    bspace = bspace + vtick           //   reserve required space
		  end if
		  bspace = bspace + vbuffer           // reserve vbuffer space for axis
		  
		  if bspace = vbuffer then            // reserve 2*vbuffer if we have           
		    bspace = 2*vbuffer                //   neither ticks nor label
		  end if
		  
		  // Now we have enough information to completely determine
		  // the vertical scales. 
		  
		  GHeight = FHeight-tspace-bspace
		  if GHeight < 100 then
		    GHeight = 0  // Indicate that we have no graph
		    GWidth = 0
		    GLeft = 0
		    GTop = 0
		  else
		    zDefineYAxis(y1, y2)
		    
		    // Set lwidth to be the widest of the labels for the y axis.
		    // If the greatest width is nonzero, we also add an hbuffer's
		    // worth of white space to separate the label from the next
		    // thing inward. If ticks are enabled, we also add the size
		    // of the maximum tick width. At the end, we also add an hbuffer
		    // of space to separate everything from the frame edge. If the 
		    // label and tick widths are zero, two hbuffers of white space will
		    // appear between the frame edge and the graph edge (looks better
		    // this way).
		    
		    lwidth = GetSSWidth(YLabel)
		    if BitwiseAnd(YFlags,192) > 0 then
		      lwidth = lwidth + zYTickWidth + 8
		    end if
		    if lwidth > 0 then
		      lwidth = lwidth + hbuffer
		    end if
		    
		    lwidth = lwidth + hbuffer
		    if lwidth = hbuffer then
		      lwidth = 2*hbuffer
		    end if
		    
		    rwidth = 2*hbuffer
		    
		    // Complete the definition of the graphics rectangle and check
		    // that it is sufficiently large.
		    
		    GLeft = lwidth
		    GTop = tspace
		    GWidth = FWidth-lwidth-rwidth
		    
		    if GWidth < 100 then  
		      GLeft = 0  // indicate that we have no graph
		      GTop = 0
		      GWidth = 0
		      GHeight = 0
		    else
		      
		      
		      // Finally, we are ready to define the x axis.
		      
		      zDefineXAxis(x1, x2)
		    end if
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub zDefineXAxis(xmn as double, xmx as double)
		  
		  // This method defines the x axis by specifying the values
		  // corresponding to its ends, deciding on a tick size,
		  // and setting various parameters needed to draw the
		  // axis and return meaningful coordinates. The parameters
		  // correspond to the minimum and maximum values to be
		  // displayed along the axis.
		  
		  Var Range, mTick, Tick, fudge, TFactor as double
		  Var MinTPix as integer
		  
		  fudge = 0.00001    // fudge factor
		  
		  // Check that we have a meaningful graph width. If not, return
		  // a GraphError so that the calling method will know.
		  
		  Range = xmx - xmn
		  if GWidth > 0 and Range > 0 then // Check that we have a graph 
		    
		    // Compute the TPower and TFactor properties. TFactor is the
		    // quantity that multiplies the tick value displayed on the graph
		    // to yield the actual tick value. This factor is always equal to
		    // 10 raised to a power that is an integer multiple of three. The
		    // power property is this integer.
		    
		    if (Range < 10000 and Range > 0.01) then
		      XTPower = 0
		      TFactor = 1
		    else
		      XTPower = 3*Floor(Log(Range)/Log(1000) + fudge)
		      TFactor = Pow(10,XTPower)
		    end if
		    
		    // Choose the major tick size. We want there to be at least MinTPix
		    // points between minor tick marks on the screen.
		    // In the case of a normal axis, we define the minor tick
		    // initially to be exactly MinTPix pixels wide on the graph,
		    // and then adjust it to the next larger tick size that is
		    // 1, 2, or 5 times some integer power of 10.
		    
		    minTPix = BitwiseAnd(XFlags,31)
		    mTick = Range*MinTPix/GWidth
		    XTickN = Ceiling(Log(mTick/TFactor)*InvLog10*3 - 0.15)
		    Tick = zTickSize(XTickN+2)
		    
		    // If the adjust flag is set, we tweak the endpoints of the
		    // range to coincide with major ticks. The 0.05s are fudge
		    // factors that make sure that even when the calculations
		    // are a tiny bit off, the rounding includes values it is
		    // meant to include.
		    
		    XMin = xmn
		    XMax = xmx
		    if BitwiseAnd(XFlags,32) > 0 then
		      XMin = Tick*Floor(XMin/(Tick*TFactor) + 0.05)*TFactor
		      XMax = Tick*Ceiling(XMax/(Tick*TFactor) - 0.05)*TFactor
		    end if
		    
		    // We now initialize the VToPix conversion factor and set
		    // enable flag after a successful initialization. This finally
		    // makes it possible for the axis to be drawn and used for
		    // translating values to coordinates.
		    
		    XVToPix = GWidth/(XMax - XMin)
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub zDefineYAxis(ymn as double, ymx as double)
		  
		  // This method defines the y axis by specifying the values
		  // corresponding to its ends, deciding on a tick size,
		  // and setting various parameters needed to draw the
		  // axis and return meaningful coordinates. The parameters
		  // correspond to the minimum and maximum values to be
		  // displayed along the axis.
		  
		  Var Range, mTick, Tick, fudge, TFactor as double
		  Var MinTPix as integer
		  
		  
		  fudge = 0.00001    // fudge factor
		  
		  // Check that we have a meaningful graph width. If not, return
		  // a GraphError so that the calling method will know.
		  
		  Range = ymx - ymn
		  if GHeight > 0 and Range > 0 then  // Check that we have a graph
		    
		    // Compute the TPower and TFactor properties. TFactor is the
		    // quantity that multiplies the tick value displayed on the graph
		    // to yield the actual tick value. This factor is always equal to
		    // 10 raised to a power that is an integer multiple of three. The
		    // power property is this integer.
		    
		    if (Range < 10000 and Range > 0.01) then
		      YTPower = 0
		      TFactor = 1
		    else
		      YTPower = 3*Floor(Log(Range)/Log(1000) + fudge)
		      TFactor = Pow(10,YTPower)
		    end if
		    
		    // Choose the major tick size. We want there to be at least MinTPix
		    // points between minor tick marks on the screen.
		    // In the case of a normal axis, we define the minor tick
		    // initially to be exactly MinTPix pixels wide on the graph,
		    // and then adjust it to the next larger tick size that is
		    // 1, 2, or 5 times some integer power of 10.
		    
		    minTPix = BitwiseAnd(YFlags,31)
		    mTick = Range*MinTPix/GHeight
		    YTickN = Ceiling(Log(mTick/TFactor)*InvLog10*3 - 0.15)
		    Tick = zTickSize(YTickN+2)
		    
		    // If the adjust flag is set, we tweak the endpoints of the
		    // range to coincide with major ticks. The 0.05s are fudge
		    // factors that make sure that even when the calculations
		    // are a tiny bit off, the rounding includes values it is
		    // meant to include.
		    
		    YMin = ymn
		    YMax = ymx
		    if BitwiseAnd(YFlags,32) > 0 then
		      YMin = Tick*Floor(YMin/(Tick*TFactor) + 0.05)*TFactor
		      YMax = Tick*Ceiling(YMax/(Tick*TFactor) - 0.05)*TFactor
		    end if
		    
		    // We now initialize the VToPix conversion factor and set
		    // enable flag after a successful initialization. This finally
		    // makes it possible for the axis to be drawn and used for
		    // translating values to coordinates.
		    
		    YVToPix = GHeight/(YMax - YMin)
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub zDrawLabels()
		  
		  dim x, y as integer
		  dim hbuffer, vbuffer, vtick, hlh as integer
		  
		  // This method draws the graph title and axis labels. This method
		  // should be called only *after* the axes have been defined by
		  // zDefineXAxis and zDefineYAxis. Note that the title and label
		  // are "styled" strings that can contain commands that effect how
		  // they are displayed. See the DrawSString method for details. If
		  // any SString is nil, it will not be drawn.
		  
		  // This section sets up constants for spacing things vertically and
		  // horizontally. These constants must agree with the ones having the
		  // same names in DefineGraph.
		  
		  hbuffer = 8
		  vbuffer = 8
		  vtick = 14
		  
		  // This section draws the title string.
		  
		  x = GLeft + GWidth/2
		  y = GTop - vbuffer
		  y = y - GetSSHeight(Title)/2
		  DrawSString(Title, x, y, 5)
		  
		  // This section draws the graph label for the bottom x-axis.
		  
		  if BitwiseAnd(XFlags,192) = 0 then
		    y = GTop + GHeight + vbuffer
		  else
		    y = GTop + GHeight + vbuffer + vtick
		  end if
		  y = y + GetSSHeight(XLabel)/2
		  DrawSString(XLabel, x, y, 5)
		  
		  // This section draws the left y-axis label.
		  
		  y = GTop + GHeight/2
		  if BitwiseAnd(YFlags,192) = 0 then
		    x = GLeft - hbuffer
		  else
		    x = GLeft - zYTickWidth - 8 - hbuffer
		  end if
		  x = x - GetSSWidth(YLabel)/2
		  DrawSString(YLabel, x, y, 5)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub zDrawXAxis()
		  
		  // This method draws tick marks/lables, grid lines, and lines through
		  // the origin for a normal x axis. This method is called by the
		  // DefineGraph method and should never called directly, as it does not
		  // do some important error checking.
		  
		  Var x, y, halfway as integer
		  Var n, nmin, nmax, NbMaj, NxMaj as integer
		  Var mh, mmh, off, poff, ins, oldSize as integer
		  Var mTick, Tick, TFactor as double
		  Var TLabel, oldFont as string
		  Var oldColor as color
		  
		  // Store the color and size for future restoration.
		  
		  oldColor = GX.DrawingColor
		  oldFont = GX.FontName
		  oldSize = GX.FontSize
		  
		  // Define the major and minor tick sizes using the TickN
		  // property set during the definition of the axis.
		  // NbMaj is the number of minor ticks in a major tick.
		  
		  mTick = zTickSize(XTickN)
		  Tick = zTickSize(XTickN+2)
		  NbMaj = Round(Tick/mTick)
		  TFactor = pow(10,XTPower)
		  
		  // nmin*mTick*TFactor is the position of the first tick
		  // nmax*mTick*TFactor is the same for the last.
		  // NxMaj is the index of the next major tick. The 0.05
		  // is a fudge that makes sure that even if the double-precision
		  // calculations end up being just a bit off, a tick mark that is
		  // supposed to be "at" VMin or VMax is not missed.
		  
		  nmin = Ceiling(XMin/(mTick*TFactor) - 0.05)
		  nmax = Floor(XMax/(mTick*TFactor) + 0.05)
		  NxMaj = NbMaj*Ceiling(XMin/(Tick*TFactor) - 0.05)
		  y = GTop + GHeight
		  
		  // Here is the main loop for drawing the tick marks. We only
		  // do it if we are drawing at least one kind of tick.
		  
		  if BitwiseAnd(XFlags,192) <> 0 then
		    
		    // Set up some important constants.
		    
		    SetFontInfo(TickFont,TickFSize)
		    mh = 5            // mark height for a major tick mark
		    mmh = 3           // mark height for a minor tick mark
		    off = 15          // offset to bottom of label
		    poff = 25         // offset to base of power label
		    ins = 1            // inset for graphics grid  
		    
		    for n = nmin to nmax             // for each x-axis tick mark
		      x = XCoord(n*mTick*TFactor)    // the tick's x-coordinate
		      
		      // If we are doing a major tick, draw a 5-point line
		      // and draw its label centered below the mark, using the format
		      // specified by the TickFormat method.
		      
		      if n = NxMaj then
		        GX.Drawline x, y, x, y+mh
		        TLabel = Format(n*mTick, zTickFormat(Tick))
		        halfway = GX.TextWidth(TLabel)/2
		        GX.DrawText TLabel, x-halfway, y+off
		        NxMaj = NxMaj + NbMaj
		        
		        // Draw power label under last major tick label
		        
		        if NxMaj > nmax and XTPower <> 0 then
		          DrawSString(SetSString("!x10^" + XTPower.ToString + "^!"), x, y+poff, 2)
		        end if
		        
		        // For minor ticks, just draw a 3-point vertical line
		        
		      elseif BitwiseAnd(XFlags,128) <> 0 then
		        GX.DrawLine x, y, x, y+mmh
		      end if
		      
		    next  // end axis drawing loop
		    
		    SetFontInfo(oldFont,oldSize)  // restore old text size
		    
		  end if  // end of check that we are drawing minor and/or major ticks
		  
		  // Here is the main loop for drawing the graphics grid
		  
		  if hasGrid then
		    SetColor(CGrid)
		    NxMaj = NbMaj*Ceiling(XMin/(Tick*TFactor) - 0.05)
		    for n = nmin to nmax
		      x = XCoord(n*mTick*TFactor)
		      if n = NxMaj then
		        if x <> GLeft and x <> GLeft + GWidth then
		          GX.Drawline x, y-ins, x, GTop+ins 
		        end if
		        NxMaj = NxMaj + NbMaj
		      end if
		    next
		    SetColor(oldColor)
		  end if
		  
		  // Here is the code for drawing the Line At Zero
		  
		  if XMin < 0 and XMax > 0 and BitwiseAnd(XFlags,256) <> 0 then
		    SetColor(Color.RGB(0,0,0))
		    x = XCoord(0)
		    GX.Drawline x, y, x, GTop
		    SetColor(oldColor)
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub zDrawYAxis()
		  
		  // This method draws tick marks/lables, grid lines, and lines through
		  // the origin for a normal y axis. This method is called by the Draw
		  // method and should never called directly, as it does not do some
		  // important error checking.
		  
		  Var x, y, backset as integer
		  Var n, nmin, nmax, NbMaj, NxMaj as integer
		  Var mh, mmh, off, ins, oldSize as integer
		  Var mTick, Tick, TFactor as double
		  Var TLabel, oldFont as string
		  Var oldColor as color
		  
		  // Store the color and text size for future restoration.
		  
		  oldColor = GX.DrawingColor
		  oldFont = GX.FontName
		  oldSize = GX.FontSize
		  
		  // Define the major and minor tick sizes using the YTickN
		  // property set during the definition of the axis.
		  // NbMaj is the number of minor ticks in a major tick.
		  
		  mTick = zTickSize(YTickN)
		  Tick = zTickSize(YTickN+2)
		  NbMaj = Round(Tick/mTick)
		  TFactor = pow(10, YTPower)
		  
		  // nmin*mTick*TFactor is the position of the first tick
		  // nmax*mTick*TFactor is the same for the last.
		  // NxMaj is the index of the next major tick. The 0.05
		  // is a fudge that makes sure that even if the double-precision
		  // calculations end up being just a bit off, a tick mark that is
		  // supposed to be "at" YMin or YMax is not missed.
		  
		  nmin = Ceiling(YMin/(mTick*TFactor) - 0.05)
		  nmax = Floor(YMax/(mTick*TFactor) + 0.05)
		  NxMaj = NbMaj*Ceiling(YMin/(Tick*TFactor) - 0.05)
		  x = GLeft
		  
		  // Here is the main loop for drawing the tick marks. We only
		  // do it if we are drawing either major ticks or minor ticks.
		  
		  if BitwiseAnd(YFlags,192) <> 0 then
		    
		    // Set up some important constants.
		    
		    mh = 5            // mark height for major tick mark
		    mmh = 3           // mark height for minor tick mark
		    off = 7           // offset to near end of label
		    ins = 1            // inset for grid line
		    
		    SetFontInfo(TickFont,TickFSize)
		    for n = nmin to nmax             // for each y-axis tick mark
		      y = YCoord(n*mTick*TFactor)    // the tick's y-coordinate
		      
		      // If we are doing a major tick, draw a 5-point line
		      // and draw its label centered below the mark, using the format
		      // specified by the TickFormat method.
		      
		      if n = NxMaj then
		        GX.Drawline x, y, x-mh, y
		        TLabel = Format(n*mTick, zTickFormat(Tick))
		        backset = GX.TextWidth(TLabel)
		        GX.DrawText TLabel, x-backset-off, y+3
		        NxMaj = NxMaj + NbMaj
		        
		        // Draw power label below last major tick label
		        
		        if NxMaj > nmax and YTPower <> 0 then
		          DrawSString(SetSString("!x10^"+YTPower.ToString+"^!"), x-off, y+13, 3)
		        end if
		        
		        // For minor ticks, just draw a 3-point horizontal line
		        
		      elseif BitwiseAnd(YFlags,128) <> 0 then
		        GX.DrawLine x, y, x-mmh, y
		      end if
		      
		    next  // end axis drawing loop
		    
		    SetFontInfo(oldFont,oldSize)
		    
		  end if  // end NoTix check
		  
		  // Here is the main loop for drawing the graphics grid
		  
		  if hasGrid then
		    SetColor(CGrid)
		    NxMaj = NbMaj*Ceiling(YMin/(Tick*TFactor) - 0.05)
		    for n = nmin to nmax
		      y = YCoord(n*mTick*TFactor)
		      if n = NxMaj then
		        if y <> GTop and y <> GTop + GHeight then
		          GX.Drawline x+ins, y, GLeft+GWidth-ins, y
		        end if
		        NxMaj = NxMaj + NbMaj
		      end if
		    next
		    SetColor(oldColor)
		  end if
		  
		  // Here is the code for drawing the Line At Zero
		  
		  if YMin < 0 and YMax > 0 and BitwiseAnd(YFlags,256) <> 0 then
		    SetColor(Color.RGB(0,0,0))
		    y = YCoord(0)
		    GX.Drawline x, y, GLeft+GWidth, y
		    SetColor(oldColor)
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub zSBHAdjust()
		  
		  // This method adjusts the horizontal scroll bar to reflect a change
		  // in either the size of the graph frame or the size of the graph
		  // control. Call this whenever one of these sizes changes. If there
		  // is no horizontal scroll bar, this method does nothing.
		  
		  if SBH <> nil then
		    
		    // Make sure that the bar's properties are set correctly
		    
		    SBH.MinimumValue = 0
		    SBH.LineStep = 10    // (can be changed if desired)
		    SBH.PageStep = 50    // (can be changed if desired)
		    
		    // If the control width now exceeds the graph size, there is no
		    // need for scrolling, so the scroll bar is disabled and the
		    // scrolling offset is set to zero.
		    
		    if FWidth - Width <= 0 then
		      SBH.Enabled = false
		      ScPixH = 0
		      
		      // Otherwise, we enable the scroll bar, and set its maximum
		      // to reflect the difference between the graph frame size and
		      // the control width.
		      
		    else
		      SBH.Enabled = true
		      SBH.MaximumValue = FWidth - Width
		    end if
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub zSBVAdjust()
		  
		  // This method adjusts the vertical scroll bar to reflect a change
		  // in either the size of the graph frame or the size of the graph
		  // control. Call this whenever one of these sizes changes. If there
		  // is no vertical scroll bar, this method does nothing.
		  
		  if SBV <> nil then
		    
		    // Make sure that the bar's properties are set correctly
		    
		    SBV.MinimumValue = 0
		    SBV.LineStep = 10   // (this can be changed if desired)
		    SBV.PageStep = 50   // (this can be changed if desired)
		    
		    // If the control width now exceeds the graph size, there is no
		    // need for scrolling, so the scroll bar is disabled and the
		    // scrolling offset is set to zero.
		    
		    if FHeight - Height <= 0 then
		      SBV.Enabled = false
		      ScPixV = 0
		      
		      // Otherwise, we enable the scroll bar, and set its maximum
		      // to reflect the difference between the graph frame size and
		      // the control width.
		      
		    else
		      SBV.Enabled = true
		      SBV.MaximumValue = FHeight - Height
		    end if
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function zTickFormat(MajTick as double) As String
		  
		  // This method provides a format string for formatting the major
		  // ticks on the x or y axes. This method is called by the DrawAxes
		  // method and will not generally be useful to anyone else. Note that
		  // this method assumes that the major tick size will be >= 0.0001,
		  // which is true for all the methods calling this method.
		  
		  if MajTick >= 1 then
		    Return "-#0"
		  elseif MajTick >= 0.1 then
		    Return "-#0.0"
		  elseif MajTick >= 0.01 then
		    Return "-#0.00"
		  elseif MajTick >= 0.001 then
		    Return "-#0.000"
		  else
		    Return "-#0.0000"
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function zTickSize(n as integer) As Double
		  
		  // This method returns an exact tick size when given
		  // an integer that is roughly 3 times the base-ten log of
		  // a rough estimate of the tick size. 
		  
		  Var ndig, npow as integer
		  Var digit as double
		  
		  if n >= 0 then
		    ndig = n mod 3
		    npow = n\3
		  else
		    ndig = (n mod 3) + 3
		    npow = n\3 - 1
		  end if
		  Select case ndig
		  Case 0
		    digit = 1
		  Case 1
		    digit = 2
		  Case 2
		    digit = 5
		  Case 3
		    digit = 10
		  end select
		  Return digit*Pow(10, npow)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function zXTickWidth() As Integer
		  
		  // This method estimates the maximum width (in pixels) of the
		  // y-axis tick labels.
		  
		  Var oldFont as string
		  Var oldSize, maxw, maxw2 as integer
		  Var oldBold, oldItal as boolean
		  Var Tick, MTickVal, TFactor as double
		  
		  // Record original font information for later restoration
		  
		  oldFont = GX.FontName
		  oldSize = GX.FontSize
		  oldBold = GX.Bold
		  oldItal = GX.Italic
		  
		  // Set standard font information for tick labels.
		  
		  SetFontInfo("Helvetica",9,false,false)
		  
		  // Estimate the tick width by finding the major
		  // tick nearest the maximum and the one nearest the
		  // minimum, and choosing whichever is widest.
		  
		  TFactor = pow(10,XTPower)
		  Tick = zTickSize(XTickN+2)
		  MTickVal = Tick*Ceiling(XMin/(Tick*TFactor) - 0.05)
		  maxw = GX.TextWidth(Format(MTickVal,zTickFormat(Tick)))
		  MTickVal = Tick*Floor(XMax/(Tick*TFactor) + 0.05)
		  maxw2 = GX.TextWidth(Format(MTickVal,zTickFormat(Tick)))
		  maxw = Max(maxw,maxw2)
		  
		  // Restore font information and return with the answer
		  
		  SetFontInfo(oldFont, oldSize, oldBold, oldItal)
		  return maxw
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function zYTickWidth() As Integer
		  
		  // This method estimates the maximum width (in pixels) of the
		  // y-axis tick labels.
		  
		  Var oldFont as string
		  Var oldSize, maxw, maxw2 as integer
		  Var oldBold, oldItal as boolean
		  Var Tick, MTickVal, TFactor as double
		  
		  // Record original font information for later restoration
		  
		  oldFont = GX.FontName
		  oldSize = GX.FontSize
		  oldBold = GX.Bold
		  oldItal = GX.Italic
		  
		  // Set standard font information for tick labels.
		  
		  SetFontInfo("Helvetica",9,false,false)
		  
		  // Estimate the tick width by finding the major
		  // tick nearest the maximum and the one nearest the
		  // minimum, and choosing whichever is widest.
		  
		  TFactor = pow(10,YTPower)
		  Tick = zTickSize(YTickN+2)
		  MTickVal = Tick*Ceiling(YMin/(Tick*TFactor) - 0.05)
		  maxw = GX.TextWidth(Format(MTickVal,zTickFormat(Tick)))
		  MTickVal = Tick*Floor(YMax/(Tick*TFactor) + 0.05)
		  maxw2 = GX.TextWidth(Format(MTickVal,zTickFormat(Tick)))
		  maxw = Max(maxw,maxw2)
		  
		  // Restore font information and return with the answer
		  
		  SetFontInfo(oldFont, oldSize, oldBold, oldItal)
		  return maxw
		  
		End Function
	#tag EndMethod


	#tag Hook, Flags = &h0
		Event Close()
	#tag EndHook

	#tag Hook, Flags = &h0
		Event DrawContent()
	#tag EndHook

	#tag Hook, Flags = &h0
		Event HeightQuery() As Integer
	#tag EndHook

	#tag Hook, Flags = &h0
		Event Open()
	#tag EndHook

	#tag Hook, Flags = &h0
		Event ScrollQueryH() As ScrollBar
	#tag EndHook

	#tag Hook, Flags = &h0
		Event ScrollQueryV() As ScrollBar
	#tag EndHook

	#tag Hook, Flags = &h0
		Event WidthQuery() As Integer
	#tag EndHook


	#tag Property, Flags = &h0
		CBlack As color
	#tag EndProperty

	#tag Property, Flags = &h0
		CGrid As color
	#tag EndProperty

	#tag Property, Flags = &h0
		CWhite As color
	#tag EndProperty

	#tag Property, Flags = &h0
		FDepth As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		FHeight As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		FWidth As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		GHeight As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		GLeft As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		GTop As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		GWidth As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		GX As graphics
	#tag EndProperty

	#tag Property, Flags = &h0
		GXP As picture
	#tag EndProperty

	#tag Property, Flags = &h0
		HasGrid As boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		InvLog10 As double
	#tag EndProperty

	#tag Property, Flags = &h0
		NoGraph As boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		PrevCurvH As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		PrevCurvV As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		Printing As boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		RErr As boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		SBH As ScrollBar
	#tag EndProperty

	#tag Property, Flags = &h0
		SBV As ScrollBar
	#tag EndProperty

	#tag Property, Flags = &h0
		ScPixH As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		ScPixV As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		TickFont As string
	#tag EndProperty

	#tag Property, Flags = &h0
		TickFSize As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		Title As string
	#tag EndProperty

	#tag Property, Flags = &h0
		TitleH As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		TitleV As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		XColor As color
	#tag EndProperty

	#tag Property, Flags = &h0
		XFlags As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		XLabel As string
	#tag EndProperty

	#tag Property, Flags = &h0
		XLabelH As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		XLabelV As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		XMax As double
	#tag EndProperty

	#tag Property, Flags = &h0
		XMin As double
	#tag EndProperty

	#tag Property, Flags = &h0
		XTickN As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		XTPower As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		XVtoPix As double
	#tag EndProperty

	#tag Property, Flags = &h0
		YColor As color
	#tag EndProperty

	#tag Property, Flags = &h0
		YFlags As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		YLabel As string
	#tag EndProperty

	#tag Property, Flags = &h0
		YLabelH As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		YLabelV As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		YMax As double
	#tag EndProperty

	#tag Property, Flags = &h0
		YMin As double
	#tag EndProperty

	#tag Property, Flags = &h0
		YTickN As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		YTPower As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		YVToPix As double
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="DoubleBuffer"
			Visible=false
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="InitialParent"
			Visible=false
			Group=""
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="AllowAutoDeactivate"
			Visible=true
			Group="Appearance"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Tooltip"
			Visible=true
			Group="Appearance"
			InitialValue=""
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="AllowFocusRing"
			Visible=true
			Group="Appearance"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="AllowFocus"
			Visible=true
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="AllowTabs"
			Visible=true
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Width"
			Visible=true
			Group="Position"
			InitialValue="100"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Height"
			Visible=true
			Group="Position"
			InitialValue="100"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LockLeft"
			Visible=true
			Group="Position"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LockTop"
			Visible=true
			Group="Position"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LockRight"
			Visible=true
			Group="Position"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LockBottom"
			Visible=true
			Group="Position"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TabPanelIndex"
			Visible=false
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TabIndex"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TabStop"
			Visible=true
			Group="Position"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Visible"
			Visible=true
			Group="Appearance"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Enabled"
			Visible=true
			Group="Appearance"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Backdrop"
			Visible=true
			Group="Appearance"
			InitialValue=""
			Type="Picture"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Transparent"
			Visible=true
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CBlack"
			Visible=false
			Group="Behavior"
			InitialValue="&c000000"
			Type="color"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CWhite"
			Visible=false
			Group="Behavior"
			InitialValue="&c000000"
			Type="color"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="FHeight"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="FWidth"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="GHeight"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="GLeft"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="GTop"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="GWidth"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="GXP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="picture"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Printing"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="RErr"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ScPixH"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ScPixV"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Title"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="string"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="CGrid"
			Visible=false
			Group="Behavior"
			InitialValue="&c000000"
			Type="color"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="XColor"
			Visible=false
			Group="Behavior"
			InitialValue="&c000000"
			Type="color"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="XLabel"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="string"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="XFlags"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="XMax"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="XMin"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="YColor"
			Visible=false
			Group="Behavior"
			InitialValue="&c000000"
			Type="color"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="YFlags"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="YLabel"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="string"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="YMax"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="YMin"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="XVtoPix"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="YVToPix"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="XTickN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="XTPower"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="YTickN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="YTPower"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="HasGrid"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="PrevCurvV"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="PrevCurvH"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="InvLog10"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="NoGraph"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TickFont"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="string"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="TickFSize"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TitleH"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TitleV"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="XLabelH"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="XLabelV"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="YLabelH"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="YLabelV"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="FDepth"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="integer"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
