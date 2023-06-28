#tag Class
Protected Class ListBoxCanvas
	#tag Method, Flags = &h0
		Sub addCell(text As String)
		  Me.cells.add(new Cell(text))
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub changeCellText(index as Integer, newContent as String)
		  if index > cells.lastIndex Then
		    MessageBox("error!")
		  else 
		    Me.Cells(index).content = newContent
		  end if 
		  
		  if cellWidth< newContent.length*7+5 Then
		    cellWidth = newContent.length*8 + 5
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(canvas As Desktopcanvas, numOfRow As Integer, cellHeight As Integer, cellWidth As Integer, Scrollable As Boolean)
		  Me.ListCanvas = canvas
		  Me.RowCount = numOfRow
		  Me.cellHeight = cellHeight
		  Me.cellWidth = cellWidth
		  Me.Scrollable = Scrollable
		  for i As Integer = 0 to numOfRow -1
		    Me.Cells().add(new Cell(""))
		  next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(canvas As DesktopCanvas, cellHeight As Integer, cellWidth As Integer, values() As String)
		  Me.ListCanvas = canvas
		  Me.cellHeight = cellHeight
		  Me.cellWidth = cellWidth
		  Me.RowCount = values().lastIndex()+1
		  for i As Integer = 0 to RowCount -1
		    Me.Cells().add(new Cell(values(i)))
		  next
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawText(g As Graphics, x As Integer, y As Integer, input As String)
		  Var expressions() As String
		  input = input.replaceAll("{", "")
		  input = input.replaceAll("\r", "@")
		  expressions() = input.Split("")
		  Var textShapeArray() As TextShape 
		  Var value As TextShape = new Textshape()
		  Var counter As Integer = 0
		  Var isReg As Boolean = True
		  Var isSub As Boolean = False
		  Var isSup As Boolean = False
		  Var isIta As Boolean   = True
		  Var currentText As String = ""
		  For n As Integer = 0 to expressions.lastIndex
		    If expressions(n) = "^" Then
		      if isSub Then
		        x=x-counter*5
		        counter = 0
		        isSub = False
		      end if
		      isSup = True
		      isReg = False
		    elseif expressions(n) = "_" Then
		      if isSup Then
		        x=x-counter*5
		        counter = 0
		        isSup = False
		      end if
		      isSub = True
		      isReg = False
		    elseIf expressions(n) = "@" Then
		      isIta = False
		    elseif expressions(n) = "\" Then
		      isIta = True
		      
		    Elseif expressions(n) = "}" Then
		      isReg = True
		    Else 
		      value.value = expressions(n)
		      value.italic = isIta
		      value.FontName = "Palatino"
		      if isReg Then
		        //draw regular character
		        value.FontSize = 15
		        value.HorizontalAlignment = TextShape.Alignment.Left
		        g.DrawObject(value, x ,y)
		        x = x+ g.Textwidth(expressions(n))+2
		        
		        counter= 0
		        
		      elseif isSub Then
		        //draw sub character
		        value.FontSize = 10
		        g.DrawObject(value, x ,y+5)
		        x = x+ g.Textwidth(expressions(n))-1
		        counter = counter +1
		        
		      elseif isSup Then
		        //draw sup character
		        value.FontSize = 10
		        g.DrawObject(value, x ,y-10)
		        x = x+ g.Textwidth(expressions(n))-1
		        counter = counter+1
		      end if 
		    End If
		    
		  Next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawText1(g As Graphics, x As Integer, y As Integer, input As String)
		  Var expressions() As String
		  input = input.replaceAll("{", "")
		  input = input.replaceAll("\r", "@")
		  expressions() = input.Split("")
		  Var textShapeArray() As TextShape 
		  Var value As TextShape = new Textshape()
		  Var counter As Integer = 0
		  Var isReg As Boolean = True
		  Var isSub As Boolean = False
		  Var isSup As Boolean = False
		  Var isIta As Boolean   = True
		  Var currentText As String = ""
		  For n As Integer = 0 to expressions.lastIndex
		    If expressions(n) = "^" Then
		      if isSub Then
		        x=x-counter*5
		        counter = 0
		        isSub = False
		        value.FontSize = 10
		        value.HorizontalAlignment = TextShape.Alignment.Left
		        value.value = currentText
		        g.DrawObject(value,x,y+5)
		        currentText = ""
		      else
		        value.FontSize = 15
		        value.HorizontalAlignment = TextShape.Alignment.Left
		        value.value = currentText
		        g.DrawObject(value,x,y)
		        currentText = ""
		      end if
		      isSup = True
		      isReg = False
		    elseif expressions(n) = "_" Then
		      if isSup Then
		        x=x-counter*5
		        counter = 0
		        isSup = False
		        value.FontSize = 10
		        value.HorizontalAlignment = TextShape.Alignment.Left
		        value.value = currentText
		        g.DrawObject(value,x,y-5)
		        currentText = ""
		      Else
		        value.FontSize = 15
		        value.HorizontalAlignment = TextShape.Alignment.Left
		        value.value = currentText
		        g.DrawObject(value,x,y)
		        currentText = ""
		      end if
		      isSub = True
		      isReg = False
		    elseIf expressions(n) = "@" Then
		      value.italic = false
		    elseif expressions(n) = "\" Then
		      value.italic = true
		    Elseif expressions(n) = "}" Then
		      
		      if isSup Then
		        x=x-counter*5
		        counter = 0
		        isSup = False
		        value.FontSize = 10
		        value.HorizontalAlignment = TextShape.Alignment.Left
		        value.value = currentText
		        g.DrawObject(value,x,y-5)
		        currentText = ""
		      Else
		        value.FontSize = 10
		        value.HorizontalAlignment = TextShape.Alignment.Left
		        value.value = currentText
		        g.DrawObject(value,x,y)
		        currentText = ""
		      end if
		      isReg = True
		      isSup = False
		      isSub = False
		    Else 
		      value.value = expressions(n)
		      value.italic = isIta
		      value.FontName = "Palatino"
		      if isReg Then
		        //draw regular character
		        currentText = currentText+ expressions(n)
		        x = x+7
		        counter= 0
		      elseif isSub Then
		        //draw sub character
		        currentText = currentText+ expressions(n)
		        x = x+5
		        counter = counter +1
		        
		      elseif isSup Then
		        //draw sup character
		        currentText = currentText+ expressions(n)
		        x = x+5
		        counter = counter+1
		      end if 
		    End If
		    
		  Next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawText2(g As Graphics, x As Integer, y As Integer, input As String)
		  Var expressions() As String
		  input = input.replaceAll("{", "")
		  input = input.replaceAll("\r", "@")
		  expressions() = input.Split("")
		  Var textShapeArray() As TextShape 
		  Var value As TextShape = new Textshape()
		  Var counter As Integer = 0
		  Var isReg As Boolean = True
		  Var isSub As Boolean = False
		  Var isSup As Boolean = False
		  Var currentText As String = ""
		  value.italic = True
		  For n As Integer = 0 to expressions.lastIndex
		    value.FontName = "Palatino"
		    
		    If expressions(n) = "^" Then //process previous strings and then go to super mode
		      if isSub then
		        x=x-counter*5
		        counter = 0
		        isSub = False
		        value.FontSize = 10
		        value.HorizontalAlignment = TextShape.Alignment.Left
		        value.value = currentText
		        g.DrawObject(value,x,y+5)
		        x = x+ 5*currentText.length
		        currentText = ""
		      else 
		        value.FontSize = 15
		        value.HorizontalAlignment = TextShape.Alignment.Left
		        value.value = currentText
		        g.DrawObject(value,x,y)
		        x = x+ 10*currentText.length
		        currentText = ""
		      end if 
		      isSup = True
		      isSub = False
		      isReg = False
		    Elseif expressions(n) = "_" Then //process previous and then go to sub mode
		      if isSup then
		        x=x-counter*5
		        counter = 0
		        isSup = False
		        value.FontSize = 10
		        value.HorizontalAlignment = TextShape.Alignment.Left
		        value.value = currentText
		        g.DrawObject(value,x,y-8)
		        x = x+ 5*currentText.length
		        currentText = ""
		      else 
		        value.FontSize = 15
		        value.HorizontalAlignment = TextShape.Alignment.Left
		        value.value = currentText
		        g.DrawObject(value,x,y)
		        x = x+ 10*currentText.length
		        currentText = ""
		      end if 
		      isSup = False
		      isSub = True
		      isReg = False
		      
		    Elseif expressions(n) = "@" Then //change italics
		      value.FontSize = 15
		      value.HorizontalAlignment = TextShape.Alignment.Left
		      value.value = currentText
		      g.DrawObject(value,x,y)
		      x = x+ 10*currentText.length+3
		      currentText = ""
		      value.italic = False
		      
		    Elseif expressions(n) = "\" Then //change italics
		      value.FontSize = 15
		      value.HorizontalAlignment = TextShape.Alignment.Left
		      value.value = currentText
		      g.DrawObject(value,x,y)
		      x = x+ 10*currentText.length
		      currentText = ""
		      value.italic = True
		      
		      if n = expressions.lastIndex Then
		        value.FontSize = 15
		        value.HorizontalAlignment = TextShape.Alignment.Left
		        value.value = currentText
		        g.DrawObject(value,x,y)
		        currentText = ""
		      end if 
		      
		    Elseif expressions(n) = "}" Then //finish prevous super/sub then go to reg mode
		      if isSup Then
		        x=x-counter*5
		        counter = 0
		        isSup = False
		        value.FontSize = 10
		        value.HorizontalAlignment = TextShape.Alignment.Left
		        value.value = currentText
		        g.DrawObject(value,x,y-8)
		        x = x+ 5*currentText.length
		        currentText = ""
		        
		      Else
		        value.FontSize = 10
		        value.HorizontalAlignment = TextShape.Alignment.Left
		        value.value = currentText
		        g.DrawObject(value,x,y+5)
		        x = x+ 5*currentText.length
		        currentText = ""
		      end if
		      isReg = True
		    Elseif n = expressions.lastIndex Then
		      currentText = currentText+expressions(n)
		      value.FontSize = 15
		      value.HorizontalAlignment = TextShape.Alignment.Left
		      value.value = currentText
		      g.DrawObject(value,x,y)
		      currentText = ""
		    Else //regular characters, add to currentStirng value
		      if isReg Then
		        //add regular character
		        currentText = currentText+ expressions(n)
		        
		        counter= 0
		      elseif isSub Then
		        //add sub character
		        currentText = currentText+ expressions(n)
		        counter = counter +1
		        
		      elseif isSup Then
		        //add sup character
		        currentText = currentText+ expressions(n)
		        counter = counter+1
		      end if 
		    end if 
		  Next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub paint(g As Graphics)
		  g.DrawingColor = Color.Gray
		  g.DrawRectangle(0,0,Me.CellWidth, Me.RowCount*CellHeight)
		  for i As integer = 1 To Me.rowCount-1
		    g.DrawLine(0,cellHeight*i, cellWidth-1, cellHeight*i)
		  Next
		  for i As integer = 1 To Me.RowCount
		    //g.drawtext(cells(i-1).Content,10,cellHeight*i - 5)
		    DrawText2(g, 5, cellHeight*i-8, cells(i-1).Content)
		  next
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		CellHeight As Integer = 1
	#tag EndProperty

	#tag Property, Flags = &h0
		Cells() As Cell
	#tag EndProperty

	#tag Property, Flags = &h0
		CellWidth As Integer = 1
	#tag EndProperty

	#tag Property, Flags = &h0
		ListCanvas As DesktopCanvas
	#tag EndProperty

	#tag Property, Flags = &h0
		RowCount As Integer = 1
	#tag EndProperty

	#tag Property, Flags = &h0
		Scrollable As Boolean = True
	#tag EndProperty


	#tag ViewBehavior
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
			InitialValue="-2147483648"
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
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="RowCount"
			Visible=false
			Group="Behavior"
			InitialValue="1"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CellHeight"
			Visible=false
			Group="Behavior"
			InitialValue="1"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CellWidth"
			Visible=false
			Group="Behavior"
			InitialValue="1"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Scrollable"
			Visible=false
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
