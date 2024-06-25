#tag Class
Protected Class StyledTextShape
Inherits Group2D
	#tag Method, Flags = &h0
		Sub SetFont(Name As String, Size As Double, BoldFlag As Boolean = False)
		  FontName = Name
		  FontSize = Size
		  Bold = BoldFlag
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetText(TheText As String)
		  // We will start from scratch: remove all objects in this group
		  While Count > 0
		    RemoveObjectAt(Count-1)
		  Wend
		  // No height or width so far
		  Width = 0.0
		  Height = 0.0
		  Var theSubString As String // will store pieces of the string
		  Var specialChars As String = "${}^_"  // defines the set of special characters
		  Var digits As String = "0123456789" // defines digits
		  Var subOffset As Double = -0.15  // subscript offset as a fraction of font size
		  Var supOffset As Double = 0.40  // superscript offset as a fraction of font size
		  Var ssFrac As Double = 0.75 // super or subscript size as a fraction of font size
		  Var doingItalic As Boolean = False
		  Var mathMode As Boolean = False
		  Var theOffset As Double = 0.0
		  Var inASubgroup As Boolean = False
		  For Each char As String In TheText.Characters
		    If specialChars.Contains(char) Then
		      SubmitSubString(theSubString, ssFrac, theOffset, doingItalic)
		      theSubString = ""
		      If char = "$" Then
		        If mathMode Then
		          doingItalic = False
		          mathMode = False
		        Else
		          mathMode = True
		          doingItalic = True
		        End If
		      Elseif char = "{" Then
		        inASubgroup = True
		      Elseif char = "}" Then
		        inASubgroup = False
		        theOffset = 0.0
		      Elseif char = "^" Then
		        theOffset = supOffset
		      Elseif char = "_" Then
		        theOffset = subOffset
		      End If
		    Elseif digits.Contains(char) Then
		      If doingItalic Then
		        SubmitSubString(theSubString, ssFrac, theOffset, doingItalic)
		        theSubString = char
		        doingItalic = False
		      Else
		        theSubString = theSubString + char
		      End If
		      If theOffset <> 0.0 And Not inASubgroup Then 
		        SubmitSubString(theSubString, ssFrac, theOffset, doingItalic)
		        theOffset = 0.0
		        theSubString = ""
		      End If
		    Else // an ordinary character
		      If mathMode And Not doingItalic Then
		        SubmitSubString(theSubString, ssFrac, theOffset, doingItalic)
		        theSubString = char
		        doingItalic = True
		      Else
		        theSubString = theSubString + char
		      End If
		      If theOffset <> 0.0 And Not inASubgroup Then 
		        SubmitSubString(theSubString, ssFrac, theOffset, doingItalic)
		        theOffset = 0.0
		        theSubString = ""
		      End If
		    End If
		  Next
		  SubmitSubString(theSubString, ssFrac, theOffset, doingItalic)
		  // Recenter the final resulting group. Note that the group's anchor will therefore
		  // be at the horizontal center and the text baseline.
		  For i As Integer = 0 To me.Count - 1
		    me.Item(i).X = me.Item(i).X - Width/2
		  Next
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub SubmitSubString(TheText As String, SizeFactor As Double, OffsetFactor As Double, ItalicFlag As Boolean)
		  If Not TheText.IsEmpty Then
		    Var theNextShape As New TextShape // create a new subshape
		    theNextShape.Bold = Bold // make it bold if the whole is bold
		    theNextShape.Italic = ItalicFlag // italic if this flag is set
		    theNextShape.FontName = FontName // same font as before
		    If OffsetFactor <> 0.0 Then
		      theNextShape.FontSize = FontSize*SizeFactor
		    Else
		      theNextShape.FontSize = FontSize
		    End If
		    theNextShape.FillColor = TextColor // and set the color
		    // we need to be left-aligned for the calculations to work
		    theNextShape.HorizontalAlignment = TextShape.Alignment.Left
		    theNextShape.X = Width // add new string to the right of anything previous
		    theNextShape.Y = me.Y - FontSize*OffsetFactor // perhaps adjust the vertical placement
		    theNextShape.Text = TheText // the text is the characters submitted so far
		    AddObject(theNextShape) // add in the next text shape
		    UpdateSize(theNextShape)  // update the width and size of the whole string
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub UpdateSize(theShape As TextShape)
		  If theShape <> Nil Then
		    Var p As New Picture(100,100,32)
		    p.Graphics.FontName = theShape.FontName
		    p.Graphics.FontUnit = FontUnits.Point
		    p.Graphics.FontSize = theShape.FontSize
		    Width = Width + p.Graphics.TextWidth(theShape.Text)
		    Height = Max(Height, p.Graphics.TextHeight(theShape.Text, 1000))
		  End If
		End Sub
	#tag EndMethod


	#tag Note, Name = Description
		This method is a subclass of Group2D that creates a kind of styled TextShape
		that one can use for graph titles and axis labels. One specifies the styles in
		a subset of LaTeX, where "$" indicates the beginning or end of italics, "^" indicates
		the beginning of a superscript, "_" the beginning of a subscript and {...} groups
		characters in a multiple-character subscript or superscript. Unlike LaTeX, though,
		one types Greek characters directly into the string, nested superscripts or
		subscripts are not possible, and superscripts, numbers are italicized *inside*
		an  $...$ enclosure, and subscripts are allowed *outside* of the $...$ enclosure.
		
		One can also make the styled string bold by setting the Bold property, but this
		will apply to the entire string.
		
		The X and Y coordinates of the group specify its horizontal center and the text baseline,
		respectively.
		
	#tag EndNote


	#tag Property, Flags = &h0
		Bold As Boolean
	#tag EndProperty

	#tag Property, Flags = &h21
		Private FontName As String = "System"
	#tag EndProperty

	#tag Property, Flags = &h21
		Private FontSize As Double = 0
	#tag EndProperty

	#tag Property, Flags = &h0
		Height As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		TextColor As Color = Color.Black
	#tag EndProperty

	#tag Property, Flags = &h0
		Width As Double
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="BorderOpacity"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="FillOpacity"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="BorderColor"
			Visible=false
			Group="Behavior"
			InitialValue="&h000000"
			Type="Color"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="BorderWidth"
			Visible=false
			Group="Behavior"
			InitialValue="0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Count"
			Visible=false
			Group="Behavior"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="FillColor"
			Visible=false
			Group="Behavior"
			InitialValue="&h000000"
			Type="Color"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Rotation"
			Visible=false
			Group="Behavior"
			InitialValue="0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Scale"
			Visible=false
			Group="Behavior"
			InitialValue="0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="X"
			Visible=false
			Group="Behavior"
			InitialValue="0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Y"
			Visible=false
			Group="Behavior"
			InitialValue="0"
			Type="Double"
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
			Name="Width"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Height"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TextColor"
			Visible=false
			Group="Behavior"
			InitialValue="Color.Black"
			Type="Color"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Bold"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
