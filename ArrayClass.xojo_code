#tag Class
Protected Class ArrayClass
	#tag Method, Flags = &h0
		Sub AddAll(NewValues() As Double)
		  //Parameters: NewValues, a list of 31 double values
		  //Return: None 
		  
		  //This method takes a list of 31 new double values and Adds them to the correct subarray within GiantArray
		  
		  Var i As Integer 
		  GiantArray.ResizeTo(30, CurrentIndex)
		  
		  for i = 0 to 30
		    GiantArray(i, CurrentIndex) = NewValues(i)
		  next
		  
		  CurrentIndex = CurrentIndex + 1
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function AddFinalArray(CurrentCase As Integer, IsInitial As Boolean, InputArray() As Double) As String
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub AddFinalValues()
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub BinaryExport()
		  //Parameters: None 
		  //Return: None
		  
		  //When called this method will prompt the user to name a .txt file and determine its location
		  //Then it will write the total time, dt, and GiantArray to that file 
		  
		  //Defining the file to output, the binary stream, and i and j that will be used in loops
		  Var OutputFile As FolderItem
		  Var Stream As BinaryStream
		  Var i,j As Integer
		  
		  //Prompts the user to choose a file name and location
		  Outputfile = FolderItem.ShowSaveFileDialog("txt", "BinaryFile.txt")
		  
		  //Opens the binary stream, then writes TotalTime and dt to it, and then writes GiantArray to it
		  If OutputFile <> Nil then
		    Stream = BinaryStream.create(Outputfile, True)
		    Stream.WriteDouble(TotalTime)
		    Stream.WriteDouble(dt)
		    For i = 0 to TotalSteps - 1
		      for j = 0 to 15 
		        Stream.WriteDouble(GiantArray(j,i))
		      next
		    next
		    
		  end if 
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub ClearArrays()
		  //Parameters: None
		  //Return: None
		  
		  //This method removes everything from GiantArray
		  
		  GiantArray.RemoveAll
		  GiantArray.ResizeTo(15, -1)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor()
		  // This class is meant to help with graphing. It allows us to store a multi-dimensional array of all the quantities we might want to graph.
		  
		  //Value Meaning  [  ∂h/∂M     ∂h/∂δ      ∂h/∂f0      ∂h/∂R     ∂h/∂β      ∂h/∂ψ      ∂h/∂λ0.     ∂h/∂Θ      ∂h/∂Φ      ∂h/∂χ10x      ∂h/∂χ10y    ∂h/∂χ10z    ∂h/∂χ20x     ∂h/χ20y     ∂h/∂χ20z        h       hp       hc        α       ι          ζ.        LNhatx     LNhaty     LNhatz    χ1hatX      χ1hatY     χ1hatZ     χ2hatX     χ2hatY     χ2hatZ     noise]
		  //Array                  [.     0.            1.             2.              3.           4.             5.             6.               7.             8.               9.                  10.               11.               12.               13.              14.            15.     16.      17.      18.    19      20.        21.            22.           23.           24.            25.           26.            27.           28.           29.           30.] 
		   
		  CurrentIndex = 0
		  'TotalTime = 1
		  'dt = 50
		  'TotalSteps  = (TotalTime*365*24*60*60) \ dt
		  
		  ResizeAll(-1)
		  
		  FinalArray.Add("Case, M, Unc in M, δ, Unc in δ, ...") // what is this for?
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CSVExport()
		  var OutputFile as folderitem
		  var Stream as TextOutputStream
		  var i, j as integer
		  var str as string
		  OutputFile = FolderItem.ShowSaveFileDialog("csv", "OutputFile.csv")
		  
		  if Outputfile <> nil Then
		    Stream = TextOutputStream.Create(OutputFile)
		    For i = 0 to FinalArray.LastIndex - 1
		      Stream.Write(FinalArray(0) + EndOfLine)
		    next
		    Stream.close
		  end if 
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetAllValues(Parameter As Integer) As Double()
		  //Parameters: An integer which specifies which sublist should be returned
		  //Return: A one dimensional list of doubles
		  
		  //This method returns all of the time propagated values for any derivative of h
		  
		  //Creates the list that will be returned, and an integer used in a loop
		  Var AllValues(-1) As Double
		  Var i As Integer
		  
		  //Adds the specified values to the list that will be returned
		  For i = 0 to CurrentIndex - 1
		    AllValues.Add(GiantArray(Parameter,i))
		  next
		  
		  return AllValues
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Getdt() As Double
		  //Parameters: None
		  //Return: dt as Double
		  
		  //This method simply returns dt
		  
		  return dt 
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetHighestValue(InputArray() As Double) As Double
		  //Parameters: InputArray() an array of double values
		  //Return: CurrentHighest as double
		  
		  //This method takes an array and finds the largest value within it
		  //It does this by comparing each item in the array to the current highest number, and if it is larger setting that number to the new current highest
		  
		  //Defines the current highest value and sets it equal to the first value in the input array
		  Var CurrentHighest As Double = InputArray(0)
		  
		  //For the rest of the values we check if they are higher than the current highest, if they are they become the current highest.
		  Var i As Integer
		  For i = 1 to InputArray.LastIndex -1 
		    if CurrentHighest < InputArray(i) then
		      CurrentHighest = InputArray(i)
		    else 
		      continue
		    end if 
		  next 
		  
		  return CurrentHighest
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetLowestValue(InputArray() As Double) As Double
		  //Parameters: InputArray() an array of double values
		  //Return: CurrentLowest as double
		  
		  //This method takes an array and finds the lowest value within it
		  //It does this by comparing each item in the array to the current lowest number, and if it is lower setting that number to the new current lowest
		  
		  //Defines the current lowest value and sets it equal to the first value in the input array
		  Var CurrentLowest As Double = InputArray(0)
		  
		  //For the rest of the values we check if they are lower than the current lowest, if they are they become the current lowest.
		  Var i As Integer
		  For i = 1 to InputArray.LastIndex -1 
		    if CurrentLowest > InputArray(i) then
		      CurrentLowest = InputArray(i)
		    else 
		      continue
		    end if 
		  next 
		  
		  return CurrentLowest
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetValue(Parameter As Integer, TimeIndex As Integer) As Double
		  //Parameters: Two integers specifying the time and which parameter to return
		  //Return: The element in GiantArray corresponding to those two parameters
		  
		  
		  return GiantArray(Parameter, TimeIndex)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub ResizeAll(n As Integer)
		  //Parameters: n as integer, the number of rows we want in our new array
		  //Return: None
		  
		  //This method resizes giantarray to n
		  
		  GiantArray.ResizeTo(30,n)
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		CurrentIndex As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		dt As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		FinalArray(-1) As String
	#tag EndProperty

	#tag Property, Flags = &h0
		GiantArray(29,-1) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		str As String
	#tag EndProperty

	#tag Property, Flags = &h0
		TotalSteps As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		TotalTime As Double
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
			Name="TotalTime"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dt"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CurrentIndex"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TotalSteps"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="str"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
