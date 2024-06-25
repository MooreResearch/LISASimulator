#tag Class
Protected Class DataWriterClass
	#tag Method, Flags = &h0
		Sub CloseData()
		  // This method closes any data files that might exist.
		  If bs.LastIndex > -1 Then
		    For i As Integer = 0 to bs.LastIndex
		      if bs(i) <> nil Then
		        bs(i).Close
		        bs(i) = Nil
		      end if
		    Next
		  End If
		  If ms.LastIndex > -1 Then ms.ResizeTo(-1)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CreateFolder(TheVNames() as String, InternalSize as Integer = -1)
		  // This method creates a folder for the data items to be saved and initializes
		  // binary streams for each of the items to be saved. It returns "OK" if all
		  // went well, and an error message otherwise.
		  
		  // The following if statement ensures that we have some variable names.
		  If TheVNames.LastIndex = -1 Then
		    Var e as New RuntimeException
		    e.Message = "CreateFolder: No variable names supplied."
		    Raise e
		  Else
		    // We also only want to execute this only once, so if we already have data set up,
		    // report an error. If you really want to save more than one data set per run
		    // (for example, if you are executing multiple cases), then use the CloseData
		    // method to start over.
		    
		    // If we are creating an "internal folder"
		    If InternalSize <> -1 Then
		      If ms.LastIndex > -1 Then
		        Var e as New RuntimeException
		        e.Message = "CreateFolder: Folder is already created."
		        Raise e
		      Else
		        ms.ResizeTo(TheVNames.LastIndex)
		        For i As Integer = 0 to TheVNames.LastIndex
		          ms(i) = New MemoryStreamClass(TheVNames(i), InternalSize)
		        Next
		      End If
		    Else // we are creating external data files
		      If bs.LastIndex > -1 Then
		        Var e as New RuntimeException
		        e.Message = "CreateFolder: Folder is already created."
		        Raise e
		      Else
		        Try
		          Var d As New FolderItem("") // get directory containing the application
		          Var dateTimeOfNow As DateTime = DateTime.Now // current date
		          Var dateTimeString As String = dateTimeOfNow.SQLDateTime
		          dateTimeString = dateTimeString.ReplaceAll(" ","") // delete spaces
		          dateTimeString = dateTimeString.ReplaceAll("-","") // delete dashes
		          dateTimeString = dateTimeString.ReplaceAll(":","") // delete colons
		          dateTimeString = dateTimeString.Mid(3,10) // cut off the beginning and the end
		          // the result at this point should be YYMMDDHHMM
		          d = d.Child("LD"+ DateTimeString) // set up folder item for the enclosing folder
		          d.CreateFolder // create the folder
		          Var f As FolderItem
		          bs.ResizeTo(TheVNames.LastIndex) // make sure we have as many elements as vNames().
		          For i As Integer = 0 to TheVNames.LastIndex // go through all the variable names
		            f = d.Child(TheVNames(i) + ".lsb") // define a file for each
		            bs(i) = BinaryStream.Create(f) // create a binary stream for each
		          Next
		        Catch e As RuntimeException
		          CloseData // if we have created any streams, destroy them
		          Raise e  // re-raise the error
		        End Try
		      End If
		    End If
		    // Whether we are creating internal or external "files",
		    // create a clone of the supplied variable names list
		    VNames.ResizeTo(TheVNames.LastIndex)
		    For i As Integer = 0 To TheVNames.LastIndex
		      VNames(i) = TheVNames(i)
		    Next
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Destructor()
		  CloseData  // Make sure that any data files are closed
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetInternalDataFor(TheName As String) As Double()
		  // If we have no names to compare with, return nothing
		  If VNames.LastIndex = -1 Then Return Nil
		  // Look through the list of names
		  For i As Integer = 0 to VNames.LastIndex
		    If VNames(i) = TheName Then // if we have found our name
		      Return ms(i).GetData // return the corresponding data
		    End If
		  Next
		  // If we got all the way through the loop, then we found no match
		  Return Nil
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetNames() As String()
		  // Create a copy of the VNames array (so the user can't mess with the protected array)
		  Var theNames() As String
		  theNames.ResizeTo(VNames.LastIndex)
		  For i As Integer = 0 to VNames.LastIndex
		    theNames(i) = VNames(i)
		  Next
		  Return theNames
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub WriteDataToFile(dValues() As Double)
		  // This method writes a record consisting of a set of double values.
		  // There should be exactly one element in the dValues array for
		  // every element in the vNames array used in the CreateFolderResult
		  // method. If there are either more or fewer, then a RuntimeError will
		  // be generated.
		  
		  If bs.LastIndex =  -1 Then
		    Var e As New RuntimeException
		    e.Message = "WriteData: No output streams defined."
		    Raise e
		  Else
		    If  bs.LastIndex <> dValues.LastIndex Then
		      Var e As New RuntimeException
		      e.Message = "WriteData: More or fewer data items than streams"
		      Raise e
		    Else
		      Try
		        For i as Integer = 0 to dValues.LastIndex
		          bs(i).WriteDouble(dValues(i))
		        Next
		      Catch e As RuntimeException
		        CloseData // fatal error means that we should close out the files
		      End Try
		    End If
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub WriteDataToMemory(dValues() As Double)
		  // This method writes a record consisting of a set of double values.
		  // There should be exactly one element in the dValues array for
		  // every element in the vNames array used in the CreateFolderResult
		  // method. If there are either more or fewer, then a RuntimeError will
		  // be generated.
		  
		  If ms.LastIndex =  -1 Then
		    Var e As New RuntimeException
		    e.Message = "WriteData: No output streams defined."
		    Raise e
		  Else
		    If  ms.LastIndex <> dValues.LastIndex Then
		      Var e As New RuntimeException
		      e.Message = "WriteData: More or fewer data items than streams"
		      Raise e
		    Else
		      For i as Integer = 0 to dValues.LastIndex
		        ms(i).Write(dValues(i))
		      Next
		    End If
		  End If
		End Sub
	#tag EndMethod


	#tag Note, Name = Class Info
		This class can be used in a version of LISA to create a binary file of data.
		It writes data in blocks of binary 8 bytes to keep things fast.
		
		In the program's main window, define a property to hold an instance of this class.
		As you start a run (perhaps in the start button action event) create a new
		instance of the class, put it in the property, and call the class's CreateFolder
		method to create the output files and the folder that contains them (this way
		you can easily abort the run if something goes wrong). This method creates
		a folder whose name has the format "LDyymmddhhmm" where the small letters
		are replaced by the date and time the folder was created (so that each folder
		is unique) and a set of data files inside that folder. Each file will have the name
		specified by an element of the array passed to this method and a final extension
		of ".lsb". If there is some kind of error, a RuntimeException will be raised. If
		you want to handle these errors yourself, then enclose the call to CreateFolder
		with a Try-Catch structure.
		
		To be consistent with the corresponding DataReader class, if the variable is
		not unitless, provide its units by including a "-" after the variable name and
		following that character with the SI unit supplied. For example, if one is writing
		out time values in seconds (as will commonly be the case), then the vName
		value should be "t-s". The DataReader class will strip the units from the
		variable name at the hyphen and do unit conversions (if necessary) based
		on the supplied unit.
		
		Then, once per main program time step, one should call the WriteData method with
		an array containing the variable values at the current time step. It is your responsibililty
		to ensure that the array contains exactly as many values as there were variable names
		defined by the vNames array and that each value is in the right order so that the
		value corresponds to the variable name defined.
		
		The resulting data files will consist of a simple list of bytes. Each set of 8 bytes
		will correspond to a single double-precision value.
		
		Finally, before or as the program stops, one should call the CloseData method to finish
		the data file. This makes sure that internal buffers are emptied and the data is actually
		written out. This will happen automatically if a RuntimeException associated with creating
		or writing the file occurs during any of the methods of this class, but may not happen if a
		different RuntimeException occurs or if the user quits the program before the file is finished.
		To make sure the data is preserved, put in the App's CancelClose and UnhandledException
		events a call to the CancelClose method. (It is OK to execute this method many times: it
		will not do anything if called when the files are already closed.)
		
		This class can also "write" data to memory instead of to files in a disk drive. To do this, call
		the CreateFolder method with the optional InternalSize parameter. This parameter
		should specify the maximum possible number of doubles you plan to store for each variable name.
		(Make sure that you never write more doubles than you specify here.) The CreateFolder
		method will then reserve enough memory to save this information, essentially creating
		an "internal folder" in memory. Call the WriteData method with the optional Boolean
		parameter set to true to write the data to memory instead of to the hard drive. You can
		retrieve the data corresponding to a certain named variable by calling the GetInternalDataFor
		method with the name of the variable whose data you want (it will return Nil if the name is
		not found). You can also get the array of stored variable names using the GetNames method.
		
	#tag EndNote


	#tag Property, Flags = &h21
		Private bs() As BinaryStream
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ms() As MemoryStreamClass
	#tag EndProperty

	#tag Property, Flags = &h21
		Private VNames() As String
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
	#tag EndViewBehavior
End Class
#tag EndClass
