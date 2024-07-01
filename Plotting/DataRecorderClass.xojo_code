#tag Class
Protected Class DataRecorderClass
	#tag Method, Flags = &h0
		Sub CloseData()
		  // This method closes any data files that might exist.
		  If Bs.LastIndex > -1 Then
		    For i As Integer = 0 to Bs.LastIndex
		      if Bs(i) <> nil Then
		        Bs(i).Close
		        Bs(i) = Nil
		      end if
		    Next
		  End If
		  If Ms.LastIndex > -1 Then Ms.ResizeTo(-1)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub CreateFolder(TheVNames() as String, InternalSize as Integer = -1)
		  // This method creates a folder for the data items to be saved and initializes
		  // binary streams for each of the items to be saved. It returns "OK" if all
		  // went well, and an error message otherwise.
		  
		  // The following if statement ensures that we have some variable names.
		  If TheVNames.LastIndex = -1 Then
		    Var e as New RuntimeException
		    e.Message = "CreateFolder: No variable names supplied."
		    Raise e
		  Else // we have names
		    //  so create a clone of the supplied variable names list
		    VNames.ResizeTo(TheVNames.LastIndex)
		    For i As Integer = 0 To TheVNames.LastIndex
		      VNames(i) = TheVNames(i)
		    Next
		    If InternalSize < 0 Then // if we are writing to hard disk
		      Try
		        Var d As New FolderItem("") // get directory containing the application
		        Var dateTimeOfNow As DateTime = DateTime.Now // current date
		        Var dateTimeString As String = dateTimeOfNow.SQLDateTime
		        dateTimeString = dateTimeString.ReplaceAll(" ","") // delete spaces
		        dateTimeString = dateTimeString.ReplaceAll("-","") // delete dashes
		        dateTimeString = dateTimeString.ReplaceAll(":","") // delete colons
		        dateTimeString = dateTimeString.Mid(3,12) // cut off the beginning and the end
		        // the result at this point should be YYMMDDHHMMSS
		        d = d.Child("LD"+ DateTimeString) // set up folder item for the enclosing folder
		        d.CreateFolder // create the folder
		        Var f As FolderItem
		        Bs.ResizeTo(TheVNames.LastIndex) // make sure we have as many elements as vNames().
		        For i As Integer = 0 to TheVNames.LastIndex // go through all the variable names
		          f = d.Child(TheVNames(i) + ".lsb") // define a file for each
		          Bs(i) = BinaryStream.Create(f) // create a binary stream for each
		        Next
		      Catch e As RuntimeException
		        CloseData // if we have created any streams, destroy them
		        Raise e  // re-raise the error
		      End Try
		      
		      If Bs.LastIndex = -1 Then // if no binary streams are created after this
		        Var e as New RuntimeException
		        e.Message = "CreateFolder: No files created."
		        Raise e
		      End If
		    Else // we must be storing data to memory instead
		      Ms.ResizeTo(TheVNames.LastIndex)
		      For i As Integer = 0 to TheVNames.LastIndex
		        Ms(i) = New MemoryStreamClass(TheVNames(i), InternalSize)
		      Next
		    End If
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Destructor()
		  CloseData  // Make sure that any data files are closed
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetDataFor(TheName As String) As Double()
		  // This method reads the array of values for the variable name.
		  // It will return Nil if the variable name is not found (this would
		  // probably indicate a typo). If for some reason the name is found
		  // but its stream is Nil, the method will also return Nil.
		  // If some kind of IO error occurs during reading, the method will
		  // raise an exception. The calling code should
		  // deal with such exceptions somehow.
		  
		  // If we have no names to compare with, return nothing
		  If VNames.LastIndex = -1 Then Return Nil
		  Var foundItemIndex As Integer = -1
		  // Look through the list of names
		  For i As Integer = 0 to VNames.LastIndex
		    If VNames(i) = TheName Then // if we have found our name
		      foundItemIndex = i
		    End If
		  Next
		  // If we got all the way through the loop and found nothing, return Nil
		  If foundItemIndex < 0 Then Return Nil
		  
		  Var theValues() As Double // define the array to contain the values
		  If Bs.LastIndex < 0 Then // if we have no files open, we must be saving to memory
		    If Ms.LastIndex < 0 Then // if we also have no memory items, then that is an error
		      Return Nil
		    Else
		      Return Ms(foundItemIndex).GetData // return the corresponding data
		    End If
		  Else // we must be looking for data from a disk file
		    Var theStream As BinaryStream = Bs(foundItemIndex) // get a local reference to the stream
		    Var theLength As Int64 = theStream.Length // determine the file length in bytes
		    Var doubCount As Integer = theLength\8 // this will be the number of doubles in the file
		    theValues.ResizeTo(doubCount-1) // resize the array to match
		    theStream.BytePosition = 0 // set this back to zero in case we've read the file before
		    For i As Integer = 0 to doubCount - 1
		      theValues(i) = theStream.ReadDouble // fill the array with read values
		    Next
		    Return theValues // return the finished array
		  End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetVariableNames() As String()
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
		Sub WriteData(Names() As String, Values() As Double, NumberOfDoubles As Integer = -1)
		  // This method writes a record consisting of a set of double values.
		  // The first time this method is called, it sets up a folder of files on disk,
		  // or if the optional parameter specifying the number of doubles to store
		  // is supplied, data is written internally to memory. 
		  
		  If Bs.LastIndex = -1 and Ms.LastIndex = -1 Then // if this is the first call to this function
		    // Make sure that there is a one-to-one correspondance between
		    // variable names and data items provided
		    If Names.LastIndex <> Values.LastIndex Then // if the arrays do not have equal length
		      Var e as New RuntimeException  // that is an error
		      e.Message = "WriteData: The name and value lists are different sizes"
		      Raise e
		    Else // if we are good, create a folder (or a set of memory streams)
		      CreateFolder(Names, NumberOfDoubles)
		    End If
		  End If
		  
		  If Ms.LastIndex =  -1 Then // If we have no memory streams, we must be writing data to disk
		    If Bs.LastIndex = -1 Then // if we also have no binary streams
		      Var e as New RuntimeException  // that is an error
		      e.Message = "WriteData: No output streams defined for data"
		      Raise e
		    Else
		      Try
		        For i as Integer = 0 to Values.LastIndex
		          Bs(i).WriteDouble(Values(i))
		        Next
		      Catch e As RuntimeException
		        CloseData // fatal error means that we should close out the files
		        Raise e // re-raise the error so that the calling program knows.
		      End Try
		    End If
		  Else // we have no binary streams, so we are writing data to memory
		    If Ms.LastIndex = -1 Then // if we also have no memory streams
		      Var e as New RuntimeException  // that is an error
		      e.Message = "WriteData: No output streams defined for data"
		      Raise e
		    Else // otherwise, write the data to the memory streams
		      For i as Integer = 0 to Values.LastIndex
		        Ms(i).Write(Values(i))
		      Next
		    End If
		  End If
		  
		End Sub
	#tag EndMethod


	#tag Note, Name = Class Info
		This class can be used in a version of LISA to create a binary file of data.
		It writes data in blocks of binary 8 bytes to keep things fast. It alternatively
		can write data to memory instead of to hard disk files.
		
		In some place accessible to the routines generating data that you want to save,
		define a property to hold an instance of this class. As you start a run create a new
		instance of the class, and put it in the property.
		
		Then, EXACTLY ONCE per main program time step, one should call the WriteData
		method with two arrays: one containing a list of variable names, and containing the
		variable values at the current time step. If either list is longer than the other,
		a RuntimeError is generated.
		
		if the variable is not unitless, provide its units by including a "-" in the variable
		name in the variable name list and follow that character with the appropriate unit.
		For example, one should ALWAYS include a time variable, which can be named either
		"t-s" or "t-y" depending on whether the time supplied has unit of seconds or years.
		
		DO NOT call the WriteData method in multiple places with different lists of variable
		names. Executions of the method after the first will ignore the name list, and generate
		an error if the array of values does not have the same number of elements as the saved
		the original call to this method did.
		
		(If you really need to do multiple calls with different names during a cycle, then define
		a separate instance of the class for each dataset you need to save.)
		
		If you wre writing data to disk, the method creates a folder whose name has the
		format "LDyymmddhhmmss" where the small letters are replaced by the date and
		time the folder was created (so that each folder is unique) and a set of data files
		inside that folder. Each file will have the name specified by an element of the array
		passed to this method and a final extension of ".lsb".
		
		You can save data to memory by including the optional integer parameter in this
		call. This integer specifies the maximum number of doubles that you expect to
		write for a given variable during a run (generally, the maximum number of steps the
		program will execute). This reserves sufficient space in the array at the beginning
		(building arrays by adding an item at a time takes time and is an inefficient way to use
		memory).
		
		If there is some kind of error, this method raises a RuntimeException.To handle these
		errors in the main program, enclose the call in a Try-Catch structure.
		
		Finally, just after the program run stops, one should call the CloseData method to finish
		the data file. This is essential for data saved to a disk file, as it makes sure that internal buffers
		are emptied and the data is actually written out if. The method is called automatically if a
		RuntimeException associated with creating or writing a file occurs, and also when the class
		itself goes out of scope. But it is OK to execute this method many times (it will not do anything
		if called when the files are already closed), so feel free to use it freely anywhere you like.
		
		To retrieve saved data, call the GetDataFor method with the name of the variable you 
		are looking for. It returns Nil if the name was not found or there was another kind of
		problem. The GetVariableNames method returns the list of variable names (which again
		will be Nil if there aren't any.
		
		
	#tag EndNote


	#tag Property, Flags = &h21
		Private Bs() As BinaryStream
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Ms() As MemoryStreamClass
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
