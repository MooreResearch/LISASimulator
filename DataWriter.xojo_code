#tag Class
Protected Class DataWriter
	#tag Method, Flags = &h0
		Sub CloseData()
		  // This method closes the data file
		  if bs <> nil then bs.Close
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function OpenFileResult() As String
		  // This method opens a binary file and writes the list of variable names to it
		  // as a header. No variable name should use a vertical bar | because that
		  // character is used as a delimiter. The method uses
		  // a dialog box to get the user's choice for a name/place to save the file, and
		  // returns a string, which will be "OK" if everything went well, and some kind
		  // of error message otherwise.
		  
		  Var f As FolderItem = FolderItem.ShowSaveFileDialog(FileTypes.LISAData, "Untitled.lsd")
		  // create a file (f) 
		  If f <> Nil Then    // if the file is correctly created...
		    bs = BinaryStream.Create(f,True) // Overwrite if exists. This step determines that the 
		    // content of the file is a binary stream.
		    Return "OK"
		  else
		    Return "Nil FolderItem"
		  End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub WriteData(dValues() as Double)
		  // This method writes a record consisting of a set of double values.
		  // There should be exactly one element in the dValues array for
		  // every element in the vNames array used in the WriteHeaderResult
		  // method.
		  if bs <> Nil then
		    For i as Integer = 0 to dValues.LastIndex
		      bs.WriteDouble(dValues(i))
		    Next
		  End if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function WriteHeader(vNames() as string) As string
		  if bs <> Nil then    // if the stream is created....
		    // we will create a header of the form "name1|name2|...| ", with at least one
		    // space after the final |. When the reader re-splits this string, it will throw
		    // away the last item
		    Var theHeader as String = String.FromArray(vNames, "|") + "| "
		    While (theHeader.LenB mod 8) <> 0  // pad with spaces to make      
		      theHeader = theHeader + " "  // the length in bytes divisible by 8
		    Wend     // .LenB: returns the number of bytes in the specified string 
		    bs.WriteInt64(theHeader.LenB) // First 8 bytes is the header's length in bytes
		    bs.Write(theHeader) // then write out the header string
		    Return "OK"
		  else
		    Return "Nil Stream"
		  End If
		End Function
	#tag EndMethod


	#tag Note, Name = Class Info
		This class can be used in a version of LISA to create a binary file of data.
		It writes data in blocks of 8 bytes to keep things fast.
		
		In LISA's main window, define a property to hold an instance of this class.
		As you start a run (perhaps in the start button action event) create a new
		instance of the class, put it in the property, and call the class's OpenFileResult
		method to create the output file (this way you can easily abort the run if something
		goes wrong). This method will show a dialog box to allow the user to define a file
		name for the data file to be created. If the user chooses a file that already exists,
		that file will be overwritten (so be careful). This method returns a string, which is
		"OK" if everything went well and some kind of error message otherwise.
		
		Somewhere as you initialize the run, call the class's WriteHeader method with
		an array of strings that specify the variable names that you will write into the file.
		
		Now, once per main program time step, one should call the WriteData method with
		an array containing the variable values at the current time step. It is your responsibililty
		to ensure that the array contains exactly as many values as there were variable names
		defined by the WriteHeaderResult and that each value is in the right order so that the
		value corresponds to the variable name defined.
		
		Finally, before the program stops, one should call the CloseData method to finish
		the data file. This makes sure that internal buffers are emptied and the data is actually
		written out.
		
	#tag EndNote


	#tag Property, Flags = &h21
		Private bs As BinaryStream
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
