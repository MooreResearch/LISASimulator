#tag Class
Class NecdetsClass
	#tag Method, Flags = &h0
		Sub Constructor()
		  // Initialize arrays if required
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetValues(Wavebuilders() As WaveBuilderClass, ep As Double)
		  If Wavebuilders.Ubound >= 1 Then
		    For i As Integer = 0 To 250  // Assuming 250 elements
		      nA(i) = (Wavebuilders(1).A(i) - Wavebuilders(2).A(i)) / (2.0 * ep)
		    Next
		  Else
		    Raise New RuntimeException("Not enough Wavebuilders instances to calculate nA(i).")
		  End If
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		nA(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ndAdι(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ndAdβ(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ndAdδ(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ndAdχax(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ndAdχay(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ndAdχaz(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ndAdχsx(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ndAdχsy(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ndAdχsz(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDVI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDιI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDαI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDχaxI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDχayI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDχazI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDχsxI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDχsyI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDχszI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDΨI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nVDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nιDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nαDN As Double
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
			Name="nVDN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="nιDN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="nαDN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
