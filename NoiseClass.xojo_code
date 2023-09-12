#tag Class
Protected Class NoiseClass
	#tag Method, Flags = &h0
		Sub Constructor(dT As Double)
		  ΔT = dT
		  b1 = 2.0654e-43
		  b2 = 4.7315e-61
		  b3 = 1.4125e-47
		  fb1 = 7.08e-4
		  fb2 = 1.778e-3
		  fc2 = 1.0e-3
		  sa = 2.31e-52
		  sx = 1.257e-41
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetNoise(fn As Double) As Double
		  //  Class subroutine to generate the noise in LISA at each frequency
		  //  This dates from a very early version of the code.
		  
		  Var stot, sb as Double
		  stot = sqrt(sx*sx + (sa/(fn*fn*fn*fn))*(sa/(fn*fn*fn*fn)))
		  stot = stot*(1+(fn*fn)/fc2)
		  sb = b1/(fn^(1.9))
		  If fn >= fb1 Then sb = b2/(fn^(7.5))
		  If fn >= fb2 Then sb = b3/(fn^(2.6))
		  stot = sqrt(stot*stot + 4*sb*sb)
		  
		  //  This correction specifies the noise value that matches
		  //  the Benacquista data at f = 0.002 Hz.  Note that this is
		  //  ONLY valid at that frequency.
		  
		  Return stot/(2*ΔT) // return the calculated noise
		  
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		b1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		b2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		b3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		fb1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		fb2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		fc2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		sa As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		sx As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΔT As Double
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
			Name="ΔT"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="b1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="b2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="b3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="fb1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="fb2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="fc2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="sa"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="sx"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
