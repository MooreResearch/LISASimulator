#tag Class
Protected Class NoiseClass
	#tag Method, Flags = &h0
		Sub Constructor(dT As Double)
		  ΔT = dT
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
		  stot = sqrt(stot^2 + 4*sb*sb)
		  
		  //  This correction specifies the noise value that matches
		  //  the Benacquista data at f = 0.002 Hz.  Note that this is
		  //  ONLY valid at that frequency.
		  
		  Return stot/(2*ΔT) // return the calculated noise
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		ΔT As Double
	#tag EndProperty


	#tag Constant, Name = b1, Type = Double, Dynamic = False, Default = \"2.0654e-43", Scope = Public
	#tag EndConstant

	#tag Constant, Name = b2, Type = Double, Dynamic = False, Default = \"4.7315e-61", Scope = Public
	#tag EndConstant

	#tag Constant, Name = b3, Type = Double, Dynamic = False, Default = \"1.41254e-47", Scope = Public
	#tag EndConstant

	#tag Constant, Name = fb1, Type = Double, Dynamic = False, Default = \"7.08e-4", Scope = Public
	#tag EndConstant

	#tag Constant, Name = fb2, Type = Double, Dynamic = False, Default = \"1.778e-3", Scope = Public
	#tag EndConstant

	#tag Constant, Name = fc2, Type = Double, Dynamic = False, Default = \"1.0e-4", Scope = Public
	#tag EndConstant

	#tag Constant, Name = sa, Type = Double, Dynamic = False, Default = \"2.31e-52", Scope = Public
	#tag EndConstant

	#tag Constant, Name = sx, Type = Double, Dynamic = False, Default = \"1.257e-41", Scope = Public
	#tag EndConstant


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
