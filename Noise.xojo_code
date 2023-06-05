#tag Class
Protected Class Noise
	#tag Method, Flags = &h0
		Sub Constructor(fn As double, dτ As Double)
		  //  Class subroutine to generate the noise in LISA at each frequency
		  
		  Var stot, sb as Double
		  
		  stot = sqrt(sx*sx + (sa/(fn*fn*fn*fn))*(sa/(fn*fn*fn*fn)))
		  stot = stot*(1+(fn*fn)/fc2)
		  sb = b1/(fn^(1.9))
		  if fn >= fb1 then sb = b2/(fn^(7.5))
		  if fn >= fb2 then sb = b3/(fn^(2.6))
		  stot = sqrt(stot^2 + 4*sb*sb)
		  
		  //  This correction specifies the noise value that matches
		  //  the Benacquista data at f = 0.002 Hz.  Note that this is
		  //  ONLY valid at that frequency.
		  
		  sn = stot/(2*dτ) // return the calculated noise
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		sn As Double
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
		#tag ViewProperty
			Name="sn"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
