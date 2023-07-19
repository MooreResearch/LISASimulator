#tag Class
Protected Class CurrentValuesClass
	#tag Method, Flags = &h0
		Sub Set2Interpolation(VN as CurrentValuesClass, VP as CurrentValuesClass, FracFromP as Double)
		  Var FracFromN As Double = 1.0 - FracFromP
		  me.V = VN.V*FracFromN + VP.V*FracFromP
		  me.ι = VN.ι*FracFromN + VP.ι*FracFromP
		  me.α = VN.α*FracFromN + VP.α*FracFromP
		  me.τr = VN.τr*FracFromN + VP.τr*FracFromP
		  me.Ψr = VN.Ψr*FracFromN + VP.Ψr*FracFromP
		  me.χa.X = VN.χa.X*FracFromN + VP.χa.X*FracFromP
		  me.χa.Y = VN.χa.Y*FracFromN + VP.χa.Y*FracFromP
		  me.χa.Z = VN.χa.Z*FracFromN + VP.χa.Z*FracFromP
		  me.χs.X = VN.χs.X*FracFromN + VP.χs.X*FracFromP
		  me.χs.Y = VN.χs.Y*FracFromN + VP.χs.Y*FracFromP
		  me.χs.Z = VN.χs.Z*FracFromN + VP.χs.Z*FracFromP
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetAsCopyOf(VN as CurrentValuesClass)
		  // This just copies all the values from the source
		  me.V = VN.V
		  me.ι = VN.ι
		  me.α = VN.α
		  me.τr = VN.τr
		  me.Ψr = VN.Ψr
		  me.χa.X = VN.χa.X
		  me.χa.Y = VN.χa.Y
		  me.χa.Z = VN.χa.Z
		  me.χs.X = VN.χs.X
		  me.χs.Y = VN.χs.Y
		  me.χs.Z = VN.χs.Z
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		V As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ι As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		α As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		τr As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χa As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χs As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		Ψr As Double
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
			Name="V"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="α"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ι"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Ψr"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="τr"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
