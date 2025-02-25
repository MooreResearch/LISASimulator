#tag Class
Protected Class BetaFuncsClass
	#tag Method, Flags = &h0
		Sub SetDerivs(theβ As Double)
		  c0β = 0.0
		  
		  // initially set the following to their real values
		  cβ = cos(theβ)
		  sβ = sin(theβ)
		  c2β = cos(2.0*theβ)
		  c3β = cos(3.0*theβ)
		  c4β = cos(4.0*theβ)
		  s2β = sin(2.0*theβ)
		  s3β = sin(3.0*theβ)
		  s4β = sin(4.0*theβ)
		  
		  // now set derivatives of the other functions
		  c2β2sβ3 = -4.0*c2β*s2β*sβ*sβ*sβ + 3.0*c2β*c2β*sβ*sβ*cβ
		  c2βs2β = -2.0*s2β*s2β + 2.0*c2β*c2β
		  c2βsβ = -2.0*s2β*sβ +  c2β*cβ
		  c2βsβ2 = -2.0*s2β*sβ +  2.0*c2β*sβ*cβ
		  c2βsβ3 = -2.0*s2β*sβ +  3.0*c2β*sβ*sβ*cβ
		  c3βsβ2 = -2.0*sin(3.0*theβ)*sβ*sβ + 2.0*c3β*sβ*cβ
		  c4βsβ = -4.0*s4β*sβ + c4β*cβ
		  c5β = -5.0*sin(5.0*theβ)
		  cβ2 = 2.0*cβ*sβ
		  cβ2sβ = 2.0*cβ*sβ*sβ  + cβ2*cβ
		  cβ3sβ = 3.0*cβ*cβ*sβ*sβ  + cβ*cβ*cβ*cβ
		  cβc2β = -sβ*c2β - 2.0*cβ*s2β
		  cβc2βsβ2 = -sβ*c2β*sβ*sβ - 2.0*cβ*s2β*sβ*sβ +  2.0*cβ*c2β*sβ*cβ
		  cβc4β = -sβ*c4β + 4.0*cβ*s4β
		  cβs3β  = -sβ*s3β + 3.0*cβ*c3β
		  cβsβ = -sβ*sβ + cβ*cβ
		  cβsβ2 = -sβ*sβ*sβ + 2.0*cβ*sβ*cβ
		  cβsβ3 = -sβ*sβ*sβ*sβ + 3.0*cβ*sβ*sβ*cβ
		  s5β = 5.0*cos(5.0*theβ)
		  sβ2 = 2.0*sβ*cβ
		  sβ3 = 3.0*sβ*sβ*cβ
		  
		  // now calculate the derivatives of the basic stuff
		  cβ = -sin(theβ)
		  sβ = cos(theβ)
		  c2β = -2.0*sin(2.0*theβ)
		  c3β = -3.0*sin(3.0*theβ)
		  c4β = -4.0*sin(4.0*theβ)
		  s2β = 2.0*cos(2.0*theβ)
		  s3β = 3.0*cos(3.0*theβ)
		  s4β = 4.0*cos(4.0*theβ)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetValues(theβ As Double)
		  c0β = 1.0
		  cβ = cos(theβ)
		  sβ = sin(theβ)
		  c2β = cos(2.0*theβ)
		  s2β = sin(2.0*theβ)
		  s3β = sin(3.0*theβ)
		  c2β2sβ3 = c2β*c2β*sβ*sβ*sβ
		  c2βs2β = c2β*s2β
		  c2βsβ = c2β*sβ
		  c2βsβ2 = c2βsβ*sβ
		  c2βsβ3 = c2βsβ2*sβ
		  c3βsβ2 = cos(3.0*theβ)
		  c4β = cos(4.0*theβ)
		  c4βsβ = c4β*sβ
		  c5β = cos(5.0*theβ)
		  cβ2 = cβ*cβ
		  cβ2sβ = cβ2*sβ 
		  cβ3sβ = cβ2*cβ*sβ 
		  cβc2β = cβ*c2β
		  cβc2βsβ2 = cβc2β*sβ*sβ
		  cβc4β = cβ*c4β
		  cβs3β  = cβ*s3β
		  cβsβ = cβ*sβ
		  cβsβ2 = cβsβ*sβ 
		  cβsβ3 = cβsβ2*sβ
		  s4β = sin(4.0*theβ)
		  s5β = sin(5.0*theβ)
		  sβ2 = sβ*sβ
		  sβ3 = sβ2*sβ
		  
		  
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		c0β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2β2sβ3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2βs2β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2βsβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2βsβ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2βsβ3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c3β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c3βsβ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c4β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c4βsβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c5β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		cβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		cβ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		cβ2sβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		cβ3sβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		cβc2β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		cβc2βsβ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		cβc4β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		cβs3β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		cβsβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		cβsβ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		cβsβ3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s2β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s3β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s4β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s5β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		sβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		sβ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		sβ3 As Double
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
			Name="c0β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="cβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2βs2β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2βsβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2βsβ2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2βsβ3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c3βsβ2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c4β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c4βsβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c5β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="cβ2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="cβ2sβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="cβ3sβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="cβc2β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="cβc2βsβ2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="cβc4β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="cβs3β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="cβsβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="cβsβ2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="cβsβ3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s2β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s3β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s4β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s5β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="sβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="sβ2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="sβ3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2β2sβ3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c3β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
