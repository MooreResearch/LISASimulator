#tag Class
Protected Class WaveAmplitudesTrigFunctions
	#tag Method, Flags = &h0
		Sub Constructor(ιDN As Double, β As Double)
		  updateTrig(ιDN,β)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub updateTrig(ιDN As Double, β As Double)
		  self.ιDN = ιDN
		  self.β = β
		  c2 = Cos(ιDN)
		  s2 = Sin(ιDN)
		  c1 = Cos(0.5*ιDN)
		  s1   = Sin(0.5*ιDN)
		  c3   = c2*c1 - s2*s1
		  s3   = s2*c1 + c2*s1
		  c4   = c2*c2-s2*s2
		  s4   = 2*c2*s2
		  c5   = c4*c1 - s4*s1
		  s5   = s4*c1 + c4*s1
		  c6   = c5*c1 - s5*s1
		  s6   = s5*c1 + c5*s1
		  c7   = c6*c1 - s6*s1
		  s7   = s6*c1 + c6*s1
		  c8   = c7*c1 - s7*s1
		  s8   = s7*c1 + c7*s1
		  c9   = c8*c1 - s8*s1
		  s9   = s8*c1 + c8*s1
		  c10   = c9*c1 - s9*s1
		  s10   = s9*c1 + c9*s1
		  c1p2   = c1*c1
		  c1p3   = c1p2*c1
		  c1p4   = c1p3*c1
		  c1p5   = c1p4*c1
		  c1p6   = c1p5*c1
		  c1p7   = c1p6*c1
		  c1p8   = c1p7*c1
		  c1p9   = c1p8*c1
		  c1p10   = c1p9*c1
		  s1p2   = s1*s1
		  s1p3   = s2*s1
		  s1p4   = s3*s1
		  s1p5   = s4*s1
		  s1p6   = s5*s1
		  s1p7   = s6*s1
		  s1p8   = s7*s1
		  s1p9   = s8*s1
		  s1p10   = s9*s1
		  c2p2   = c2*c2
		  c2p3   = c2p2*c2
		  c2p4   = c2p3*c2
		  s2p2   = s2*s2
		  s2p3   = s2p2*s2
		  s2p4   = s2p3*s2
		  s2p5   = s2p4*s2
		  
		  // Define local β trig functions
		  cβ   = Cos(β)
		  sβ   = Sin(β)
		  c2β   = cβ*cβ - sβ*sβ
		  s2β   = 2*sβ*cβ
		  c3β   = c2β*cβ - s2β*sβ
		  s3β   = s2β*cβ + c2β*sβ
		  c4β   = c3β*cβ - s3β*sβ
		  s4β   = s3β*cβ + c3β*sβ
		  c5β   = c4β*cβ - s4β*sβ
		  s5β   = s4β*cβ + c4β*sβ
		  cβ2   = cβ*cβ
		  cβ3   = c2β*cβ
		  sβ2   = sβ*sβ
		  sβ3   = s2β*sβ
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		c1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c10 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1p10 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1p2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1p3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1p4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1p5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1p6 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1p7 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1p8 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c1p9 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2p2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2p3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2p4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c2β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c3β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c4β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c5β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c6 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c7 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c8 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		c9 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		cβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		cβ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		cβ3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s10 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s1p10 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s1p2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s1p3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s1p4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s1p5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s1p6 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s1p7 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s1p8 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s1p9 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s2p2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s2p3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s2p4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s2p5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s2β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s3β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s4β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s5β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s6 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s7 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s8 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		s9 As Double
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

	#tag Property, Flags = &h0
		ιDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		β As Double
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
			Name="ιDN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c4"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c5"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c6"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c7"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c8"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c9"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c10"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s4"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s5"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s6"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s7"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s8"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s9"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s10"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1p2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1p3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1p4"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1p5"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1p6"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1p7"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1p8"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1p9"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1p10"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s1p2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s1p3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s1p4"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s1p5"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s1p6"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s1p7"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s1p8"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s1p9"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s1p10"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2p2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2p3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2p4"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s2p2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s2p3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s2p4"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="s2p5"
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
			Name="cβ2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="cβ3"
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
			Name="c3β"
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
			Name="c5β"
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
	#tag EndViewBehavior
End Class
#tag EndClass
