#tag Class
Protected Class LISAHalf
Implements DetectorInterface
	#tag Method, Flags = &h0
		Function GetFPAndFX(Tau As Double) As Double()
		  // Part of the DetectorInterface interface.
		  Var ρ As Double = Params.GMΩe*Tau
		  Var oneΦ  As Double = Params.Φ
		  Var twoΦ As Double = 2*oneΦ
		  Var s210 As Double = Sin(2.0*ρ - σ1)
		  Var s012 As Double = Sin(σ1 - twoΦ)
		  Var s412 As Double = Sin(4.0*ρ - σ1 - twoΦ)
		  Var s311 As Double = Sin(3.0*ρ - σ1 - oneΦ)
		  Var s111 As Double = Sin(ρ - σ1 + oneΦ)
		  Var c012 As Double = Cos(σ1 - twoΦ)
		  Var c412 As Double = Cos(4.0*ρ - σ1 -twoΦ)
		  Var c311 As Double = Cos(3.0*ρ - σ1 - oneΦ)
		  Var c111 As Double = Cos(ρ - σ1 + oneΦ)
		  
		  Var dp As Double = Dpc1*(-6.0*s210 + 9.0*s012 - s412) _
		  + Dpc2*C2Θ*(18.0*s210 + 9.0*s012 - s412) _
		  - Dpc3*S2Θ*(s311 - 3.0*s111)
		  Var dx As Double = Dxc1*CΘ*(9.0*c012 - c412) _
		  - Dxc2*SΘ*(c311 - 3.0*c111)
		  
		  Return Array(Cos2ψ*dp - Sin2ψ*dx, Sin2ψ*dp + Cos2ψ*dx)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetName() As String
		  Return "LISAHalf"
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetNoise(Freq As Double) As Double
		  // Part of the DetectorInterface interface.
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Initialize(Parameters As CaseInfoClass)
		  // Part of the DetectorInterface interface.
		  Params = Parameters
		  
		  // Initialize detector constants
		  Cos2ψ = Cos(2*Params.ψ)
		  Sin2ψ = Sin(2*Params.ψ)
		  σ1 = 1.5*π + 2*Params.ρ0
		  
		  SΘ = Sin(Params.Θ)
		  CΘ= Cos(Params.Θ)
		  S2Θ = 2*sΘ*cΘ
		  C2Θ = cΘ*cΘ - sΘ*sΘ
		  
		  Dpc2 = Sqrt(3)/128
		  Dpc1 = 3.0*Dpc2
		  Dpc3 = -3/32
		  
		  Dxc1 = 1/32
		  Dxc2 = -3.0*Dxc1
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h21
		Private C2Θ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Cos2ψ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private CΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dpc1 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dpc2 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dpc3 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dxc1 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dxc2 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Params As CaseInfoClass
	#tag EndProperty

	#tag Property, Flags = &h21
		Private S2Θ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Sin2ψ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private SΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private σ1 As Double
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
