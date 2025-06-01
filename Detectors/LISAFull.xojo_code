#tag Class
Protected Class LISAFull
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
		  Var c012 As Double = Cos(σ1 - 2.0*oneΦ)
		  Var c412 As Double = Cos(4.0*ρ - σ1 -twoΦ)
		  Var c311 As Double = Cos(3.0*ρ - σ1 - oneΦ)
		  Var c111 As Double = Cos(ρ - σ1 + oneΦ)
		  
		  Var dp As Double = Dpc1*(-6.0*s210 + 9.0*s012 - s412) _
		  + Dpc2*C2Θ*(18.0*s210 + 9.0*s012 - s412) _
		  - Dpc3*S2Θ*(s311 - 3.0*s111)
		  Var dx As Double = Dxc1*CΘ*(9.0*c012 - c412) _
		  - Dxc2*SΘ*(c311 - 3.0*c111)
		  
		  Var fp1 As Double = Cos2ψ*dp - Sin2ψ*dx
		  Var fx1 As Double = Sin2ψ*dp + Cos2ψ*dx
		  
		  // repeat the whole thing again for detector 2
		  
		  s210 = Sin(2.0*ρ - σ2)
		  s012 = Sin(σ2 - twoΦ)
		  s412 = Sin(4.0*ρ - σ2 - twoΦ)
		  s311 = Sin(3.0*ρ - σ2 - oneΦ)
		  s111 = Sin(ρ - σ2 - oneΦ)
		  c012 = Cos(σ2 - twoΦ)
		  c412 = Cos(4.0*ρ - σ2 - twoΦ)
		  c311 = Cos(3.0*ρ - σ2 - oneΦ)
		  c111 = Cos(ρ - σ2 + oneΦ)
		  
		  dp = Dpc1*(-6.0*s210 + 9.0*s012 - s412) _
		  + Dpc2*C2Θ*(18.0*s210 + 9.0*s012 - s412) _
		  - Dpc3*S2Θ*(s311 - 3.0*s111)
		  dx = Dxc1*CΘ*(9.0*c012 - c412) - Dxc2*SΘ*(c311 - 3.0*c111)
		  Var fp2 As Double = Cos2ψ*dp - Sin2ψ*dx
		  Var fx2 As Double = Sin2ψ*dp + Cos2ψ*dx
		  
		  Return Array(fp1 + fp2, fx1 + fx2)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetName() As String
		  Return "LISAFull"
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetNoise(Freq As Double) As Double
		  // Part of the DetectorInterface interface.
		  // This computes the LISA noise as a function of frequency
		  Var ePlus As Double = Exp(1680*(0.00215-Freq))
		  Var eMin As Double = 1/ePlus
		  Var tanhArg As Double = (ePlus-eMin)/(ePlus + eMin)
		  Var Sc As Double
		  
		  If Freq < 0.015 then 
		    Sc = (9e-45)*Freq^2.33333333333333 * Exp(-0.171*Freq + 292*Freq*sin(1020*Freq))*(1 + tanhArg)
		  else
		    Sc = 0 
		  end if 
		  
		  Var PACC As Double = (9e-30)*(1.0+(0.0004/Freq)*(0.0004/Freq))
		  Var ω4 As Double = 2*π*Freq
		  ω4 =ω4*ω4
		  ω4 = ω4*ω4
		  
		  Var Sn As Double = (5.33e-19)*(2.25e-22 + 4.0*PACC/ω4)*(1.0+1644.7*Freq*Freq)+Sc
		  
		  Return Sn/(2*Params.ΔT)
		  
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
		  σ2 = σ1 + 4*π/3
		  
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

	#tag Property, Flags = &h21
		Private σ2 As Double
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
