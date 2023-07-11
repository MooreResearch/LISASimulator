#tag Class
Protected Class VEvolverClass
	#tag Method, Flags = &h0
		Sub Constructor(Parameters As CaseParametersClass, Dτ0 As Double, IsSideCase As Boolean = False)
		  // This method initializes a VEvolver case.
		  // Each case has its own associated SpinEvolverClass instance
		  SpinEvolver = New SpinEvolverClass(Parameters, Dτ0)
		  // Initialize some properties of the class. Note that the derivative
		  // terms need no initialization, since they are all zero initially.
		  // Also γE initializes itself.
		  V0 = Parameters.V0
		  InverseZPlus1 = 1.0/(Parameters.Z + 1)
		  VN = V0
		  VP = VN
		  SideCase = IsSideCase
		  
		  // Initialize constants that will be used in the evolution equations.
		  Var δ As Double = Parameters.δ
		  Var η As Double = 0.25*(1 - δ*δ)
		  Var π As Double = Parameters.π
		  Var γE As Double = 0.5772156649015328606
		  Var χs𝓁 As Double = SpinEvolver.χs𝓁
		  Var χa𝓁 As Double = SpinEvolver.χa𝓁
		  C0 = 32*η/5
		  C2 = -743/336 - 11*η/4
		  C3 = 4*π - 47*χs𝓁/3 - δ*25*χa𝓁/4
		  C4 = 34103/18144 + 13661*η/2016 + 59*η*η/18
		  C5 = (-5861/144 + 1001*η/12)*χs𝓁 + δ*(-809/84 + 281*η/8)*χa𝓁
		  C5 = C5 + 4159*π/672 + 189*π*η/8
		  C6 = 16477322263.0/139708800 - 1712*γE/105 + 16*π*π/3
		  C6 = C6 + (-56198689/217728 + 451*π*π/48)*η
		  C6 = C6 + 541*η*η/896 - 5605*η*η*η/2592 - 856*Log(32)/105
		  C6L = 856/105
		  C7 = π*(-4415/4032 + 358675*η/6048 + 91495*η*η/1512)
		  CSD0 = C0
		  CSD1 = -47/3
		  CSD2 = -5861/144 + 1001*η/12
		  CAD0 = C0*δ
		  CAD1 = -25/4
		  CAD2 = -809/84 + 281*η/8
		  
		  // Do a trial first step to determine DτIdeal. (We will take the real
		  // first step later.) Note that taking a half-step with VN = VP is the
		  // same as an Euler step.
		  DoStep(0.5*Dτ0, 0.5*Dτ0)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoStep(DτF As Double, DτP As Double)
		  Var dτRatio As Double = DτF/DτP // calculate this ratio once so we don't have to do it many times
		  Var oneMinusRatio As Double = 1.0 - DτRatio // Calculate this only once also
		  Var twoDτF As Double = 2.0*DτF
		  // Calculate new past values using interpolation (note that this effectively does nothing if DτF/DτP = 1,
		  // but it is probably faster just to do the calculation than to do a check and then a calculation
		  VP = oneMinusRatio*VN + dτRatio*VP
		  VDotN = GetVDot(VN)
		  VF = VP + twoDτF*VDotN
		  SpinEvolver.DoStep(DτF, DτP, VN) // Do spin evolution step also
		  αDotN = SpinEvolver.αDotN // Get the current value of αDot
		  If not SideCase Then // If this is the main case, we will also calculate some derivatives
		    DvDz = -InverseZPlus1*(VN-V0)
		    // Calculate the adjusted past value of DvDχa𝓁 and compute its future value
		    DvDχa𝓁P = OneMinusRatio*DvDχa𝓁N + DτRatio*DvDχa𝓁P
		    DvDχa𝓁F = DvDχa𝓁P + TwoDτF*CAD0*VN^12*(CAD1 + CAD2*VN*VN)
		    // Calculate the adjusted past value of DvDχs𝓁 and compute its future value
		    DvDχs𝓁P = OneMinusRatio*DvDχs𝓁N + DτRatio*DvDχs𝓁P
		    DvDχs𝓁F = DvDχs𝓁P + TwoDτF*CSD0*VN^12*(CSD1 + CSD2*VN*VN)
		    // Use these to calculate the derivatives with respect to the spin parameters
		    DvDχ10x = DvDχa𝓁N*SpinEvolver.DχaDχ10x + DvDχs𝓁N*SpinEvolver.DχsDχ10x
		    DvDχ10y = DvDχa𝓁N*SpinEvolver.DχaDχ10y + DvDχs𝓁N*SpinEvolver.DχsDχ10y
		    DvDχ10z = DvDχa𝓁N*SpinEvolver.DχaDχ10z + DvDχs𝓁N*SpinEvolver.DχsDχ10z
		    DvDχ20x = DvDχa𝓁N*SpinEvolver.DχaDχ20x + DvDχs𝓁N*SpinEvolver.DχsDχ20x
		    DvDχ20y = DvDχa𝓁N*SpinEvolver.DχaDχ20y + DvDχs𝓁N*SpinEvolver.DχsDχ20y
		    DvDχ20z = DvDχa𝓁N*SpinEvolver.DχaDχ20z + DvDχs𝓁N*SpinEvolver.DχsDχ20z
		    // This part chooses a time step such that the change in 
		    // the value of v is equal to ε times its magnitude.
		    Var ε As Double = (1.0-VN)*1.0e-2
		    DτIdeal = SpinEvolver.Infinity // value will be infinity if vDot is zero
		    // If the magnitude of the change is not strictly zero, then calculate
		    // what time step would lead to a change of 1/100 in the value of v.
		    If vDotN > 0.0 Then DτIdeal = ε/vDotN  // Note that vDot should never be negative
		  End If
		  If VN > 0.6 Then Raise New RuntimeException("Reached Speed Limit")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetVDot(VNow as Double) As Double
		  Var v2 As Double = VNow*VNow
		  Var v3 As Double = v2*VN
		  Var v4 As Double = v2*v2
		  Var v5 As Double = v2*v3
		  Var v6 As Double = v3*v3
		  Var v7 As Double = v3*v4
		  Var v9 As Double = v4*v5
		  Return C0*v9*(1 + C2*v2 + C3*v3 + C4*v4 + C5*v5 + (C6 + C6L*Log(VNow))*v6 + C7*v7)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub MakeFuturePresent()
		  // This moves present values to the past, and future values to the present
		  SpinEvolver.MakeFuturePresent // first do this for the spin evolver
		  // Now handle local variables.
		  VP = VN
		  VN = VF
		  DvDχa𝓁P = DvDχa𝓁N
		  DvDχa𝓁N = DvDχa𝓁F
		  DvDχs𝓁P = DvDχs𝓁N
		  DvDχs𝓁N = DvDχs𝓁F
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		C0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C6 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C6L As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C7 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CAD0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CAD1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CAD2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CSD0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CSD1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CSD2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDz As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχa𝓁F As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχa𝓁N As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχa𝓁P As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχs𝓁F As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχs𝓁N As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχs𝓁P As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DτIdeal As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		InverseZPlus1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		SideCase As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		SpinEvolver As SpinEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		V0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		VDotN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		VF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		VN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		VP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αDotN As Double
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
			Name="DvDz"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχa𝓁F"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχa𝓁N"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχa𝓁P"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχs𝓁F"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχs𝓁N"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχs𝓁P"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="VF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="VN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="VP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="InverseZPlus1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="V0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C4"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C5"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C6"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C7"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C6L"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CSD0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CSD1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CSD2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CAD0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CAD1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CAD2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DτIdeal"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SideCase"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="VDotN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="αDotN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
