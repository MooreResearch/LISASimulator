#tag Class
Protected Class CaseInfoClass
	#tag Method, Flags = &h0
		Function Clone() As CaseInfoClass
		  Var P As New CaseInfoClass
		  P.Detectors = Detectors
		  P.DZDR = DZDR
		  P.GM = GM
		  P.GMΩe = GMΩe
		  P.OneOver1PlusZ = OneOver1PlusZ
		  P.M = M
		  P.PNOrder = PNOrder
		  P.R = R
		  P.RunDuration = RunDuration
		  P.SolveFor.ResizeTo(SolveFor.LastIndex)
		  For i As Integer = 0 to SolveFor.LastIndex
		    P.SolveFor(i) = SolveFor(i)
		  Next
		  P.SolveForχ1 = SolveForχ1
		  P.SolveForχ2 = SolveForχ2
		  P.StorePlotInfo = StorePlotInfo
		  P.T0 = T0
		  P.V0 = V0
		  P.Ve = Ve
		  P.Year = Year
		  P.Z = Z
		  P.β = β
		  P.δ = δ
		  P.ΔT = ΔT
		  P.Θ = Θ
		  P.λ0 = λ0
		  P.π = π
		  P.ρ0 = ρ0
		  P.Φ = Φ
		  P.χ10x = χ10x
		  P.χ10y = χ10y
		  P.χ10z = χ10z
		  P.χ20x = χ20x
		  P.χ20y = χ20y
		  P.χ20z = χ20z
		  P.ψ = ψ
		  Return P
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub FinishConstruction()
		  // This method takes the information provided by the main window and fleshes out the parameter list.
		  // This assumes that M is in solar masses, F0 (the leading-order gravitational wave frequency
		  // in the detector frame) is in mHz, R in lightyears, and angles in degrees.
		  // The spin variables are assumed to be already unitless (in units of the star's squared  mass).
		  // The run duration is assumed to be in years, but the step time is in seconds.
		  
		  Year = 3.15576e7  // length of the year in seconds
		  GM = 4.9267e-6*M   // the mass of the sun in seconds times the mass of the system in solar masses
		  R = R*Year // get R in seconds
		  Var universe As New UniverseClass(R) // Create a universe class to solve the Z(R) problem
		  Z = universe.GetZ // get the Z value for the given value of R
		  DZDR = universe.GetDZDR // get the derivative of Z with respect to R
		  OneOver1PlusZ = 1.0/(1.0 + Z)
		  π = 3.14159265358979324  // record the value of pi so that we only have to define it once
		  V0 = Pow(GM*2.0*π*(1.0 + Z)/T0,1/3)  // Initialize V0
		  // convert all angles from radians to degrees
		  Var radiansFromDegrees As Double = π/180.0
		  β = radiansFromDegrees*β
		  ψ = radiansFromDegrees*ψ
		  λ0 = radiansFromDegrees*λ0
		  ρ0 = radiansFromDegrees*ρ0
		  Θ = radiansFromDegrees*Θ
		  Φ = radiansFromDegrees*Φ
		  Ve = 9.936e-5   //Average orbital speed of the LISA detector
		  GMΩe = GM*1.99213231e-7 //Unitless value of LISA's orbital frequency
		  // If we are generating a solution for any component of spin 1, we must generate the solution for all
		  SolveForχ1 = SolveFor(Integer(Param.chi10x)) or SolveFor(Integer(Param.chi10y)) or SolveFor(Integer(Param.chi10z))
		  // the same for spin 2
		  SolveForχ2 = SolveFor(Integer(Param.chi20x)) or SolveFor(Integer(Param.chi20y)) or SolveFor(Integer(Param.chi20z))
		  Uncertainties.ResizeTo(Integer(Param.NItems) - 1)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetPlotNames() As String()
		  // Make sure that we have one entry for every item in the PlotItems enumeration.
		  // Order is not important as long as the index agrees with the name.
		  Var Names() As String
		  Names.ResizeTo(PlotItem.NItems-1)
		  Names(Integer(PlotItem.H)) = "H"
		  Names(Integer(PlotItem.HP)) = "HP"
		  Names(Integer(PlotItem.HX)) = "HX"
		  Names(Integer(PlotItem.V)) = "V"
		  Names(Integer(PlotItem.PsiR)) = "Ψr"
		  Names(Integer(PlotItem.Iota)) = "ι"
		  Names(Integer(PlotItem.Alpha)) = "α"
		  Names(Integer(PlotItem.ChiSx)) = "χsx"
		  Names(Integer(PlotItem.ChiSy)) = "χsy"
		  Names(Integer(PlotItem.ChiSz)) = "χsz"
		  Names(Integer(PlotItem.ChiAx)) = "χax"
		  Names(Integer(PlotItem.ChiAy)) = "χay"
		  Names(Integer(PlotItem.ChiAz)) = "χaz"
		  Names(Integer(PlotItem.dHdM)) = "dHdM"
		  Names(Integer(PlotItem.dHdDelta)) = "dHdδ"
		  Names(Integer(PlotItem.dHdV0)) = "dHdV0"
		  Names(Integer(PlotItem.dHdR)) = "dHdR"
		  Names(Integer(PlotItem.dHdBeta)) = "dHdβ"
		  Names(Integer(PlotItem.dHdPsi)) = "dHdψ"
		  Names(Integer(PlotItem.dHdLambda0)) = "dHdλ0"
		  Names(Integer(PlotItem.dHdTheta)) = "dHdΘ"
		  Names(Integer(PlotItem.dHdPhi)) = "dHdΦ"
		  Names(Integer(PlotItem.dHdChi10x)) = "dHdχ10x"
		  Names(Integer(PlotItem.dHdChi10y)) = "dHdχ10y"
		  Names(Integer(PlotItem.dHdChi10z)) = "dHdχ10z"
		  Names(Integer(PlotItem.dHdChi20x)) = "dHdχ20x"
		  Names(Integer(PlotItem.dHdChi20y)) = "dHdχ20y"
		  Names(Integer(PlotItem.dHdChi20z)) = "dHdχ20z"
		  Return Names
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		Detectors As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		DZDR As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		GM As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		GMΩe As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		M As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		OneOver1PlusZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		PlotRecords() As PlotRecord
	#tag EndProperty

	#tag Property, Flags = &h0
		PNOrder As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		R As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		RunDuration As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveFor() As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveForχ1 As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveForχ2 As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		StorePlotInfo As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h0
		T0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Uncertainties() As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		V0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Ve As Double = 0.993362e-5
	#tag EndProperty

	#tag Property, Flags = &h0
		Year As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		δ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΔT As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Θ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		λ0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		π As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ρ0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Φ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ψ As Double
	#tag EndProperty


	#tag Enum, Name = Param, Type = Integer, Flags = &h0
		M
		  delta
		  V0
		  R
		  beta
		  psi
		  lambda0
		  theta
		  phi
		  chi10x
		  chi10y
		  chi10z
		  chi20x
		  chi20y
		  chi20z
		NItems
	#tag EndEnum

	#tag Enum, Name = PlotItem, Type = Integer, Flags = &h0
		H
		  HP
		  HX
		  V
		  PsiR
		  Iota
		  Alpha
		  ChiSx
		  ChiSy
		  ChiSz
		  ChiAx
		  ChiAy
		  ChiAz
		  dHdM
		  dHdDelta
		  dHdV0
		  dHdR
		  dHdBeta
		  dHdPsi
		  dHdLambda0
		  dHdTheta
		  dHdPhi
		  dHdChi10x
		  dHdChi10y
		  dHdChi10z
		  dHdChi20x
		  dHdChi20y
		  dHdChi20z
		NItems
	#tag EndEnum


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
			Name="Detectors"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="GMΩe"
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
			Name="Ve"
			Visible=false
			Group="Behavior"
			InitialValue="0.993362e-5"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Z"
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
			Name="δ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
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
			Name="Θ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="λ0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ρ0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Φ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ψ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="T0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="M"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="RunDuration"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="π"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="GM"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="PNOrder"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="R"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="OneOver1PlusZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SolveForχ1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SolveForχ2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DZDR"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Year"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="StorePlotInfo"
			Visible=false
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
