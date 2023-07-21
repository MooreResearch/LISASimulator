#tag Class
Protected Class CaseSupervisorClass
	#tag Method, Flags = &h0
		Sub Constructor(currentCaseParameterSet As CaseParametersClass)
		  // The CaseSupervisor class handles the running of each individual case.
		  // The constructor accepts the list of parameters from the currentCase ParameterSet
		  // and initializes the calculation
		  
		  StartTicks = System.Ticks
		  CaseParameters = currentCaseParameterSet // save the parameters for the current case
		  InitializeParameters // flesh out the supplied parameters and put them into the right units
		  τr = -Dτr // set this back a step so that the first step is at time τr = 0.
		  Evolver = New EvolverClass(CaseParameters, Dτr) // create an instance of the Evolver class and initialize it
		  HCalculator = New HCalculatorClass(CaseParameters) // create an instance of the HCalculatorClass and initialize it
		  UncertaintyCalculator = New UncertaintyCalculatorClass(CaseParameters)
		  ATAMatrix = New Matrix(15) // Initalize an empty 15x15 matrix
		  ATAMatrix.InverseTest // Check that Matrix code is working
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoSteps()
		  // This method actually executes the steps for the current case in question.
		  // When it is done, the uncertainties should be in the Uncertainties property.
		  Try
		    For N = 0 to NSteps
		      τr = τr + Dτr // one step to the future
		      If Evolver.DidMainStepOK(n) Then  // If the evolver was able to execute a step
		        // update values to the present step
		        Var currentValues As CurrentValuesClass = Evolver.ValuesN // get the all values we need
		        Var currentDerivs As CurrentDerivativesClass = Evolver.DerivativesN // get all the derivatives
		        HCalculator.Calculate(currentValues, currentDerivs, ATAMatrix) // calculate the polarizations and update ATA matrix
		      Else  // If the evolver was not able to complete the step, we are at coalescence
		        TerminationMessage = "Coalescence Happened"
		        Exit  // Abort the loop
		      End If
		    Next
		    Uncertainty = UncertaintyCalculator.Calculate(ATAMatrix, CaseParameters.Θ) // solve for the uncertainties
		  Catch err As RuntimeException
		    TerminationMessage = err.Message + " at step " + N.ToString
		  End Try
		  If TerminationMessage = "" Then terminationMessage = "Normal termination."
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub InitializeParameters()
		  // This method takes the information provided by the main window and fleshes out the parameter list.
		  // This assumes that M1, M2 are in solar masses, F0 in mHz, R in lightyears, and angles in degrees.
		  // The spin variables are assumed to be already unitless (in units of the star's squared  mass).
		  // The run duration is assumed to be in years, but the step time is in seconds.
		  
		  Var m As Double = CaseParameters.M1 + CaseParameters.M2  // total mass in solar masses
		  Var gm As Double = GMSun*m
		  CaseParameters.GM = gm
		  Var localδ As Double = (CaseParameters.M1 - CaseParameters.M2)/m // calculate delta
		  Var localη As Double = (1-localδ*localδ)/4. // calculate eta
		  CaseParameters.δ = localδ // record in parameter list
		  CaseParameters.η = localη // record in parameter list
		  Var localR As Double = CaseParameters.R*Year // get R in seconds
		  CaseParameters.H0 = 2*gm*localη/localR // Calculate and record H0
		  Var universe As New UniverseClass // Create a universe class to solve the Z(R) problem
		  Var localZ As Double = universe.GetZFrom(localR) // get the Z value for the given value of R
		  CaseParameters.Z = localZ // record the value of Z*1000 (this scales the parameter to be closer to 1)
		  CaseParameters.V0 = Pow(gm*CaseParameters.F0*2*π*(1+localZ)/1000,1/3)  // Initialize V0
		  CaseParameters.π = π  // record the value of pi so that we only have to define it once
		  // convert all angles from radians to degrees
		  Var radiansFromDegrees As Double = π/180
		  CaseParameters.β = radiansFromDegrees*CaseParameters.β
		  CaseParameters.ψ = radiansFromDegrees*CaseParameters.ψ
		  CaseParameters.λ0 = radiansFromDegrees*CaseParameters.λ0
		  CaseParameters.ρ0 = radiansFromDegrees*CaseParameters.ρ0
		  CaseParameters.Θ = radiansFromDegrees*CaseParameters.Θ
		  CaseParameters.Φ = radiansFromDegrees*CaseParameters.Φ
		  CaseParameters.Ve = 9.936e-5   //Average orbital speed of the LISA detector
		  CaseParameters.GMΩe = gm*1.99213231e-7 //Unitless value of LISA's orbital frequency
		  Dτr = CaseParameters.ΔT/GM // this is the unitless value of the main time step in the solar system
		  τr = 0.0 // no elapsed time so far
		  // the following gives the number of main time steps to execute
		  NSteps = Round(CaseParameters.RunDuration*Year/CaseParameters.ΔT)
		  
		  
		  
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		ATAMatrix As Matrix
	#tag EndProperty

	#tag Property, Flags = &h0
		CaseParameters As CaseParametersClass
	#tag EndProperty

	#tag Property, Flags = &h0
		Dτr As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Evolver As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		GMSun As Double = 4.92708e-6
	#tag EndProperty

	#tag Property, Flags = &h0
		HCalculator As HCalculatorClass
	#tag EndProperty

	#tag Property, Flags = &h0
		N As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		NSteps As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		StartTicks As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		TerminationMessage As String
	#tag EndProperty

	#tag Property, Flags = &h0
		Uncertainty As UncertaintyValuesClass
	#tag EndProperty

	#tag Property, Flags = &h0
		UncertaintyCalculator As UncertaintyCalculatorClass
	#tag EndProperty

	#tag Property, Flags = &h0
		Year As Double = 3.1556e7
	#tag EndProperty

	#tag Property, Flags = &h0
		π As Double = 3.141592653589793
	#tag EndProperty

	#tag Property, Flags = &h0
		τr As Double
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
			Name="τr"
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
			InitialValue="3.141592653589793"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="GMSun"
			Visible=false
			Group="Behavior"
			InitialValue="4.92708e-6"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Year"
			Visible=false
			Group="Behavior"
			InitialValue="3.1556e7"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Dτr"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="NSteps"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="N"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TerminationMessage"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="StartTicks"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
