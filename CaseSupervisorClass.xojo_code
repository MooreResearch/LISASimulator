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
		  // the following gives the number of main time steps to execute
		  NSteps = Round(CaseParameters.RunDuration*Year/CaseParameters.ΔT)
		  Dτr = CaseParameters.ΔT/CaseParameters.GM
		  τr = -Dτr // set this back a step so that the first step is at time τr = 0.
		  Var ε As Double = 1.0e-5
		  Evolver = New EvolverClass(CaseParameters) // create the base-case Evolver class and initialize it
		  // Create and initialize all the side classes
		  EvolverForM1Minus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.M1, -ε), Evolver)
		  EvolverForM1Plus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.M1, ε), Evolver)
		  EvolverForM2Minus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.M2, -ε), Evolver)
		  EvolverForM2Plus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.M2, ε), Evolver)
		  EvolverForV0Minus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.V0, -ε), Evolver)
		  EvolverForV0Plus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.V0, ε), Evolver)
		  EvolverForZMinus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.Z, -ε), Evolver)
		  EvolverForZPlus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.Z, ε), Evolver)
		  EvolverForχ10xMinus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ10x, -ε), Evolver)
		  EvolverForrχ10xPlus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ10x, ε), Evolver)
		  EvolverForχ10yMinus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ10y, -ε), Evolver)
		  EvolverForrχ10yPlus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ10y, ε), Evolver)
		  EvolverForχ10zMinus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ10z, -ε), Evolver)
		  EvolverForrχ10zPlus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ10z, ε), Evolver)
		  EvolverForχ20xMinus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ20x, -ε), Evolver)
		  EvolverForrχ20xPlus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ20x, ε), Evolver)
		  EvolverForχ20yMinus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ20y, -ε), Evolver)
		  EvolverForrχ20yPlus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ20y, ε), Evolver)
		  EvolverForχ20zMinus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ20z, -ε), Evolver)
		  EvolverForrχ20zPlus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ20z, ε), Evolver)
		  // Create and initialize the ATA matrix
		  ATAMatrix = New Matrix(15) // Initalize an empty 15x15 matrix
		  ATAMatrix.InverseTest // Check that Matrix code is working
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DidMainStepOK() As Boolean
		  // This method will execute as many steps of the source evolution code as necessary to stay ahead of
		  // (or at least in step with) steps of the main program.
		  
		  Var OKToContinue As Boolean = True
		  If N = 0 Then // If this is the first step
		    WhereInSourceStep = 0 // and we will report the present values
		  ElseIf StepPowerF > 0 Then  // If the step that will be taken is bigger than the main step
		    If LastSourceStep > N Then // and the last source step (which might have been bigger) is still ahead
		      WhereInSourceStep = WhereInSourceStep + 1   // Update the "WhereInSourceStep" counter and we are done
		    ElseIf LastSourceStep = N Then  // if we have caught up with the source
		      WhereInSourceStep = 0  // and we are back at the beginning of the current window
		    Else  // main program is now ahead of the source
		      DoSourceStep  // Take a new source step
		      MainStepsInSourceStep = 2^StepPowerP // This is the number of main steps within the source step just taken
		      // update the source step counter in units of the main step
		      LastSourceStep = LastSourceStep + MainStepsInSourceStep
		      WhereInSourceStep = 1 // we are now at the first step within that total range
		    End If
		  ElseIf StepPowerF = 0 Then // If the next source step will be equal to the main program step
		    If LastSourceStep < MainStep Then // If source is behind the main step
		      DoSourceStep
		      LastSourceStep = LastSourceStep + 1   // update the source step counter
		      WhereInSourceStep = 0  // and we will report the present values
		    ElseIf LastSourceStep = MainStep Then   // I don't think this should happen, but if it does
		      WhereInSourceStep = 0 // we will just report the present values
		    End If
		  Else  // the next source step size will be smaller than the main step size
		    Var stepsToDo As Integer = 2^(-StepPowerF) // get the number of steps to execute in units of the current step size
		    Var stepUnitPower As Integer = StepPowerF // these are the units of StepsToDo
		    Var stepsDone As Integer = 0
		    Do
		      DoSourceStep  // Do a source step
		      If StepPowerF < -10 Then
		        OKToContinue = False
		        Exit
		      End If
		      stepsDone = stepsDone + 1  // Count the step
		      If StepPowerF < stepUnitPower And stepsDone < stepsToDo Then
		        // If the next step size will be smaller and we have not reached the target
		        stepsToDo = 2^(-stepPowerF)   // re-express the target in terms of the next step size
		        stepsDone = stepsDone*2^(stepUnitPower-StepPowerF)  // and rescale the steps already done
		      End If
		    Loop Until stepsDone = stepsToDo
		    If OKToContinue Then // if we haven't exited becase we are too close to coalescence
		      WhereInSourceStep = 0 // and we will report the present values
		    End If
		  End If
		  If OKToContinue Then UpdateValues  // Report values at the main step if we can
		  Return OKToContinue
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoSteps()
		  // This method actually executes the steps for the current case in question.
		  // When it is done, the uncertainties should be in the Uncertainties property.
		  Try
		    For N = 0 to NSteps
		      τr = τr + Dτr // one step to the future
		      If DidMainStepOK Then  // If the evolver was able to execute a step
		        LoadATA  // Load the ATA matrix with the current values
		      Else  // If the evolver was not able to complete the step, we are at coalescence
		        TerminationMessage = "Coalescence Happened"
		        Exit  // Abort the loop
		      End If
		    Next
		    Uncertainty = CalculateUncertainties
		  Catch err As RuntimeException
		    TerminationMessage = err.Message + " at step " + N.ToString
		  End Try
		  If TerminationMessage = "" Then terminationMessage = "Normal termination."
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub FindValuesOfH()
		  Var stepRatio As Double = WhereInSourceStep/MainStepsInSourceStep
		  Evolver.FindHAtMainStep(stepRatio)
		  EvolverForM1Minus.FindHAtMainStep(stepRatio)
		  EvolverForM1Plus.FindHAtMainStep(stepRatio)
		  EvolverForM2Minus.FindHAtMainStep(stepRatio)
		  EvolverForM2Plus.FindHAtMainStep(stepRatio)
		  EvolverForV0Minus.FindHAtMainStep(stepRatio)
		  EvolverForV0Plus.FindHAtMainStep(stepRatio)
		  EvolverForZMinus.FindHAtMainStep(stepRatio)
		  EvolverForZPlus.FindHAtMainStep(stepRatio)
		  EvolverForβMinus.FindHAtMainStep(stepRatio)
		  EvolverForβPlus.FindHAtMainStep(stepRatio)
		  EvolverForχ10xMinus.FindHAtMainStep(stepRatio)
		  EvolverForrχ10xPlus.FindHAtMainStep(stepRatio)
		  EvolverForχ10yMinus.FindHAtMainStep(stepRatio)
		  EvolverForχ10yPlus.FindHAtMainStep(stepRatio)
		  EvolverForχ10zMinus.FindHAtMainStep(stepRatio)
		  EvolverForχ10zPlus.FindHAtMainStep(stepRatio)
		  EvolverForχ20xMinus.FindHAtMainStep(stepRatio)
		  EvolverForrχ20xPlus.FindHAtMainStep(stepRatio)
		  EvolverForχ20yMinus.FindHAtMainStep(stepRatio)
		  EvolverForχ20yPlus.FindHAtMainStep(stepRatio)
		  EvolverForχ20zMinus.FindHAtMainStep(stepRatio)
		  EvolverForχ20zPlus.FindHAtMainStep(stepRatio)
		  
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
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub LoadATA()
		  // This method calculates all the derivatives and loads their products into the ATA matrix
		  
		  
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		ATAMatrix As Matrix
	#tag EndProperty

	#tag Property, Flags = &h0
		CaseParameters As CaseParametersClass
	#tag EndProperty

	#tag Property, Flags = &h0
		DτF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DτFF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DτP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Dτr As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Evolver As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForM1Minus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForM1Plus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForM2Minus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForM2Plus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForV0Minus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForV0Plus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForZMinus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForZPlus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForβMinus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForβPlus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForχ10xMinus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForχ10xPlus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForχ10yMinus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForχ10yPlus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForχ10zMinus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForχ10zPlus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForχ20xMinus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForχ20xPlus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForχ20yMinus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForχ20yPlus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForχ20zMinus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForχ20zPlus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		GMSun As Double = 4.92708e-6
	#tag EndProperty

	#tag Property, Flags = &h0
		H0PLastIndex As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		H0XLastIndex As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		H1PLastIndex As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		H1XLastIndex As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		H2PLastIndex As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		H2XLastIndex As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		H3PLastIndex As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		H3XLastIndex As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		LastSourceStep As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		MainStepsInSourceStep As Integer
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
		StepPowerF As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		StepPowerFF As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		StepPowerP As Integer
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
		WhereInSourceStep As Integer
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
		#tag ViewProperty
			Name="DτF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DτFF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DτP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LastSourceStep"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="MainStepsInSourceStep"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="WhereInSourceStep"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="StepPowerF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="StepPowerFF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="StepPowerP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H0PLastIndex"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H0XLastIndex"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H1PLastIndex"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H1XLastIndex"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H2PLastIndex"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H2XLastIndex"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H3PLastIndex"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H3XLastIndex"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
