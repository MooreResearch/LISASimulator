#tag Class
Protected Class CaseSupervisorClass
	#tag Method, Flags = &h0
		Sub CalcDataAtMainStep()
		  Var StepRatio As Double = 0.0
		End Sub
	#tag EndMethod

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
		  Evolver.IsBaseCase = True
		  // Create and initialize all the side classes
		  EvolverForM1Minus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.M1, -ε), Evolver)
		  EvolverForM1Plus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.M1, ε), Evolver)
		  EvolverForM2Minus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.M2, -ε), Evolver)
		  EvolverForM2Plus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.M2, ε), Evolver)
		  EvolverForV0Minus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.V0, -ε), Evolver)
		  EvolverForV0Plus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.V0, ε), Evolver)
		  EvolverForΛMinus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.Λ, -ε), Evolver)
		  EvolverForΛPlus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.Λ, ε), Evolver)
		  EvolverForβMinus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.β, -ε), Evolver)
		  EvolverForβPlus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.β, ε), Evolver)
		  EvolverForψMinus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.ψ, -ε), Evolver)
		  EvolverForψPlus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.ψ, ε), Evolver)
		  EvolverForΘMinus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.Θ, -ε), Evolver)
		  EvolverForΘPlus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.Θ, ε), Evolver)
		  EvolverForΦMinus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.Φ, -ε), Evolver)
		  EvolverForΦPlus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.Φ, ε), Evolver)
		  EvolverForχ10xMinus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ10x, -ε), Evolver)
		  EvolverForχ10xPlus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ10x, ε), Evolver)
		  EvolverForχ10yMinus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ10y, -ε), Evolver)
		  EvolverForχ10yPlus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ10y, ε), Evolver)
		  EvolverForχ10zMinus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ10z, -ε), Evolver)
		  EvolverForχ10zPlus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ10z, ε), Evolver)
		  EvolverForχ20xMinus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ20x, -ε), Evolver)
		  EvolverForχ20xPlus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ20x, ε), Evolver)
		  EvolverForχ20yMinus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ20y, -ε), Evolver)
		  EvolverForχ20yPlus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ20y, ε), Evolver)
		  EvolverForχ20zMinus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ20z, -ε), Evolver)
		  EvolverForχ20zPlus = New EvolverClass(CaseParameters.GetTweaked(CaseParametersClass.Item.χ20z, ε), Evolver)
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
		    If LastSourceStep < N Then // If source is behind the main step
		      DoSourceStep
		      LastSourceStep = LastSourceStep + 1   // update the source step counter
		      WhereInSourceStep = 0  // and we will report the present values
		    ElseIf LastSourceStep = N Then   // I don't think this should happen, but if it does
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
		  If OKToContinue Then CalcDataAtMainStep  // Calculate H at the main step if we can
		  Return OKToContinue
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoSourceStep()
		  // This method performs a source step
		  
		  // First, make the future the present
		  StepPowerP = StepPowerF
		  StepPowerF = StepPowerFF
		  DτP = DτF
		  DτF = DτFF
		  
		  // Now perform the step
		  // Step the main case and all side cases
		  Evolver.DoStep(DτF, DτP)
		  EvolverForM1Minus.DoStep(DτF, DτP)
		  EvolverForM1Plus.DoStep(DτF, DτP)
		  EvolverForM2Minus.DoStep(DτF, DτP)
		  EvolverForM2Plus.DoStep(DτF, DτP)
		  EvolverForV0Minus.DoStep(DτF, DτP)
		  EvolverForV0Plus.DoStep(DτF, DτP)
		  EvolverForΛMinus.DoStep(DτF, DτP)
		  EvolverForΛPlus.DoStep(DτF, DτP)
		  EvolverForβMinus.DoStep(DτF, DτP)
		  EvolverForβPlus.DoStep(DτF, DτP)
		  EvolverForψMinus.DoStep(DτF, DτP)
		  EvolverForψPlus.DoStep(DτF, DτP)
		  EvolverForΘMinus.DoStep(DτF, DτP)
		  EvolverForΘPlus.DoStep(DτF, DτP)
		  EvolverForΦMinus.DoStep(DτF, DτP)
		  EvolverForΦPlus.DoStep(DτF, DτP)
		  EvolverForχ10xMinus.DoStep(DτF, DτP)
		  EvolverForχ10xPlus.DoStep(DτF, DτP)
		  EvolverForχ10yMinus.DoStep(DτF, DτP)
		  EvolverForχ10yPlus.DoStep(DτF, DτP)
		  EvolverForχ10zMinus.DoStep(DτF, DτP)
		  EvolverForχ10zPlus.DoStep(DτF, DτP)
		  EvolverForχ20xMinus.DoStep(DτF, DτP)
		  EvolverForχ20xPlus.DoStep(DτF, DτP)
		  EvolverForχ20yMinus.DoStep(DτF, DτP)
		  EvolverForχ20yPlus.DoStep(DτF, DτP)
		  EvolverForχ20zMinus.DoStep(DτF, DτP)
		  EvolverForχ20zPlus.DoStep(DτF, DτP)
		  
		  // This chooses the next time step to be a multiple or fraction of a power of 2
		  // times the main program time step (as seen in the source frame)
		  // First, get the ideal time step from the various evolvers
		  Var DτIdeal As Double = Evolver.DτIdeal
		  // The ratio of the real future step will be some power of two of the main step.
		  // Compute that power of two
		  Var NewStepPower as Integer = Floor(Log(DτIdeal*(1.0+CaseParameters.Z)/Dτr)/Log(2))
		  If N = 0 Then  // If this is the first step
		    StepPowerFF = NewStepPower // initalize the CurrentStepPower
		    StepPowerF = NewStepPower
		    DτFF = Dτr*2^StepPowerF/(1.0 + CaseParameters.Z) // and initialize DτFF
		    DτF = DτFF // and set DτF
		    DτP = DτF  // and DτP to be the same
		  Else
		    If NewStepPower > StepPowerF Then NewStepPower = StepPowerF // This power should never increase
		    If NewStepPower < StepPowerF Then // if the new step is smaller
		      StepPowerFF = NewStepPower // this will be the step power for the next step
		      DτFF = Dτr*(2^NewStepPower)/(1.0 + CaseParameters.Z) // change the value of DτF
		    End If
		    // note that if the power is NOT smaller, everything will remain the same
		  End If
		End Sub
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
		Sub InitializeParameters()
		  // This method takes the information provided by the main window and fleshes out the parameter list.
		  // This assumes that M1, M2 are in solar masses, F0 in mHz, R in lightyears, and angles in degrees.
		  // The spin variables are assumed to be already unitless (in units of the star's squared  mass).
		  // The run duration is assumed to be in years, but the step time is in seconds.
		  
		  Var m As Double = CaseParameters.M1 + CaseParameters.M2  // total mass in solar masses
		  Var gm As Double = GMSun*m
		  CaseParameters.GM = gm
		  Var localδ As Double = (CaseParameters.M1 - CaseParameters.M2)/m // calculate delta
		  Var localη As Double = (1-localδ*localδ)/4.0 // calculate eta
		  CaseParameters.δ = localδ // record in parameter list
		  CaseParameters.η = localη // record in parameter list
		  Var localR As Double = CaseParameters.R*Year // get R in seconds
		  CaseParameters.R0 = 1.0e7*Year  // Defines the reference for R (10 Mly)
		  CaseParameters.Λ = localR/CaseParameters.R0  // This is the unitless luminosity distance
		  Var universe As New UniverseClass // Create a universe class to solve the Z(R) problem
		  Var localZ As Double = universe.GetZFrom(localR) // get the Z value for the given value of R
		  CaseParameters.Z = localZ // record the value of Z
		  CaseParameters.V0 = Pow(gm*CaseParameters.F0*2*π*(1+localZ)/1000,1/3)  // Initialize V0
		  CaseParameters.π = π  // record the value of pi so that we only have to define it once
		  // convert all angles from radians to degrees
		  Var radiansFromDegrees As Double = π/180.0
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
		  
		  // Define some variables
		  Var dHDq(14) As Double  // This will hold the derivatives
		  Var dh As Double
		  Var dΨr As Double
		  Var idε As Double
		  Var dhDΨ As Double = Evolver.DHDΨ  // get a local reference to this from the base case
		  
		  // Calculate derivative for M1
		  dh = EvolverForM1Plus.H - EvolverForM1Minus.H
		  dΨr = EvolverForM1Plus.ΨrMN - EvolverForM1Minus.ΨrMN
		  idε = EvolverForM1Plus.Parameters.InvDε
		  dHDq(0) = dh*idε + dhDΨ*dΨr*idε
		  
		  // Calculate derivative for M2
		  dh = EvolverForM2Plus.H - EvolverForM2Minus.H
		  dΨr = EvolverForM2Plus.ΨrMN - EvolverForM2Minus.ΨrMN
		  idε = EvolverForM2Plus.Parameters.InvDε
		  dHDq(1) = dh*idε + dhDΨ*dΨr*idε
		  
		  // Calculate derivative for V0
		  dh = EvolverForV0Plus.H - EvolverForV0Minus.H
		  dΨr = EvolverForV0Plus.ΨrMN - EvolverForV0Minus.ΨrMN
		  idε = EvolverForV0Plus.Parameters.InvDε
		  dHDq(2) = dh*idε + dhDΨ*dΨr*idε
		  
		  // Calculate derivative for Λ = R/R0
		  dh = EvolverForΛPlus.H - EvolverForΛMinus.H
		  dΨr = EvolverForΛPlus.ΨrMN - EvolverForΛMinus.ΨrMN
		  idε = EvolverForΛPlus.Parameters.InvDε
		  dHDq(3) = dh*idε + dhDΨ*dΨr*idε
		  
		  // Calculate derivative for β
		  // In this case, β does not affect the phase
		  dh = EvolverForβPlus.H - EvolverForβMinus.H
		  idε = EvolverForβPlus.Parameters.InvDε
		  dHDq(4) = dh*idε
		  
		  // Calculate derivative for ψ
		  // In this case, ψ does not affect the phase
		  dh = EvolverForψPlus.H - EvolverForψMinus.H
		  idε = EvolverForψPlus.Parameters.InvDε
		  dHDq(5) = dh*idε
		  
		  // Calculate derivative for λ0
		  // In this case, λ0 only affects the phase, and we use the base case,
		  // and the derivative dΨr/dλ0 = 1
		  dHDq(6) = dhDΨ
		  
		  // Calculate derivative for Θ
		  dh = EvolverForΘPlus.H - EvolverForΘMinus.H
		  dΨr = EvolverForΘPlus.ΨrMN - EvolverForΘMinus.ΨrMN
		  idε = EvolverForΘPlus.Parameters.InvDε
		  dHDq(7) = dh*idε + dhDΨ*dΨr*idε
		  
		  // Calculate derivative for Φ
		  dh = EvolverForΦPlus.H - EvolverForΘMinus.H
		  dΨr = EvolverForΦPlus.ΨrMN - EvolverForΦMinus.ΨrMN
		  idε = EvolverForΦPlus.Parameters.InvDε
		  dHDq(8) = dh*idε + dhDΨ*dΨr*idε
		  
		  // Calculate derivative for χ10x
		  dh = EvolverForχ10xPlus.H - EvolverForχ10xMinus.H
		  dΨr = EvolverForχ10xPlus.ΨrMN - EvolverForχ10xMinus.ΨrMN
		  idε = EvolverForχ10xPlus.Parameters.InvDε
		  dHDq(9) = dh*idε + dhDΨ*dΨr*idε
		  
		  // Calculate derivative for χ10y
		  dh = EvolverForχ10yPlus.H - EvolverForχ10yMinus.H
		  dΨr = EvolverForχ10yPlus.ΨrMN - EvolverForχ10yMinus.ΨrMN
		  idε = EvolverForχ10yPlus.Parameters.InvDε
		  dHDq(10) = dh*idε + dhDΨ*dΨr*idε
		  
		  // Calculate derivative for χ10z
		  dh = EvolverForχ10zPlus.H - EvolverForχ10zMinus.H
		  dΨr = EvolverForχ10zPlus.ΨrMN - EvolverForχ10zMinus.ΨrMN
		  idε = EvolverForχ10zPlus.Parameters.InvDε
		  dHDq(11) = dh*idε + dhDΨ*dΨr*idε
		  
		  // Calculate derivative for χ20x
		  dh = EvolverForχ20xPlus.H - EvolverForχ20xMinus.H
		  dΨr = EvolverForχ20xPlus.ΨrMN - EvolverForχ20xMinus.ΨrMN
		  idε = EvolverForχ20xPlus.Parameters.InvDε
		  dHDq(12) = dh*idε + dhDΨ*dΨr*idε
		  
		  // Calculate derivative for χ20y
		  dh = EvolverForχ20yPlus.H - EvolverForχ20yMinus.H
		  dΨr = EvolverForχ20yPlus.ΨrMN - EvolverForχ20yMinus.ΨrMN
		  idε = EvolverForχ20yPlus.Parameters.InvDε
		  dHDq(13) = dh*idε + dhDΨ*dΨr*idε
		  
		  // Calculate derivative for χ20z
		  dh = EvolverForχ20zPlus.H - EvolverForχ20zMinus.H
		  dΨr = EvolverForχ20zPlus.ΨrMN - EvolverForχ20zMinus.ΨrMN
		  idε = EvolverForχ20zPlus.Parameters.InvDε
		  dHDq(14) = dh*idε + dhDΨ*dΨr*idε
		  
		  // Put everything in the ATA matrix
		  For j As Integer = 0 To 14
		    For k As Integer = 0 to 14
		      ATAMatrix.PData(j,k) = dHDq(j)*dHDq(k)
		    Next
		  Next
		  
		  
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
		EvolverForβMinus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForβPlus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForΘMinus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForΘPlus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForΛMinus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForΛPlus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForΦMinus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForΦPlus As EvolverClass
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
		EvolverForψMinus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		EvolverForψPlus As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		GMSun As Double = 4.92708e-6
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
	#tag EndViewBehavior
End Class
#tag EndClass
