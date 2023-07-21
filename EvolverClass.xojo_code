#tag Class
Protected Class EvolverClass
	#tag Method, Flags = &h0
		Sub ChooseDτF()
		  // This chooses the next time step to be a multiple or fraction of a power of 2
		  // times the main program time step (as seen in the source frame)
		  // First, get the ideal time step from the various evolvers
		  Var DτIdeal As Double = Min(VEvolver.DτIdeal, VEvolver.SpinEvolver.DτIdeal)
		  // The ratio of the real future step will be some power of two of the main step.
		  // Compute that power of two
		  Var NewStepPower as Integer = Floor(Log(DτIdeal*(1.0+Parameters.Z)/Dτr)/Log(2))
		  If MainStep = 0 Then  // If this is the first step
		    StepPowerFF = NewStepPower // initalize the CurrentStepPower
		    StepPowerF = NewStepPower
		    DτFF = Dτr*InverseOnePlusZ*2^StepPowerF // and initialize DτFF
		    DτF = DτFF // and set DτF
		    DτP = DτF  // and DτP to be the same
		  Else
		    If NewStepPower > StepPowerF Then NewStepPower = StepPowerF // This power should never increase
		    If NewStepPower < StepPowerF Then // if the new step is smaller
		      StepPowerFF = NewStepPower // this will be the step power for the next step
		      DτFF = Dτr*InverseOnePlusZ*2^NewStepPower // change the value of DτF
		    End If
		    // note that if the power is NOT smaller, everything will remain the same
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(CaseParameters As CaseParametersClass, MyDτr As Double)
		  // This method sets up the main cases and side cases and initializes them
		  Parameters = CaseParameters  // save a reference to the parameters
		  ε = 1.0e-5 // initialize our epsilon for calculating derivatives
		  InverseOnePlusZ = 1.0/(1.0 + Parameters.Z)
		  Dτr = MyDτr // record the main time step
		  Var Dτ0 As Double = MyDτr*InverseOnePlusZ // define a first step in the source frame
		  Var IsSide As Boolean = True  // Set up a flag to designate side cases
		  τ = 0.0  // currently, we are at time step zero
		  
		  // Initialize main VEvolver (which includes the main SpinEvolver)
		  VEvolver = New VEvolverClass(Parameters, Dτ0)
		  
		  // Initialize the VEvolver side cases (each of which includes a SpinEvolver also)
		  VEvolverForV0Minus = New VEvolverClass(Parameters.GetTweaked(CaseParametersClass.Item.v0, -ε), Dτ0, IsSide)
		  VEvolverForV0Plus = New VEvolverClass(Parameters.GetTweaked(CaseParametersClass.Item.v0, ε), Dτ0, IsSide)
		  VEvolverForδMinus = New VEvolverClass(Parameters.GetTweaked(CaseParametersClass.Item.δ, -ε), Dτ0, IsSide)
		  VEvolverForδPlus = New VEvolverClass(Parameters.GetTweaked(CaseParametersClass.Item.δ, ε), Dτ0, IsSide)
		  VEvolverForχ10xMinus = New VEvolverClass(Parameters.GetTweaked(CaseParametersClass.Item.χ10x, -ε), Dτ0, IsSide)
		  VEvolverForχ10xPlus = New VEvolverClass(Parameters.GetTweaked(CaseParametersClass.Item.χ10x, ε), Dτ0, IsSide)
		  VEvolverForχ10yMinus = New VEvolverClass(Parameters.GetTweaked(CaseParametersClass.Item.χ10y, -ε), Dτ0, IsSide)
		  VEvolverForχ10yPlus = New VEvolverClass(Parameters.GetTweaked(CaseParametersClass.Item.χ10y, ε), Dτ0, IsSide)
		  VEvolverForχ10zMinus = New VEvolverClass(Parameters.GetTweaked(CaseParametersClass.Item.χ10z, -ε), Dτ0, IsSide)
		  VEvolverForχ10zPlus = New VEvolverClass(Parameters.GetTweaked(CaseParametersClass.Item.χ10z, ε), Dτ0, IsSide)
		  VEvolverForχ20xMinus = New VEvolverClass(Parameters.GetTweaked(CaseParametersClass.Item.χ20x, -ε), Dτ0, IsSide)
		  VEvolverForχ20xPlus = New VEvolverClass(Parameters.GetTweaked(CaseParametersClass.Item.χ20x, ε), Dτ0, IsSide)
		  VEvolverForχ20yMinus = New VEvolverClass(Parameters.GetTweaked(CaseParametersClass.Item.χ20y, -ε), Dτ0, IsSide)
		  VEvolverForχ20yPlus = New VEvolverClass(Parameters.GetTweaked(CaseParametersClass.Item.χ20y, ε), Dτ0, IsSide)
		  VEvolverForχ20zMinus = New VEvolverClass(Parameters.GetTweaked(CaseParametersClass.Item.χ20z, -ε), Dτ0, IsSide)
		  VEvolverForχ20zPlus = New VEvolverClass(Parameters.GetTweaked(CaseParametersClass.Item.χ20z, ε), Dτ0, IsSide)
		  
		  // Initialize the PhaseEvolver
		  PhaseEvolver = New PhaseEvolverClass(Parameters, Dτ0)
		  
		  // Create instances of the CurrentValuesClass and CurrentDerivativesClass and initialize those values
		  ValuesN = New CurrentValuesClass
		  UpdateValues
		  ValuesP = ValuesN
		  DerivativesN = New CurrentDerivativesClass
		  UpdateDerivatives
		  DerivativesP = DerivativesN
		  LastSourceStep = 0 // We have not yet begun to step
		  MainStep = 0
		  WhereInSourceStep = 0 // we will look at the current step for values
		  
		  //Determine the actual step size we want (rather than the trial step taken during initialization)
		  ChooseDτF
		  DτF = 0.5*DτF
		  DτP = 0.5*DτP
		  DoStepAll // Do an Euler step for main and all the side cases
		  DτF = 2*DτF
		  DτP = 2*DτP
		  
		  
		  
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DidMainStepOK(TheMainStep As Integer) As Boolean
		  // This method will execute as many steps of the source evolution code as necessary to stay ahead of
		  // (or at least in step with) steps of the main program.
		  
		  Var OKToContinue As Boolean = True
		  MainStep = TheMainStep  // Record the current main step number
		  If MainStep = 0 Then // If this is the first step
		    UpdateValues  // Get the new values and store them in the present values property
		    UpdateDerivatives  // and the same with the derivatives
		    WhereInSourceStep = 0 // and we will report the present values
		  ElseIf StepPowerF > 0 Then  // If the step that will be taken is bigger than the main step
		    If LastSourceStep > MainStep Then // and the last source step (which might have been bigger) is still ahead
		      WhereInSourceStep = WhereInSourceStep + 1   // Update the "WhereInSourceStep" counter and we are done
		    ElseIf LastSourceStep = MainStep Then  // if we have caught up with the source
		      ValuesP = ValuesN  // values at present become past values
		      DerivativesP = DerivativesN  // Same for derivatives
		      WhereInSourceStep = 0  // and we are back at the beginning of the current window
		    Else  // main program is now ahead of the source
		      DoSourceStep  // Take a new source step
		      UpdateValues  // Get the new values and store them in the present values property
		      UpdateDerivatives  // and the same with the derivatives
		      MainStepsInSourceStep = 2^StepPowerP // This is the number of main steps within the source step just taken
		      // update the source step counter in units of the main step
		      LastSourceStep = LastSourceStep + MainStepsInSourceStep
		      WhereInSourceStep = 1 // we are now at the first step within that total range
		    End If
		  ElseIf StepPowerF = 0 Then // If the next source step will be equal to the main program step
		    If LastSourceStep < MainStep Then // If source is behind the main step
		      DoSourceStep
		      UpdateValues  // Get the new values and store them in the present values property
		      UpdateDerivatives  // and the same with the derivatives
		      LastSourceStep = LastSourceStep + 1   // update the source step counter
		      WhereInSourceStep = 0  // and we will report the present values
		    ElseIf LastSourceStep = MainStep Then   // I don't think this should happen, but if it does
		      WhereInSourceStep = 0 // we will just report the present values
		    End If
		  Else  // the next source step size will be smaller than the main step size
		    Var StepsToDo As Integer = 2^(-StepPowerF) // get the number of steps to execute in units of the current step size
		    Var StepUnitPower As Integer = StepPowerF // these are the units of StepsToDo
		    Var StepsDone As Integer = 0
		    Do
		      DoSourceStep  // Do a source step
		      If StepPowerF < -10 Then
		        OKToContinue = False
		        Exit
		      End If
		      StepsDone = StepsDone + 1  // Count the step
		      If StepPowerF < StepUnitPower And StepsDone < StepsToDo Then
		        // If the next step size will be smaller and we have not reached the target
		        StepsToDo = 2^(-StepPowerF)   // re-express the target in terms of the next step size
		        StepsDone = StepsDone*2^(StepUnitPower-StepPowerF)  // and rescale the steps already done
		      End If
		    Loop Until StepsDone = StepsToDo
		    If OKToContinue Then // if we haven't exited becase we are too close to coalescence
		      UpdateValues  // Get the new values and store them in the present values property
		      UpdateDerivatives  // and the same with the derivatives
		      WhereInSourceStep = 0 // and we will report the present values
		    End If
		  End If
		  Return OKToContinue
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoMakeFuturePresent()
		  VEvolver.MakeFuturePresent
		  VEvolverForV0Minus.MakeFuturePresent
		  VEvolverForV0Plus.MakeFuturePresent
		  VEvolverForδMinus.MakeFuturePresent
		  VEvolverForδPlus.MakeFuturePresent
		  VEvolverForχ10xMinus.MakeFuturePresent
		  VEvolverForχ10xPlus.MakeFuturePresent
		  VEvolverForχ10yMinus.MakeFuturePresent
		  VEvolverForχ10yPlus.MakeFuturePresent
		  VEvolverForχ10zMinus.MakeFuturePresent
		  VEvolverForχ10zPlus.MakeFuturePresent
		  VEvolverForχ20xMinus.MakeFuturePresent
		  VEvolverForχ20xPlus.MakeFuturePresent
		  VEvolverForχ20yMinus.MakeFuturePresent
		  VEvolverForχ20yPlus.MakeFuturePresent
		  VEvolverForχ20zMinus.MakeFuturePresent
		  VEvolverForχ20zPlus.MakeFuturePresent
		  PhaseEvolver.MakeFuturePresent
		  StepPowerP = StepPowerF
		  StepPowerF = StepPowerFF
		  DτP = DτF
		  DτF = DτFF
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoPhaseStep()
		  Var inverseTwoε As Double = 1.0/(2*ε)
		  PhaseEvolver.V = VEvolver.VN
		  PhaseEvolver.VDot = VEvolver.VDotN
		  Var SpinEvolver As SpinEvolverClass = VEvolver.SpinEvolver
		  PhaseEvolver.Cosι = SpinEvolver.CosιN
		  PhaseEvolver.αDot = SpinEvolver.αDotN
		  PhaseEvolver.DvDV0 = (VEvolverForV0Plus.VN - VEvolverForV0Minus.VN)*inverseTwoε
		  PhaseEvolver.DvDotDV0 = (VEvolverForV0Plus.VDotN - VEvolverForV0Minus.VDotN)*inverseTwoε
		  PhaseEvolver.DαDotDV0 = (VEvolverForV0Plus.αDotN - VEvolverForV0Minus.αDotN)*inverseTwoε
		  PhaseEvolver.DvDδ = (VEvolverForδPlus.VN - VEvolverForδMinus.VN)*inverseTwoε
		  PhaseEvolver.DvDotDδ = (VEvolverForδPlus.VDotN - VEvolverForδMinus.VDotN)*inverseTwoε
		  PhaseEvolver.DαDotDδ = (VEvolverForδPlus.αDotN - VEvolverForδMinus.αDotN)*inverseTwoε
		  PhaseEvolver.DvDχ10x = (VEvolverForχ10xPlus.VN - VEvolverForχ10xMinus.VN)*inverseTwoε
		  PhaseEvolver.DvDotDχ10x = (VEvolverForχ10xPlus.VDotN - VEvolverForχ10xMinus.VDotN)*inverseTwoε
		  PhaseEvolver.DαDotDχ10x = (VEvolverForχ10xPlus.αDotN - VEvolverForχ10xMinus.αDotN)*inverseTwoε
		  PhaseEvolver.DvDχ10y = (VEvolverForχ10yPlus.VN - VEvolverForχ10yMinus.VN)*inverseTwoε
		  PhaseEvolver.DvDotDχ10y = (VEvolverForχ10yPlus.VDotN - VEvolverForχ10yMinus.VDotN)*inverseTwoε
		  PhaseEvolver.DαDotDχ10y = (VEvolverForχ10yPlus.αDotN - VEvolverForχ10yMinus.αDotN)*inverseTwoε
		  PhaseEvolver.DvDχ10z = (VEvolverForχ10zPlus.VN - VEvolverForχ10zMinus.VN)*inverseTwoε
		  PhaseEvolver.DvDotDχ10z = (VEvolverForχ10zPlus.VDotN - VEvolverForχ10zMinus.VDotN)*inverseTwoε
		  PhaseEvolver.DαDotDχ10z = (VEvolverForχ10zPlus.αDotN - VEvolverForχ10zMinus.αDotN)*inverseTwoε
		  PhaseEvolver.DvDχ20x = (VEvolverForχ20xPlus.VN - VEvolverForχ20xMinus.VN)*inverseTwoε
		  PhaseEvolver.DvDotDχ20x = (VEvolverForχ20xPlus.VDotN - VEvolverForχ20xMinus.VDotN)*inverseTwoε
		  PhaseEvolver.DαDotDχ20x = (VEvolverForχ20xPlus.αDotN - VEvolverForχ20xMinus.αDotN)*inverseTwoε
		  PhaseEvolver.DvDχ20y = (VEvolverForχ20yPlus.VN - VEvolverForχ20yMinus.VN)*inverseTwoε
		  PhaseEvolver.DvDotDχ20y = (VEvolverForχ20yPlus.VDotN - VEvolverForχ20yMinus.VDotN)*inverseTwoε
		  PhaseEvolver.DαDotDχ20y = (VEvolverForχ20yPlus.αDotN - VEvolverForχ20yMinus.αDotN)*inverseTwoε
		  PhaseEvolver.DvDχ20z = (VEvolverForχ20zPlus.VN - VEvolverForχ20zMinus.VN)*inverseTwoε
		  PhaseEvolver.DvDotDχ20z = (VEvolverForχ20zPlus.VDotN - VEvolverForχ20zMinus.VDotN)*inverseTwoε
		  PhaseEvolver.DαDotDχ20z = (VEvolverForχ20zPlus.αDotN - VEvolverForχ20zMinus.αDotN)*inverseTwoε
		  Var τr As Double = τ*(1.0 + Parameters.Z)
		  PhaseEvolver.DoStep(DτF, DτP, τr)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoSourceStep()
		  // This method performs a source step
		  DoMakeFuturePresent // Make the future step the present step for all cases
		  DoStepAll // Do the next step
		  τ = τ + DτP  // The current time at Now is equal to the previous time times the magnitude of the past time step
		  ChooseDτF  // Set the size of the still next step
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoStepAll()
		  // Step all main and side cases
		  VEvolver.DoStep(DτF, DτP)
		  VEvolverForV0Minus.DoStep(DτF, DτP)
		  VEvolverForV0Plus.DoStep(DτF, DτP)
		  VEvolverForδMinus.DoStep(DτF, DτP)
		  VEvolverForδPlus.DoStep(DτF, DτP)
		  VEvolverForχ10xMinus.DoStep(DτF, DτP)
		  VEvolverForχ10xPlus.DoStep(DτF, DτP)
		  VEvolverForχ10yMinus.DoStep(DτF, DτP)
		  VEvolverForχ10yPlus.DoStep(DτF, DτP)
		  VEvolverForχ10zMinus.DoStep(DτF, DτP)
		  VEvolverForχ10zPlus.DoStep(DτF, DτP)
		  VEvolverForχ20xMinus.DoStep(DτF, DτP)
		  VEvolverForχ20xPlus.DoStep(DτF, DτP)
		  VEvolverForχ20yMinus.DoStep(DτF, DτP)
		  VEvolverForχ20yPlus.DoStep(DτF, DτP)
		  VEvolverForχ20zMinus.DoStep(DτF, DτP)
		  VEvolverForχ20zPlus.DoStep(DτF, DτP)
		  DoPhaseStep // We have to do a phase step last
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetDerivativesAtMainNow() As CurrentDerivativesClass
		  // This is how the HCalculator class gets information about derivatives
		  If WhereInSourceStep = 0 Then // if we are getting information about the current step,
		    Return DerivativesN // just return the values corresponding to the current step.
		  Else // if we are interpolating between the current step and a future step,
		    // calculate the fraction the current step represents of the total step.
		    Var StepRatio As Double = WhereInSourceStep/MainStepsInSourceStep
		    // Get the interpolated values and return them
		    Return (1.0-StepRatio)*DerivativesP + StepRatio*DerivativesN
		  End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetValuesAtMainNow() As CurrentValuesClass
		  // This is how the HCalculator gets information about the current values
		  If WhereInSourceStep = 0 Then // if we are getting information about the current step,
		    Return ValuesN // just return the values corresponding to the current step.
		  Else // if we are interpolating between the current step and a future step,
		    // calculate the fraction the current step represents of the total step.
		    Var StepRatio As Double = WhereInSourceStep/MainStepsInSourceStep
		    // Get the interpolated values and return them
		    Return (1.0-StepRatio)*ValuesP + StepRatio*ValuesN
		  End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UpdateAlphaDerivatives()
		  DerivativesN.DαDZ = VEvolver.SpinEvolver.DαDZ
		  Var inverse2ε As Double = 1/(2*ε)
		  DerivativesN.DαDZ = VEvolver.SpinEvolver.DαDZ
		  DerivativesN.DαDV0 = (VEvolverForV0Plus.SpinEvolver.αN - VEvolverForV0Minus.SpinEvolver.αN)*inverse2ε
		  DerivativesN.DαDδ = (VEvolverForδPlus.SpinEvolver.αN - VEvolverForδMinus.SpinEvolver.αN)*inverse2ε
		  DerivativesN.DαDχ10x = (VEvolverForχ10xPlus.SpinEvolver.αN - VEvolverForχ10xMinus.SpinEvolver.αN)*inverse2ε
		  DerivativesN.DαDχ10y = (VEvolverForχ10yPlus.SpinEvolver.αN - VEvolverForχ10yMinus.SpinEvolver.αN)*inverse2ε
		  DerivativesN.DαDχ10z = (VEvolverForχ10zPlus.SpinEvolver.αN - VEvolverForχ10zMinus.SpinEvolver.αN)*inverse2ε
		  DerivativesN.DαDχ20x = (VEvolverForχ20xPlus.SpinEvolver.αN - VEvolverForχ20xMinus.SpinEvolver.αN)*inverse2ε
		  DerivativesN.DαDχ20y = (VEvolverForχ20yPlus.SpinEvolver.αN - VEvolverForχ20yMinus.SpinEvolver.αN)*inverse2ε
		  DerivativesN.DαDχ20z = (VEvolverForχ20zPlus.SpinEvolver.αN - VEvolverForχ20zMinus.SpinEvolver.αN)*inverse2ε
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UpdateCosιDerivatives()
		  DerivativesN.DCosιDZ = VEvolver.SpinEvolver.DCosιDZ
		  DerivativesN.DCosιDV0 = PhaseEvolver.DCosιDV0
		  DerivativesN.DCosιDδ = PhaseEvolver.DCosιDδ
		  DerivativesN.DCosιDχ10x = PhaseEvolver.DCosιDχ10x
		  DerivativesN.DCosιDχ10y = PhaseEvolver.DCosιDχ10y
		  DerivativesN.DCosιDχ10z = PhaseEvolver.DCosιDχ10z
		  DerivativesN.DCosιDχ20x = PhaseEvolver.DCosιDχ20x
		  DerivativesN.DCosιDχ20y = PhaseEvolver.DCosιDχ20y
		  DerivativesN.DCosιDχ20z = PhaseEvolver.DCosιDχ20z
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UpdateDerivatives()
		  UpdateCosιDerivatives
		  UpdateVDerivatives
		  UpdateAlphaDerivatives
		  UpdateχaDerivatives
		  UpdateχsDerivatives
		  UpdateΨrDerivatives
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UpdateValues()
		  Var SpinEvolver As SpinEvolverClass = VEvolver.SpinEvolver
		  Var Xa As Vector = SpinEvolver.χaN
		  Var Xs As Vector = SpinEvolver.χsN
		  ValuesN.Cosι = SpinEvolver.CosιN
		  ValuesN.V = VEvolver.VN
		  ValuesN.α = SpinEvolver.αN
		  ValuesN.χax = Xa.X
		  ValuesN.χay = Xa.Y
		  ValuesN.χaz = Xa.Z
		  ValuesN.χsx = Xs.X
		  ValuesN.χsy = Xs.Y
		  ValuesN.χsz = Xs.Z
		  ValuesN.Ψr = ΨrN
		  ValuesN.τr = τ*(1.0 + Parameters.Z)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UpdateVDerivatives()
		  DerivativesN.DvDV0 = PhaseEvolver.DvDV0
		  DerivativesN.DvDδ = PhaseEvolver.DvDδ
		  DerivativesN.DvDχ10x = PhaseEvolver.DvDχ10x
		  DerivativesN.DvDχ10y = PhaseEvolver.DvDχ10y
		  DerivativesN.DvDχ10z = PhaseEvolver.DvDχ10z
		  DerivativesN.DvDχ20x = PhaseEvolver.DvDχ20x
		  DerivativesN.DvDχ20y = PhaseEvolver.DvDχ20y
		  DerivativesN.DvDχ20z = PhaseEvolver.DvDχ20z
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UpdateχaDerivatives()
		  Var inverse2ε As Double = 1.0/(2*ε)
		  Var Dχa As Vector = VEvolver.SpinEvolver.χaN - VEvolver.SpinEvolver.χa0
		  DerivativesN.DχaxDZ = -Dχa.X*InverseOnePlusZ
		  DerivativesN.DχayDZ = -Dχa.Y*InverseOnePlusZ
		  DerivativesN.DχazDZ = -Dχa.Z*InverseOnePlusZ
		  Dχa = VEvolverForV0Plus.SpinEvolver.χaN -  VEvolverForV0Minus.SpinEvolver.χaN
		  DerivativesN.DχaxDV0 = Dχa.X*inverse2ε
		  DerivativesN.DχayDV0 = Dχa.Y*inverse2ε
		  DerivativesN.DχazDV0 = Dχa.Z*inverse2ε
		  Dχa = VEvolverForδPlus.SpinEvolver.χaN -  VEvolverForδMinus.SpinEvolver.χaN
		  DerivativesN.DχaxDδ = Dχa.X*inverse2ε
		  DerivativesN.DχayDδ = Dχa.Y*inverse2ε
		  DerivativesN.DχazDδ = Dχa.Z*inverse2ε
		  Dχa = VEvolverForχ10xPlus.SpinEvolver.χaN -  VEvolverForχ10xMinus.SpinEvolver.χaN
		  DerivativesN.DχaxDχ10x = Dχa.X*inverse2ε
		  DerivativesN.DχayDχ10x = Dχa.Y*inverse2ε
		  DerivativesN.DχazDχ10x = Dχa.Z*inverse2ε
		  Dχa = VEvolverForχ10yPlus.SpinEvolver.χaN -  VEvolverForχ10yMinus.SpinEvolver.χaN
		  DerivativesN.DχaxDχ10y = Dχa.X*inverse2ε
		  DerivativesN.DχayDχ10y = Dχa.Y*inverse2ε
		  DerivativesN.DχazDχ10y = Dχa.Z*inverse2ε
		  Dχa = VEvolverForχ10zPlus.SpinEvolver.χaN -  VEvolverForχ10zMinus.SpinEvolver.χaN
		  DerivativesN.DχaxDχ10z = Dχa.X*inverse2ε
		  DerivativesN.DχayDχ10z = Dχa.Y*inverse2ε
		  DerivativesN.DχazDχ10z = Dχa.Z*inverse2ε
		  Dχa = VEvolverForχ20xPlus.SpinEvolver.χaN -  VEvolverForχ20xMinus.SpinEvolver.χaN
		  DerivativesN.DχaxDχ20x = Dχa.X*inverse2ε
		  DerivativesN.DχayDχ20x = Dχa.Y*inverse2ε
		  DerivativesN.DχazDχ20x = Dχa.Z*inverse2ε
		  Dχa = VEvolverForχ20yPlus.SpinEvolver.χaN -  VEvolverForχ20yMinus.SpinEvolver.χaN
		  DerivativesN.DχaxDχ20y = Dχa.X*inverse2ε
		  DerivativesN.DχayDχ20y = Dχa.Y*inverse2ε
		  DerivativesN.DχazDχ20y = Dχa.Z*inverse2ε
		  Dχa = VEvolverForχ20zPlus.SpinEvolver.χaN -  VEvolverForχ20zMinus.SpinEvolver.χaN
		  DerivativesN.DχaxDχ20z = Dχa.X*inverse2ε
		  DerivativesN.DχayDχ20z = Dχa.Y*inverse2ε
		  DerivativesN.DχazDχ20z = Dχa.Z*inverse2ε
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UpdateχsDerivatives()
		  Var Dχs As Vector = VEvolver.SpinEvolver.χaN - VEvolver.SpinEvolver.χa0
		  DerivativesN.DχsxDZ = -Dχs.X*InverseOnePlusZ
		  DerivativesN.DχsyDZ = -Dχs.Y*InverseOnePlusZ
		  DerivativesN.DχszDZ = -Dχs.Z*InverseOnePlusZ
		  Var inverse2ε As Double = 1.0/(2*ε)
		  Dχs = VEvolverForV0Plus.SpinEvolver.χaN -  VEvolverForV0Minus.SpinEvolver.χaN
		  DerivativesN.DχsxDV0 = Dχs.X*inverse2ε
		  DerivativesN.DχsyDV0 = Dχs.Y*inverse2ε
		  DerivativesN.DχszDV0 = Dχs.Z*inverse2ε
		  Dχs = VEvolverForδPlus.SpinEvolver.χaN -  VEvolverForδMinus.SpinEvolver.χaN
		  DerivativesN.DχsxDδ = Dχs.X*inverse2ε
		  DerivativesN.DχsyDδ = Dχs.Y*inverse2ε
		  DerivativesN.DχszDδ = Dχs.Z*inverse2ε
		  Dχs = VEvolverForχ10xPlus.SpinEvolver.χaN -  VEvolverForχ10xMinus.SpinEvolver.χaN
		  DerivativesN.DχsxDχ10x= Dχs.X*inverse2ε
		  DerivativesN.DχsyDχ10x = Dχs.Y*inverse2ε
		  DerivativesN.DχszDχ10x= Dχs.Z*inverse2ε
		  Dχs = VEvolverForχ10yPlus.SpinEvolver.χaN -  VEvolverForχ10yMinus.SpinEvolver.χaN
		  DerivativesN.DχsxDχ10y= Dχs.X*inverse2ε
		  DerivativesN.DχsyDχ10y = Dχs.Y*inverse2ε
		  DerivativesN.DχszDχ10y= Dχs.Z*inverse2ε
		  Dχs = VEvolverForχ10zPlus.SpinEvolver.χaN -  VEvolverForχ10zMinus.SpinEvolver.χaN
		  DerivativesN.DχsxDχ10z= Dχs.X*inverse2ε
		  DerivativesN.DχsyDχ10z = Dχs.Y*inverse2ε
		  DerivativesN.DχszDχ10z= Dχs.Z*inverse2ε
		  Dχs = VEvolverForχ20xPlus.SpinEvolver.χaN -  VEvolverForχ20xMinus.SpinEvolver.χaN
		  DerivativesN.DχsxDχ20x= Dχs.X*inverse2ε
		  DerivativesN.DχsyDχ20x = Dχs.Y*inverse2ε
		  DerivativesN.DχszDχ20x= Dχs.Z*inverse2ε
		  Dχs = VEvolverForχ20yPlus.SpinEvolver.χaN -  VEvolverForχ20yMinus.SpinEvolver.χaN
		  DerivativesN.DχsxDχ20y= Dχs.X*inverse2ε
		  DerivativesN.DχsyDχ20y = Dχs.Y*inverse2ε
		  DerivativesN.DχszDχ20y= Dχs.Z*inverse2ε
		  Dχs = VEvolverForχ20zPlus.SpinEvolver.χaN -  VEvolverForχ20zMinus.SpinEvolver.χaN
		  DerivativesN.DχsxDχ20z= Dχs.X*inverse2ε
		  DerivativesN.DχsyDχ20z = Dχs.Y*inverse2ε
		  DerivativesN.DχszDχ20z= Dχs.Z*inverse2ε
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UpdateΨrDerivatives()
		  DerivativesN.DΨrDZ = PhaseEvolver.DΨrDZN
		  DerivativesN.DΨrDV0 = PhaseEvolver.DΨrDV0N
		  DerivativesN.DΨrDδ = PhaseEvolver.DΨrDδN
		  DerivativesN.DΨrDΘ = PhaseEvolver.DΨrDΘN
		  DerivativesN.DΨrDΦ = PhaseEvolver.DΨrDΦN
		  DerivativesN.DΨrDχ10x = PhaseEvolver.DΨrDχ10xN
		  DerivativesN.DΨrDχ10y = PhaseEvolver.DΨrDχ10yN
		  DerivativesN.DΨrDχ10z = PhaseEvolver.DΨrDχ10zN
		  DerivativesN.DΨrDχ20x = PhaseEvolver.DΨrDχ20xN
		  DerivativesN.DΨrDχ20y = PhaseEvolver.DΨrDχ20yN
		  DerivativesN.DΨrDχ20z = PhaseEvolver.DΨrDχ20zN
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		DerivativesN As CurrentDerivativesClass
	#tag EndProperty

	#tag Property, Flags = &h0
		DerivativesP As CurrentDerivativesClass
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
		InverseOnePlusZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		LastSourceStep As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		MainStep As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		MainStepsInSourceStep As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		Parameters As CaseParametersClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolver As PhaseEvolverClass
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
		ValuesN As CurrentValuesClass
	#tag EndProperty

	#tag Property, Flags = &h0
		ValuesP As CurrentValuesClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VEvolver As VEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VEvolverForV0Minus As VEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VEvolverForV0Plus As VEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VEvolverForδMinus As VEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VEvolverForδPlus As VEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VEvolverForχ10xMinus As VEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VEvolverForχ10xPlus As VEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VEvolverForχ10yMinus As VEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VEvolverForχ10yPlus As VEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VEvolverForχ10zMinus As VEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VEvolverForχ10zPlus As VEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VEvolverForχ20xMinus As VEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VEvolverForχ20xPlus As VEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VEvolverForχ20yMinus As VEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VEvolverForχ20yPlus As VEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VEvolverForχ20zMinus As VEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VEvolverForχ20zPlus As VEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		WhereInSourceStep As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		ε As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		τ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨrF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨrN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨrP As Double
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
			Name="DτF"
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
			Name="ε"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ΨrN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ΨrF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ΨrP"
			Visible=false
			Group="Behavior"
			InitialValue=""
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
			Name="InverseOnePlusZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
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
			Name="MainStepsInSourceStep"
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
			Name="LastSourceStep"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="MainStep"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="τ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
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
			Name="DτFF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass