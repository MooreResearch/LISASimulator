#tag Class
Protected Class CaseSupervisorClass
	#tag Method, Flags = &h0
		Sub AssembleDerivatives()
		  // This method calculates the derivative of the signal h with respect
		  // to each parameter
		  
		  Var A() As Double = BaseWaveBuilder.A
		  Var w() As Double = BaseWaveBuilder.W
		  
		  // First get the value of the signal H
		  Var hSum As Double = 0.0
		  For j As Integer = 0 to LastWaveTermIndex
		    hSum = hSum + A(j)*w(j)
		  Next
		  H = hSum
		  
		  // Now calculate the derivatives
		  Var dwdα() As Double = BaseWaveBuilder.DWDα
		  Var dwdΨ() As Double = BaseWaveBuilder.DWDΨ
		  For i As Integer = 0 to LastParamIndex
		    If OneI2ε(i) = 0.0 Then // This is the signal that we are ignoring this parameter
		      DHI(i) = 0.0  // setting the derivative equal to zero is the signal for this
		    Else // otherwise we will calculate the derivative
		      Var oneOver2ε As Double = OneI2ε(i)
		      Var wbPlus As WaveBuilderClass = SideWBPlusFor(i)
		      Var wbMin As WaveBuilderClass = SideWBMinusFor(i)
		      Var AP() As Double = wbPlus.A
		      Var AM() As Double = wbMin.A
		      Var dαdq As Double = (wbPlus.αDN - wbMin.αDN)*oneOver2ε
		      Var dΨdq As Double = (wbPlus.ΨrDN - wbMin.ΨrDN)*oneOver2ε
		      Var dhdq As Double = 0.0
		      
		      // Calculate derivatives
		      For j As Integer = 0 to LastWaveTermIndex
		        dhdq = dhdq + (AP(j) - AM(j))*oneOver2ε*w(j) + A(j)*(dwdα(j)*dαdq + dwdΨ(j)*dΨdq)
		      Next
		      Var dhdqString As String = dhdq.ToString
		      DHI(i) = dhdq
		    End If
		  Next
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(currentCaseInfo As CaseInfoClass)
		  // The CaseSupervisor class handles running a particular case
		  StartTicks = System.Ticks // record starting time
		  BaseCase = currentCaseInfo // save the parameters for the current case
		  BaseWaveBuilder = New WaveBuilderClass(BaseCase)  // create the base wavebuilder
		  
		  // the following items specify the unitless time between detector steps
		  Δτr = BaseCase.ΔT / BaseCase.GM
		  NSteps = Floor(BaseCase.RunDuration / BaseCase.ΔT)
		  
		  // Set up difference factors and side case WaveBuilders
		  Var εVals() As Double = BaseCase.GetεVals
		  For i As Integer = 0 to LastParamIndex
		    If εVals(i) = 0.0 Then
		      OneI2ε(i) = 0.0  // This is the signal to ignore this case
		      // Note that SideWBPlusFor(i) = SideWBMinus(i) = Nil for this case
		    Else
		      OneI2ε(i) = 1/(2*εVals(i))
		      SideWBPlusFor(i) = New WaveBuilderClass(BaseCase.GetTweakedClone(i,1))
		      SideWBMinusFor(i) = New WaveBuilderClass(BaseCase.GetTweakedClone(i,-1))
		    End If
		  Next
		  
		  // Set up the DataRecorder
		  DataRecorder = New DataRecorderClass(BaseCase.GetVars2Save, BaseCase.GetDataDestination, self, NSteps)
		  
		  // Create and initialize the ATA matrix
		  //ATAMatrix = New Matrix(15) // Initalize an empty 15x15 matrix
		  //ATAMatrix.InverseTest // Check that Matrix code is working
		  
		  // Create The Uncertainty Calculator
		  //UncertaintyCalculator = New UncertaintyCalculatorClass(CaseInfo)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoSteps()
		  // This is the main program loop that does the calculations for each case
		  TerminationMessage = "" // clear any previous message
		  Var allOK As Boolean
		  Try // Various runtime errors may occur, so we trap those errors
		    For N = 0 to NSteps // loop over the possible steps
		      τr = N*Δτr // this is the current tau time in the detector frame (needed to update the user interface)
		      allOK = BaseWaveBuilder.DidDetectorStepOK(N) // do the base case step
		      If allOK Then // if the base case hasn't coalesced yet, then
		        For i As Integer = 0 to LastParamIndex // Execute all the side cases
		          If SideWBPlusFor(i) <> Nil Then // If we have a side case to execute
		            allOK = allOK And SideWBPlusFor(i).DidDetectorStepOK(N) And SideWBMinusFor(i).DidDetectorStepOK(N)
		            If not allOK Then Exit // if any of the side cases coalesce, then abort the side-case loop
		          End If
		        Next
		      End If
		      If allOK Then
		        AssembleDerivatives
		        DataRecorder.WriteData
		        // Add to the ATA and calculate uncertainties
		      Else
		        Exit // if we encountered any coalescence in any case, then abort the main loop
		      End If
		    Next
		    If allOK Then
		      // Calculate and solve for the uncertainties, which still might lead to runtime errors
		    Else
		      TerminationMessage = "Coalescence happened."
		    End If
		  Catch err As RuntimeException  // This will pick up any runtime errors
		    TerminationMessage = err.Message + " at step " + N.ToString
		  End Try
		  If TerminationMessage.IsEmpty Then terminationMessage = "Normal termination."
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub LoadATA(DHDQ() As Double)
		  // Add everything into the ATA matrix
		  For j As Integer = 0 To 14
		    For k As Integer = 0 to 14
		      ATAMatrix.PData(j,k) = ATAMatrix.PData(j,k) + DHDq(j)*DHDq(k)
		    Next
		  Next		  
		  
		  
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		ATAMatrix As Matrix
	#tag EndProperty

	#tag Property, Flags = &h0
		BaseCase As CaseInfoClass
	#tag EndProperty

	#tag Property, Flags = &h0
		BaseWaveBuilder As WaveBuilderClass
	#tag EndProperty

	#tag Property, Flags = &h0
		DataRecorder As DataRecorderClass
	#tag EndProperty

	#tag Property, Flags = &h0
		DHI(LastParamIndex) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		H As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		N As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		NSteps As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		OneI2ε(LastParamIndex) As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		SideWBMinusFor(LastParamIndex) As WaveBuilderClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SideWBPlusFor(LastParamIndex) As WaveBuilderClass
	#tag EndProperty

	#tag Property, Flags = &h0
		StartTicks As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		TerminationMessage As String
	#tag EndProperty

	#tag Property, Flags = &h0
		UncertaintyCalculator As UncertaintyCalculatorClass
	#tag EndProperty

	#tag Property, Flags = &h0
		Δτr As Double
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
			Name="Δτr"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H"
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
	#tag EndViewBehavior
End Class
#tag EndClass
