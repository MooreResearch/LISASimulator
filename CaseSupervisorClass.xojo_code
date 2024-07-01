#tag Class
Protected Class CaseSupervisorClass
	#tag Method, Flags = &h0
		Sub Constructor(currentCaseInfo As CaseInfoClass)
		  // The CaseSupervisor class handles the running of each individual case.
		  // The constructor accepts the list of parameters from the currentCase ParameterSet
		  // and initializes the calculation
		  
		  StartTicks = System.Ticks
		  CaseInfo = currentCaseInfo // save the parameters for the current case
		  
		  // the following gives the number of main time steps to execute
		  Δτr = CaseInfo.ΔT/CaseInfo.GM
		  WaveBuilder = New WaveBuilderClass(CaseInfo) // create the WaveBuilder and initialize it
		  
		  // Create and initialize the ATA matrix
		  ATAMatrix = New Matrix(15) // Initalize an empty 15x15 matrix
		  ATAMatrix.InverseTest // Check that Matrix code is working
		  
		  // Create The Uncertainty Calculator
		  UncertaintyCalculator = New UncertaintyCalculatorClass(CaseInfo)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoSteps()
		  // This method actually executes the steps for the current case in question.
		  Try
		    For N = 0 to CaseInfo.NSteps
		      τr = N*Δτr // this is the current tau time (needed to update the user interface)
		      If WaveBuilder.DidDetectorStepOK(N) Then  // If the WaveBuilder was able to execute a sample step
		        'LoadATA(WaveBuilder.DHDq) // load the ATA matrix with the current values
		      Else  // If the WaveBuilder was not able to complete the sample step, we are at coalescence
		        TerminationMessage = "Coalescence Happened"
		        Exit  // Abort the loop
		      End If
		    Next
		    'UncertaintyCalculator.Calculate(ATAMatrix) // solve for the uncertainties
		  Catch err As RuntimeException
		    TerminationMessage = err.Message + " at step " + N.ToString
		  End Try
		  If TerminationMessage = "" Then terminationMessage = "Normal termination."
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
		CaseInfo As CaseInfoClass
	#tag EndProperty

	#tag Property, Flags = &h0
		CurrentStep As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		DataRecorder As DataRecorderClass
	#tag EndProperty

	#tag Property, Flags = &h0
		N As Integer
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
		WaveBuilder As WaveBuilderClass
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
			Name="CurrentStep"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
