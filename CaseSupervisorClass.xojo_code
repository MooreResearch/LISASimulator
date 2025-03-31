#tag Class
Protected Class CaseSupervisorClass
	#tag Method, Flags = &h0
		Sub Constructor(currentCaseInfo As CaseInfoClass)
		  // The CaseSupervisor class handles running multiple cases
		  StartTicks = System.Ticks
		  CaseInfo = currentCaseInfo // save the parameters for the current case
		  // the following gives the number of main time steps to execute
		  Δτr = CaseInfo.ΔT/CaseInfo.GM
		  // Define the parameter shift
		  ep = 1.0e-6
		  // Define the Necdet class to store data from side cases
		  necdet = New NecdetsClass(CaseInfo, ep)
		  // Create and initialize cases with the shift in the parameter
		  CaseList(0) = CaseInfo // this is an identity equality: CaseList(0) *is* CaseInfo
		  // create clones for side cases. Clones are different objects but with the same parameters
		  CaseList(1) = CaseInfo.clone
		  CaseList(2) = CaseInfo.clone
		  // Adjust the parameter for the side cases
		  
		  CaseList(1).β = CaseInfo.β + ep
		  CaseList(2).β = CaseInfo.β - ep
		  
		  //CaseList(1).δ = CaseInfo.δ + ep
		  //CaseList(2).δ = CaseInfo.δ - ep
		  
		  // Create Wavebuilders for each side case
		  WaveBuilders(0) = New WaveBuilderClass(CaseInfo)
		  WaveBuilder = WaveBuilders(0)  // make double reference to the center case
		  // The center case will provide data for the graphs (side cases report to the necdet class)
		  CaseList(0).DataRecorder.SetDataSource(WaveBuilder)
		  WaveBuilders(1) = New WaveBuilderClass(CaseList(1))
		  WaveBuilders(2) = New WaveBuilderClass(CaseList(2))
		  WaveBuilder.necdet = necdet
		  
		  // Pass the WaveBuilders array and ep to calculate nA
		  
		  // Create and initialize the ATA matrix
		  //ATAMatrix = New Matrix(15) // Initalize an empty 15x15 matrix
		  //ATAMatrix.InverseTest // Check that Matrix code is working
		  
		  // Create The Uncertainty Calculator
		  //UncertaintyCalculator = New UncertaintyCalculatorClass(CaseInfo)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoSteps()
		  TerminationMessage = ""
		  TrY
		    For N = 0 to CaseInfo.NSteps
		      τr = N*Δτr // this is the current tau time (needed to update the user interface)
		      For i As Integer = 0 to 2
		        If WaveBuilders(i).DidDetectorStepOK(N) Then  // If the WaveBuilder was able to execute a sample step
		          // LoadATA(WaveBuilder.DHDq) // load the ATA matrix with the current values
		        Else  // If the WaveBuilder was not able to complete the sample step, we are at coalescence
		          TerminationMessage = "Coalescence Happened"
		          Exit  // Abort the loop
		        End If
		      Next
		      necdet.SetValues(WaveBuilders)
		      If TerminationMessage <> "" Then Exit
		      // Set up Wavebuilders(0).necdet with data
		    Next
		    // UncertaintyCalculator.Calculate(ATAMatrix) // solve for the uncertainties
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
		CaseList(2) As CaseInfoClass
	#tag EndProperty

	#tag Property, Flags = &h0
		CurrentStep As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		DataRecorder As DataRecorderClass
	#tag EndProperty

	#tag Property, Flags = &h0
		ep As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		N As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		necdet As NecdetsClass
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
		UncertaintyCalculators(2) As UncertaintyCalculatorClass
	#tag EndProperty

	#tag Property, Flags = &h0
		WaveBuilder As WaveBuilderClass
	#tag EndProperty

	#tag Property, Flags = &h0
		WaveBuilders(2) As WaveBuilderClass
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
		#tag ViewProperty
			Name="ep"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
