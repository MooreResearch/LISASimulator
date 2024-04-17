#tag Class
Protected Class CaseSupervisorClass
	#tag Method, Flags = &h0
		Sub Constructor(currentCaseParameterSet As CaseParametersClass)
		  // The CaseSupervisor class handles the running of each individual case.
		  // The constructor accepts the list of parameters from the currentCase ParameterSet
		  // and initializes the calculation
		  
		  StartTicks = System.Ticks
		  CaseParameters = currentCaseParameterSet // save the parameters for the current case
		  // the following gives the number of main time steps to execute
		  NSteps = Round(CaseParameters.RunDuration*CaseParameters.Year/CaseParameters.ΔT)
		  DτrD = CaseParameters.ΔT/CaseParameters.GM
		  WaveBuilder = New WaveBuilderClass(CaseParameters) // create the WaveBuilder and initialize it
		  // Create and initialize the ATA matrix
		  ATAMatrix = New Matrix(15) // Initalize an empty 15x15 matrix
		  ATAMatrix.InverseTest // Check that Matrix code is working
		  // Create The Uncertainty Calculator
		  UncertaintyCalculator = New UncertaintyCalculatorClass(CaseParameters)
		  MainWindow.ChartArray.ResizeTo(22,NSteps)
		  MainWindow.TotSteps = 0
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoSteps()
		  // This method actually executes the steps for the current case in question.
		  // When it is done, the uncertainties should be in the Uncertainties property.
		  
		  Try
		    For N = 0 to NSteps
		      τr = N*DτrD // this is the current tau time (needed to update the user interface)
		      If WaveBuilder.DidDetectorStepOK(N) Then  // If the WaveBuilder was able to execute a sample step
		        LoadATA(WaveBuilder.DHDq)
		        LoadChartArray(WaveBuilder, N)
		        // Load the ATA matrix with the current values
		      Else  // If the WaveBuilder was not able to complete the sample step, we are at coalescence
		        TerminationMessage = "Coalescence Happened"
		        Exit  // Abort the loop
		      End If
		    Next
		    
		    
		    
		    Uncertainty = UncertaintyCalculator.Calculate(ATAMatrix, CaseParameters.Θ) // solve for the uncertainties
		    
		    MainWindow.UncertaintyList.ResizeTo(MainWindow.UncertaintyList.LastIndex + 1, 15)
		    
		    MainWindow.UncertaintyList(MainWindow.UncertaintyList.LastIndex, 0) = Uncertainty.OfM
		    MainWindow.UncertaintyList(MainWindow.UncertaintyList.LastIndex, 1) = Uncertainty.Ofδ
		    MainWindow.UncertaintyList(MainWindow.UncertaintyList.LastIndex, 2) = Uncertainty.OfT0
		    MainWindow.UncertaintyList(MainWindow.UncertaintyList.LastIndex, 3) = Uncertainty.OfR
		    MainWindow.UncertaintyList(MainWindow.UncertaintyList.LastIndex, 4) = Uncertainty.Ofβ
		    MainWindow.UncertaintyList(MainWindow.UncertaintyList.LastIndex, 5) = Uncertainty.Ofψ
		    MainWindow.UncertaintyList(MainWindow.UncertaintyList.LastIndex, 6) = Uncertainty.Ofλ0
		    MainWindow.UncertaintyList(MainWindow.UncertaintyList.LastIndex, 7) = Uncertainty.OfΘ
		    MainWindow.UncertaintyList(MainWindow.UncertaintyList.LastIndex, 8) = Uncertainty.OfΦ
		    MainWindow.UncertaintyList(MainWindow.UncertaintyList.LastIndex, 9) = Uncertainty.OfΩ
		    MainWindow.UncertaintyList(MainWindow.UncertaintyList.LastIndex, 10) = Uncertainty.Ofχ10x
		    MainWindow.UncertaintyList(MainWindow.UncertaintyList.LastIndex, 11) = Uncertainty.Ofχ10y
		    MainWindow.UncertaintyList(MainWindow.UncertaintyList.LastIndex, 12) = Uncertainty.Ofχ10z
		    MainWindow.UncertaintyList(MainWindow.UncertaintyList.LastIndex, 13) = Uncertainty.Ofχ20x
		    MainWindow.UncertaintyList(MainWindow.UncertaintyList.LastIndex, 14) = Uncertainty.Ofχ20y
		    MainWindow.UncertaintyList(MainWindow.UncertaintyList.LastIndex, 15) = Uncertainty.Ofχ20z
		    
		    
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

	#tag Method, Flags = &h0
		Sub LoadChartArray(WaveBuilder As WaveBuilderClass, N As Integer)
		  //Loads data into ChartArray
		  
		  
		  MainWindow.TotSteps = MainWindow.TotSteps + 1
		  MainWindow.ChartArray(0,N) = N*50.0 //0th row is time values
		  MainWindow.ChartArray(1,N) = Sqrt(WaveBuilder.HP^2 + WaveBuilder.HX^2) // First Row is H array
		  MainWindow.ChartArray(2,N) = WaveBuilder.HP // 2nd row is HP
		  MainWindow.ChartArray(3,N) = WaveBuilder.HX // 3rd row is HX
		  MainWindow.ChartArray(4,N) = WaveBuilder.VDN //4th row is V
		  MainWindow.ChartArray(5,N) = WaveBuilder.ιDN //5th row is ι
		  MainWindow.ChartArray(6,N) = WaveBuilder.αDN //6th row is α
		  MainWindow.ChartArray(7,N) = WaveBuilder.DHDq(0) // 7th row is dhdM
		  MainWindow.ChartArray(8,N) = WaveBuilder.DHDq(1) // 8th row is dhdψ
		  MainWindow.ChartArray(9,N) = WaveBuilder.DHDq(2) // 9th row is dhdλ0
		  MainWindow.ChartArray(10,N) = WaveBuilder.DHDq(3) //10th row is dhdΘ
		  MainWindow.ChartArray(11,N) = WaveBuilder.DHDq(4) //11th row is dhdΦ
		  MainWindow.ChartArray(12,N) = WaveBuilder.DHDq(5) //12th row is dhdβ
		  MainWindow.ChartArray(13,N) = WaveBuilder.DHDq(6) //13th row is dhdR
		  MainWindow.ChartArray(14,N) = WaveBuilder.DHDq(7) //14th row is dhdV0
		  MainWindow.ChartArray(15,N) = WaveBuilder.DHDq(8) //15th row is dhdδ
		  MainWindow.ChartArray(16,N) = WaveBuilder.DHDq(9) //16th row is χ10x
		  MainWindow.ChartArray(17,N) = WaveBuilder.DHDq(10) //17th row is χ10y
		  MainWindow.ChartArray(18,N) = WaveBuilder.DHDq(11) //18th row is χ10z
		  MainWindow.ChartArray(19,N) = WaveBuilder.DHDq(12) //19th row is χ20x
		  MainWindow.ChartArray(20,N) = WaveBuilder.DHDq(13) //20th row is χ20y
		  MainWindow.ChartArray(20,N) = WaveBuilder.DHDq(14) //21st row is χ20z
		  
		  
		  
		  
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		ATAMatrix As Matrix
	#tag EndProperty

	#tag Property, Flags = &h0
		CaseParameters As CaseParametersClass
	#tag EndProperty

	#tag Property, Flags = &h0
		CurrentStep As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		DτrD As Double
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
		WaveBuilder As WaveBuilderClass
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
			Name="DτrD"
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
