#tag DesktopWindow
Begin DesktopWindow GraphWindow
   Backdrop        =   0
   BackgroundColor =   &cFFFFFF
   Composite       =   False
   DefaultLocation =   2
   FullScreen      =   False
   HasBackgroundColor=   False
   HasCloseButton  =   True
   HasFullScreenButton=   False
   HasMaximizeButton=   True
   HasMinimizeButton=   True
   HasTitleBar     =   True
   Height          =   800
   ImplicitInstance=   True
   MacProcID       =   0
   MaximumHeight   =   32000
   MaximumWidth    =   32000
   MenuBar         =   ""
   MenuBarVisible  =   False
   MinimumHeight   =   800
   MinimumWidth    =   1000
   Resizeable      =   True
   Title           =   "Graph for Run"
   Type            =   0
   Visible         =   True
   Width           =   1000
   Begin DesktopLabel CaptionForGraphChoiceLabel
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   20
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Multiline       =   False
      Scope           =   0
      Selectable      =   False
      TabIndex        =   0
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "Graph The Checked Variables:"
      TextAlignment   =   0
      TextColor       =   &c000000
      Tooltip         =   ""
      Top             =   44
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   194
   End
   Begin DesktopPopupMenu GraphChoicePopupMenu
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   26
      Index           =   -2147483648
      InitialValue    =   ""
      Italic          =   False
      Left            =   20
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Scope           =   0
      SelectedRowIndex=   -1
      TabIndex        =   1
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   68
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   126
   End
   Begin DesktopPopupMenu ScaleMenu
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      InitialValue    =   "1.0\n0.5\n0.2\n0.1\n0.05\n0.02\n0.01\n0.005\n0.002\n0.001\n0.0005\n0.0002\n0.0001\n0.00005\n0.00001\n"
      Italic          =   False
      Left            =   889
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Scope           =   0
      SelectedRowIndex=   0
      TabIndex        =   2
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   72
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   91
   End
   Begin DesktopSlider StartSlider
      AllowAutoDeactivate=   True
      AllowLiveScrolling=   True
      Enabled         =   True
      Height          =   30
      Index           =   -2147483648
      Left            =   541
      LineStep        =   1
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      MaximumValue    =   1000
      MinimumValue    =   0
      PageStep        =   100
      Scope           =   0
      TabIndex        =   3
      TabPanelIndex   =   0
      TabStop         =   True
      TickMarkStyle   =   2
      Tooltip         =   ""
      Top             =   40
      Transparent     =   False
      Value           =   0
      Visible         =   True
      Width           =   439
   End
   Begin DesktopLabel CaptionForStartSliderLabel
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   372
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Multiline       =   False
      Scope           =   0
      Selectable      =   False
      TabIndex        =   4
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "Start (y):"
      TextAlignment   =   0
      TextColor       =   &c000000
      Tooltip         =   ""
      Top             =   42
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   60
   End
   Begin PlotCanvas MyPlotCanvas
      AllowAutoDeactivate=   True
      AllowFocus      =   False
      AllowFocusRing  =   True
      AllowTabs       =   False
      Backdrop        =   0
      DrawGrid        =   True
      Enabled         =   True
      Height          =   674
      Index           =   -2147483648
      Left            =   20
      LockBottom      =   True
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   True
      LockTop         =   True
      MarginBottom    =   20.0
      MarginLeft      =   20.0
      MarginRight     =   40.0
      MarginTop       =   20.0
      Scope           =   0
      TabIndex        =   5
      TabPanelIndex   =   0
      TabStop         =   True
      TheTitle        =   ""
      TitleFont       =   "System"
      TitleFontSize   =   18.0
      TitleOffset     =   15.0
      Tooltip         =   ""
      Top             =   106
      Transparent     =   True
      Visible         =   True
      Width           =   960
   End
   Begin DesktopLabel ValueOfGraphStartLabel
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   434
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Multiline       =   False
      Scope           =   0
      Selectable      =   False
      TabIndex        =   6
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "0.00000000"
      TextAlignment   =   0
      TextColor       =   &c000000
      Tooltip         =   ""
      Top             =   42
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   82
   End
   Begin DesktopLabel CaptionForGraphWidthLabel
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   743
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Multiline       =   False
      Scope           =   0
      Selectable      =   False
      TabIndex        =   7
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "Graph Width / All Data:"
      TextAlignment   =   0
      TextColor       =   &c000000
      Tooltip         =   ""
      Top             =   69
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   141
   End
   Begin DesktopButton BumpUpButton
      AllowAutoDeactivate=   True
      Bold            =   False
      Cancel          =   False
      Caption         =   "Up"
      Default         =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   453
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      MacButtonStyle  =   0
      Scope           =   0
      TabIndex        =   8
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   69
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   51
   End
   Begin DesktopLabel CaptionForBumpLabel
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   372
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Multiline       =   False
      Scope           =   0
      Selectable      =   False
      TabIndex        =   9
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "Bump Start"
      TextAlignment   =   0
      TextColor       =   &c000000
      Tooltip         =   ""
      Top             =   69
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   77
   End
   Begin DesktopButton BumpDownButton
      AllowAutoDeactivate=   True
      Bold            =   False
      Cancel          =   False
      Caption         =   "Down"
      Default         =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   516
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      MacButtonStyle  =   0
      Scope           =   0
      TabIndex        =   10
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   69
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   51
   End
   Begin DesktopLabel CaptionForBumpTypeLabel
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   579
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Multiline       =   False
      Scope           =   0
      Selectable      =   False
      TabIndex        =   11
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "by"
      TextAlignment   =   0
      TextColor       =   &c000000
      Tooltip         =   ""
      Top             =   69
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   24
   End
   Begin DesktopPopupMenu BumpTypePopupMenu
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      InitialValue    =   "Single Step\nWindow Width\n10 Steps\n100 Steps\n1000 Steps\n0.00001 Of Span\n0.0001 Of Span\n0.001 Of Span\n0.01 Of Span\n0.1 Of Span"
      Italic          =   False
      Left            =   603
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Scope           =   0
      SelectedRowIndex=   0
      TabIndex        =   12
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   72
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   128
   End
   Begin DesktopLabel CaptionVsLabel
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   154
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Multiline       =   False
      Scope           =   0
      Selectable      =   False
      TabIndex        =   13
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "vs"
      TextAlignment   =   0
      TextColor       =   &c000000
      Tooltip         =   ""
      Top             =   72
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   18
   End
   Begin DesktopPopupMenu GraphHorizPopupMenu
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   False
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      InitialValue    =   "t-y"
      Italic          =   False
      Left            =   181
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Scope           =   0
      SelectedRowIndex=   0
      TabIndex        =   14
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   73
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   126
   End
   Begin DesktopLabel CaptionForGraphSourceLabel
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   20
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Multiline       =   False
      Scope           =   0
      Selectable      =   False
      TabIndex        =   15
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "Data Source:"
      TextAlignment   =   0
      TextColor       =   &c000000
      Tooltip         =   ""
      Top             =   12
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   87
   End
   Begin DesktopPopupMenu GraphSourcePopupMenu
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   False
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      InitialValue    =   "Memory\nFolder"
      Italic          =   False
      Left            =   110
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Scope           =   0
      SelectedRowIndex=   0
      TabIndex        =   16
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   14
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   104
   End
   Begin DesktopLabel GraphSourceFolderLabel
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   247
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Multiline       =   False
      Scope           =   0
      Selectable      =   False
      TabIndex        =   17
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   ""
      TextAlignment   =   0
      TextColor       =   &c000000
      Tooltip         =   ""
      Top             =   12
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   269
   End
End
#tag EndDesktopWindow

#tag WindowCode
	#tag Method, Flags = &h0
		Sub DoBump(Down as Boolean)
		  Var trialStep As Integer
		  Select Case BumpTypePopupMenu.SelectedRowText
		  Case "Single Step"
		    trialStep = 1
		  Case "Window Width"
		    Var selectedScale As Double = ScaleMenu.SelectedRowText.ToDouble
		    trialStep = Round(selectedScale * PlotTimes.LastIndex)
		  Case "10 Steps"
		    trialStep = 10
		  Case "100 Steps"
		    trialStep = 100
		  Case "1000 Steps"
		    trialStep = 1000
		  Case "0.00001 Of Span"
		    trialStep = Round(0.00001*PlotTimes.LastIndex)
		  Case "0.0001 Of Span"
		    trialStep = Round(0.0001*PlotTimes.LastIndex)
		  Case "0.001 Of Span"
		    trialStep = Round(0.001*PlotTimes.LastIndex)
		  Case "0.01 Of Span"
		    trialStep = Round(0.01*PlotTimes.LastIndex)
		  Case "0.1 of Span"
		    trialStep = Round(0.01*PlotTimes.LastIndex)
		  End Select
		  If Down Then
		    PlotStartIndex = PlotStartIndex - trialStep
		    If PlotStartIndex < 0 Then PlotStartIndex = 0
		  Else
		    PlotStartIndex = PlotStartIndex + trialStep
		    If PlotStartIndex > PlotTimes.LastIndex - 5 Then PlotStartIndex = PlotTimes.LastIndex - 5
		  End If
		  Setting = True
		  StartSlider.Value = Round(PlotStartIndex/PlotTimes.LastIndex*StartSlider.MaximumValue)
		  Setting = False
		  UpdatePlot
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub PlotSelectedItem(TheItem As String)
		  If MyPlotData = Nil Then  // if this is the first call, then we must load the start/stop settings
		    // Set the start of the plot
		    PlotStartIndex = Round(StartSlider.Value/StartSlider.MaximumValue*PlotTimes.LastIndex)
		    If PlotStartIndex > PlotTimes.LastIndex -5 Then PlotStartIndex = PlotTimes.LastIndex - 5
		    
		    // Get the plot scale
		    Var selectedScale As Double = ScaleMenu.SelectedRowText.ToDouble
		    
		    // Calculate the new EndTimeIndex
		    PlotEndIndex = Round(selectedScale * PlotTimes.LastIndex) + PlotStartIndex
		    
		    // Ensure EndTimeIndex does not exceed the maximum index
		    If PlotEndIndex > PlotTimes.LastIndex Then
		      PlotEndIndex = PlotTimes.LastIndex
		    End If
		  End If
		  
		  MyPlotData = New PlotData
		  // Var values() As Double = TheCases(0).DataRecorder.GetDataFor(TheItem)
		  //MyPlotData.SetPlotArrays(PlotTimes, values)
		  MyPlotData.SetPlotIndexRange(PlotStartIndex, PlotEndIndex)
		  MyPlotCanvas.TheTitle = "Plot of " + TheItem + " as a Function of Time"
		  MyPlotCanvas.SetXAxisLabel("$t$ in years")
		  MyPlotCanvas.SetYAxisLabel(TheItem)
		  MyPlotCanvas.ClearPlotData
		  MyPlotCanvas.AddDataToPlot(MyPlotData)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UpdatePlot()
		  If MyPlotData = Nil Then Return // no data to plot
		  
		  // Make sure that the start index is small enough to display at least 5 steps
		  If PlotStartIndex > PlotTimes.LastIndex - 5 Then PlotStartIndex = PlotTimes.LastIndex - 5
		  
		  // Get the plot scale
		  Var selectedScale As Double = ScaleMenu.SelectedRowText.ToDouble
		  
		  // Calculate the new EndTimeIndex
		  PlotEndIndex = Round(selectedScale * PlotTimes.LastIndex) + PlotStartIndex
		  
		  // Ensure EndTimeIndex does not exceed the maximum index
		  If PlotEndIndex > PlotTimes.LastIndex Then
		    PlotEndIndex = PlotTimes.LastIndex
		  End If
		  
		  MyPlotData.SetPlotIndexRange(PlotStartIndex, PlotEndIndex)
		  MyPlotCanvas.AddDataToPlot(Nil)
		  MyPlotCanvas.Refresh
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		MyPlotData As PlotData
	#tag EndProperty

	#tag Property, Flags = &h0
		PlotEndIndex As Integer = -1
	#tag EndProperty

	#tag Property, Flags = &h0
		PlotStartIndex As Integer = -1
	#tag EndProperty

	#tag Property, Flags = &h0
		PlotTimes() As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Setting As Boolean
	#tag EndProperty


#tag EndWindowCode

#tag Events GraphChoicePopupMenu
	#tag Event
		Sub SelectionChanged(item As DesktopMenuItem)
		  If not Setting Then
		    PlotSelectedItem(me.SelectedRowText)
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events ScaleMenu
	#tag Event
		Sub SelectionChanged(item As DesktopMenuItem)
		  UpdatePlot
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events StartSlider
	#tag Event
		Sub ValueChanged()
		  If Not Setting Then
		    PlotStartIndex = Round(me.Value/me.MaximumValue*PlotTimes.LastIndex)
		    ValueOfGraphStartLabel.Text = Format(me.Value/me.MaximumValue*PlotTimes(PlotTimes.LastIndex), "0.00000000")
		    UpdatePlot
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events BumpUpButton
	#tag Event
		Sub Pressed()
		  DoBump(False)
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events BumpDownButton
	#tag Event
		Sub Pressed()
		  DoBump(True)
		End Sub
	#tag EndEvent
#tag EndEvents
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
		Name="Interfaces"
		Visible=true
		Group="ID"
		InitialValue=""
		Type="String"
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
		Name="Width"
		Visible=true
		Group="Size"
		InitialValue="600"
		Type="Integer"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="Height"
		Visible=true
		Group="Size"
		InitialValue="400"
		Type="Integer"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="MinimumWidth"
		Visible=true
		Group="Size"
		InitialValue="64"
		Type="Integer"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="MinimumHeight"
		Visible=true
		Group="Size"
		InitialValue="64"
		Type="Integer"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="MaximumWidth"
		Visible=true
		Group="Size"
		InitialValue="32000"
		Type="Integer"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="MaximumHeight"
		Visible=true
		Group="Size"
		InitialValue="32000"
		Type="Integer"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="Type"
		Visible=true
		Group="Frame"
		InitialValue="0"
		Type="Types"
		EditorType="Enum"
		#tag EnumValues
			"0 - Document"
			"1 - Movable Modal"
			"2 - Modal Dialog"
			"3 - Floating Window"
			"4 - Plain Box"
			"5 - Shadowed Box"
			"6 - Rounded Window"
			"7 - Global Floating Window"
			"8 - Sheet Window"
			"9 - Modeless Dialog"
		#tag EndEnumValues
	#tag EndViewProperty
	#tag ViewProperty
		Name="Title"
		Visible=true
		Group="Frame"
		InitialValue="Untitled"
		Type="String"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="HasCloseButton"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="HasMaximizeButton"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="HasMinimizeButton"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="HasFullScreenButton"
		Visible=true
		Group="Frame"
		InitialValue="False"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="HasTitleBar"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="Resizeable"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="Composite"
		Visible=false
		Group="OS X (Carbon)"
		InitialValue="False"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="MacProcID"
		Visible=false
		Group="OS X (Carbon)"
		InitialValue="0"
		Type="Integer"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="FullScreen"
		Visible=true
		Group="Behavior"
		InitialValue="False"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="DefaultLocation"
		Visible=true
		Group="Behavior"
		InitialValue="2"
		Type="Locations"
		EditorType="Enum"
		#tag EnumValues
			"0 - Default"
			"1 - Parent Window"
			"2 - Main Screen"
			"3 - Parent Window Screen"
			"4 - Stagger"
		#tag EndEnumValues
	#tag EndViewProperty
	#tag ViewProperty
		Name="Visible"
		Visible=true
		Group="Behavior"
		InitialValue="True"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="ImplicitInstance"
		Visible=true
		Group="Window Behavior"
		InitialValue="True"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="HasBackgroundColor"
		Visible=true
		Group="Background"
		InitialValue="False"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="BackgroundColor"
		Visible=true
		Group="Background"
		InitialValue="&cFFFFFF"
		Type="ColorGroup"
		EditorType="ColorGroup"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Backdrop"
		Visible=true
		Group="Background"
		InitialValue=""
		Type="Picture"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="MenuBar"
		Visible=true
		Group="Menus"
		InitialValue=""
		Type="DesktopMenuBar"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="MenuBarVisible"
		Visible=true
		Group="Deprecated"
		InitialValue="False"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="PlotStartIndex"
		Visible=false
		Group="Behavior"
		InitialValue="-1"
		Type="Integer"
		EditorType=""
	#tag EndViewProperty
#tag EndViewBehavior
