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
   Height          =   806
   ImplicitInstance=   True
   MacProcID       =   0
   MaximumHeight   =   32000
   MaximumWidth    =   32000
   MenuBar         =   ""
   MenuBarVisible  =   False
   MinimumHeight   =   806
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
      Left            =   226
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
      Top             =   40
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   126
   End
   Begin DesktopSlider WidthSlider
      AllowAutoDeactivate=   False
      AllowLiveScrolling=   True
      Enabled         =   False
      Height          =   30
      Index           =   -2147483648
      Left            =   580
      LineStep        =   1
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   True
      LockTop         =   True
      MaximumValue    =   800
      MinimumValue    =   0
      PageStep        =   100
      Scope           =   0
      TabIndex        =   3
      TabPanelIndex   =   0
      TabStop         =   True
      TickMarkStyle   =   2
      Tooltip         =   ""
      Top             =   44
      Transparent     =   False
      Value           =   0
      Visible         =   True
      Width           =   400
   End
   Begin PlotCanvas MyPlotCanvas
      AllowAutoDeactivate=   True
      AllowFocus      =   False
      AllowFocusRing  =   True
      AllowTabs       =   False
      Backdrop        =   0
      DrawGrid        =   True
      Enabled         =   True
      Height          =   680
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
      Top             =   78
      Transparent     =   True
      Visible         =   True
      Width           =   960
   End
   Begin DesktopLabel ValueOfGraphWidthLabel
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   678
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
      Text            =   "0"
      TextAlignment   =   0
      TextColor       =   &c000000
      Tooltip         =   ""
      Top             =   12
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   302
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
      Left            =   364
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
      Top             =   44
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
      InitialValue    =   "t-s"
      Italic          =   False
      Left            =   394
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
      Top             =   47
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
      Left            =   226
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
      Width           =   119
   End
   Begin DesktopLabel CaptionForWidthSliderLabel
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      InitialParent   =   ""
      Italic          =   False
      Left            =   580
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Multiline       =   False
      Scope           =   0
      Selectable      =   False
      TabIndex        =   18
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "Graph Width:"
      TextAlignment   =   0
      TextColor       =   &c000000
      Tooltip         =   ""
      Top             =   12
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   86
   End
   Begin DesktopScrollbar StartScrollbar
      AllowAutoDeactivate=   True
      AllowFocus      =   True
      AllowLiveScrolling=   True
      Enabled         =   False
      Height          =   15
      Index           =   -2147483648
      Left            =   20
      LineStep        =   1
      LockBottom      =   True
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   True
      LockTop         =   False
      MaximumValue    =   1000
      MinimumValue    =   0
      PageStep        =   20
      Scope           =   0
      TabIndex        =   19
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   771
      Transparent     =   False
      Value           =   0
      Visible         =   True
      Width           =   960
   End
End
#tag EndDesktopWindow

#tag WindowCode
	#tag Event
		Sub Activated()
		  PlotTimes = TheSupervisor.DataRecorder.GetDataFor("t-s")
		  Setting = True
		  GraphChoicePopupMenu.RemoveAllRows
		  Var theNames() As String = TheSupervisor.DataRecorder.GetVariableNames
		  For i As Integer = 1 To theNames.LastIndex
		    GraphChoicePopupMenu.AddRow(theNames(i))
		  Next
		  GraphChoicePopupMenu.SelectedRowIndex = -1
		  StartScrollbar.Enabled = False
		  WidthSlider.Enabled = False
		  ValueOfGraphWidthLabel.Text = ""
		  Setting = False
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Function CalcTimeFromWidthValue(TheWidth As Integer) As Double
		  Return Pow(10.0, TheWidth/100)*10.0
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function CalcWidthValueFromTime(TheTime As Double) As Integer
		  Return Round((Log(TheTime)/Log(10.0) - 1.0)*100.0)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ChangeStart(StartValue As Integer)
		  PlotStartIndex = Round(StartScrollbar.Value/1000*PlotTimes.LastIndex)
		  PlotEndIndex = PlotStartIndex + Round(StartScrollbar.PageStep/1000*PlotTimes.LastIndex)
		  UpdatePlot
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ChangeWidth(WidthValue As Integer)
		  Var theTime As Double = CalcTimeFromWidthValue(WidthValue)
		  SetWidthDisplay(theTime)
		  Var theBarWidth As Integer = Round(1000*theTime/EndTime)
		  If theBarWidth >= 1000 Then 
		    StartScrollbar.Enabled = False
		  Else
		    Setting = True
		    StartScrollbar.MaximumValue = 1000 - theBarWidth
		    StartScrollbar.PageStep = theBarWidth
		    Setting = False
		    PlotStartIndex = Round(StartScrollbar.Value/1000*PlotTimes.LastIndex)
		    PlotEndIndex = PlotStartIndex + Round(theBarWidth/1000*PlotTimes.LastIndex)
		    If PlotStartIndex >= PlotEndIndex - 5 Then PlotStartIndex = PlotEndIndex - 5
		    StartScrollbar.Enabled = True
		  End If
		  UpdatePlot
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub PlotSelectedItem(TheItem As String)
		  If MyPlotData = Nil Then  // if this is the first call, then we will initialize the start and stop indices
		    EndTime = PlotTimes.LastIndex * TheSupervisor.BaseCase.ΔT // this is the maximum plot time span
		    SetStartToZero // set the start scale to zero
		    SetWidthToMax // Set scale slider to reflect the maximum
		  End If
		  
		  MyPlotData = New PlotData
		  Var values() As Double = TheSupervisor.DataRecorder.GetDataFor(TheItem)
		  MyPlotData.SetPlotArrays(PlotTimes, values)
		  MyPlotData.SetPlotIndexRange(PlotStartIndex, PlotEndIndex)
		  MyPlotCanvas.TheTitle = "Plot of " + TheItem + " as a Function of Time"
		  MyPlotCanvas.SetXAxisLabel("$t$ in seconds")
		  MyPlotCanvas.SetYAxisLabel(TheItem)
		  MyPlotCanvas.ClearPlotData
		  MyPlotCanvas.AddDataToPlot(MyPlotData)
		  WidthSlider.Enabled = True
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetStartToZero()
		  Setting = True
		  StartScrollbar.Value = 0
		  Setting = False
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetWidthDisplay(TheTime As Double)
		  Var theYears As Double = TheTime / TheSupervisor.BaseCase.Year
		  Var theDays As Double = TheTime / (3600*24)
		  ValueOfGraphWidthLabel.Text = TheTime.ToString + " s = " + TheDays.ToString + " d = " + TheYears.ToString + " y"
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetWidthToMax()
		  Setting = True
		  WidthSlider.Value = CalcWidthValueFromTime(EndTime)
		  StartScrollbar.Value = 0
		  StartScrollbar.MaximumValue = 1000
		  Setting = False
		  SetWidthDisplay(EndTime)
		  StartScrollbar.Enabled = False
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UpdatePlot()
		  // Call this after updating the plot starting time or width
		  If MyPlotData = Nil Then Return // no data to plot
		  
		  // Make sure that the start index is small enough to display at least 5 steps
		  If PlotStartIndex > PlotTimes.LastIndex - 5 Then PlotStartIndex = PlotTimes.LastIndex - 5
		  
		  MyPlotData.SetPlotIndexRange(PlotStartIndex, PlotEndIndex)
		  MyPlotCanvas.AddDataToPlot(Nil)
		  MyPlotCanvas.Refresh
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		EndTime As Double
	#tag EndProperty

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

	#tag Property, Flags = &h0
		TheSupervisor As CaseSupervisorClass
	#tag EndProperty


#tag EndWindowCode

#tag Events GraphChoicePopupMenu
	#tag Event
		Sub SelectionChanged(item As DesktopMenuItem)
		  If not Setting Then PlotSelectedItem(me.SelectedRowText)
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events WidthSlider
	#tag Event
		Sub ValueChanged()
		  If Not Setting Then ChangeWidth(me.Value)
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events StartScrollbar
	#tag Event
		Sub ValueChanged()
		  If Not Setting Then ChangeStart(me.Value)
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
	#tag ViewProperty
		Name="PlotEndIndex"
		Visible=false
		Group="Behavior"
		InitialValue="-1"
		Type="Integer"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="Setting"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
	#tag ViewProperty
		Name="EndTime"
		Visible=false
		Group="Behavior"
		InitialValue=""
		Type="Double"
		EditorType=""
	#tag EndViewProperty
#tag EndViewBehavior
