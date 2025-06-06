#tag DesktopWindow
Begin DesktopWindow RunWindow
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
   Height          =   776
   ImplicitInstance=   True
   MacProcID       =   0
   MaximumHeight   =   32000
   MaximumWidth    =   32000
   MenuBar         =   1095792639
   MenuBarVisible  =   False
   MinimumHeight   =   664
   MinimumWidth    =   1000
   Resizeable      =   True
   Title           =   "LISA Simulator"
   Type            =   0
   Visible         =   True
   Width           =   779
   Begin DesktopRectangle RectForRunState
      AllowAutoDeactivate=   True
      BorderColor     =   &c000000
      BorderThickness =   1.0
      CornerSize      =   0.0
      Enabled         =   True
      FillColor       =   &cFFFFFFFF
      Height          =   373
      Index           =   -2147483648
      Left            =   427
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Scope           =   0
      TabIndex        =   11
      TabPanelIndex   =   0
      Tooltip         =   ""
      Top             =   72
      Transparent     =   False
      Visible         =   True
      Width           =   305
      Begin DesktopLabel CaptionForStepRatioLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForRunState"
         Italic          =   False
         Left            =   450
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   1
         TabPanelIndex   =   0
         TabStop         =   True
         Text            =   "Source/Display Step Ratio:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   313
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   169
      End
      Begin DesktopLabel CaptionForVLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForRunState"
         Italic          =   False
         Left            =   450
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   2
         TabPanelIndex   =   0
         TabStop         =   True
         Text            =   "Current PN Factor v:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   283
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
      End
      Begin DesktopLabel CaptionForSimTimeLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForRunState"
         Italic          =   False
         Left            =   450
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   3
         TabPanelIndex   =   0
         TabStop         =   True
         Text            =   "Simulation Time (y):"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   253
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
      End
      Begin DesktopLabel CaptionForStepNumLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForRunState"
         Italic          =   False
         Left            =   450
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
         Text            =   "Current Step Number:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   223
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
      End
      Begin DesktopLabel CaptionForRunTimeLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForRunState"
         Italic          =   False
         Left            =   450
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   5
         TabPanelIndex   =   0
         TabStop         =   True
         Text            =   "Computation Time (s):"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   193
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
      End
      Begin ProgressBar CaseProgressBar
         AllowAutoDeactivate=   True
         Enabled         =   True
         Height          =   14
         Indeterminate   =   False
         Index           =   -2147483648
         InitialParent   =   "RectForRunState"
         Left            =   450
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         MaximumValue    =   100
         Scope           =   0
         TabIndex        =   6
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   135
         Transparent     =   True
         Value           =   0.0
         Visible         =   True
         Width           =   259
      End
      Begin DesktopLabel CaptionForProgressLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForRunState"
         Italic          =   False
         Left            =   447
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
         Text            =   "Case Progress:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   116
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   99
      End
      Begin DesktopLabel ValueOfStatusLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForRunState"
         Italic          =   False
         Left            =   508
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   8
         TabPanelIndex   =   0
         TabStop         =   True
         Text            =   "Not Started"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   84
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   164
      End
      Begin DesktopLabel CaptionForStatusLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForRunState"
         Italic          =   False
         Left            =   447
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
         Text            =   "Status:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   84
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   49
      End
      Begin DesktopLabel ValueOfStopReasonLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   32
         Index           =   -2147483648
         InitialParent   =   "RectForRunState"
         Italic          =   False
         Left            =   450
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   True
         Scope           =   0
         Selectable      =   False
         TabIndex        =   10
         TabPanelIndex   =   0
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   371
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   255
      End
      Begin DesktopLabel CaptionForStopReasonLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForRunState"
         Italic          =   False
         Left            =   450
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
         Text            =   "Reason For Stopping:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   343
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
      End
      Begin DesktopLabel CaptionForTc
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   24
         Index           =   -2147483648
         InitialParent   =   "RectForRunState"
         Italic          =   False
         Left            =   450
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   12
         TabPanelIndex   =   0
         TabStop         =   True
         Text            =   "Time to Coalescence (y):"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   163
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   158
      End
      Begin DesktopLabel ValueOfTc
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForRunState"
         Italic          =   False
         Left            =   618
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
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   167
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   95
      End
      Begin DesktopLabel ValueOfRunTimeLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForRunState"
         Italic          =   False
         Left            =   618
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   14
         TabPanelIndex   =   0
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   193
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   95
      End
      Begin DesktopLabel ValueOfSimTimeLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForRunState"
         Italic          =   False
         Left            =   618
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
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   253
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   95
      End
      Begin DesktopLabel ValueOfStepNumberLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForRunState"
         Italic          =   False
         Left            =   618
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   16
         TabPanelIndex   =   0
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   225
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   95
      End
      Begin DesktopLabel ValueOfStepRatioLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForRunState"
         Italic          =   False
         Left            =   618
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
         Top             =   313
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   95
      End
      Begin DesktopLabel ValueOfVLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForRunState"
         Italic          =   False
         Left            =   618
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
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   285
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   95
      End
      Begin DesktopButton GraphButton
         AllowAutoDeactivate=   True
         Bold            =   False
         Cancel          =   False
         Caption         =   "Graph Data"
         Default         =   False
         Enabled         =   False
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForRunState"
         Italic          =   False
         Left            =   447
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         MacButtonStyle  =   0
         Scope           =   0
         TabIndex        =   19
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   415
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   95
      End
   End
   Begin Timer InterfaceUpdateTimer
      Enabled         =   True
      Index           =   -2147483648
      LockedInPosition=   False
      Period          =   500
      RunMode         =   0
      Scope           =   0
      TabPanelIndex   =   0
   End
   Begin MainThreadClass MainThread
      DebugIdentifier =   ""
      Index           =   -2147483648
      LockedInPosition=   False
      Priority        =   5
      Scope           =   0
      StackSize       =   0
      TabPanelIndex   =   0
      ThreadID        =   0
      ThreadState     =   0
      Type            =   0
   End
   Begin DesktopListBox ParamNameListBox
      AllowAutoDeactivate=   True
      AllowAutoHideScrollbars=   True
      AllowExpandableRows=   False
      AllowFocusRing  =   False
      AllowResizableColumns=   False
      AllowRowDragging=   False
      AllowRowReordering=   False
      Bold            =   False
      ColumnCount     =   1
      ColumnWidths    =   "85"
      DefaultRowHeight=   26
      DropIndicatorVisible=   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      GridLineStyle   =   3
      HasBorder       =   True
      HasHeader       =   True
      HasHorizontalScrollbar=   False
      HasVerticalScrollbar=   False
      HeadingIndex    =   -1
      Height          =   674
      Index           =   -2147483648
      InitialValue    =   "Parameter\nM (sols)*\nδ\nTorb (s)*\nλ0\nχ1	\nθ1 (°)\nφ1 (°)\nχ2\nθ2 (°)\nφ2 (°)\nβ(°)	\nψ (°)\nR (ly)*\nΘ (°)\nΦ (°)	\nΩ (/sky)\nΔT (s)\nDuration (y)\nPN (Amp)\nPN (Phase)\nDetector\nDetΘ0 (°)\nDetΦ0 (°)\nDetρ0 (°)\nCase ID"
      Italic          =   False
      Left            =   47
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      RequiresSelection=   False
      RowSelectionType=   0
      Scope           =   0
      TabIndex        =   0
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   72
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   85
      _ScrollOffset   =   0
      _ScrollWidth    =   -1
   End
   Begin DesktopListBox CaseListBoxParams
      AllowAutoDeactivate=   True
      AllowAutoHideScrollbars=   True
      AllowExpandableRows=   False
      AllowFocusRing  =   True
      AllowResizableColumns=   True
      AllowRowDragging=   False
      AllowRowReordering=   False
      Bold            =   False
      ColumnCount     =   1
      ColumnWidths    =   "80"
      DefaultRowHeight=   26
      DropIndicatorVisible=   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      GridLineStyle   =   3
      HasBorder       =   True
      HasHeader       =   True
      HasHorizontalScrollbar=   False
      HasVerticalScrollbar=   False
      HeadingIndex    =   -1
      Height          =   414
      Index           =   -2147483648
      InitialValue    =   "Case 1\n10000\n0.1\n500\n0\n0\n0\n0\n0\n0\n0\n39\n0\n1.0e7\n5\n268.5"
      Italic          =   False
      Left            =   131
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      RequiresSelection=   False
      RowSelectionType=   0
      Scope           =   0
      TabIndex        =   1
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   72
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   92
      _ScrollOffset   =   0
      _ScrollWidth    =   -1
   End
   Begin DesktopListBox UncertaintyListBox
      AllowAutoDeactivate=   True
      AllowAutoHideScrollbars=   True
      AllowExpandableRows=   False
      AllowFocusRing  =   False
      AllowResizableColumns=   False
      AllowRowDragging=   False
      AllowRowReordering=   False
      Bold            =   False
      ColumnCount     =   1
      ColumnWidths    =   "160"
      DefaultRowHeight=   26
      DropIndicatorVisible=   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      GridLineStyle   =   1
      HasBorder       =   True
      HasHeader       =   True
      HasHorizontalScrollbar=   False
      HasVerticalScrollbar=   False
      HeadingIndex    =   -1
      Height          =   441
      Index           =   -2147483648
      InitialValue    =   "Uncertainty\n?\n?\n?\n?\n?\n?\n?\n?\n?\n?\n?\n?\n?\n?\n?\n?"
      Italic          =   False
      Left            =   282
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      RequiresSelection=   False
      RowSelectionType=   0
      Scope           =   0
      TabIndex        =   8
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   72
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   133
      _ScrollOffset   =   0
      _ScrollWidth    =   -1
   End
   Begin DesktopRectangle RectForFile
      AllowAutoDeactivate=   True
      BorderColor     =   &c000000
      BorderThickness =   1.0
      CornerSize      =   0.0
      Enabled         =   True
      FillColor       =   &cFFFFFFFF
      Height          =   40
      Index           =   -2147483648
      Left            =   282
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Scope           =   0
      TabIndex        =   10
      TabPanelIndex   =   0
      Tooltip         =   ""
      Top             =   20
      Transparent     =   False
      Visible         =   True
      Width           =   450
      Begin DesktopCheckBox RunFileCheckBox
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   "Run Case File"
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForFile"
         Italic          =   False
         Left            =   293
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         TabIndex        =   0
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   30
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   0
         Width           =   120
      End
      Begin DesktopLabel CaptionForFProgLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForFile"
         Italic          =   False
         Left            =   425
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   1
         TabPanelIndex   =   0
         TabStop         =   True
         Text            =   "File Progress:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   30
         Transparent     =   False
         Underline       =   False
         Visible         =   False
         Width           =   99
      End
      Begin ProgressBar FileProgressBar
         AllowAutoDeactivate=   True
         Enabled         =   True
         Height          =   14
         Indeterminate   =   False
         Index           =   -2147483648
         InitialParent   =   "RectForFile"
         Left            =   522
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         MaximumValue    =   100
         Scope           =   0
         TabIndex        =   2
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   30
         Transparent     =   True
         Value           =   0.0
         Visible         =   False
         Width           =   191
      End
   End
   Begin DesktopRectangle RectForSave
      AllowAutoDeactivate=   True
      BorderColor     =   &c000000
      BorderThickness =   1.0
      CornerSize      =   0.0
      Enabled         =   True
      FillColor       =   &cFFFFFFFF
      Height          =   299
      Index           =   -2147483648
      Left            =   427
      LockBottom      =   True
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Scope           =   0
      TabIndex        =   12
      TabPanelIndex   =   0
      Tooltip         =   ""
      Top             =   457
      Transparent     =   False
      Visible         =   True
      Width           =   305
      Begin DesktopLabel CaptionForSaveDataLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForSave"
         Italic          =   False
         Left            =   447
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
         Text            =   "Save Plotting Data To:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   468
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   141
      End
      Begin DesktopPopupMenu DataDestinationMenu
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForSave"
         InitialValue    =   "memory\nfile"
         Italic          =   False
         Left            =   600
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         SelectedRowIndex=   0
         TabIndex        =   1
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   469
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   112
      End
      Begin DesktopListBox PlotItemsListBox
         AllowAutoDeactivate=   True
         AllowAutoHideScrollbars=   True
         AllowExpandableRows=   False
         AllowFocusRing  =   True
         AllowResizableColumns=   False
         AllowRowDragging=   False
         AllowRowReordering=   False
         Bold            =   False
         ColumnCount     =   1
         ColumnWidths    =   ""
         DefaultRowHeight=   22
         DropIndicatorVisible=   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         GridLineStyle   =   1
         HasBorder       =   True
         HasHeader       =   False
         HasHorizontalScrollbar=   False
         HasVerticalScrollbar=   True
         HeadingIndex    =   -1
         Height          =   200
         Index           =   -2147483648
         InitialParent   =   "RectForSave"
         InitialValue    =   ""
         Italic          =   False
         Left            =   566
         LockBottom      =   True
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         RequiresSelection=   False
         RowSelectionType=   0
         Scope           =   0
         TabIndex        =   2
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   535
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   146
         _ScrollOffset   =   0
         _ScrollWidth    =   -1
      End
      Begin DesktopLabel CaptionForSaveListLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForSave"
         Italic          =   False
         Left            =   447
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   3
         TabPanelIndex   =   0
         TabStop         =   True
         Text            =   "Saved Items List:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   535
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   114
      End
      Begin DesktopPopupMenu ChooseVariablePopupMenu
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForSave"
         InitialValue    =   ""
         Italic          =   False
         Left            =   566
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         SelectedRowIndex=   -1
         TabIndex        =   4
         TabPanelIndex   =   0
         TabStop         =   True
         Tooltip         =   ""
         Top             =   501
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   146
      End
      Begin DesktopLabel CaptionForChooseSaveLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "RectForSave"
         Italic          =   False
         Left            =   447
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   5
         TabPanelIndex   =   0
         TabStop         =   True
         Text            =   "Choose To Save:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   500
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   107
      End
   End
   Begin DesktopButton StartStopButton
      AllowAutoDeactivate=   True
      Bold            =   False
      Cancel          =   False
      Caption         =   "Run"
      Default         =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      InitialParent   =   ""
      Italic          =   False
      Left            =   47
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      MacButtonStyle  =   0
      Scope           =   0
      TabIndex        =   13
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   31
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   80
   End
   Begin DesktopButton AnalyzeButton
      AllowAutoDeactivate=   True
      Bold            =   False
      Cancel          =   False
      Caption         =   "Analyze"
      Default         =   False
      Enabled         =   False
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   20
      Index           =   -2147483648
      Italic          =   False
      Left            =   295
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      MacButtonStyle  =   0
      Scope           =   0
      TabIndex        =   14
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   525
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   95
   End
   Begin DesktopListBox εListBox
      AllowAutoDeactivate=   True
      AllowAutoHideScrollbars=   True
      AllowExpandableRows=   False
      AllowFocusRing  =   True
      AllowResizableColumns=   False
      AllowRowDragging=   False
      AllowRowReordering=   False
      Bold            =   False
      ColumnCount     =   1
      ColumnWidths    =   ""
      DefaultRowHeight=   26
      DropIndicatorVisible=   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      GridLineStyle   =   1
      HasBorder       =   True
      HasHeader       =   True
      HasHorizontalScrollbar=   False
      HasVerticalScrollbar=   False
      HeadingIndex    =   -1
      Height          =   414
      Index           =   -2147483648
      InitialValue    =   "ε\n1e-6\n1e-6\n1e-6\n1e-6\n1e-6\n1e-6\n1e-6\n1e-6\n1e-6\n1e-6\n1e-6\n1e-6\n1e-6\n1e-6\n1e-6"
      Italic          =   False
      Left            =   222
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      RequiresSelection=   False
      RowSelectionType=   0
      Scope           =   0
      TabIndex        =   16
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   72
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   61
      _ScrollOffset   =   0
      _ScrollWidth    =   -1
   End
   Begin DesktopLabel InfoLabel
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   11.0
      FontUnit        =   0
      Height          =   57
      Index           =   -2147483648
      Italic          =   False
      Left            =   235
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Multiline       =   True
      Scope           =   0
      Selectable      =   False
      TabIndex        =   18
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "*Uncertainties and ε for starred parameters are fractional. Uncertainties for parameters with ε=0 are not calculated."
      TextAlignment   =   0
      TextColor       =   &c000000
      Tooltip         =   ""
      Top             =   557
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   180
   End
   Begin DesktopListBox CaseListBoxDetData
      AllowAutoDeactivate=   True
      AllowAutoHideScrollbars=   True
      AllowExpandableRows=   False
      AllowFocusRing  =   True
      AllowResizableColumns=   True
      AllowRowDragging=   False
      AllowRowReordering=   False
      Bold            =   False
      ColumnCount     =   1
      ColumnWidths    =   "80"
      DefaultRowHeight=   26
      DropIndicatorVisible=   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      GridLineStyle   =   3
      HasBorder       =   True
      HasHeader       =   False
      HasHorizontalScrollbar=   False
      HasVerticalScrollbar=   False
      HeadingIndex    =   -1
      Height          =   235
      Index           =   -2147483648
      InitialValue    =   "20\n1.0\n3\n7\nFixed20\n0\n0\n0\n\n"
      Italic          =   False
      Left            =   131
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      RequiresSelection=   False
      RowSelectionType=   0
      Scope           =   0
      TabIndex        =   9
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   511
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   92
      _ScrollOffset   =   0
      _ScrollWidth    =   -1
   End
End
#tag EndDesktopWindow

#tag WindowCode
	#tag Event
		Sub Opening()
		  LoadPlotItemsList
		  UpdateChooseVarMenu
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Function ConvertToDegrees(Value As Double) As Double
		  Var degFromRad As Double = 180.0/3.14159265358979
		  If Value.IsNotANumber Then
		    Return Value
		  Else
		    Return Value*degFromRad
		  End If
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CustomizeChosenPlotItem(TheItem As PlotItemClass)
		  If TheItem.GetIndexMax = LastParamIndex Then
		    Var chosenParameter As String
		    SelectParameterDialog.ShowModal
		    chosenParameter = SelectParameterDialog.Param
		    If chosenParameter = "(Cancel)" Then
		      Return
		    Else
		      TheItem.SetIndex(Index4Derivative(chosenParameter))
		    End If
		  ElseIf TheItem.GetIndexMax = LastWaveTermIndex Then
		    Var chosenParameter As String
		    SelectIndexDialog.ShowModal
		    chosenParameter = SelectIndexDialog.SubmittedIndex
		    If chosenParameter = "" Then  // if we have cancelled
		      Return
		    Else
		      TheItem.SetIndex(chosenParameter.ToInteger)
		    End If
		  End If
		  PlotItemsListBox.AddRow(TheItem.GetName)
		  PlotItemsListBox.RowTagAt(PlotItemsListBox.LastAddedRowIndex) = TheItem
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DisableInterface()
		  // This method prevents the user from doing anything except stop while cases are running
		  RunFileCheckBox.Enabled = False
		  CaseListBoxParams.Enabled = False
		  CaseListBoxDetData.Enabled = False
		  εListBox.Enabled = False
		  UncertaintyListBox.Enabled = False
		  GraphButton.Enabled = False
		  AnalyzeButton.Enabled = False
		  DataDestinationMenu.Enabled = False
		  ChooseVariablePopupMenu.Enabled = False
		  PlotItemsListBox.Enabled = False
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DisplayUncertainties(CaseInfo As CaseInfoClass)
		  'UncertaintyListBox.CellTextAt(0) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.M)))
		  'UncertaintyListBox.CellTextAt(1) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.δ)))
		  'UncertaintyListBox.CellTextAt(2) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.V0)))
		  'UncertaintyListBox.CellTextAt(3) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.R)))
		  'UncertaintyListBox.CellTextAt(4) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.β)))
		  'UncertaintyListBox.CellTextAt(5) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.ψ)))
		  'UncertaintyListBox.CellTextAt(6) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.λ0)))
		  'Var uTheta As Double = CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.Θ))
		  'Var uPhi As Double = CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.Φ))
		  'UncertaintyListBox.CellTextAt(7) = GetUncertaintyString(uTheta)
		  'UncertaintyListBox.CellTextAt(8) = GetUncertaintyString(uPhi)
		  'Var d2r As Double = CaseInfo.π/180.0
		  'Var omega As Double = Sin(CaseInfo.Θ)*uTheta*uPhi/(4*CaseInfo.π)*d2r*d2r
		  'UncertaintyListBox.CellTextAt(9) = GetUncertaintyString(omega)
		  'UncertaintyListBox.CellTextAt(10) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.chi10x)))
		  'UncertaintyListBox.CellTextAt(11) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.chi10y)))
		  'UncertaintyListBox.CellTextAt(12) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.chi10z)))
		  'UncertaintyListBox.CellTextAt(13) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.chi20x)))
		  'UncertaintyListBox.CellTextAt(14) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.chi20y)))
		  'UncertaintyListBox.CellTextAt(15) = GetUncertaintyString(CaseInfo.Uncertainties(Integer(CaseInfoClass.Param.chi20z)))
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoStart()
		  // Start running a case or cases
		  TheCases.RemoveAll  // Clear out any pre-existing cases
		  LastCaseSupervisor = Nil // We do not yet have a case to analyze or graph
		  if RunFileCheckBox.Value then // if we are running cases from a file
		    Try
		      GetCasesFromFile // get the cases from the file (this might generate an exception)
		    Catch e As RuntimeException // catch any problems
		      MessageBox(e.Message) // display the error message
		      Return // and bail out of the rest of the method
		    End Try
		    // if we survive getting the cases, then
		    OutputFile = FolderItem.ShowSaveFileDialog(FileTypeGroup1.Text, "Untitled.txt") // define an output file
		    If OutputFile = Nil Then Return // or bail out if the user has cancelled
		  Else // we are getting the case from the display
		    TheCases.Add(GetDisplayCase) // get the case
		  End if
		  LoadPlotItemsList // reload the plot items list with fresh values
		  UpdateChooseVarMenu // update the menu
		  AllCasesDone = False // set the flag indicating whether we have run all cases
		  DisableInterface // disable any parameter or saving information while we are running
		  ValueOfStatusLabel.Text = "Running" // indicate the status
		  ValueOfStopReasonLabel.Text = "" // clear any stop reason message
		  MainThread.LoadCases(TheCases) // load cases into the thread
		  MainThread.Priority = Thread.HighPriority // set the thread priority to be high
		  MainThread.Start // start the thread running
		  // The interface update timer fires every so often to allow items in the user interface to be updated
		  InterfaceUpdateTimer.RunMode = Timer.RunModes.Multiple // set the timer going
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoStop()
		  // Perform a manual stop of the current run
		  MainThread.Stop
		  ValueOfStatusLabel.Text = "Stopped"
		  ValueOfStopReasonLabel.Text = "Manual Stop"
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub EnableInterface()
		  // This method enables the user interface when cases have stopped running
		  RunFileCheckBox.Enabled = True
		  CaseListBoxParams.Enabled = True
		  CaseListBoxDetData.Enabled = True
		  εListBox.Enabled = True
		  UncertaintyListBox.Enabled = True
		  GraphButton.Enabled = True
		  AnalyzeButton.Enabled = True
		  DataDestinationMenu.Enabled = True
		  ChooseVariablePopupMenu.Enabled = True
		  PlotItemsListBox.Enabled = True
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub GetCasesFromFile()
		  // To make a text file that can be read by this code:
		  //    Rows should parameter values,
		  //    in the order shown in the CaseListBoxParams and CaseListBoxDetData listboxes, separated by tabs.
		  //    All parameters need values. Any parameters whose uncertainties are not to be solved for
		  //    should have the letter "x" as the first character in the parameter value (this will be ignored for the last 5 values).
		  //    The parameter for the PNOrder item (item 17) should be two integers separated by a comma.
		  //    Do not write data in text file with spaces (not even after an x). The file should have UTF8 encoding.
		  //    Note that the method produces an exception if any line in the file does not have the correct number of
		  //    parameter values. This exception should be trapped by whatever routine calls this one.
		  Var f As FolderItem
		  Var textInput As TextInputStream
		  Var rowFromFile As String
		  
		  f = FolderItem.ShowOpenFileDialog(FileTypeGroup1.Text) // ask the user for a text file containing a set of cases to process
		  
		  If f <> Nil Then
		    textInput = TextInputStream.Open(f)  // open the file
		    textInput.Encoding = Encodings.UTF8  // set the encoding so that we interpret it correctly
		    Do
		      rowFromFile = textInput.ReadLine // read a line from the file
		      Var values() As String = rowFromFile.ToArray(String.Chr(9))  // convert the line to an array
		      If values.LastIndex > 0 Then // ignore any blank lines
		        Var theID As String = values(0) // get the ID value
		        values.RemoveAt(0) // and remove that value from the list
		        If values.LastIndex <> LastCaseIndex + 1 Then Raise New RuntimeException("Case file format is incorrect.")
		        Var solveList(14) As Boolean // create a new list of flags telling whether we are solving for a parameter uncertainty or not
		        For i As Integer = 0 to values.LastIndex // go through 22 parameter values we have obtained
		          // but note that only the first 15 correspond to actual binary parameters we might solve for
		          If i < 15 Then solveList(i) = True  // the default for whether we solve for a parameter's uncertainty is True
		          If values(i).Contains("x") Then // But if we have an "x" anywhere in the item except for the ID
		            values(i) = values(i).ReplaceAll("x","") // remove it from that item
		            If i < 15 Then solveList(i) = False  // and we will not solve for the uncertainty of this parameter
		          End If
		        Next
		        //Var thisCase As CaseInfoClass = GetCaseFromValues(values, solveList, theID)  // create the case from the file
		        //TheCases.Add(thisCase)  // add the case to the list to be processed
		      End If
		    Loop Until textInput.EndOfFile  // keep going until we get to the end of the file
		    textInput.Close // close the input file
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetDisplayCase() As CaseInfoClass
		  // This method gets information from the user interface to run a particular case.
		  Var theParams As String = CaseListBoxParams.CellTextAt(0) // Get first entry in the list box
		  For i As Integer = 1 to CaseListBoxParams.LastRowIndex // for all remaining entries in the main parameter list box
		    theParams = theParams + "," + CaseListBoxParams.CellTextAt(i) // add a comma and the next item in the list box
		  Next
		  For i As Integer = 0 to CaseListBoxDetData.LastRowIndex // for all entries in the detector parameter list box
		    theParams = theParams + "," + CaseListBoxDetData.CellTextAt(i) // add the text entered in the list box
		  Next
		  Var theEps As String =  εListBox.CellTextAt(0) // get the first entry in the epsilion list box
		  For i As Integer = 1 to εListBox.LastRowIndex  // for all remaining entries in the  epsilon list box
		    theEps = theEps + "," + εListBox.CellTextAt(i) // add a comma followed by the next item in the list box
		  Next
		  Return New CaseInfoClass(theParams, theEps, GetMyPlotItems, DataDestinationMenu.SelectedRowText = "file")
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetMyPlotItems() As PlotItemClass()
		  // This method returns an array consisting of the list of plot items 
		  // in the PlotItemsListBox. Note that to be consistent with other
		  // methods, we must always have a "t-s" item appearing first.
		  Var thePlotItems() As PlotItemClass // create the array to return
		  If PlotItemsListBox.LastRowIndex > -1 Then // if we have any items at all
		    // Look through the items for "t-s", and remove it if it appears
		    For i As Integer = 0 to PlotItemsListBox.LastRowIndex
		      If PlotItemsListBox.CellTextAt(i) = "t-s" Then
		        PlotItemsListBox.RemoveRowAt(i)
		      End If
		    Next
		    If PlotItemsListBox.LastRowIndex > -1 Then // if we have any items left
		      PlotItemsListBox.AddRowAt(0,"t-s") // make sure the first row is "t-s"
		      PlotItemsListBox.RowTagAt(0) = New PlotItemTs
		      // Now read the items from the listbox and add them to theNames
		      For i As Integer = 0 to PlotItemsListBox.LastRowIndex // add the remaining items to the array
		        thePlotItems.Add(PlotItemsListBox.RowTagAt(i))
		      Next
		    End If
		  End If
		  Return thePlotItems
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetUncertaintyString(uc As Double) As String
		  If uc.IsNotANumber then
		    Return "(Imaginary)"
		  ElseIf uc.IsInfinite Then
		    Return "(Not Solved For)"
		  Else
		    Return "± " + uc.ToString
		  End If
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub LoadPlotItemsList()
		  // This method creates a list of possible variables that we might plot.
		  // There should be one item for each subclass of PlotItemsClass defined in the
		  // PlotItemSubclasses folder.
		  
		  PlotItemsList.RemoveAll 
		  PlotItemsList.Add(New PlotItemH)
		  PlotItemsList.Add(New PlotItemDH)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OpenGraphWindow()
		  If LastCaseSupervisor <> Nil And LastCaseSupervisor.DataRecorder.HasItems Then
		    GraphWindow.TheSupervisor = LastCaseSupervisor
		    GraphWindow.Show
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function SultanUncertainty(value As Double, uncertainty As Double) As String
		  'Number Formatting:
		  
		  'Start the program.
		  'In the first input field or prompt, enter the value that you want to measure the uncertainty for. This can be done via a pop-up window, a slider, a number picker, or any other user interface element depending on your website design.
		  'This should be a numerical value. If any non-numeric characters are entered, a message box will appear saying "Invalid value entered!".
		  'In the second input field or prompt, enter the associated uncertainty of the value you have just entered. This can also be done via a pop-up window, a slider, a number picker, or any other user interface element.
		  'This should be a positive numerical value. The program considers uncertainty as a positive value. If you enter a negative number or zero for uncertainty, you will get a message box saying "Uncertainty must be greater than zero!".
		  'After you've entered the value and its uncertainty, submit your data. This can be done by clicking the 'Submit' button, pressing the 'Enter' key, or another action based on your website design.
		  
		  'Calculation Process:
		  
		  'Once the values are inputted correctly, the program will calculate the base-10 logarithm for both the value and uncertainty. It will then round the value and uncertainty according to the computed exponents.
		  'The program will display a message box with the computed exponents and rounded uncertainty.
		  
		  'Result Output:
		  
		  'The program will finally adjust the rounded uncertainty for the final display and determine the number of spaces needed for proper alignment.
		  'It checks whether the number is negative, and accordingly adds spaces or a minus sign before the number for proper formatting.
		  'Depending on the exponent of the value, the program will generate the final output in two formats: normal and scientific notation.
		  'If the exponent of the value is within the range from -3 to 6, both the value and the uncertainty will be displayed in normal notation.
		  'Otherwise, both will be displayed in scientific notation.
		  
		  'Special Case:
		  
		  'There is a special case where the exponent of the value is greater than 0 and the exponent of the uncertainty is less than 0. In this case, the program will prepare the value with all decimal places (0 to valueExponent) and uncertainty to 8 decimal places.
		  'Remember, this program is designed to handle and display uncertainty values correctly, but it is your responsibility to ensure the accuracy and correctness of the input data.
		  
		  If uncertainty.IsNotANumber Then
		    Return value.ToString + EndOfLine + "(Imaginary)"
		  Elseif uncertainty.IsInfinite Then
		    Return value.ToString + EndOfLine + "(Not Solved For)"
		  End If
		  
		  ' Ensure the uncertainty is a positive value. If it's not, raise an exception and display a message box
		  Try
		    If uncertainty <= 0 Then
		      Raise New RuntimeException("Uncertainty must be greater than zero!")
		    End If
		  Catch e As RuntimeException
		    MsgBox(e.Message)
		    Return ""
		  End Try
		  
		  ' Calculate the exponent of the value and uncertainty, which is the integer part of their base-10 logarithm
		  Var valueExponent As Integer = Floor(Log(Abs(value)) / Log(10))
		  Var uncertaintyExponent As Integer = Floor(Log(uncertainty) / Log(10))
		  
		  ' Compute the rounded value mantissa, which is the absolute value divided by 10 raised to its exponent, then rounded to 3 decimal places
		  Var valueMantissa As Double = abs(value / (10 ^ valueExponent))
		  Var roundedValueMantissa As Double = Round(valueMantissa * 1000) / 1000
		  
		  ' Compute the rounded uncertainty in a similar way, but round to the nearest integer
		  Var uncertaintyMantissa As Double = uncertainty / (10 ^ uncertaintyExponent)
		  Var roundedUncertainty As Double = Round(uncertaintyMantissa * 10)
		  
		  
		  ' Adjust the rounded uncertainty for final display by multiplying by 10^-3 and rounding to 3 decimal places
		  Var uncertaintyForDisplay As Double = roundedUncertainty * 10 ^ (-3)
		  
		  ' Determine whether a space or a minus sign is needed before the value
		  Var BeforeValue As String = "  "
		  if value < 0 then BeforeValue = "- "
		  
		  ' Determine the number of spaces needed for proper alignment in the final display
		  Var spacesNeeded As Integer
		  Var spaceString As String = ""
		  
		  ' Set the format string
		  Dim formattedValue, formattedUncertainty As String 
		  formattedValue = Format(value, "0.00000000") 
		  
		  ' If uncertainty exceeds e-7, add an asterisk after the uncertainty value
		  if uncertaintyExponent < -7 Then
		    formattedUncertainty = "0.00000000*"
		  else
		    formattedUncertainty = Format(uncertainty, "0.00000000")
		  End If
		  
		  ' Determine the number of spaces needed to align the uncertainty under the last digit of the value
		  spacesNeeded = Len(formattedValue) - Len(formattedUncertainty)
		  
		  ' Generate the string of spaces needed for alignment
		  For i As Integer = 1 To spacesNeeded
		    spaceString = spaceString + " "
		  Next
		  
		  
		  
		  ' Check for the special case where the exponent of the value is greater than 0 and the exponent of the uncertainty is less than 0.
		  If valueExponent > 0 And uncertaintyExponent < 0 Then
		    ' Prepare the value with all decimal places (0 to valueExponent) and uncertainty to 8 decimal places
		    Var formatStr As String = "0."
		    For i As Integer = 1 To valueExponent
		      formatStr = formatStr + "0"
		    Next
		    
		    Var valueStr As String = Format(value, formatStr)
		    Var uncertaintyStr As String
		    ' If uncertainty exceeds e-7, add an asterisk after the uncertainty value
		    if uncertaintyExponent < -7 Then
		      uncertaintyStr = "0.00000000*"
		    else
		      uncertaintyStr = Format(uncertainty, "0.00000000")
		    End If
		    
		    ' Determine the number of spaces needed to align the uncertainty under the last digit of the value
		    spacesNeeded = valueStr.Length - uncertaintyStr.Length + 2  ' add two more for the space and "±"
		    
		    ' Generate the string of spaces needed for alignment
		    spaceString = ""
		    For i As Integer = 1 To spacesNeeded
		      spaceString = spaceString + " "
		    Next
		    
		    ' Return the final formatted string
		    Return BeforeValue + valueStr + EndOfLine + "±" + spaceString + uncertaintyStr
		  End If
		  
		  ' Check if the value exponent is within the range -3 to 6. If it is, format the value and uncertainty accordingly
		  If valueExponent >= -3 And valueExponent <= 6 Then
		    ' Format value and uncertainty
		    Var valueStr As String
		    Var uncertaintyStr As String
		    
		    ' Find number of decimal places in uncertainty
		    Var numDecimalPlacesUncertainty As Integer = Len(Uncertainty.ToString) - InStr(Uncertainty.ToString, ".")
		    
		    ' Select correct format based on number of decimal places in uncertainty
		    If numDecimalPlacesUncertainty = 1 Then
		      valueStr = Format(value, "0.0")
		      uncertaintyStr = Format(uncertainty, "0.0")
		    Else
		      valueStr = Format(value, "0.00")
		      If valueExponent > -4 Then
		        uncertaintyStr = Format(uncertainty, "0.0") ' Round uncertainty to one decimal place
		      Else
		        uncertaintyStr = Format(uncertainty, "0.00")
		      End If
		    End If
		    
		    ' Determine the number of spaces needed to align the uncertainty under the value
		    spacesNeeded = valueStr.Length - uncertaintyStr.Length
		    
		    ' Generate the string of spaces needed for alignment
		    spaceString = ""
		    For i As Integer = 1 To spacesNeeded
		      spaceString = spaceString + " "
		    Next
		    
		    ' Return the final formatted string
		    Return BeforeValue + valueStr + EndOfLine + spaceString + "± " + uncertaintyStr
		  Else
		    ' If the value exponent is outside the range -3 to 6, use scientific notation for the value and uncertainty
		    Var plusMinusLine As String
		    ' If uncertainty exceeds e-7, add an asterisk after the uncertainty value
		    if uncertaintyExponent < -7 Then
		      plusMinusLine = spaceString + "± 0.00000000* e" + Str(valueExponent)
		    else
		      plusMinusLine = spaceString + "± " + Str(uncertaintyForDisplay) + "e" + Str(valueExponent)
		    End If
		    Return BeforeValue + Str(roundedValueMantissa) + "e" + Str(valueExponent) + EndOfLine + plusMinusLine
		  End If
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UpdateChooseVarMenu()
		  Setting = True
		  ChooseVariablePopupMenu.RemoveAllRows
		  For Each plotitem As PlotItemClass in PlotItemsList
		    Var Found As Boolean = False
		    For i As Integer = 0 to PlotItemsListBox.LastRowIndex
		      Found = Found Or plotitem = PlotItemsListBox.RowTagAt(i)
		      If Found Then Exit
		    Next
		    If Not Found Then
		      ChooseVariablePopupMenu.AddRow(plotitem.GetName)
		      ChooseVariablePopupMenu.RowTagAt(ChooseVariablePopupMenu.LastAddedRowIndex) = plotitem
		    End If
		  Next
		  ChooseVariablePopupMenu.SelectedRowIndex = -1
		  Setting = False
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UpdateInterface()
		  // This method is called by the InterfaceUpdateTimer when it is running
		  Var theSuper As CaseSupervisorClass = MainThread.CaseSupervisor  // Get a reference to the supervisor
		  Var theCase As CaseInfoClass = theSuper.BaseCase  // get a reference to the current case info
		  // Whether the thread is running or not, update display of these values
		  ValueOfTc.Text = Format(theCase.τc*theCase.GM/theCase.Year, "0.0000e#")  // time to coalescence
		  ValueOfSimTimeLabel.Text = Format(theSuper.τr*theCase.GM/theCase.Year, "0.0000000")  // current simulation time
		  ValueOfVLabel.Text = Format(theSuper.BaseWaveBuilder.SpinResults.V,"0.000000")  // current value of V
		  ValueOfRunTimeLabel.Text = Format((System.Ticks - theSuper.StartTicks)/60.0, "###0.00")  // current run time
		  ValueOfStepNumberLabel.Text = theSuper.N.ToString // step number
		  If MainThread.State = Thread.Running then  // if the thread is running
		    CaseProgressBar.Value = Round(theSuper.N*100/theSuper.NSteps)  // update the progress bar
		    If LastCaseSupervisor <> Nil Then // if we have a completed case to analyze or graph, enable these buttons
		      AnalyzeButton.Enabled = True
		      GraphButton.Enabled = LastCaseSupervisor.DataRecorder.HasItems // we need to have data to graph to enable
		    End If
		  Else // the thread has stopped, meaning that all cases are done
		    CaseProgressBar.Value = 0  // reset the progress bar
		    StartStopButton.Caption = "Run" // fix the button label
		    InterfaceUpdateTimer.RunMode = Timer.RunModes.Off // and we need no more updates
		    ValueOfStatusLabel.Text = "Stopped" // let the user know we have stopped
		    ValueOfStopReasonLabel.Text = TheSuper.TerminationMessage // update the termination message
		    'If TheSuper.CaseInfo.Uncertainties <> Nil Then  // if we have uncertainties to display
		    'DisplayUncertainties(theCase) // display them
		    'MatrixChoicePopupMenu.SelectedRowIndex = 0 // reset the matrix chooser to display the ATA matrix
		    'DisplayMatrix(TheSuper.ATAMatrix) // display that matrix
		    'ValueOfConditionLabel.Text = Format(TheSuper.UncertaintyCalculator.Condition, "0.000e-0##") // and condition number
		    'End if
		    
		    // If we had been running cases from a file, then write out the results
		    if RunFileCheckBox.Value and AllCasesDone Then WriteUncertaintyFiles
		    
		    EnableInterface // re-enable the user interface
		    
		  End if          
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub WriteUncertaintyFiles()
		  // When we are done running a set of cases from a case file
		  
		  'Var t As TextOutputStream = TextOutputStream.Create(OutputFile) // create a text file using the name the user created
		  'Var tab As String = Chr(9)
		  '// create a header listing the variables and uncertainties in order
		  't.WriteLine("ID"+tab+"M"+tab+"δ"+tab+"τc"+tab+"λ0"+tab+"χ1"+tab+"θ1"+tab+"φ1"+tab+"χ2" _
		  '+tab+"θ2"+tab+"φ2"+tab+"β"+tab+"ψ"+tab+"R"+tab+"Θ"+tab+"Φ"+tab+"ρ0"+tab+"PNOrder"+tab+"Detcs" _
		  '+tab+"ΔT"+tab+"Dur"+tab +"σM"+tab+"σδ"+tab+"στc"+tab+"σλ0"+tab+"σχ1"+tab+"σθ1"+tab+"σφ1"+tab+"σχ2" _
		  '+tab+"σθ2"+tab+"σφ2"+tab+"σβ"+tab+"σψ"+tab+"σR"+tab+"σΘ"+tab+"σΦ"+tab+"σΩ")
		  '// then for each case we have run,
		  'For Each caseItem As CaseInfoClass In TheCases
		  'Var theID As String = caseitem.ID // get the idea
		  'Var theValues(19) As String  // will be a list of values
		  'Var value As Double // temporary storage for numbers
		  'theValues(0) = caseItem.M.ToString
		  'theValues(1) = caseItem.δ.ToString
		  'value = caseitem.τc*caseitem.GM/caseitem.Year
		  'theValues(2) = value.ToString
		  'theValues(3) = ConvertToDegrees(caseitem.λ0).ToString
		  'theValues(4) = caseitem.χ1.ToString
		  'theValues(5) = ConvertToDegrees(caseitem.θ1).ToString
		  'theValues(6) = ConvertToDegrees(caseitem.φ1).ToString
		  'theValues(7) = caseitem.χ2.ToString
		  'theValues(8) = ConvertToDegrees(caseitem.θ2).ToString
		  'theValues(9) = ConvertToDegrees(caseitem.φ2).ToString
		  'theValues(10) = ConvertToDegrees(caseitem.β).ToString
		  'theValues(11) = ConvertToDegrees(caseitem.ψ).ToString
		  'value = caseitem.R/caseitem.Year
		  'theValues(12) = value.ToString
		  'theValues(13) = ConvertToDegrees(caseitem.Θ).ToString
		  'theValues(14) = ConvertToDegrees(caseitem.Φ).ToString
		  'theValues(15) = ConvertToDegrees(caseItem.ρ0).ToString
		  'theValues(16) = caseitem.PNOrder.ToString + ", " + caseitem.PNForV.ToString
		  'theValues(17) = caseitem.Detectors.ToString
		  'theValues(18) = caseitem.ΔT.ToString
		  'theValues(19) = caseitem.RunDuration.ToString
		  'Var theUncertainties(15) As String
		  '// assemble all the uncertainty values as an array of strings
		  'For i As Integer = 0 to 15
		  'theUncertainties(i) = caseitem.Uncertainties(i).ToString
		  'Next
		  '// convert the array into a single tab-delimited string
		  'Var theString As String = theID+tab+String.FromArray(theValues, tab)+tab+String.FromArray(theUncertainties, tab)
		  't.WriteLine(theString) // write it out to the file
		  'Next  // go on to the next case
		  't.Close  // when we are done, close the text file
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		AllCasesDone As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h0
		LastCaseSupervisor As CaseSupervisorClass
	#tag EndProperty

	#tag Property, Flags = &h0
		OutputFile As FolderItem
	#tag EndProperty

	#tag Property, Flags = &h0
		PlotItemsList() As PlotItemClass
	#tag EndProperty

	#tag Property, Flags = &h0
		Setting As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		TheCases() As CaseInfoClass
	#tag EndProperty


#tag EndWindowCode

#tag Events GraphButton
	#tag Event
		Sub Pressed()
		  OpenGraphWindow
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events InterfaceUpdateTimer
	#tag Event
		Sub Action()
		  UpdateInterface
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events ParamNameListBox
	#tag Event
		Function HeaderPressed(column as Integer) As Boolean
		  Return True  // do not allow sorting
		End Function
	#tag EndEvent
#tag EndEvents
#tag Events CaseListBoxParams
	#tag Event
		Function CellPressed(row As Integer, column As Integer, x As Integer, y As Integer) As Boolean
		  me.EditCellAt(row, column)
		End Function
	#tag EndEvent
	#tag Event
		Sub Opening()
		  me.ColumnTypeAt(0) = DesktopListBox.CellTypes.TextField
		End Sub
	#tag EndEvent
	#tag Event
		Function HeaderPressed(column as Integer) As Boolean
		  Return True // do not allow sorting
		End Function
	#tag EndEvent
#tag EndEvents
#tag Events UncertaintyListBox
	#tag Event
		Function PaintCellBackground(g As Graphics, row As Integer, column As Integer) As Boolean
		  // If the row tag for this row has been set (or in the case of the row for Ω, the row tags
		  // for the Θ and/or Φ rows have been set), then make the background grey, indicating that
		  // we are not calculating the uncertainty for this parameter.
		  If row < 16 Then
		    If me.RowTagAt(row) = True Or (row = 15 And (me.RowTagAt(14) = True Or me.RowTagAt(15) = True)) Then
		      g.DrawingColor = Color.RGB(230,230,230)
		      g.FillRectangle(0,0, g.Width, g.Height)
		    End If
		  End If
		End Function
	#tag EndEvent
	#tag Event
		Function HeaderPressed(column as Integer) As Boolean
		  Return True  // do not allow sorting
		End Function
	#tag EndEvent
	#tag Event
		Function CellPressed(row As Integer, column As Integer, x As Integer, y As Integer) As Boolean
		  // Toggle whether uncertainty for this row is calculated or not
		  If row < 15 Then // don't allow toggle on the uncertainy for Ω
		    If me.RowTagAt(row) = True Then
		      me.RowTagAt(row) = False
		    Else
		      me.RowTagAt(row) = True
		    End If
		  End If
		  me.RefreshCell(row, column)
		End Function
	#tag EndEvent
	#tag Event
		Sub SelectionChanged()
		  If Not Setting Then
		    Setting = True
		    me.SelectedRowIndex = -1
		    Setting = False
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events RunFileCheckBox
	#tag Event
		Sub ValueChanged()
		  If me.Value Then
		    DataDestinationMenu.SelectedRowIndex = 1
		    DataDestinationMenu.Enabled = False
		  Else
		    DataDestinationMenu.SelectedRowIndex = 0
		    DataDestinationMenu.Enabled = True
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events PlotItemsListBox
	#tag Event
		Function KeyDown(key As String) As Boolean
		  // trap a backspace or delete key
		  If key = Chr(8) Or key = Chr(127) Then
		    // if there is nothing selected
		    If me.SelectedRowIndex = DesktopListBox.NoSelection Then
		      System.Beep // report nothing to delete
		    Else  // otherwise
		      Var row As Integer = me.SelectedRowIndex
		      Var plotitem As PlotItemClass = me.RowTagAt(row)
		      plotitem.Reset
		      me.RemoveRowAt(row)
		    End If
		    UpdateChooseVarMenu
		    Return True // indicates we have handled the keystroke
		  End If
		  Return False // if not delete or backspace, let someone else handle it
		End Function
	#tag EndEvent
#tag EndEvents
#tag Events ChooseVariablePopupMenu
	#tag Event
		Sub SelectionChanged(item As DesktopMenuItem)
		  If Not Setting Then
		    CustomizeChosenPlotItem(me.RowTagAt(me.SelectedRowIndex))
		    UpdateChooseVarMenu
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events StartStopButton
	#tag Event
		Sub Pressed()
		  If me.Caption = "Run" Then
		    DoStart
		    me.Caption = "Stop"
		  Else
		    me.Caption = "Run"
		    DoStop
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events εListBox
	#tag Event
		Function HeaderPressed(column as Integer) As Boolean
		  Return True // Do not allow sorting
		  
		End Function
	#tag EndEvent
	#tag Event
		Sub Opening()
		  me.ColumnTypeAt(0) = DesktopListBox.CellTypes.TextField
		End Sub
	#tag EndEvent
	#tag Event
		Function CellPressed(row As Integer, column As Integer, x As Integer, y As Integer) As Boolean
		  me.EditCellAt(row, column)
		End Function
	#tag EndEvent
#tag EndEvents
#tag Events CaseListBoxDetData
	#tag Event
		Function CellPressed(row As Integer, column As Integer, x As Integer, y As Integer) As Boolean
		  me.EditCellAt(row, column)
		  
		End Function
	#tag EndEvent
	#tag Event
		Sub Opening()
		  me.ColumnTypeAt(0) = DesktopListBox.CellTypes.TextField
		End Sub
	#tag EndEvent
	#tag Event
		Function HeaderPressed(column as Integer) As Boolean
		  Return True // do not allow sorting
		End Function
	#tag EndEvent
#tag EndEvents
#tag ViewBehavior
	#tag ViewProperty
		Name="HasTitleBar"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType=""
	#tag EndViewProperty
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
		Visible=false
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
		Group="Windows Behavior"
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
		Name="AllCasesDone"
		Visible=false
		Group="Behavior"
		InitialValue="False"
		Type="Boolean"
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
#tag EndViewBehavior
