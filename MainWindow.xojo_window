#tag DesktopWindow
Begin DesktopWindow MainWindow
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
   Height          =   800
   ImplicitInstance=   True
   MacProcID       =   0
   MaximumHeight   =   32000
   MaximumWidth    =   32000
   MenuBar         =   1095792639
   MenuBarVisible  =   False
   MinimumHeight   =   650
   MinimumWidth    =   1000
   Resizeable      =   True
   Title           =   "LISA Simulator"
   Type            =   0
   Visible         =   True
   Width           =   1000
   Begin Timer InterfaceUpdateTimer
      Enabled         =   True
      Index           =   -2147483648
      LockedInPosition=   False
      Period          =   500
      RunMode         =   0
      Scope           =   0
      TabPanelIndex   =   0
   End
   Begin DesktopTabPanel MainTabPanel
      AllowAutoDeactivate=   True
      Bold            =   False
      Enabled         =   True
      FontName        =   "System"
      FontSize        =   0.0
      FontUnit        =   0
      Height          =   800
      Index           =   -2147483648
      Italic          =   False
      Left            =   0
      LockBottom      =   True
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   True
      LockTop         =   True
      Panels          =   ""
      Scope           =   0
      SmallTabs       =   False
      TabDefinition   =   "Set Up\rRun\rAnalyze\rGraph"
      TabIndex        =   30
      TabPanelIndex   =   0
      TabStop         =   True
      Tooltip         =   ""
      Top             =   0
      Transparent     =   False
      Underline       =   False
      Value           =   0
      Visible         =   True
      Width           =   1000
      Begin DesktopListBox ParamNameListBox
         AllowAutoDeactivate=   True
         AllowAutoHideScrollbars=   True
         AllowExpandableRows=   False
         AllowFocusRing  =   True
         AllowResizableColumns=   False
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
         Height          =   547
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "Parameter\nM1 (sols)\nM2 (sols)\nf (mHz)\nR (ly)\nβ (°)	\nψ (°)\nλ0\nΘ (°)\nΦ(°)	\nχ10x	\nχ10y	\nχ10z	\nχ20x\nχ20y\nx20z\nρ0\nPN Order\nDetectors\nΔT (s)\nDuration (y)"
         Italic          =   False
         Left            =   31
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         RequiresSelection=   False
         RowSelectionType=   0
         Scope           =   0
         TabIndex        =   0
         TabPanelIndex   =   1
         TabStop         =   True
         Tooltip         =   ""
         Top             =   38
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   86
         _ScrollOffset   =   0
         _ScrollWidth    =   -1
      End
      Begin DesktopButton AddCaseColumnButton
         AllowAutoDeactivate=   True
         Bold            =   False
         Cancel          =   False
         Caption         =   "Add New Case"
         Default         =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   120
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         MacButtonStyle  =   0
         Scope           =   0
         TabIndex        =   3
         TabPanelIndex   =   1
         TabStop         =   True
         Tooltip         =   ""
         Top             =   640
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   124
      End
      Begin ProgressBar CaseProgressBar
         AllowAutoDeactivate=   True
         Enabled         =   True
         Height          =   14
         Indeterminate   =   False
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Left            =   589
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         MaximumValue    =   100
         Scope           =   0
         TabIndex        =   0
         TabPanelIndex   =   2
         TabStop         =   True
         Tooltip         =   ""
         Top             =   50
         Transparent     =   True
         Value           =   0.0
         Visible         =   True
         Width           =   230
      End
      Begin DesktopButton StartStopButton
         AllowAutoDeactivate=   True
         Bold            =   False
         Cancel          =   False
         Caption         =   "Run Cases"
         Default         =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   844
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         MacButtonStyle  =   0
         Scope           =   0
         TabIndex        =   2
         TabPanelIndex   =   2
         TabStop         =   True
         Tooltip         =   ""
         Top             =   50
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   124
      End
      Begin DesktopLabel CaptionForGraphChoiceLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   263
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   0
         TabPanelIndex   =   4
         TabStop         =   True
         Text            =   "Graph of:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   64
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   64
      End
      Begin DesktopPopupMenu GraphChoicePopupMenu
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   ""
         Italic          =   False
         Left            =   330
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         SelectedRowIndex=   0
         TabIndex        =   1
         TabPanelIndex   =   4
         TabStop         =   True
         Tooltip         =   ""
         Top             =   66
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   92
      End
      Begin DesktopButton DrawGraphButton
         AllowAutoDeactivate=   True
         Bold            =   False
         Cancel          =   False
         Caption         =   "Graph"
         Default         =   False
         Enabled         =   False
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   447
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         MacButtonStyle  =   0
         Scope           =   0
         TabIndex        =   7
         TabPanelIndex   =   4
         TabStop         =   True
         Tooltip         =   ""
         Top             =   66
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   80
      End
      Begin DesktopListBox ResultsListBox1
         AllowAutoDeactivate=   True
         AllowAutoHideScrollbars=   False
         AllowExpandableRows=   False
         AllowFocusRing  =   True
         AllowResizableColumns=   False
         AllowRowDragging=   False
         AllowRowReordering=   False
         Bold            =   False
         ColumnCount     =   1
         ColumnWidths    =   "130"
         DefaultRowHeight=   40
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
         Height          =   264
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "Value ± Uncertainty"
         Italic          =   False
         Left            =   101
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         RequiresSelection=   False
         RowSelectionType=   0
         Scope           =   0
         TabIndex        =   18
         TabPanelIndex   =   3
         TabStop         =   True
         Tooltip         =   ""
         Top             =   124
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   131
         _ScrollOffset   =   0
         _ScrollWidth    =   -1
      End
      Begin DesktopChart ProgressChart
         AllowAutoDeactivate=   True
         AllowFocus      =   False
         AllowFocusRing  =   True
         AllowTabs       =   False
         BackgroundColor =   &cFFFFFF
         Bold            =   False
         DoubleBuffer    =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   11.0
         FontUnit        =   0
         GridColor       =   &ca6a6a6
         HasLegend       =   True
         Height          =   638
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   322
         LockBottom      =   True
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   True
         LockTop         =   True
         Mode            =   0
         Scope           =   0
         TabIndex        =   3
         TabPanelIndex   =   2
         TabStop         =   True
         TextColor       =   &c000000
         Title           =   ""
         Tooltip         =   ""
         Top             =   142
         Underline       =   False
         Visible         =   True
         Width           =   658
      End
      Begin DesktopLabel CaptionForCaseLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
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
         TabIndex        =   4
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   "Current Case Number:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   50
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
         InitialParent   =   "MainTabPanel"
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
         TabIndex        =   5
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   "Computation Time (s):"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   80
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
      End
      Begin DesktopLabel CaptionForTcLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
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
         TabIndex        =   6
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   "Time to Coalescence (y):"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   170
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
         InitialParent   =   "MainTabPanel"
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
         TabIndex        =   7
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   "Current Step Number:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   110
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
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
         InitialParent   =   "MainTabPanel"
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
         TabIndex        =   8
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   "Current PN Factor v:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   200
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
      End
      Begin DesktopLabel CaptionForSNRLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
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
         TabIndex        =   9
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   "Current Signal-to-Noise:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   260
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
      End
      Begin DesktopButton ImportCasesButton
         AllowAutoDeactivate=   True
         Bold            =   False
         Cancel          =   False
         Caption         =   "Import Cases"
         Default         =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   22
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   706
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         MacButtonStyle  =   0
         Scope           =   0
         TabIndex        =   4
         TabPanelIndex   =   1
         TabStop         =   True
         Tooltip         =   ""
         Top             =   640
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   118
      End
      Begin DesktopButton ExportCasesButton
         AllowAutoDeactivate=   True
         Bold            =   False
         Cancel          =   False
         Caption         =   "Export Cases"
         Default         =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   844
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         MacButtonStyle  =   0
         Scope           =   0
         TabIndex        =   5
         TabPanelIndex   =   1
         TabStop         =   True
         Tooltip         =   ""
         Top             =   640
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   118
      End
      Begin DesktopButton ClearCasesButton
         AllowAutoDeactivate=   True
         Bold            =   False
         Cancel          =   False
         Caption         =   "Clear Cases"
         Default         =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   267
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         MacButtonStyle  =   0
         Scope           =   0
         TabIndex        =   6
         TabPanelIndex   =   1
         TabStop         =   True
         Tooltip         =   ""
         Top             =   640
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   96
      End
      Begin DesktopChart MainGraphDataChart
         AllowAutoDeactivate=   True
         AllowFocus      =   False
         AllowFocusRing  =   True
         AllowTabs       =   False
         BackgroundColor =   &cFFFFFF
         Bold            =   False
         DoubleBuffer    =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   11.0
         FontUnit        =   0
         GridColor       =   &ca6a6a6
         HasLegend       =   True
         Height          =   677
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   20
         LockBottom      =   True
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   True
         LockTop         =   True
         Mode            =   6
         Scope           =   0
         TabIndex        =   8
         TabPanelIndex   =   4
         TabStop         =   True
         TextColor       =   &c000000
         Title           =   ""
         Tooltip         =   ""
         Top             =   102
         Underline       =   False
         Visible         =   True
         Width           =   960
      End
      Begin DesktopLabel CaptionForStartTimeLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   577
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   9
         TabPanelIndex   =   4
         TabStop         =   True
         Text            =   "Starting τ:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   66
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   80
      End
      Begin DesktopTextField StartTimeTextField
         AllowAutoDeactivate=   True
         AllowFocusRing  =   True
         AllowSpellChecking=   False
         AllowTabs       =   False
         BackgroundColor =   &cFFFFFF
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Format          =   ""
         HasBorder       =   True
         Height          =   22
         Hint            =   ""
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   654
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         MaximumCharactersAllowed=   0
         Password        =   False
         ReadOnly        =   False
         Scope           =   0
         TabIndex        =   10
         TabPanelIndex   =   4
         TabStop         =   True
         Text            =   "Min"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   66
         Transparent     =   False
         Underline       =   False
         ValidationMask  =   ""
         Visible         =   True
         Width           =   110
      End
      Begin DesktopLabel CaptionForEndTimeLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   789
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   11
         TabPanelIndex   =   4
         TabStop         =   True
         Text            =   "Ending τ:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   66
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   76
      End
      Begin DesktopTextField EndTimeTextField
         AllowAutoDeactivate=   True
         AllowFocusRing  =   True
         AllowSpellChecking=   False
         AllowTabs       =   False
         BackgroundColor =   &cFFFFFF
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Format          =   ""
         HasBorder       =   True
         Height          =   22
         Hint            =   ""
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   865
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         MaximumCharactersAllowed=   0
         Password        =   False
         ReadOnly        =   False
         Scope           =   0
         TabIndex        =   12
         TabPanelIndex   =   4
         TabStop         =   True
         Text            =   "Max"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   66
         Transparent     =   False
         Underline       =   False
         ValidationMask  =   ""
         Visible         =   True
         Width           =   115
      End
      Begin DesktopLabel ValueOfCaseLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   188
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   15
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   "1"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   50
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   114
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
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   188
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   16
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   80
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   100
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
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   188
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   17
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   110
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   114
      End
      Begin DesktopLabel ValueOfTcLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   188
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   18
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   170
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   114
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
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   188
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   19
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   200
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   114
      End
      Begin DesktopLabel ValueOfSNRLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   188
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   20
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   260
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   114
      End
      Begin DesktopLabel CaptionForGraphCaseLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
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
         TabIndex        =   13
         TabPanelIndex   =   4
         TabStop         =   True
         Text            =   "Current Case:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   38
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   90
      End
      Begin DesktopLabel ValueOfGraphCaseLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   115
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   14
         TabPanelIndex   =   4
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   38
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   124
      End
      Begin DesktopListBox CasesListBox
         AllowAutoDeactivate=   True
         AllowAutoHideScrollbars=   True
         AllowExpandableRows=   False
         AllowFocusRing  =   True
         AllowResizableColumns=   True
         AllowRowDragging=   False
         AllowRowReordering=   False
         Bold            =   False
         ColumnCount     =   1
         ColumnWidths    =   "100"
         DefaultRowHeight=   26
         DropIndicatorVisible=   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         GridLineStyle   =   3
         HasBorder       =   True
         HasHeader       =   True
         HasHorizontalScrollbar=   True
         HasVerticalScrollbar=   False
         HeadingIndex    =   -1
         Height          =   558
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "Case 1\n5000\n5000\n2.00	\n1000\n39\n24\n0\n5\n268.5\nx 0\nx 0\nx 0\nx 0\nx 0\nx 0\nx 0\n3\n2\n50\n1.0"
         Italic          =   False
         Left            =   120
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   True
         LockTop         =   True
         RequiresSelection=   False
         RowSelectionType=   0
         Scope           =   0
         TabIndex        =   7
         TabPanelIndex   =   1
         TabStop         =   True
         Tooltip         =   ""
         Top             =   38
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   842
         _ScrollOffset   =   0
         _ScrollWidth    =   -1
      End
      Begin DesktopCheckBox AutoSaveCheckbox
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   "Automatically Save Graph Data"
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   20
         LockBottom      =   True
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   False
         Scope           =   0
         TabIndex        =   21
         TabPanelIndex   =   2
         TabStop         =   True
         Tooltip         =   ""
         Top             =   736
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   0
         Width           =   218
      End
      Begin DesktopCheckBox AutoStopCheckBox
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   "Auto Stop After Each Case"
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   20
         LockBottom      =   True
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   False
         Scope           =   0
         TabIndex        =   22
         TabPanelIndex   =   2
         TabStop         =   True
         Tooltip         =   ""
         Top             =   713
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   1
         Width           =   218
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
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   322
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   23
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   "Status:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   50
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   49
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
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   383
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   24
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   "Not Started"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   50
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   164
      End
      Begin DesktopCheckBox AutoSaveResultsCheckbox
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   "Automatically Save Summary"
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   20
         LockBottom      =   True
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   False
         Scope           =   0
         TabIndex        =   25
         TabPanelIndex   =   2
         TabStop         =   True
         Tooltip         =   ""
         Top             =   760
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   0
         Width           =   218
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
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   322
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   26
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   "Reason For Stopping:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   76
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
      End
      Begin DesktopLabel ValueOfStopReasonLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   490
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   27
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   76
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   351
      End
      Begin DesktopButton LoadGraphDataButton
         AllowAutoDeactivate=   True
         Bold            =   False
         Cancel          =   False
         Caption         =   "Load Case From File"
         Default         =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   20
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         MacButtonStyle  =   0
         Scope           =   0
         TabIndex        =   15
         TabPanelIndex   =   4
         TabStop         =   True
         Tooltip         =   ""
         Top             =   68
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   144
      End
      Begin DesktopListBox ParamNameListBox1
         AllowAutoDeactivate=   True
         AllowAutoHideScrollbars=   True
         AllowExpandableRows=   False
         AllowFocusRing  =   True
         AllowResizableColumns=   False
         AllowRowDragging=   False
         AllowRowReordering=   False
         Bold            =   False
         ColumnCount     =   1
         ColumnWidths    =   "75"
         DefaultRowHeight=   40
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
         Height          =   264
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "Parameter\nH0\nδ	\nV0\nZ\nβ (°)	\nψ (°)"
         Italic          =   False
         Left            =   27
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         RequiresSelection=   False
         RowSelectionType=   0
         Scope           =   0
         TabIndex        =   20
         TabPanelIndex   =   3
         TabStop         =   True
         Tooltip         =   ""
         Top             =   124
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   77
         _ScrollOffset   =   0
         _ScrollWidth    =   -1
      End
      Begin DesktopLabel CaptionForGraphCaseLabel1
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   27
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   21
         TabPanelIndex   =   3
         TabStop         =   True
         Text            =   "Current Case:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   48
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   90
      End
      Begin DesktopLabel ValueOfAnalyzeCaseLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   129
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   22
         TabPanelIndex   =   3
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   48
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   124
      End
      Begin DesktopButton LoadCaseResultsButton
         AllowAutoDeactivate=   True
         Bold            =   False
         Cancel          =   False
         Caption         =   "Load Case From File"
         Default         =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   27
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         MacButtonStyle  =   0
         Scope           =   0
         TabIndex        =   23
         TabPanelIndex   =   3
         TabStop         =   True
         Tooltip         =   ""
         Top             =   80
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   144
      End
      Begin DesktopListBox ResultsListBox2
         AllowAutoDeactivate=   True
         AllowAutoHideScrollbars=   False
         AllowExpandableRows=   False
         AllowFocusRing  =   True
         AllowResizableColumns=   False
         AllowRowDragging=   False
         AllowRowReordering=   False
         Bold            =   False
         ColumnCount     =   1
         ColumnWidths    =   "130"
         DefaultRowHeight=   40
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
         Height          =   304
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "Value ± Uncertainty"
         Italic          =   False
         Left            =   335
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         RequiresSelection=   False
         RowSelectionType=   0
         Scope           =   0
         TabIndex        =   24
         TabPanelIndex   =   3
         TabStop         =   True
         Tooltip         =   ""
         Top             =   84
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   130
         _ScrollOffset   =   0
         _ScrollWidth    =   -1
      End
      Begin DesktopListBox ParamNameListBox2
         AllowAutoDeactivate=   True
         AllowAutoHideScrollbars=   True
         AllowExpandableRows=   False
         AllowFocusRing  =   True
         AllowResizableColumns=   False
         AllowRowDragging=   False
         AllowRowReordering=   False
         Bold            =   False
         ColumnCount     =   1
         ColumnWidths    =   "75"
         DefaultRowHeight=   40
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
         Height          =   304
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "Parameter\nλ0 (°)\nΘ (°)\nΦ (°)\nΩ (/sky)\nχ10x\nχ10y\nχ10z"
         Italic          =   False
         Left            =   260
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         RequiresSelection=   False
         RowSelectionType=   0
         Scope           =   0
         TabIndex        =   25
         TabPanelIndex   =   3
         TabStop         =   True
         Tooltip         =   ""
         Top             =   84
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   77
         _ScrollOffset   =   0
         _ScrollWidth    =   -1
      End
      Begin DesktopListBox MatrixListBox
         AllowAutoDeactivate=   True
         AllowAutoHideScrollbars=   True
         AllowExpandableRows=   False
         AllowFocusRing  =   True
         AllowResizableColumns=   False
         AllowRowDragging=   False
         AllowRowReordering=   False
         Bold            =   False
         ColumnCount     =   16
         ColumnWidths    =   "24, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80"
         DefaultRowHeight=   21
         DropIndicatorVisible=   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         GridLineStyle   =   3
         HasBorder       =   True
         HasHeader       =   True
         HasHorizontalScrollbar=   True
         HasVerticalScrollbar=   True
         HeadingIndex    =   -1
         Height          =   357
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "\n1	+0.00e+00\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15"
         Italic          =   False
         Left            =   20
         LockBottom      =   True
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   True
         LockTop         =   True
         RequiresSelection=   False
         RowSelectionType=   0
         Scope           =   0
         TabIndex        =   26
         TabPanelIndex   =   3
         TabStop         =   True
         Tooltip         =   ""
         Top             =   400
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   960
         _ScrollOffset   =   0
         _ScrollWidth    =   -1
      End
      Begin DesktopListBox ResultsListBox3
         AllowAutoDeactivate=   True
         AllowAutoHideScrollbars=   False
         AllowExpandableRows=   False
         AllowFocusRing  =   True
         AllowResizableColumns=   False
         AllowRowDragging=   False
         AllowRowReordering=   False
         Bold            =   False
         ColumnCount     =   1
         ColumnWidths    =   "150"
         DefaultRowHeight=   40
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
         Height          =   304
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "Value ± Uncertainty"
         Italic          =   False
         Left            =   566
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         RequiresSelection=   False
         RowSelectionType=   0
         Scope           =   0
         TabIndex        =   27
         TabPanelIndex   =   3
         TabStop         =   True
         Tooltip         =   ""
         Top             =   84
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   152
         _ScrollOffset   =   0
         _ScrollWidth    =   -1
      End
      Begin DesktopListBox ParamNameListBox3
         AllowAutoDeactivate=   True
         AllowAutoHideScrollbars=   True
         AllowExpandableRows=   False
         AllowFocusRing  =   True
         AllowResizableColumns=   False
         AllowRowDragging=   False
         AllowRowReordering=   False
         Bold            =   False
         ColumnCount     =   1
         ColumnWidths    =   "75"
         DefaultRowHeight=   40
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
         Height          =   304
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "Parameter\nχ20x\nχ20y\nχ20z\nM1 (sols)\nM2 (sols)\nF0 (mHz)\nR (ly)"
         Italic          =   False
         Left            =   493
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         RequiresSelection=   False
         RowSelectionType=   0
         Scope           =   0
         TabIndex        =   28
         TabPanelIndex   =   3
         TabStop         =   True
         Tooltip         =   ""
         Top             =   84
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   77
         _ScrollOffset   =   0
         _ScrollWidth    =   -1
      End
      Begin DesktopPopupMenu MatrixChoicePopupMenu
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   24
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "ATA\nY Original\nY Inverted\nY^-1 x Y"
         Italic          =   False
         Left            =   747
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Scope           =   0
         SelectedRowIndex=   0
         TabIndex        =   29
         TabPanelIndex   =   3
         TabStop         =   True
         Tooltip         =   ""
         Top             =   364
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   96
      End
      Begin DesktopLabel CaptionForMatrixLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   747
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   30
         TabPanelIndex   =   3
         TabStop         =   True
         Text            =   "Matrix To Display:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   339
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   136
      End
      Begin DesktopLabel XExplanationLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   120
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   8
         TabPanelIndex   =   1
         TabStop         =   True
         Text            =   "Typing 'x' in the cells for β through χ20z will toggle whether that variable's uncertainty is calculate or not (a displayed 'x' indicates 'not')."
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   597
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   842
      End
      Begin DesktopListBox ParamNameListBox4
         AllowAutoDeactivate=   True
         AllowAutoHideScrollbars=   True
         AllowExpandableRows=   False
         AllowFocusRing  =   True
         AllowResizableColumns=   False
         AllowRowDragging=   False
         AllowRowReordering=   False
         Bold            =   False
         ColumnCount     =   1
         ColumnWidths    =   "75"
         DefaultRowHeight=   40
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
         Height          =   224
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "Parameter\nρ0 (°)\nPN Order\nDetectors\nΔT (s)\nRan for (y)"
         Italic          =   False
         Left            =   747
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         RequiresSelection=   False
         RowSelectionType=   0
         Scope           =   0
         TabIndex        =   31
         TabPanelIndex   =   3
         TabStop         =   True
         Tooltip         =   ""
         Top             =   84
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   77
         _ScrollOffset   =   0
         _ScrollWidth    =   -1
      End
      Begin DesktopListBox ResultsListBox4
         AllowAutoDeactivate=   True
         AllowAutoHideScrollbars=   False
         AllowExpandableRows=   False
         AllowFocusRing  =   True
         AllowResizableColumns=   False
         AllowRowDragging=   False
         AllowRowReordering=   False
         Bold            =   False
         ColumnCount     =   1
         ColumnWidths    =   "150"
         DefaultRowHeight=   40
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
         Height          =   224
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "Value"
         Italic          =   False
         Left            =   823
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         RequiresSelection=   False
         RowSelectionType=   0
         Scope           =   0
         TabIndex        =   32
         TabPanelIndex   =   3
         TabStop         =   True
         Tooltip         =   ""
         Top             =   84
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   152
         _ScrollOffset   =   0
         _ScrollWidth    =   -1
      End
      Begin DesktopLabel CaptionForStepRatioLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
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
         TabIndex        =   28
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   "Current Step Ratio:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   230
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
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
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   188
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   29
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   230
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   114
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
         InitialParent   =   "MainTabPanel"
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
         TabIndex        =   30
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   "Simulation Time (y):"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   140
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   156
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
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   188
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   31
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   140
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   114
      End
      Begin DesktopButton CopyMatrixButton
         AllowAutoDeactivate=   True
         Bold            =   False
         Cancel          =   False
         Caption         =   "Copy Matrix"
         Default         =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   878
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         MacButtonStyle  =   0
         Scope           =   0
         TabIndex        =   33
         TabPanelIndex   =   3
         TabStop         =   True
         Tooltip         =   ""
         Top             =   364
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   97
      End
      Begin DesktopLabel CaptionForConditionLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
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
         TabIndex        =   34
         TabPanelIndex   =   3
         TabStop         =   True
         Text            =   "Condition Number:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   760
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   123
      End
      Begin DesktopLabel ValueOfConditionLabel
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   155
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   35
         TabPanelIndex   =   3
         TabStop         =   True
         Text            =   "Untitled"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   760
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   151
      End
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
   End
End
#tag EndDesktopWindow

#tag WindowCode
	#tag Event
		Sub Opening()
		  'SetUpMainLB()
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
		Sub DisplayMatrix(TheMatrix As Matrix)
		  Var n As Integer = TheMatrix.pDim-1
		  For j as Integer = 0 to 14
		    for k as Integer = 0 to 14
		      If j > n or k > n Then
		        MatrixListBox.CellTextAt(j,k+1) = ""
		      Else
		        MatrixListBox.CellTextAt(j,k+1) = Format(TheMatrix.pData(j,k), "+0.00e+00")
		      End If
		    Next
		  Next
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DisplayUncertainties(Params As CaseParametersClass, UV As UncertaintyValuesClass)
		  ResultsListBox1.CellTextAt(0,0) = Params.H0.toString + EndOfLine + GetUncertaintyString(UV.OfH0)
		  ResultsListBox1.CellTextAt(1,0) = Params.δ.toString + EndOfLine + GetUncertaintyString(UV.Ofδ)
		  ResultsListBox1.CellTextAt(2,0) = Params.V0.toString + EndOfLine + GetUncertaintyString(UV.OfV0)
		  ResultsListBox1.CellTextAt(3,0) = Params.Z.toString + EndOfLine + GetUncertaintyString(UV.OfZ)
		  ResultsListBox1.CellTextAt(4,0) = ConvertToDegrees(Params.β).toString + EndOfLine + GetUncertaintyString(ConvertToDegrees(UV.Ofβ))
		  ResultsListBox1.CellTextAt(5,0) = ConvertToDegrees(Params.ψ).toString + EndOfLine + GetUncertaintyString(ConvertToDegrees(UV.Ofψ))
		  ResultsListBox2.CellTextAt(0,0) = ConvertToDegrees(Params.λ0).toString + EndOfLine + GetUncertaintyString(ConvertToDegrees(UV.Ofλ0))
		  ResultsListBox2.CellTextAt(1,0) = ConvertToDegrees(Params.Θ).toString + EndOfLine + GetUncertaintyString(ConvertToDegrees(UV.OfΘ))
		  ResultsListBox2.CellTextAt(2,0) = ConvertToDegrees(Params.Φ).toString + EndOfLine + GetUncertaintyString(ConvertToDegrees(UV.OfΦ))
		  ResultsListBox2.CellTextAt(3,0) = UV.OfΩ.ToString
		  ResultsListBox2.CellTextAt(4,0) = Params.χ10x.toString + EndOfLine + GetUncertaintyString(UV.Ofχ10x)
		  ResultsListBox2.CellTextAt(5,0) = Params.χ10y.toString + EndOfLine + GetUncertaintyString(UV.Ofχ10y)
		  ResultsListBox2.CellTextAt(6,0) = Params.χ10z.toString + EndOfLine + GetUncertaintyString(UV.Ofχ10z)
		  ResultsListBox3.CellTextAt(0,0) = Params.χ20x.toString + EndOfLine + GetUncertaintyString(UV.Ofχ20x)
		  ResultsListBox3.CellTextAt(1,0) = Params.χ20y.toString + EndOfLine + GetUncertaintyString(UV.Ofχ20y)
		  ResultsListBox3.CellTextAt(2,0) = Params.χ20z.toString + EndOfLine + GetUncertaintyString(UV.Ofχ20z)
		  ResultsListBox3.CellTextAt(3,0) = Params.M1.ToString
		  ResultsListBox3.CellTextAt(4,0) = Params.M2.ToString
		  ResultsListBox3.CellTextAt(5,0) = Params.F0.ToString
		  ResultsListBox3.CellTextAt(6,0) = Params.R.ToString
		  ResultsListBox4.CellTextAt(0,0) = Params.ρ0.ToString
		  ResultsListBox4.CellTextAt(1,0) = Params.PNOrder.ToString
		  ResultsListBox4.CellTextAt(2,0) = Params.Detectors.ToString
		  ResultsListBox4.CellTextAt(3,0) = Params.ΔT.ToString
		  ResultsListBox4.CellTextAt(4,0) = ValueOfSimTimeLabel.Text
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTimeToCoalescence(TheSuper As CaseSupervisorClass, SpinStuff As SpinEvolverClass) As Double
		  Var parameters As CaseParametersClass = TheSuper.CaseParameters
		  Var δ As Double = parameters.δ
		  Var η As Double = parameters.η
		  Var χa𝓁 As Double = SpinStuff.χa𝓁
		  Var χs𝓁 As Double = SpinStuff.χs𝓁
		  Var v0 As Double = parameters.V0
		  Var Threepi As Double = 3.0*parameters.π
		  Var c As Double = 743/2688 + (11/32)*η 
		  Var Term2 As Double = (32/3)*c
		  Var Term3 As Double = (64/3)*((47/40)*χs𝓁 + (15/32)*δ*χa𝓁 - Threepi/10)
		  Var Term4 As Double = 64*c*c + (128/9)*(1855099/14450688 + (56975/258048)*η - (371/2048)*η*η)
		  Var A As Double = 5*Parameters.GM/(256*η*TheSuper.Year*v0^8)
		  Return A*(1 + Term2*v0*v0 + Term3*v0*v0*v0 + Term4*v0*v0*v0*v0)
		  
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
		Function GetValueAndSolveFlag(ByRef Solve as Boolean, Source as String) As Double
		  If Source.BeginsWith("x ") Then
		    Solve = False
		    Return Source.Middle(2).ToDouble
		  Else
		    Solve = True
		    Return Source.ToDouble
		  End If
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub oldCreateArrays()
		  '//This method does the majority of the work in the main window. It feeds the necessary parameters
		  '//to a new instance of the EvolverClass class in order to perform the DoStep method enough times to make
		  '//sufficiently long arrays of v and each derivative of v, as well as the spin variables.
		  '
		  'τ.ResizeTo(-1)  // reset the time axis
		  '
		  'myMain = New Main(M, δ, f0, R, β, ψangle, λ0, Θ, Φ, χ1Initial, χ2Initial, PNOrder, Detectors, dτ0, K)  // instantiate the Main class
		  '
		  '// these lines are remnants from the spin-only program. The program now runs whether there's spin evolution or not
		  ''GraphAllowed = myMain.Evolver.ReadyToGo
		  ''if GraphAllowed then      // Checks to see if we are ready to go (i.e. if ι0 and its derivatives are nonzero)
		  '
		  'GraphArray = New ArrayClass  // set up a new array to help with graphing
		  '
		  'Var NewValues(30) As Double  // create the array to load into GraphArray
		  '
		  '//Stores the initial value of v. These values are set up in the EvolverClass constructor
		  'v = myMain.v0
		  '
		  '//Add the initial values of τ to the τ array so that the y- and x-axes match
		  'Var τnew As Double = 0
		  'τ.Add(τnew)
		  '
		  '// Perform the adaptive stepping technique to determine the time step
		  '
		  'While v < .2 And (τnew*M) < 31536000
		  'myMain.DoMainStep
		  '
		  'v = myMain.vF       //This updates the "current" value of v 
		  'τnew = τnew + dτ0          //This performs a step in τ . . .
		  'τ.Add(τnew)               //. . . and this adds it to the τ array
		  '
		  '// Add the new values to the graphing array
		  'For i As Integer = 0 To 14
		  'NewValues(i) = myMain.dzd(i)
		  'Next
		  '
		  'NewValues(15) = myMain.h
		  'NewValues(16) = myMain.hp
		  'NewValues(17) = myMain.hc
		  'NewValues(18) = myMain.Evolver.αAcc
		  'NewValues(19) = myMain.ιF
		  'NewValues(20) = myMain.ζ
		  'NewValues(21) = myMain.Evolver.LNhatF.x
		  'NewValues(22) = myMain.Evolver.LNhatF.y
		  'NewValues(23) = myMain.Evolver.LNhatF.z
		  'NewValues(24) = myMain.Evolver.χ1hatF.x
		  'NewValues(25) = myMain.Evolver.χ1hatF.y
		  'NewValues(26) = myMain.Evolver.χ1hatF.z
		  'NewValues(27) = myMain.Evolver.χ2hatF.x
		  'NewValues(28) = myMain.Evolver.χ2hatF.y
		  'NewValues(29) = myMain.Evolver.χ2hatF.z
		  'NewValues(30) = myMain.sn2
		  '
		  'GraphArray.AddAll(NewValues)
		  'Wend
		  ''end if 
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function oldGetCurrentCaseValues() As Double()
		  '// Returns an array with the current case values
		  '
		  'Var CurrentValues(19) As Double
		  '
		  'CurrentValues(0) = CaseCounter
		  '
		  'CurrentValues(1) = M
		  '
		  'CurrentValues(2) = δ
		  '
		  'CurrentValues(3) = f0
		  '
		  'CurrentValues(4) = R
		  '
		  'CurrentValues(5) = β
		  '
		  'CurrentValues(6) = ψangle
		  '
		  'CurrentValues(7) = λ0
		  '
		  'CurrentValues(8) = Θ
		  '
		  'CurrentValues(9) = Φ
		  '
		  'CurrentValues(10) = χ1Initial.x
		  '
		  'CurrentValues(11) = χ1Initial.y
		  '
		  'CurrentValues(12) = χ1Initial.z
		  '
		  'CurrentValues(13) = χ2Initial.x
		  '
		  'CurrentValues(14) = χ2Initial.y
		  '
		  'CurrentValues(15) = χ2Initial.z
		  '
		  'CurrentValues(16) = z0
		  '
		  'CurrentValues(17) = PNOrder
		  '
		  'CurrentValues(18) = Detectors
		  '
		  'CurrentValues(19) = dτ0
		  '
		  '
		  'return CurrentValues
		  
		  
		  
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function oldGetHighestValue(InputArray() As Double, Min As Integer, Max As Integer) As Double
		  //Parameters: InputArray() an array of double values
		  '//Return: CurrentHighest as double
		  '
		  '//This method takes an array and finds the largest value within it
		  '//It does this by comparing each item in the array to the current highest number, and if it is larger setting that number to the new current highest
		  '
		  '//Defines the current highest value and sets it equal to the first value in the input array
		  '
		  'Var CurrentHighest As Double = InputArray(Min)
		  '
		  '//For the rest of the values we check if they are higher than the current highest, if they are they become the current highest.
		  'Var i As Integer
		  'For i = Min + 1 to Max
		  'if CurrentHighest < InputArray(i) then
		  'CurrentHighest = InputArray(i)
		  'else 
		  'continue
		  'end if 
		  'next 
		  '
		  'return CurrentHighest
		  '
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function oldGetIndexOfClosest(InputValue As String) As Integer
		  '// This method helps with graphing (GetLowerBound and GetUpperBound in particular) by returning the index of the value closest to the
		  '// input value.
		  '
		  'If InputValue = "Min" then
		  'return 0
		  'elseif InputValue = "Max" then
		  'return τ.LastIndex - 1
		  'else
		  '
		  'var i As integer 
		  '
		  'Var CurrentClosest As integer = 0 
		  '
		  'Var TargetValue As Double = InputValue.ToDouble
		  '
		  'Var CurrentSmallestDifference As Double = Abs(τ(0) - TargetValue)
		  '
		  '
		  '
		  '
		  'For i = 1 to τ.LastIndex 
		  'Var Difference as Double = Abs(τ(i) - TargetValue)
		  'if Difference < CurrentSmallestDifference then
		  'CurrentSmallestDifference = Difference
		  'CurrentClosest = i
		  'else
		  'continue
		  'end if
		  'next 
		  '
		  '
		  'return CurrentClosest
		  '
		  '
		  'end if 
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function oldGetLowerBound() As Integer
		  '// Returns the lower bound based on user input
		  '
		  'Var LowerBound As Integer = GetIndexOfClosest(StartTimeTextField.Text)
		  '
		  'Return LowerBound
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function oldGetLowestValue(InputArray() As Double, Min As Integer, Max As Integer) As Double
		  '//Parameters: InputArray() an array of double values
		  '//Return: CurrentLowest as double
		  '
		  '//This method takes an array and finds the lowest value within it
		  '//It does this by comparing each item in the array to the current lowest number, and if it is lower setting that number to the new current lowest
		  '
		  '//Defines the current lowest value and sets it equal to the first value in the input array
		  'Var CurrentLowest As Double = InputArray(Min)
		  '
		  '//For the rest of the values we check if they are lower than the current lowest, if they are they become the current lowest.
		  '
		  'Var i As Integer
		  'For i = Min + 1 to Max
		  'if CurrentLowest > InputArray(i) then
		  'CurrentLowest = InputArray(i)
		  'else 
		  'continue
		  'end if 
		  'next 
		  '
		  'return CurrentLowest
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function oldGetUpperBound() As Integer
		  '// Returns the upper bound based on user input
		  '
		  'Var UpperBound As Integer = GetIndexOfClosest(EndTimeTextField.Text)
		  '
		  'Return UpperBound
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub oldPopulateUncertainties(ATA(, ) As Double)
		  '// This method takes the ATA matrix and calculates the uncertainties from it before displaying them on the UI.
		  '
		  'Var thisUncertaintyCalculator As New UncertaintyCalculator(ATA, solveList)  // set up an uncertainty calculator
		  '
		  '// normalize and arrange the matrix
		  'thisUncertaintyCalculator.DiagNormalize
		  'thisUncertaintyCalculator.Arrange
		  '
		  '// invert the matrix
		  'Var invertCheck As Integer = thisUncertaintyCalculator.InvertY
		  '
		  'While invertCheck <> 0
		  'thisUncertaintyCalculator.Y.RemoveInds(invertCheck, invertCheck)
		  'MessageBox("Error in inverting matrix. Row: " + invertCheck.toString + ". Row was removed.")
		  'thisUncertaintyCalculator.solveList(invertCheck - 1) = False
		  'invertCheck = thisUncertaintyCalculator.InvertY
		  'Wend
		  '
		  '// if the inversion was successful, unarrange the matrix, then calculate and populate the uncertainties
		  'thisUncertaintyCalculator.Unarrange
		  'thisUncertaintyCalculator.CalculateUncertainties
		  '
		  '// populate the uncertainties in the main window
		  'For i As Integer = 0 to 14
		  'If thisUncertaintyCalculator.σ(i) < 1e-98 Then
		  'MainListBox.CellTextAt(i,3+2*CaseCounter) = "-"
		  'Else
		  'MainListBox.CellTextAt(i, 3+2*CaseCounter) = Format(thisUncertaintyCalculator.σ(i), "#.##e")
		  'End If
		  'Next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub oldSetInitialValues(CaseCounter As Integer)
		  '//This method takes the inputs from LBInputValues and sets the properties in the main window equal to them
		  '
		  'Var π As Double = 3.141592653589793238462
		  '
		  '// check if we are solving for the various parameters and create a list of booleans to store this data
		  'Var values() As Double
		  'For i As Integer = 0 To 18
		  'If MainListBox.CellCheckBoxStateAt(i, 1+2*CaseCounter) = DesktopCheckbox.VisualStates.Checked Then
		  'values.Add(MainListBox.CellTextAt(i, 2+2*CaseCounter).ToDouble)
		  'solveList.Add(True)
		  'Else
		  'values.Add(MainListBox.CellTextAt(i, 2+ 2*CaseCounter).Replace("*", "").ToDouble)
		  'solveList.Add(False)
		  'End If
		  'Next
		  '
		  'M = values(0)*(4.927e-6) // total mass of the system (in seconds)
		  '
		  'δ = values(1)  // Defined to be (m1 − m2)/M , so characterizes the mass difference
		  '
		  'f0 = values(2)  //The initial value of the system’s orbital frequency in the system’s own reference frame 
		  '
		  'R = values(3) * 9.454254955488e15 / 2.99792458e8  // The system’s luminosity distance from our solar system (in seconds)
		  '
		  'β = values(4)*(π/180)  // angle between initial total angular momentum and line of sight
		  '
		  'ψangle = values(5)*(π/180)  // angle of the total angular momentum around line of sight
		  '
		  'λ0 = values(6)  // initial angle of the vector that points from the more massive to less massive star and the precessed x-axis
		  '
		  'Θ = values(7)*(π/180)  // altitude angle of the source in the sky
		  '
		  'Φ = values(8)*(π/180) // azimuth angle of the source around the ecliptic 
		  '
		  'χ1Initial = New Vector(values(9), values(10), values(11))  // the spin vector of star 1 (in units of (m1)^2
		  '
		  'χ2Initial = New Vector(values(12), values(13), values(14)) // the spin vector of star 2 (in units of (m2)^2
		  '
		  'PNOrder = values(15)  // post-Newtonian order
		  '
		  'Detectors = values(16)  // number of detectors
		  '
		  'dτ0 = values(17)/M  // initial unitless time step
		  '
		  'K = values(18)  // initial constant angle value needed for detector functions
		  '
		  '
		  '
		  'CaseCounter = CaseCounter + 1
		  
		  
		  
		End Sub
	#tag EndMethod


#tag EndWindowCode

#tag Events InterfaceUpdateTimer
	#tag Event
		Sub Action()
		  Var TheSuper As CaseSupervisorClass = MainThread.CaseSupervisor  // Get a reference to the supervisor
		  If MainThread.State = Thread.Running then  // if the thread is running
		    CaseProgressBar.Value = Round(TheSuper.N*100/TheSuper.NSteps)  // update the progress bar
		    ValueOfSimTimeLabel.Text = Format(TheSuper.τr*TheSuper.CaseParameters.GM/TheSuper.Year, "0.0000000")
		    ValueOfVLabel.Text = Format(TheSuper.Evolver.ValuesN.V,"0.000000")
		    ValueOfRunTimeLabel.Text = Format((System.Ticks - TheSuper.StartTicks)/60.0, "###0.00")
		    ValueOfStepNumberLabel.Text = TheSuper.N.ToString
		    If ValueOfTcLabel.Text = "" Then
		      Var TheSpinEvolver As SpinEvolverClass = MainThread.CaseSupervisor.Evolver.VEvolver.SpinEvolver
		      ValueOfTcLabel.Text = GetTimeToCoalescence(TheSuper, TheSpinEvolver).ToString
		    end if
		    Var theStepPower As Integer = TheSuper.Evolver.StepPowerP
		    Var theFactor as Integer
		    If theStepPower < 0 Then
		      theFactor = 2^(-theStepPower)
		      ValueOfStepRatioLabel.Text = "1/" + theFactor.ToString
		    Else
		      theFactor = 2^theStepPower
		      ValueOfStepRatioLabel.Text = theFactor.ToString
		    End if
		  Else // the thread has stopped, meaning that this case is done
		    CaseProgressBar.Value = 0  // set
		    StartStopButton.Caption = "Run Cases"
		    me.RunMode = Timer.RunModes.Off // and we need no more updates
		    ValueOfStatusLabel.Text = "Stopped"
		    ValueOfStopReasonLabel.Text = TheSuper.TerminationMessage
		    
		    If TheSuper.Uncertainty <> Nil Then
		      DisplayUncertainties(TheSuper.CaseParameters, TheSuper.Uncertainty)
		      MatrixChoicePopupMenu.SelectedRowIndex = 0
		      DisplayMatrix(TheSuper.ATAMatrix)
		      ValueOfConditionLabel.Text = Format(TheSuper.UncertaintyCalculator.Condition, "0.000e-0##")
		    End if
		  End if		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events AddCaseColumnButton
	#tag Event
		Sub Pressed()
		  '//Make Column Widths String
		  'str = MainListBox.ColumnWidths + ","+ "25,100,150"
		  '// Add 3 New Columns
		  'MainListBox.ColumnCount = MainListBox.ColumnCount + 3
		  '//Set Column Widths
		  'MainListBox.ColumnWidths = str
		  '//Create Case Header
		  'MainListBox.HeaderAt(MainListBox.ColumnCount - 2) = "Case: " + Str((MainListBox.ColumnCount - 1) / 3)
		  '//Create Uncertainity Header
		  'MainListBox.HeaderAt(MainListBox.ColumnCount -1) = "Uncertainity"
		  '//Make Case Column Text Field
		  'MainListBox.ColumnTypeAt(MainListBox.ColumnCount - 2) = DesktopListBox.CellTypes.TextField
		  '//Make Use Column Check Boxes
		  'For i As Integer = 0 to 18
		  'MainListBox.CellTypeAt(i,MainListBox.ColumnCount -3) = DesktopListBox.CellTypes.CheckBox
		  'MainListBox.CellCheckBoxStateAt(i, MainListBox.ColumnCount -1) = DesktopCheckbox.VisualStates.Checked
		  'Next
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CaseProgressBar
	#tag Event
		Sub Open()
		  me.MaximumValue = 100
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events StartStopButton
	#tag Event
		Sub Pressed()
		  If me.Caption = "Run Cases" Then
		    me.Caption = "Stop"
		    Var n As Integer = CasesListBox.LastColumnIndex
		    Var TheCases() As CaseParametersClass
		    Var Flag As Boolean
		    For j As Integer = 0 to n
		      Var ThisCase As New CaseParametersClass // Create new parameter list
		      // Set the first four parameters
		      ThisCase.M1 = CasesListBox.CellTextAt(0,j).ToDouble
		      ThisCase.M2 = CasesListBox.CellTextAt(1,j).ToDouble
		      ThisCase.F0 = CasesListBox.CellTextAt(2,j).ToDouble
		      ThisCase.R = CasesListBox.CellTextAt(3,j).ToDouble
		      // Specially handle cases we might not solve for
		      ThisCase.β = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(4,j))
		      ThisCase.SolveForβ = Flag
		      ThisCase.ψ = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(5,j))
		      ThisCase.SolveForψ = Flag
		      ThisCase.λ0 = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(6,j))
		      ThisCase.SolveForλ0 = Flag
		      ThisCase.Θ = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(7,j))
		      ThisCase.SolveForΘ = Flag
		      ThisCase.Φ = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(8,j))
		      ThisCase.SolveForΦ = Flag
		      ThisCase.χ10x = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(9,j))
		      ThisCase.SolveForχ10x = Flag
		      ThisCase.χ10y = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(10,j))
		      ThisCase.SolveForχ10y = Flag
		      ThisCase.χ10z = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(11,j))
		      ThisCase.SolveForχ10z = Flag
		      ThisCase.χ20x = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(12,j))
		      ThisCase.SolveForχ20x = Flag
		      ThisCase.χ20y = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(13,j))
		      ThisCase.SolveForχ20y = Flag
		      ThisCase.χ20z = GetValueAndSolveFlag(Flag, CasesListBox.CellTextAt(14,j))
		      ThisCase.SolveForχ20z = Flag
		      // Handle the remaining case items
		      ThisCase.ρ0 = CasesListBox.CellTextAt(15,j).ToDouble
		      ThisCase.PNOrder = CasesListBox.CellTextAt(16,j).ToDouble
		      ThisCase.Detectors = CasesListBox.CellTextAt(17,j).ToDouble
		      ThisCase.ΔT = CasesListBox.CellTextAt(18,j).ToDouble
		      ThisCase.RunDuration = CasesListBox.CellTextAt(19,j).ToDouble
		      // Add the case to the list
		      TheCases.Add(ThisCase)
		    Next
		    ValueOfStatusLabel.Text = "Running"
		    ValueOfStopReasonLabel.Text = ""
		    ValueOfTcLabel.Text = ""
		    MainThread.LoadCases(TheCases)
		    MainThread.Priority = Thread.HighPriority
		    MainThread.Start
		    InterfaceUpdateTimer.RunMode = Timer.RunModes.Multiple
		  Else
		    MainThread.Stop
		    InterfaceUpdateTimer.RunMode = Timer.RunModes.Multiple
		    me.Caption = "Run Cases"
		    ValueOfStatusLabel.Text = "Stopped"
		    ValueOfStopReasonLabel.Text = "Stop Request"
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events ResultsListBox1
	#tag Event
		Sub Opening()
		  // Define cells as TextAreas to allow multiple lines
		  me.AddRow("")  // Add six rows
		  me.AddRow("")
		  me.AddRow("")
		  me.AddRow("")
		  me.AddRow("")
		  me.AddRow("")
		  me.ColumnTypeAt(0) = DesktopListBox.CellTypes.TextArea
		End Sub
	#tag EndEvent
	#tag Event
		Function PaintCellText(g as Graphics, row as Integer, column as Integer, x as Integer, y as Integer) As Boolean
		  Var arr() As String = Me.CellTextAt(row, column).Split(EndOfLine)
		  If arr.LastIndex = 0 Then
		    g.DrawString(arr(0), x, y)
		  Else
		    For i As Integer = 0 To arr.LastIndex
		      g.DrawString(arr(i), x, y - 0.5*g.TextHeight + i * g.TextHeight)
		    Next
		  End If
		  Return True
		End Function
	#tag EndEvent
#tag EndEvents
#tag Events CasesListBox
	#tag Event
		Function CellKeyDown(row as Integer, column as Integer, key as String) As Boolean
		  If key = "x" And row > 3 And row < 15 Then
		    If me.CellTextAt(row,column).BeginsWith("x ") Then
		      me.ActiveTextControl.Text = me.ActiveTextControl.Text.Middle(2)
		      Return True
		    Else
		      me.ActiveTextControl.Text = "x " + me.ActiveTextControl.Text
		      Return True
		    End If
		  Else
		    Return False
		  End If
		End Function
	#tag EndEvent
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
#tag EndEvents
#tag Events ResultsListBox2
	#tag Event
		Sub Opening()
		  // Define cells as TextAreas to allow multiple lines
		  me.AddRow("")  // Add seven rows
		  me.AddRow("")
		  me.AddRow("")
		  me.AddRow("")
		  me.AddRow("")
		  me.AddRow("")
		  me.AddRow("")
		  me.ColumnTypeAt(0) = DesktopListBox.CellTypes.TextArea
		End Sub
	#tag EndEvent
	#tag Event
		Function PaintCellText(g as Graphics, row as Integer, column as Integer, x as Integer, y as Integer) As Boolean
		  Var arr() As String = Me.CellTextAt(row, column).Split(EndOfLine)
		  If arr.LastIndex = 0 Then
		    g.DrawString(arr(0), x, y)
		  Else
		    For i As Integer = 0 To arr.LastIndex
		      g.DrawString(arr(i), x, y - 0.5*g.TextHeight + i * g.TextHeight)
		    Next
		  End If
		  Return True
		End Function
	#tag EndEvent
#tag EndEvents
#tag Events ResultsListBox3
	#tag Event
		Sub Opening()
		  // Define cells as TextAreas to allow multiple lines
		  me.AddRow("")  // Add seven rows
		  me.AddRow("")
		  me.AddRow("")
		  me.AddRow("")
		  me.AddRow("")
		  me.AddRow("")
		  me.AddRow("")
		  me.ColumnTypeAt(0) = DesktopListBox.CellTypes.TextArea
		End Sub
	#tag EndEvent
	#tag Event
		Function PaintCellText(g as Graphics, row as Integer, column as Integer, x as Integer, y as Integer) As Boolean
		  Var arr() As String = Me.CellTextAt(row, column).Split(EndOfLine)
		  If arr.LastIndex = 0 Then
		    g.DrawString(arr(0), x, y)
		  Else
		    For i As Integer = 0 To arr.LastIndex
		      g.DrawString(arr(i), x, y - 0.5*g.TextHeight + i * g.TextHeight)
		    Next
		  End If
		  Return True
		End Function
	#tag EndEvent
#tag EndEvents
#tag Events MatrixChoicePopupMenu
	#tag Event
		Sub SelectionChanged(item As DesktopMenuItem)
		  Var theATA As Matrix = MainThread.CaseSupervisor.ATAMatrix
		  Var theY As Matrix = MainThread.CaseSupervisor.UncertaintyCalculator.Y
		  Var theY0 As Matrix = MainThread.CaseSupervisor.UncertaintyCalculator.Y0
		  Var theProd As Matrix = MainThread.CaseSupervisor.UncertaintyCalculator.YInvXY
		  If me.SelectedRowValue = "ATA" Then
		    DisplayMatrix(theATA)
		  ElseIf me.SelectedRowValue = "Y Original" Then
		    DisplayMatrix(theY0)
		  ElseIf me.SelectedRowValue = "Y Inverted" Then
		    DisplayMatrix(theY)
		  Else
		    DisplayMatrix(theProd)
		  End If
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events ResultsListBox4
	#tag Event
		Sub Opening()
		  // Define cells as TextAreas to allow multiple lines
		  me.AddRow("")  // Add five rows
		  me.AddRow("")
		  me.AddRow("")
		  me.AddRow("")
		  me.AddRow("")
		  me.ColumnTypeAt(0) = DesktopListBox.CellTypes.TextArea
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events CopyMatrixButton
	#tag Event
		Sub Pressed()
		  Var c As New Clipboard
		  Var s As String
		  Var delim As String
		  For j As Integer = 0 to MatrixListBox.LastRowIndex
		    For k As Integer = 1 to MatrixListBox.LastColumnIndex
		      s = s+ delim + MatrixListBox.CellTextAt(j,k)
		      delim = ", "
		    Next
		    delim = EndOfLine 
		  Next
		  c.Text = s
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
			"9 - Metal Window"
			"11 - Modeless Dialog"
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
#tag EndViewBehavior
