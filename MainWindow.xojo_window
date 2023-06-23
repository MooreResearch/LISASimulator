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
   Height          =   600
   ImplicitInstance=   True
   MacProcID       =   0
   MaximumHeight   =   32000
   MaximumWidth    =   32000
   MenuBar         =   1095792639
   MenuBarVisible  =   False
   MinimumHeight   =   64
   MinimumWidth    =   64
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
      Height          =   613
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
      Top             =   -13
      Transparent     =   False
      Underline       =   False
      Value           =   3
      Visible         =   True
      Width           =   1000
      Begin DesktopListBox MainListBox
         AllowAutoDeactivate=   True
         AllowAutoHideScrollbars=   True
         AllowExpandableRows=   False
         AllowFocusRing  =   True
         AllowResizableColumns=   True
         AllowRowDragging=   False
         AllowRowReordering=   False
         Bold            =   False
         ColumnCount     =   3
         ColumnWidths    =   "75,40,80,150"
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
         HasVerticalScrollbar=   True
         HeadingIndex    =   -1
         Height          =   523
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "Unit	Used	Case: 1\nM (sols)		10000			\nδ		0			\nf(mHz)		2.00			\nR(ly)		1000			\nβ (°)		39			\nψ (°)		24			\nλ0		0			\nΘ (°)		5			\nΦ(°)		268.5			\nχ10x		0			\nχ10y		0			\nχ10z		0			\nχ20x		0			\nχ20y		0			\nx20z		0\nσ0		0			\nPN Order		3			\nDetectors		2			\ndt(s)		50			"
         Italic          =   False
         Left            =   31
         LockBottom      =   True
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   True
         LockTop         =   True
         RequiresSelection=   False
         RowSelectionType=   0
         Scope           =   0
         TabIndex        =   0
         TabPanelIndex   =   1
         TabStop         =   True
         Tooltip         =   ""
         Top             =   25
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   949
         _ScrollOffset   =   0
         _ScrollWidth    =   -1
      End
      Begin DesktopButton RunCasesButton
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
         Left            =   880
         LockBottom      =   True
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   False
         MacButtonStyle  =   0
         Scope           =   0
         TabIndex        =   1
         TabPanelIndex   =   1
         TabStop         =   True
         Tooltip         =   ""
         Top             =   560
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   100
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
         Left            =   147
         LockBottom      =   True
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   False
         MacButtonStyle  =   0
         Scope           =   0
         TabIndex        =   3
         TabPanelIndex   =   1
         TabStop         =   True
         Tooltip         =   ""
         Top             =   560
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
         Left            =   579
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
         Top             =   42
         Transparent     =   True
         Value           =   0.0
         Visible         =   True
         Width           =   295
      End
      Begin DesktopProgressWheel RunningProgressWheel
         Active          =   False
         AllowAutoDeactivate=   True
         AllowTabStop    =   True
         Enabled         =   True
         Height          =   37
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Left            =   507
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   True
         PanelIndex      =   0
         Scope           =   0
         TabIndex        =   1
         TabPanelIndex   =   2
         Tooltip         =   ""
         Top             =   36
         Transparent     =   False
         Visible         =   False
         Width           =   33
         _mIndex         =   0
         _mInitialParent =   ""
         _mName          =   ""
         _mPanelIndex    =   0
      End
      Begin DesktopButton StopButton
         AllowAutoDeactivate=   True
         Bold            =   False
         Cancel          =   False
         Caption         =   "Stop"
         Default         =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   900
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
         Top             =   42
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   80
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
         Top             =   23
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
         InitialValue    =   "dh/dM\ndh/dδ\ndh/df0\ndh/dR\ndh/dβ\ndh/dψ\ndh/dλ0\ndh/dΘ\ndh/dΦ\ndh/dχ10x\ndh/dχ10y\ndh/dχ10z\ndh/dχ20x\ndh/dχ20y\ndh/dχ20z\nh\nhp\nhc\nα\nι\n ζ\nLNhatx\nLNhaty\nLNhatz\nχ1hatX\nχ1hatY\nχ1hatZ\nχ2hatX\nχ2hatY\nχ2hatZ\nnoise"
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
         Top             =   25
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
         Top             =   25
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   80
      End
      Begin DesktopListBox ResultsListBox
         AllowAutoDeactivate=   True
         AllowAutoHideScrollbars=   True
         AllowExpandableRows=   False
         AllowFocusRing  =   True
         AllowResizableColumns=   True
         AllowRowDragging=   False
         AllowRowReordering=   False
         Bold            =   False
         ColumnCount     =   3
         ColumnWidths    =   "75,80,150"
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
         HasVerticalScrollbar=   True
         HeadingIndex    =   -1
         Height          =   547
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "Unit	Case: 1 	Uncertainity	\nM (sols)	10000			\nδ	0			\nf(mHz)	2.00			\nR(ly)	1000			\nβ (°)	39			\nψ (°)	24			\nλ0	0			\nΘ (°)	5			\nΦ(°)	268.5			\nχ10x	0			\nχ10y	0			\nχ10z	0			\nχ20x	0			\nχ20y	0			\nx20z	0\nσ0	0		\nPN Order	4			\nDetectors	2			\ndt(s)	50			\nΩ				"
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
         TabIndex        =   18
         TabPanelIndex   =   3
         TabStop         =   True
         Tooltip         =   ""
         Top             =   33
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   322
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
         Height          =   471
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   500
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
         Top             =   109
         Underline       =   False
         Visible         =   True
         Width           =   480
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
         Top             =   25
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
         Top             =   53
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
         Top             =   117
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
         Text            =   "Simulation Time (y):"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   85
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
         Top             =   149
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
         Top             =   181
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
         Left            =   294
         LockBottom      =   True
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   False
         MacButtonStyle  =   0
         Scope           =   0
         TabIndex        =   4
         TabPanelIndex   =   1
         TabStop         =   True
         Tooltip         =   ""
         Top             =   558
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
         Left            =   431
         LockBottom      =   True
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   False
         MacButtonStyle  =   0
         Scope           =   0
         TabIndex        =   5
         TabPanelIndex   =   1
         TabStop         =   True
         Tooltip         =   ""
         Top             =   560
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
         Left            =   31
         LockBottom      =   True
         LockedInPosition=   False
         LockLeft        =   False
         LockRight       =   True
         LockTop         =   False
         MacButtonStyle  =   0
         Scope           =   0
         TabIndex        =   6
         TabPanelIndex   =   1
         TabStop         =   True
         Tooltip         =   ""
         Top             =   560
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   96
      End
      Begin DesktopLabel OmegaValueLabel
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
         Left            =   804
         LockBottom      =   False
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   True
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   19
         TabPanelIndex   =   3
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   482
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   131
      End
      Begin DesktopChart DataChart
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
         Height          =   527
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   20
         LockBottom      =   True
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   True
         LockTop         =   True
         Mode            =   0
         Scope           =   0
         TabIndex        =   8
         TabPanelIndex   =   4
         TabStop         =   True
         TextColor       =   &c000000
         Title           =   ""
         Tooltip         =   ""
         Top             =   55
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
         Top             =   25
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
         Top             =   25
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
         Top             =   25
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
         Top             =   25
         Transparent     =   False
         Underline       =   False
         ValidationMask  =   ""
         Visible         =   True
         Width           =   115
      End
      Begin DesktopLabel CaptionForAutoSaveLabel
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
         Left            =   47
         LockBottom      =   True
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   False
         Multiline       =   False
         Scope           =   0
         Selectable      =   False
         TabIndex        =   10
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   "Automatically Save All Cases as:"
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   559
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   203
      End
      Begin DesktopPopupMenu ExportTypePopupMenu
         AllowAutoDeactivate=   True
         Bold            =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         InitialValue    =   "Binary\n.csv\n"
         Italic          =   False
         Left            =   255
         LockBottom      =   True
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   False
         Scope           =   0
         SelectedRowIndex=   0
         TabIndex        =   12
         TabPanelIndex   =   2
         TabStop         =   True
         Tooltip         =   ""
         Top             =   560
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   80
      End
      Begin DesktopCheckBox AutoSaveCheckbox
         AllowAutoDeactivate=   True
         Bold            =   False
         Caption         =   "Untitled"
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
         TabIndex        =   13
         TabPanelIndex   =   2
         TabStop         =   True
         Tooltip         =   ""
         Top             =   559
         Transparent     =   False
         Underline       =   False
         Value           =   False
         Visible         =   True
         VisualState     =   0
         Width           =   18
      End
      Begin DesktopButton SaveThisCaseButton
         AllowAutoDeactivate=   True
         Bold            =   False
         Cancel          =   False
         Caption         =   "Save This Case"
         Default         =   False
         Enabled         =   True
         FontName        =   "System"
         FontSize        =   0.0
         FontUnit        =   0
         Height          =   20
         Index           =   -2147483648
         InitialParent   =   "MainTabPanel"
         Italic          =   False
         Left            =   375
         LockBottom      =   True
         LockedInPosition=   False
         LockLeft        =   True
         LockRight       =   False
         LockTop         =   False
         MacButtonStyle  =   0
         Scope           =   0
         TabIndex        =   14
         TabPanelIndex   =   2
         TabStop         =   True
         Tooltip         =   ""
         Top             =   559
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   113
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
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   25
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   100
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
         Top             =   53
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   100
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
         TabIndex        =   17
         TabPanelIndex   =   2
         TabStop         =   True
         Text            =   ""
         TextAlignment   =   0
         TextColor       =   &c000000
         Tooltip         =   ""
         Top             =   85
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   100
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
         Top             =   117
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   100
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
         Top             =   149
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   100
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
         Top             =   181
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   100
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
         Top             =   25
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
         Top             =   25
         Transparent     =   False
         Underline       =   False
         Visible         =   True
         Width           =   124
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
		  SetUpMainLB()
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub CreateArrays()
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
		Function GetCurrentCaseValues() As Double()
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
		Function GetHighestValue(InputArray() As Double, Min As Integer, Max As Integer) As Double
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
		Function GetIndexOfClosest(InputValue As String) As Integer
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
		Function GetLowerBound() As Integer
		  '// Returns the lower bound based on user input
		  '
		  'Var LowerBound As Integer = GetIndexOfClosest(StartTimeTextField.Text)
		  '
		  'Return LowerBound
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetLowestValue(InputArray() As Double, Min As Integer, Max As Integer) As Double
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
		Function GetUpperBound() As Integer
		  '// Returns the upper bound based on user input
		  '
		  'Var UpperBound As Integer = GetIndexOfClosest(EndTimeTextField.Text)
		  '
		  'Return UpperBound
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub PopulateUncertainties(ATA(, ) As Double)
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
		Sub SetInitialValues(CaseCounter As Integer)
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

	#tag Method, Flags = &h0
		Sub SetUpMainLB()
		  'For i As Integer = 0 to 18
		  'MainListBox.CellTypeAt(i,1) = DesktopListBox.CellTypes.CheckBox
		  'if (i <9 or i >14) then
		  'MainListBox.CellCheckBoxStateAt(i, 1) = DesktopCheckbox.VisualStates.Checked
		  'end
		  'Next
		  'MainListBox.ColumnTypeAt(2) = DesktopListBox.CellTypes.TextField
		  
		End Sub
	#tag EndMethod


#tag EndWindowCode

#tag Events InterfaceUpdateTimer
	#tag Event
		Sub Action()
		  'if MainThread.State = Thread.Running then  // if the thread is running
		  'PGBProgress.value = myMain.UpdateProgress  // show progress on current case
		  'else // the thread has stopped, meaning all cases are done
		  'PGBProgress.value = 0 //  so reset progress bar
		  'me.RunMode = Timer.RunModes.Off // and we need no more updates
		  'end if
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events MainListBox
	#tag Event
		Function CellPressed(row As Integer, column As Integer, x As Integer, y As Integer) As Boolean
		  If column = 3 Then 
		    If Me.CellCheckBoxStateAt(row, column) = DesktopCheckbox.VisualStates.Checked Then
		      // The checkbox was just checked.
		      // Add an asterisk to the start of the text in the cell to the left.
		      If   not Me.CellTextAt(row, column - 2).contains("*") then
		        Me.CellTextAt(row, column - 2) = "*" + Me.CellTextAt(row, column - 2)
		      end
		      
		    Else
		      // The checkbox was just unchecked.
		      // Remove the asterisk from the start of the text in the cell to the left.
		      Me.CellTextAt(row, column - 2) = Me.CellTextAt(row, column - 2).Replace("*", "")
		    End If
		  End If
		End Function
	#tag EndEvent
#tag EndEvents
#tag Events RunCasesButton
	#tag Event
		Sub Pressed()
		  'SetInitialValues(CaseCounter)
		  '
		  'GraphChosenButtonMain.Enabled = False
		  '
		  'RunningProgressWheel.Visible = True
		  '
		  'UIUpdateTimer.RunMode = Timer.RunModes.Multiple
		  '
		  'MainThread.Priority = 10
		  'MainThread.run
		  '
		  
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
#tag Events DrawGraphButton
	#tag Event
		Sub Pressed()
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events ResultsListBox
	#tag Event
		Function CellPressed(row As Integer, column As Integer, x As Integer, y As Integer) As Boolean
		  If column = 3 Then 
		    If Me.CellCheckBoxStateAt(row, column) = DesktopCheckbox.VisualStates.Checked Then
		      // The checkbox was just checked.
		      // Add an asterisk to the start of the text in the cell to the left.
		      If   not Me.CellTextAt(row, column - 2).contains("*") then
		        Me.CellTextAt(row, column - 2) = "*" + Me.CellTextAt(row, column - 2)
		      end
		      
		    Else
		      // The checkbox was just unchecked.
		      // Remove the asterisk from the start of the text in the cell to the left.
		      Me.CellTextAt(row, column - 2) = Me.CellTextAt(row, column - 2).Replace("*", "")
		    End If
		  End If
		End Function
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
