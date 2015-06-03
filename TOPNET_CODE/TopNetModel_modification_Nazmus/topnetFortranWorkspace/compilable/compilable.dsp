# Microsoft Developer Studio Project File - Name="compilable" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=compilable - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "compilable.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "compilable.mak" CFG="compilable - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "compilable - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "compilable - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "compilable - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x1409 /d "NDEBUG"
# ADD RSC /l 0x1409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "compilable - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /automatic /browser /check:bounds /compile_only /dbglibs /debug:full /nologo /recursive /traceback /warn:argument_checking /warn:nofileopt
# SUBTRACT F90 /assume:buffered_io /check:overflow /check:underflow
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /GZ /c
# ADD BASE RSC /l 0x1409 /d "_DEBUG"
# ADD RSC /l 0x1409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /stack:0x2faf0800 /subsystem:console /incremental:no /debug /machine:I386 /out:"Debug/topnet.exe" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "compilable - Win32 Release"
# Name "compilable - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=..\..\topnetcodes\AllocateWaterToUsers.f90
DEP_F90_ALLOC=\
	".\Debug\constant_definitions.mod"\
	".\Debug\findmodule.mod"\
	".\Debug\input_structures.mod"\
	".\Debug\other_structures.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\Append_To_Output_Tables.f90
DEP_F90_APPEN=\
	".\Debug\constant_definitions.mod"\
	".\Debug\findmodule.mod"\
	".\Debug\input_structures.mod"\
	".\Debug\other_structures.mod"\
	".\Debug\TimeVaryingOutput.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\AssignDrainageFlows.f90
DEP_F90_ASSIG=\
	".\Debug\constant_definitions.mod"\
	".\Debug\findmodule.mod"\
	".\Debug\input_structures.mod"\
	".\Debug\other_structures.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\AssignPriorityOrder.f90
DEP_F90_ASSIGN=\
	".\Debug\constant_definitions.mod"\
	".\Debug\findmodule.mod"\
	".\Debug\input_structures.mod"\
	".\Debug\other_structures.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\BalanceFlowsAtReservoirs.f90
DEP_F90_BALAN=\
	".\Debug\constant_definitions.mod"\
	".\Debug\findmodule.mod"\
	".\Debug\input_structures.mod"\
	".\Debug\other_structures.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\BalanceFlowsAtStreamNodes.f90
DEP_F90_BALANC=\
	".\Debug\constant_definitions.mod"\
	".\Debug\findmodule.mod"\
	".\Debug\input_structures.mod"\
	".\Debug\other_structures.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\BuildDrainageOrder.f90
DEP_F90_BUILD=\
	".\Debug\findmodule.mod"\
	".\Debug\input_structures.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\BuildLinkStructure.f90
DEP_F90_BUILDL=\
	".\Debug\constant_definitions.mod"\
	".\Debug\findmodule.mod"\
	".\Debug\input_structures.mod"\
	".\Debug\other_structures.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\BuildNodeStructure.f90
DEP_F90_BUILDN=\
	".\Debug\constant_definitions.mod"\
	".\Debug\findmodule.mod"\
	".\Debug\input_structures.mod"\
	".\Debug\other_structures.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\CalculateDemand.f90
DEP_F90_CALCU=\
	".\Debug\constant_definitions.mod"\
	".\Debug\findmodule.mod"\
	".\Debug\input_structures.mod"\
	".\Debug\other_structures.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\calv46sn_v7.f
DEP_F90_CALV4=\
	"..\..\topnetcodes\LUNS.INC"\
	"..\..\topnetcodes\maxvariables.inc"\
	"..\..\topnetcodes\tdims_v7.INC"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\Data_Read.f90
DEP_F90_DATA_=\
	"..\..\topnetcodes\maxvariables.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\et_v7_fromdave.f
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\find.f90
DEP_F90_FIND_=\
	".\Debug\findmodule.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\hydatasn_v7.f
DEP_F90_HYDAT=\
	"..\..\topnetcodes\LUNS.INC"\
	"..\..\topnetcodes\maxvariables.inc"\
	"..\..\topnetcodes\tdims_v7.INC"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\ImposeMeasuredFlows.f90
DEP_F90_IMPOS=\
	".\Debug\constant_definitions.mod"\
	".\Debug\findmodule.mod"\
	".\Debug\input_structures.mod"\
	".\Debug\other_structures.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\indepth1_v7.f
DEP_F90_INDEP=\
	"..\..\topnetcodes\maxvariables.inc"\
	"..\..\topnetcodes\tdims_v7.INC"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\Initialise_Output_Tables.f90
DEP_F90_INITI=\
	".\Debug\constant_definitions.mod"\
	".\Debug\findmodule.mod"\
	".\Debug\input_structures.mod"\
	".\Debug\other_structures.mod"\
	".\Debug\TimeVaryingOutput.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\mddata_v10_7.f
DEP_F90_MDDAT=\
	"..\..\topnetcodes\LUNS.INC"\
	"..\..\topnetcodes\maxvariables.inc"\
	"..\..\topnetcodes\tdims_v7.INC"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\MIC.FOR
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\modules.f90
DEP_F90_MODUL=\
	"..\..\topnetcodes\types.f90"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\newt.for
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\PropagateWaterViaUser.f90
DEP_F90_PROPA=\
	".\Debug\constant_definitions.mod"\
	".\Debug\findmodule.mod"\
	".\Debug\input_structures.mod"\
	".\Debug\other_structures.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\PTRATE.FOR
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\read_inputs.f90
DEP_F90_READ_=\
	".\Debug\constant_definitions.mod"\
	".\Debug\data_array.mod"\
	".\Debug\findmodule.mod"\
	".\Debug\input_structures.mod"\
	".\Debug\other_structures.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\read_struct_from_text.f90
DEP_F90_READ_S=\
	".\Debug\data_array.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\snowdgtv22.f
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\SnowLSub.f
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\SnowWrap.f
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\snowxv22.f
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\top_main_v7.f
DEP_F90_TOP_M=\
	"..\..\topnetcodes\nlfit.inc"\
	"..\..\topnetcodes\tdims_v7.INC"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\toplim_v7.f
DEP_F90_TOPLI=\
	"..\..\topnetcodes\LUNS.INC"\
	"..\..\topnetcodes\maxvariables.inc"\
	"..\..\topnetcodes\tdims_v7.INC"\
	".\Debug\Data_Read.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\topmoddgt_v7.f
DEP_F90_TOPMO=\
	"..\..\topnetcodes\LUNS.INC"\
	"..\..\topnetcodes\maxvariables.inc"\
	"..\..\topnetcodes\tdims_v7.INC"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\watermgmt.f90
DEP_F90_WATER=\
	".\Debug\constant_definitions.mod"\
	".\Debug\findmodule.mod"\
	".\Debug\input_structures.mod"\
	".\Debug\other_structures.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\Write_Output_line.f90
DEP_F90_WRITE=\
	".\Debug\constant_definitions.mod"\
	".\Debug\input_structures.mod"\
	".\Debug\other_structures.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\topnetcodes\Write_Output_Tables.f90
DEP_F90_WRITE_=\
	".\Debug\constant_definitions.mod"\
	".\Debug\data_array.mod"\
	".\Debug\findmodule.mod"\
	".\Debug\input_structures.mod"\
	".\Debug\other_structures.mod"\
	".\Debug\TimeVaryingOutput.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
