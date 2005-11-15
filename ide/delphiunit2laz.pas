{  $Id$  }
{
 /***************************************************************************
                          delphiunit2laz.pas
                          ------------------

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    Functions to convert delphi units to lcl units.

}
unit DelphiUnit2Laz;

{$mode objfpc}{$H+}

interface

uses
  // FCL+LCL
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics,
  Dialogs, Buttons, StdCtrls, FileUtil,
  // Components
  SynEdit, CodeCache, CodeToolManager, DefineTemplates,
  // IDE
  DialogProcs, IDEProcs, LazarusIDEStrConsts;

type
  TDelphi2LazarusDialog = class(TForm)
  private
  public
  end;

var
  Delphi2LazarusDialog: TDelphi2LazarusDialog;
  
function CheckDelphiFileExt(const Filename: string): TModalResult;
function CheckFilenameForLCLPaths(const Filename: string): TModalResult;
function ConvertDelphiToLazarusFilename(const DelphiFilename: string): string;
function ConvertDFMToLFMFilename(const DFMFilename: string;
  KeepCase: boolean): string;
function FindDFMFileForDelphiUnit(const DelphiFilename: string): string;
function RenameDelphiUnitToLazarusUnit(const DelphiFilename: string;
  RenameDFMFile: boolean;
  var LazarusFilename, LFMFilename: string): TModalResult;
function ConvertDFMFileToLFMFile(const DFMFilename: string): TModalResult;
function ConvertDelphiSourceToLazarusSource(const LazarusUnitFilename: string;
  AddLRSCode: boolean): TModalResult;
function FixMissingUnits(const LazarusUnitFilename: string): TModalResult;
function LoadUnitAndLFMFile(const UnitFileName: string;
  var UnitCode, LFMCode: TCodeBuffer; LFMMustExist: boolean): TModalResult;
function ConvertLFMtoLRSfile(const LFMFilename: string): TModalResult;
function CheckDelphiProjectExt(const Filename: string): TModalResult;
function CreateLPRFileForDPRFile(const DelphiProjectFilename: string;
  AddLRSCode: boolean; var LPRCode: TCodeBuffer): TModalResult;


implementation


function CheckDelphiFileExt(const Filename: string): TModalResult;
begin
  if CompareFileExt(Filename,'.pas',false)<>0 then begin
    Result:=MessageDlg(lisNotADelphiUnit,
      Format(lisTheFileIsNotADelphiUnit, ['"', Filename, '"']),
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;
  Result:=mrOk;
end;

function CheckFilenameForLCLPaths(const Filename: string): TModalResult;
// check if the unitpath of the directory of filename contains the path to the
// LCL
var
  Directory: String;
  UnitPath: String;
  LazarusSrcDir: string;
  LCLPath: String;
  NextStartPos: Integer;
begin
  // get directory of filename
  Directory:=ExtractFilePath(Filename);
  // get unitpath definition of directory
  UnitPath:=CodeToolBoss.GetUnitPathForDirectory(Directory);
  // get lazarus source directory
  LazarusSrcDir:=
           CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'LazarusDir'];
  // create base path to LCL compiled units <LazarusSrcDir>/lcl/units/
  LCLPath:=TrimFilename(LazarusSrcDir+SetDirSeparators('/lcl/units/'));
  NextStartPos:=1;
  //writeln('CheckFilenameForLCLPaths UnitPath="',UnitPath,'" LCLPath="',LCLPath,'"');
  if GetNextUsedDirectoryInSearchPath(UnitPath,LCLPath,NextStartPos)='' then
  begin
    LCLPath:=LCLPath+'$(TargetCPU)-$(TargetOS)';
    Result:=MessageDlg(lisLCLUnitPathMissing,
      Format(lisTheCurrentUnitPathForTheFileIsThePathToTheLCLUnits, [#13, '"',
        Filename, '"', #13, '"', UnitPath, '"', #13, #13, '"', LCLPath, '"',
        #13, #13, #13]),
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;
  Result:=mrOk;
end;

function ConvertDelphiToLazarusFilename(const DelphiFilename: string): string;
begin
  Result:=ExtractFilePath(DelphiFilename)
          +lowercase(ExtractFileName(DelphiFilename));
end;

function ConvertDFMToLFMFilename(const DFMFilename: string;
  KeepCase: boolean): string;
begin
  if DFMFilename<>'' then begin
    // platform and fpc independent unitnames are lowercase, so are the lfm files
    Result:=lowercase(ExtractFilenameOnly(DFMFilename));
    if KeepCase then
      Result:=ExtractFilenameOnly(DFMFilename);
    Result:=ExtractFilePath(DFMFilename)+Result+'.lfm';
  end else
    Result:='';
end;

function FindDFMFileForDelphiUnit(const DelphiFilename: string): string;
begin
  Result:=ChangeFileExt(DelphiFilename,'.dfm');
  if FileExists(Result) then exit;
  Result:=ChangeFileExt(DelphiFilename,'.DFM');
  if FileExists(Result) then exit;
  Result:=ChangeFileExt(DelphiFilename,'.xfm');
  if FileExists(Result) then exit;
  Result:=ChangeFileExt(DelphiFilename,'.XFM');
  if FileExists(Result) then exit;
  Result:='';
end;

function RenameDelphiUnitToLazarusUnit(const DelphiFilename: string;
  RenameDFMFile: boolean;
  var LazarusFilename, LFMFilename: string): TModalResult;
var
  DFMFilename: String;
begin
  LazarusFilename:=ConvertDelphiToLazarusFilename(DelphiFilename);
  LFMFilename:='';
  //writeln('RenameDelphiUnitToLazarusUnit Unit "',DelphiFilename,'" -> "',LazarusFilename,'"');
  Result:=RenameFileWithErrorDialogs(DelphiFilename,LazarusFilename,[mbAbort]);
  if Result<>mrOK then exit;
  if RenameDFMFile then begin
    DFMFilename:=FindDFMFileForDelphiUnit(DelphiFilename);
    if DFMFilename<>'' then begin
      LFMFilename:=ConvertDFMToLFMFilename(DFMFilename,false);
      //writeln('RenameDelphiUnitToLazarusUnit Unit "',DFMFilename,'" -> "',LFMFilename,'"');
      Result:=RenameFileWithErrorDialogs(DFMFilename,LFMFilename,[mbAbort]);
      if Result<>mrOK then exit;
    end;
  end;
  Result:=mrOk;
end;

function ConvertDFMFileToLFMFile(const DFMFilename: string): TModalResult;
var
  DFMStream, LFMStream: TMemoryStream;
  LFMFilename: string;
begin
  Result:=mrOk;
  DFMStream:=TMemoryStream.Create;
  LFMStream:=TMemoryStream.Create;
  try
    try
      DFMStream.LoadFromFile(DFMFilename);
    except
      on E: Exception do begin
        Result:=MessageDlg(lisCodeToolsDefsReadError, Format(
          lisUnableToReadFileError, ['"', DFMFilename, '"', #13, E.Message]),
          mtError,[mbIgnore,mbAbort],0);
        exit;
      end;
    end;
    try
      FormDataToText(DFMStream,LFMStream);
    except
      on E: Exception do begin
        Result:=MessageDlg(lisFormatError,
          Format(lisUnableToConvertFileError, ['"', DFMFilename, '"', #13,
            E.Message]),
          mtError,[mbIgnore,mbAbort],0);
        exit;
      end;
    end;
    // converting dfm file, without renaming unit -> keep case
    LFMFilename:=ConvertDFMToLFMFilename(DFMFilename,true);
    //writeln('ConvertDFMFileToLFMFile LFMFilename="',LFMFilename,'"');
    try
      LFMStream.SaveToFile(LFMFilename);
    except
      on E: Exception do begin
        Result:=MessageDlg(lisCodeToolsDefsWriteError,
          Format(lisUnableToWriteFileError, ['"', LFMFilename, '"', #13,
            E.Message]),
          mtError,[mbIgnore,mbAbort],0);
        exit;
      end;
    end;
  finally
    LFMSTream.Free;
    DFMStream.Free;
  end;
end;

function ConvertDelphiSourceToLazarusSource(const LazarusUnitFilename: string;
  AddLRSCode: boolean): TModalResult;
var
  LazUnitCode: TCodeBuffer;
  CTResult: Boolean;
begin
  Result:=LoadCodeBuffer(LazUnitCode,LazarusUnitFilename,
                         [lbfCheckIfText,lbfUpdateFromDisk]);
  if Result<>mrOk then exit;
  CTResult:=CodeToolBoss.ConvertDelphiToLazarusSource(LazUnitCode,AddLRSCode);
  if not CTResult then begin
    Result:=mrCancel;
    exit;
  end;
  Result:=mrOk;
end;

function FixMissingUnits(const LazarusUnitFilename: string): TModalResult;
var
  LazUnitCode: TCodeBuffer;
  CTResult: Boolean;
  MissingUnits: TStrings;
  MissingUnitsText: String;
  i: Integer;
  Msg: String;
begin
  Result:=LoadCodeBuffer(LazUnitCode,LazarusUnitFilename,
                         [lbfCheckIfText,lbfUpdateFromDisk]);
  if Result<>mrOk then exit;
  MissingUnits:=nil;
  try
    // find missing units
    DebugLn('FixMissingUnits FindMissingUnits');
    CTResult:=CodeToolBoss.FindMissingUnits(LazUnitCode,MissingUnits);
    if not CTResult then begin
      Result:=mrCancel;
      exit;
    end;
    if (MissingUnits=nil) or (MissingUnits.Count=0) then begin
      Result:=mrOk;
      exit;
    end;

    MissingUnitsText:='';
    for i:=0 to MissingUnits.Count-1 do begin
      if MissingUnitsText<>'' then
        MissingUnitsText:=MissingUnitsText+', ';
      MissingUnitsText:=MissingUnitsText+MissingUnits[i];
    end;
    DebugLn('FixMissingUnits FindMissingUnits="',MissingUnitsText,'"');
    // ask user if missing units should be commented
    if MissingUnits.Count=1 then
      Msg:=lisUnitNotFound
    else
      Msg:=lisUnitsNotFound2;
    Result:=MessageDlg(Msg,
      Format(lisTheFollowingUnitsWereNotFound1EitherTheseUnitsAreN, [#13,
        MissingUnitsText, #13, #13, #13, #13, #13, #13]),
      mtConfirmation,[mbYes,mbAbort],0);
    if Result<>mrYes then exit;

    // comment missing units
    DebugLn('FixMissingUnits CommentUnitsInUsesSections');
    CTResult:=CodeToolBoss.CommentUnitsInUsesSections(LazUnitCode,MissingUnits);
    if not CTResult then begin
      Result:=mrCancel;
      exit;
    end;

  finally
    MissingUnits.Free;
  end;
  Result:=mrOk;
end;

function LoadUnitAndLFMFile(const UnitFileName: string;
  var UnitCode, LFMCode: TCodeBuffer; LFMMustExist: boolean): TModalResult;
var
  LFMFilename: string;
begin
  UnitCode:=nil;
  LFMCode:=nil;
  Result:=LoadCodeBuffer(UnitCode,UnitFileName,
                         [lbfCheckIfText,lbfUpdateFromDisk]);
  if Result<>mrOk then exit;
  LFMFilename:=ChangeFileExt(UnitFileName,'.lfm');
  if FileExists(LFMFilename) then begin
    Result:=LoadCodeBuffer(LFMCode,LFMFilename,
                           [lbfCheckIfText,lbfUpdateFromDisk]);
    if Result<>mrOk then exit;
  end else if LFMMustExist then begin
    Result:=MessageDlg(lisLFMFileNotFound,
                       Format(lisUnitLFMFile, [UnitFileName, #13, LFMFilename]),
                       mtError,[mbCancel,mbAbort],0);
  end;
end;

function ConvertLFMtoLRSfile(const LFMFilename: string): TModalResult;
begin
  if not LFMtoLRSfile(LFMFilename) then begin
    Result:=MessageDlg(lisErrorCreatingLrs,
      lisUnableToConvertLfmToLrsAndWriteLrsFile,
      mtError,[mbCancel],0);
    exit;
  end;
  Result:=mrOk;
end;

function CheckDelphiProjectExt(const Filename: string): TModalResult;
begin
  if CompareFileExt(Filename,'.dpr',false)<>0 then begin
    Result:=MessageDlg(lisNotADelphiProject,
      Format(lisTheFileIsNotADelphiProjectDpr, ['"', Filename, '"']),
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;
  Result:=mrOk;
end;

function CreateLPRFileForDPRFile(const DelphiProjectFilename: string;
  AddLRSCode: boolean; var LPRCode: TCodeBuffer): TModalResult;
var
  LPRFilename: String;
  CTResult: Boolean;
begin
  LPRFilename:=ChangeFileExt(DelphiProjectFilename,'.lpr');
  Result:=CopyFileWithErrorDialogs(DelphiProjectFilename,LPRFilename,[]);
  if Result<>mrOk then exit;
  Result:=LoadCodeBuffer(LPRCode,LPRFilename,
                         [lbfCheckIfText,lbfUpdateFromDisk]);
  if Result<>mrOk then exit;
  CTResult:=CodeToolBoss.ConvertDelphiToLazarusSource(LPRCode,AddLRSCode);
  debugln('CreateLPRFileForDPRFile: ',LPRCode.Source);
  if not CTResult then begin
    Result:=mrCancel;
    exit;
  end;
  Result:=mrOk;
end;

initialization
  {$I delphiunit2laz.lrs}

end.

