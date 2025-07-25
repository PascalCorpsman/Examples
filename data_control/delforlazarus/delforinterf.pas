(*
  Modified by Corpsman, to Support Lazarus Linux. 01.09.2009
*)
Unit delforinterf;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

Interface

Uses Classes, delfortypes;

Procedure Formatter_Destroy;
Procedure Formatter_Create;
Procedure Formatter_LoadFromFile(AFileName: PChar; ASettings: PSettings;
  SizeOfSettings: Integer);
Procedure Formatter_LoadFromList(AList: TStringList; ASettings: PSettings;
  SizeOfSettings: Integer);
Function Formatter_Parse(ASettings: PSettings; SizeOfSettings: Integer): Boolean;
Procedure Formatter_Clear;
Procedure Formatter_WriteToFile(AFileName: PChar);
Function Formatter_GetTextStr: PChar;
Procedure Formatter_SetTextStr(AText: PChar);
Procedure Formatter_SetOnProgress(AOnProgress: TProgressEvent);
Procedure Formatter_LoadCapFile(ACapFile: PChar);
Procedure Formatter_SaveCapFile(ACapFile: PChar);
Function Formatter_Version: Integer;

Implementation

Uses delforengine;

Procedure Formatter_Destroy;
Begin
  DelforParser.Free;
  DelforParser := Nil;
End;

Procedure Formatter_Create;
Begin
  If assigned(DelforParser) Then DelforParser.free;
  DelforParser := TDelforParser.Create;
End;

Procedure CopySettings(ASettings: PSettings; SizeOfSettings: Integer);
Var
  TheSettings: TSettings;
Begin
  {Some measures to improve future compatibility}
  If SizeOfSettings < SizeOf(TSettings) Then Begin
    FillChar(TheSettings, sizeof(TheSettings), 0);
    Move(ASettings^, TheSettings, SizeOfSettings);
    DelforParser.Settings := TheSettings;
  End
  Else
    DelforParser.Settings := ASettings^;
End;

Procedure Formatter_LoadFromFile(AFileName: PChar; ASettings: PSettings;
  SizeOfSettings: Integer);
Begin
  CopySettings(ASettings, SizeOFSettings);
  DelforParser.LoadFromFile(AFileName);
End;

Procedure Formatter_LoadFromList(AList: TStringList; ASettings: PSettings;
  SizeOfSettings: Integer);
Begin
  CopySettings(ASettings, SizeOFSettings);
  DelforParser.LoadFromList(AList);
End;

Function Formatter_Parse(ASettings: PSettings; SizeOfSettings: Integer): Boolean;
Begin
  CopySettings(ASettings, SizeOFSettings);
  Result := DelforParser.Parse;
End;

Procedure Formatter_Clear;
Begin
  DelforParser.Clear;
End;

Procedure Formatter_WriteToFile(AFileName: PChar);
Begin
  DelforParser.WriteToFile(AFileName);
End;

Function Formatter_GetTextStr: PChar;
Begin
  Result := DelforParser.Text;
End;

Procedure Formatter_SetTextStr(AText: PChar);
Begin
  DelforParser.Text := AText;
End;

Procedure Formatter_SetOnProgress(AOnProgress: TProgressEvent);
Begin
  DelforParser.OnProgress := AOnProgress;
End;

Procedure Formatter_LoadCapFile(ACapFile: PChar);
Begin
  DelforParser.LoadCapFile(ACapFile)
End;

Procedure Formatter_SaveCapFile(ACapFile: PChar);
Begin
  DelforParser.SaveCapFile(ACapFile)
End;

Function Formatter_Version: Integer;
Begin
  Result := CurrentDllVersion;
  {Change this version number only if the new version
  is not backwards compatible}
End;

End.

