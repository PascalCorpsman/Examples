(******************************************************************************)
(* umultipartformdatastream.pas                                    ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : <Module_description>                                         *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*                                                                            *)
(******************************************************************************)
(* Inspiration: https://bbs.csdn.net/topics/360157293                         *)
(*  -> die scheinen aber auch nur bei TIDMultiPartFormDataStream abgeschrieben*)
(*     zu haben ...                                                           *)
(******************************************************************************)
Unit umultipartformdatastream;

{$MODE ObjFPC}{$H+}

Interface

(*
 * Usage:
 *    - TMultiPartFormDataStream.create
 *    - [Add*]
 *    - PrepareStreamForDispatch
 *    - overhand  TMultiPartFormDataStream with TMultiPartFormDataStream.size bytes
 *)

Uses
  SysUtils, Classes;

Type

  { TMultiPartFormDataStream }

  TMultiPartFormDataStream = Class(TMemoryStream)
  private
    FBoundary: String;
    FRequestContentType: String;
    Function GenerateUniqueBoundary: String;
  public
    Property Boundary: String read FBoundary;
    Property RequestContentType: String read FRequestContentType;

    Constructor Create;

    Procedure AddFormField(Const FieldName, FieldValue: String);

    Procedure AddMeta(Const FieldName, ContentType: String; Content: String);

    Procedure AddFile(Const FieldName, FileName, ContentType: String); overload;
    Procedure AddFile(Const FieldName, FileName, ContentType: String; FileData: TStream); overload;

    Procedure PrepareStreamForDispatch;
  End;

Implementation

Const
  CONTENT_TYPE = 'multipart/form-data; boundary=';
  CRLF = #13#10;
  CONTENT_DISPOSITION = 'Content-Disposition: form-data; name="%s"';
  FILE_NAME_PLACE_HOLDER = '; filename="%s"';
  CONTENT_TYPE_PLACE_HOLDER = 'Content-Type: %s';
  CONTENT_LENGTH = 'Content-Length: %d';

  { TMsMultiPartFormDataStream }

Constructor TMultiPartFormDataStream.Create;
Begin
  Inherited;
  FBoundary := GenerateUniqueBoundary;
  FRequestContentType := CONTENT_TYPE + FBoundary;
End;

Procedure TMultiPartFormDataStream.AddFile(Const FieldName, FileName,
  ContentType: String; FileData: TStream);
Var
  sFormFieldInfo: String;
  iSize: Int64;
Begin
  iSize := FileData.Size;
  sFormFieldInfo := Format(
    CRLF + '--' + fBoundary +
    CRLF + CONTENT_DISPOSITION + FILE_NAME_PLACE_HOLDER +
    CRLF + CONTENT_LENGTH +
    CRLF + CONTENT_TYPE_PLACE_HOLDER +
    CRLF + CRLF,
    [FieldName, FileName, iSize, ContentType]);
  Write(sFormFieldInfo[1], Length(sFormFieldInfo));
  FileData.Position := 0;
  CopyFrom(FileData, iSize);
End;

Procedure TMultiPartFormDataStream.AddMeta(Const FieldName,
  ContentType: String; Content: String);
Var
  sFormFieldInfo: String;
Begin
  If ContentType = '' Then Begin
    sFormFieldInfo := Format(
      CRLF + '--' + fBoundary +
      CRLF + CONTENT_DISPOSITION +
      CRLF + CRLF,
      [FieldName]);
  End
  Else Begin
    sFormFieldInfo := Format(
      CRLF + '--' + fBoundary +
      CRLF + CONTENT_DISPOSITION +
      CRLF + CONTENT_TYPE_PLACE_HOLDER +
      CRLF + CRLF,
      [FieldName, ContentType]);
  End;
  Write(sFormFieldInfo[1], Length(sFormFieldInfo));

  Write(Content[1], Length(Content));
End;

Procedure TMultiPartFormDataStream.AddFile(Const FieldName, FileName,
  ContentType: String);
Var
  FileStream: TFileStream;
Begin
  FileStream := TFileStream.Create(FileName, fmOpenRead Or fmShareDenyWrite);
  Try
    AddFile(FieldName, FileName, ContentType, FileStream);
  Finally
    FileStream.Free;
  End;
End;

Procedure TMultiPartFormDataStream.AddFormField(Const FieldName,
  FieldValue: String);
Begin
  AddMeta(FieldName, '', FieldValue);
End;

Function TMultiPartFormDataStream.GenerateUniqueBoundary: String;
Begin
  Result := '--------' + FormatDateTime('mmddyyhhnnsszzz', Now);
End;

Procedure TMultiPartFormDataStream.PrepareStreamForDispatch;
Var
  sFormFieldInfo: String;
Begin
  sFormFieldInfo := CRLF + '--' + fBoundary + '--' + CRLF;
  Write(sFormFieldInfo[1], Length(sFormFieldInfo));
  Position := 0;
End;

End.

