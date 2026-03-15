(******************************************************************************)
(* uStableIndexVector                                              14.03.2016 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : A dynamic array which support ID's that are stable even if   *)
(*               elements are deleted.                                        *)
(*               ADD:    O(1) ( if preallocated buffer is big enough)         *)
(*               DEL:    O(1)                                                 *)
(*               ACCESS: O(1)                                                 *)
(*                                                                            *)
(* Inspired by : https://www.youtube.com/watch?v=L4xOCvELWlU                  *)
(*               https://github.com/johnBuffer/StableIndexVector              *)
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

Unit uStableIndexVector;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Type

  TID = Integer; // To state clear that this is a ID and not a index !

  { TStableIndexVector }

  Generic TStableIndexVector < T > = Class
  Type
    PT = ^T;
    TIterAllEvent = Procedure(Sender: TObject; Var aIterator: T) Of Object;
  private
    fData: Array Of T; // Container that stores the Actual Data
    fData_To_ID: Array Of Integer;
    fID_To_Data: Array Of Integer;
    fDataCount: Integer; // Number of elements actual used in fData
    fBlockSize: integer; // When Allocating a new Element allocate space for at least fBlocksize Elements
    fIteratorIndex: integer; // Used for Iterator Pattern
    Function getCapacity: integer;
    Function getDataElement(index: Integer): T;
    Function getElement(ID: TID): T;
    Procedure setDataElement(index: Integer; AValue: T);
    Procedure setElement(ID: TID; AValue: T);
  public
    Property Capacity: integer read getCapacity; // Length of preallocated buffer, if add more elements a new block will be allocated
    Property Count: integer read fDataCount; // Number of valid elements in Buffer
    Property DataElement[index: Integer]: T read getDataElement write setDataElement; // Access to data via 0..Count-1 (ignoring ID's)
    Property Element[ID: TID]: T read getElement write setElement; default; // Access to a element by ID (NOT Index !)

    Constructor Create(aBlockSize: integer = 16); virtual;
    Destructor Destroy(); override;

    Function Add(aValue: T): TID;
    Function Del(ID: TID): Boolean; // True if deletion succeed
    Function isValid(ID: TID): Boolean; // True if element with ID exists in data

    Function IndexToID(aIndex: Integer; Out ID: TID): Boolean; // Converts a Index to a ID

    Procedure Clear;

    (*
     * Deleting of Iterater element is allowed, but not other elements!
     *)
    Function IterReset(Out aIterator: PT): Boolean; // True if at least 1 Element is in Data, points to first Iteration Element
    Function IterNext(Out aIterator: PT): Boolean; // True if there is a next element,

    // Fastest access to all Elements, no deletion of Iterator allowed!
    Procedure IterAll(aIteratorCallback: TIterAllEvent);
  End;

Implementation

{ TStableIndexVector }

Constructor TStableIndexVector.Create(aBlockSize: integer);
Var
  i: Integer;
Begin
  assert(aBlockSize > 0);
  Inherited Create;
  setlength(fdata, aBlockSize);
  setlength(fID_To_Data, aBlockSize);
  setlength(fData_To_ID, aBlockSize);
  fBlockSize := aBlockSize;
  For i := 0 To high(fID_To_Data) Do Begin
    fID_To_Data[i] := i;
    fData_To_ID[i] := i;
  End;
  fDataCount := 0;
End;

Destructor TStableIndexVector.Destroy;
Begin
  setlength(fData, 0);
  setlength(fData_To_ID, 0);
  setlength(fID_To_Data, 0);
End;

Function TStableIndexVector.getElement(ID: TID): T;
Begin
  Assert(id >= 0);
  Assert(id < length(fID_To_Data));
  Assert(fID_To_Data[ID] < fDataCount);
  result := fData[fID_To_Data[id]];
End;

Procedure TStableIndexVector.setDataElement(index: Integer; AValue: T);
Begin
  Assert(index >= 0);
  Assert(index < fDataCount);
  fData[index] := AValue;
End;

Function TStableIndexVector.getCapacity: integer;
Begin
  result := length(fData);
End;

Function TStableIndexVector.getDataElement(index: Integer): T;
Begin
  Assert(index >= 0);
  Assert(index < fDataCount);
  result := fData[index];
End;

Procedure TStableIndexVector.setElement(ID: TID; AValue: T);
Begin
  Assert(id >= 0);
  Assert(id < length(fID_To_Data));
  Assert(fID_To_Data[ID] < fDataCount);
  fData[fID_To_Data[id]] := AValue;
End;

Function TStableIndexVector.Add(aValue: T): TID;
Var
  len, i: Integer;
Begin
  // Der Platz ist ausgegangen -> Neu Allokieren
  If fDataCount >= high(fData) Then Begin
    len := length(fData);
    setlength(fdata, len + fBlockSize);
    setlength(fID_To_Data, len + fBlockSize);
    setlength(fData_To_ID, len + fBlockSize);
    // Alle neuen Plätze müssen als 1:1 Mapping initialisiert werden
    For i := len To high(fData) Do Begin
      fID_To_Data[i] := i;
      fData_To_ID[i] := i;
    End;
  End;
  // Abspeichern in der Struktur und Rückgabe der ID
  fData[fDataCount] := aValue;
  result := fData_To_ID[fDataCount];
  inc(fDataCount);
End;

Function TStableIndexVector.Del(ID: TID): Boolean;
Var
  tmpI, Data_Index, LastDataIndex: Integer;
  tmpT: T;
Begin
  Assert(iD >= 0);
  Assert(iD < length(fID_To_Data));
  result := false;
  // Get Aktual Element Indes
  Data_Index := fID_To_Data[Id];
  // We are trying to delete a already deleted ID -> exit with no success
  If Data_Index >= fDataCount Then exit;
  // Move Element to last Position
  LastDataIndex := fDataCount - 1;
  tmpT := fData[LastDataIndex];
  fData[LastDataIndex] := fData[Data_Index];
  fData[Data_Index] := tmpT;
  // Also update fData_To_ID
  tmpI := fData_To_ID[LastDataIndex];
  fData_To_ID[LastDataIndex] := fData_To_ID[Data_Index];
  fData_To_ID[Data_Index] := tmpI;
  // and fID_To_Data
  fID_To_Data[fData_To_ID[LastDataIndex]] := LastDataIndex;
  fID_To_Data[fData_To_ID[Data_Index]] := Data_Index;
  // Remove Last Element
  dec(fDataCount);
  result := true;
End;

Function TStableIndexVector.isValid(ID: TID): Boolean;
Begin
  result := false;
  If (id < 0) Or
    (id > high(fID_To_Data)) Or
    (fID_To_Data[id] >= fDataCount) Then exit;
  result := true;
End;

Function TStableIndexVector.IndexToID(aIndex: Integer; Out ID: TID): Boolean;
Begin
  result := false;
  If (aIndex < 0) Or
    (aIndex >= fDataCount) Then exit;
  ID := fData_To_ID[aIndex];
  result := true;
End;

Procedure TStableIndexVector.Clear;
Var
  i: Integer;
Begin
  fDataCount := 0;
  For i := 0 To high(fData) Do Begin
    fData_To_ID[i] := i;
    fID_To_Data[i] := i;
  End;
End;

Function TStableIndexVector.IterReset(Out aIterator: PT): Boolean;
Begin
  result := fDataCount > 0;
  fIteratorIndex := fDataCount - 1;
  aIterator := Nil;
  If result Then Begin
    aIterator := @fData[fIteratorIndex];
  End
End;

Function TStableIndexVector.IterNext(Out aIterator: PT): Boolean;
Begin
  fIteratorIndex := fIteratorIndex - 1;
  result := fIteratorIndex >= 0;
  If result Then Begin
    aIterator := @fData[fIteratorIndex];
  End;
End;

Procedure TStableIndexVector.IterAll(aIteratorCallback: TIterAllEvent);
Var
  i: Integer;
Begin
  For i := 0 To fDataCount - 1 Do Begin
    aIteratorCallback(self, fData[i]);
  End;
End;

End.

