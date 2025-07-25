{|----------------------------------------------------------------------
 | Unit:        OObjects
 |
 | Author:      Egbert van Nes
 |
 | Description: Some changed TLists (more compatible with Borland Pascal 7)
 |
 | Copyright (c) 2000  Egbert van Nes
 |   All rights reserved
 |   Disclaimer and licence notes: see license.txt
 |
 |----------------------------------------------------------------------
}
(*
  Modified by Corpsman, to Support Lazarus Linux. 01.09.2009
*)
Unit oobjects;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

Interface

Uses Classes;

Const
  MaxCollectionSize = Maxint Div (SizeOf(Integer) * 2);

Type
  TOCollection = Class(TList)
  public
    Constructor Create(ACapacity: Integer);
    Procedure AtFree(Index: Integer);
    Procedure FreeAll;
    Procedure DoFree(Item: Pointer);
    Procedure FreeItem(Item: Pointer); virtual;
    Destructor Destroy; override;
  End;

  TNoOwnerCollection = Class(TOCollection)
  public
    Procedure FreeItem(Item: Pointer); override;
  End;

  { TSortedCollection object }

  TSortedCollection = Class(TOCollection)
  public
    Duplicates: Boolean;
    Constructor Create(ACapacity: Integer);
    Function Compare(Key1, Key2: Pointer): Integer; virtual; abstract;
    Function IndexOf(Item: Pointer): Integer; virtual;
    Procedure Add(Item: Pointer); virtual;
    Procedure AddReplace(Item: Pointer); virtual;
    {if duplicate then replace the duplicate else add}
    Function KeyOf(Item: Pointer): Pointer; virtual;
    Function Search(Key: Pointer; Var Index: Integer): Boolean; virtual;
  End;

  { TStrCollection object }

  TStrCollection = Class(TSortedCollection)
  public
    Function Compare(Key1, Key2: Pointer): Integer; override;
    Procedure FreeItem(Item: Pointer); override;
  End;

Implementation

Uses SysUtils;

Constructor TOCollection.Create(ACapacity: Integer);
Begin
  Inherited Create;
  SetCapacity(ACapacity);
  {Delta is automatic in TList}
End;

Destructor TOCollection.Destroy;
Begin
  FreeAll;
  Inherited Destroy;
End;

Procedure TOCollection.AtFree(Index: Integer);
Var
  Item: Pointer;
Begin
  Item := Items[Index];
  Delete(Index);
  FreeItem(Item);
End;

Procedure TOCollection.FreeAll;
Var
  I: Integer;
Begin
  Try
    For I := 0 To Count - 1 Do
      FreeItem(Items[I]);
  Finally
    Count := 0;
  End;
End;

Procedure TOCollection.DoFree(Item: Pointer);
Begin
  AtFree(IndexOf(Item));
End;

Procedure TOCollection.FreeItem(Item: Pointer);
Begin
  If (Item <> Nil) Then
    With TObject(Item) As TObject Do
      Free;
End;

{----------------------------------------------------------------virtual;
  Implementing TNoOwnerCollection
  -----------------------------------------------------------------}

Procedure TNoOwnerCollection.FreeItem(Item: Pointer);
Begin
End;

{ TSortedCollection }

Constructor TSortedCollection.Create(ACapacity: Integer);
Begin
  Inherited Create(ACapacity);
  Duplicates := False;
End;

Function TSortedCollection.IndexOf(Item: Pointer): Integer;
Var
  I: Integer;
Begin
  IndexOf := -1;
  If Search(KeyOf(Item), I) Then Begin
    If Duplicates Then
      While (I < Count) And (Item <> Items[I]) Do
        inc(I);
    If I < Count Then IndexOf := I;
  End;
End;

Procedure TSortedCollection.AddReplace(Item: Pointer);
Var
  Index: Integer;
Begin
  If Search(KeyOf(Item), Index) Then
    Delete(Index);
  Add(Item);
End;

Procedure TSortedCollection.Add(Item: Pointer);
Var
  I: Integer;
Begin
  i := 0; // Shutdown Compiler Warning.
  If Not Search(KeyOf(Item), I) Or Duplicates Then
    Insert(I, Item);
End;

Function TSortedCollection.KeyOf(Item: Pointer): Pointer;
Begin
  KeyOf := Item;
End;

Function TSortedCollection.Search(Key: Pointer; Var Index: Integer): Boolean;
Var
  L, H, I, C: Integer;
Begin
  Search := False;
  L := 0;
  H := Count - 1;
  While L <= H Do Begin
    I := (L + H) Shr 1;
    C := Compare(KeyOf(Items[I]), Key);
    If C < 0 Then
      L := I + 1
    Else Begin
      H := I - 1;
      If C = 0 Then Begin
        Search := True;
        If Not Duplicates Then L := I;
      End;
    End;
  End;
  Index := L;
End;

{ TStrCollection }

Function TStrCollection.Compare(Key1, Key2: Pointer): Integer;
Begin
  Compare := StrComp(Key1, Key2);
End;

Procedure TStrCollection.FreeItem(Item: Pointer);
Begin
  StrDispose(Item);
End;

End.

