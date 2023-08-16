(******************************************************************************)
(* uSQL_Helper.pas                                                 01.10.2022 *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Collection of usefull SQLLite3 commands and routines         *)
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
(*               0.02 - Add GetTypeOfTableColumn                              *)
(*               0.03 - Add TableExists                                       *)
(*                                                                            *)
(******************************************************************************)

Unit usqlite_helper;

{$MODE ObjFPC}{$H+}

Interface

Uses
  sqldb, Classes, SysUtils;

Type
  // Source: https://www.sqlite.org/datatype3.html
  TSQLColumnType = (ctNull, ctInteger, ctNumeric, ctReal, ctText, ctBlob);

  (*
   * Startet eine SQLQuery im Anschluss muss diese nur noch ausgewertet werden.
   * Result = false, Fehler
   * Result = True, Auswertung via :
   *
   *  While (Not SQLQuery.EOF) Do Begin
   *    -- Auswertung des jeweiligen Datensatzes
   *    SQLQuery.Next;
   *  End;
   *  SQLQuery.Active := false; // -- Optional beenden des Kommunikationskanals
   *)
Function StartSQLQuery(Const SQLQuery: TSQLQuery; Query: String): Boolean;

(*
 * Using : - Call CommitSQLTransactionQuery as many times you want.
 *         - Write your results to the database by
 *           if assigned(SQLTransaction) the function will automatically commit otherwise you have to call
 *           SQLTransaction.Commit;
 *           by yourself !
 *)
Function CommitSQLTransactionQuery(Const SQLQuery: TSQLQuery; aText: String; SQLTransaction: TSQLTransaction = Nil): Boolean;

Function TableExists(Const SQLQuery: TSQLQuery; TableName: String): Boolean;
Function ColumnExistsInTable(Const SQLQuery: TSQLQuery; ColumnName, TableName: String): Boolean;
Function GetAllColumsFromTable(Const SQLQuery: TSQLQuery; TableName: String): TStringlist;
Function GetPrimkeyFromTable(Const SQLQuery: TSQLQuery; TableName: String): String;
Function GetTypeOfTableColumn(Const SQLQuery: TSQLQuery; ColumnName, TableName: String): TSQLColumnType;

(*
 * Exportiert den Inhalt der Tabelle Tablename als SQLite Statements in Filename
 *)
Function ExportTableContent(Const SQLQuery: TSQLQuery; Const Filename: String; TableName: String): Boolean;

(*
 * Entfernt alle SQL-Kommentare aus einer Query
 *)
Function RemoveCommentFromSQLQuery(Query: String): String;

(*
 * Strings dürfen nicht direkt an SQL Statements übergeben werden, alle Strings müssen vorher
 * umgewandelt werden (entfernen/ Umwandeln unerlaubter Symbole ...)
 *)
Function ToSQLString(Value: String): String;
Function FromSQLString(Value: String): String;

Implementation

Uses DB, Dialogs;

Function ToSQLString(Value: String): String;
Begin
  value := StringReplace(value, '+', '++', [rfReplaceAll]);
  value := StringReplace(value, '-', '+5', [rfReplaceAll]); // -- ist ein Einleitender Kommentar -> das muss verhindert werden
  value := StringReplace(value, ';', '+4', [rfReplaceAll]); // Die Commit Routine reagiert empfindlich auf ";"
  value := StringReplace(value, #13, '+3', [rfReplaceAll]);
  value := StringReplace(value, '''', '+2', [rfReplaceAll]); // ' = Trennzeichen String in SQL => so wird eine SQL-Injection erschwert
  value := StringReplace(value, '"', '+1', [rfReplaceAll]); // " =  Trennzeichen String in SQL => so wird eine SQL-Injection erschwert
  result := StringReplace(value, #10, '+0', [rfReplaceAll]);
End;

Function FromSQLString(Value: String): String;
Var
  i: integer;
Begin
  // Das geht nicht via Stringreplace, da ein String der Form 8+13 nach 8++13
  // übersetzt wird und die Rückübersetzung macht daraus dann 8+"3
  result := '';
  i := 1;
  While i <= length(value) Do Begin
    If value[i] = '+' Then Begin
      Case value[i + 1] Of
        '0': result := result + #10;
        '1': result := result + '"';
        '2': result := result + '''';
        '3': result := result + #13;
        '4': result := result + ';';
        '5': result := result + '-';
      Else
        result := result + value[i + 1]; // Unbekannt
      End;
      inc(i);
    End
    Else Begin
      result := result + value[i];
    End;
    inc(i);
  End;
End;

Function RemoveCommentFromSQLQuery(Query: String): String;
Var
  j, i: integer;
  instring, instring2: Boolean;
Begin
  result := Query + LineEnding; // Falls die Query mit einem Kommentar endet
  instring := false; // " Kommentare
  instring2 := false; // ' Kommentare
  i := 1;
  j := -1;
  While i < length(result) Do Begin
    If j = -1 Then Begin
      If result[i] = '"' Then Begin // Die String Erkennung, darf in Kommentaren nicht getriggert werden.
        instring := Not instring;
      End;
      If result[i] = '''' Then Begin // Die String Erkennung, darf in Kommentaren nicht getriggert werden.
        instring2 := Not instring2;
      End;
      If (Not instring) And (Not instring2) Then Begin
        // Start eines Kommentars
        If (result[i] = '-') And (result[i + 1] = '-') Then Begin
          // Wir haben den Begin eines Kommentars gefunden
          j := i;
        End;
      End;
    End
    Else Begin
      // Der Kommentar endet beim cr oder lf
      If ((result[i] = #13) Or (result[i] = #10)) Then Begin
        delete(result, j, i - j);
        i := j - 1;
        j := -1;
      End;
    End;
    i := i + 1;
  End;
End;

Function StartSQLQuery(Const SQLQuery: TSQLQuery; Query: String): Boolean;
Begin
  Query := RemoveCommentFromSQLQuery(Query);
  result := false;
  // Unnatürlicher Abbruch
  If Not assigned(SQLQuery) Then exit;
  If (Not assigned(SQLQuery.SQLConnection)) Or (Not SQLQuery.SQLConnection.Connected) Then Begin
    ShowMessage('Error, not connected.');
    exit;
  End;
  If trim(query) = '' Then exit;
  SQLQuery.Active := false;
  SQLQuery.SQL.Clear;
  SQLQuery.SQL.Text := Query;
  Try
    SQLQuery.Open;
  Except
    On e: Exception Do Begin
      ShowMessage('Error invalid query.' + LineEnding + 'Errormessage:' + LineEnding + e.Message);
      exit;
    End;
  End;
  result := true;
End;

Function CommitSQLTransactionQuery(Const SQLQuery: TSQLQuery; aText: String;
  SQLTransaction: TSQLTransaction): Boolean;
Var
  t: String;
  i: integer;
  instring, instring2: Boolean;
Begin
  result := false;
  //Unnatürlicher Abbruch
  If Not assigned(SQLQuery) Then exit;
  If (Not assigned(SQLQuery.SQLConnection)) Or (Not SQLQuery.SQLConnection.Connected) Then Begin
    ShowMessage('Error, not connected.');
    exit;
  End;
  If trim(aText) = '' Then exit;
  SQLQuery.Active := false;
  SQLQuery.SQL.Clear;
  // Unterstützung für "Viele" Befehle hintereinander, bereinigen unnötiger Symbole
  aText := RemoveCommentFromSQLQuery(aText);
  aText := trim(aText) + ';';
  instring := false; // " Kommentare
  instring2 := false; // ' Kommentare
  t := '';
  Try
    For i := 1 To length(aText) Do Begin
      If aText[i] = '"' Then Begin // Die String Erkennung
        instring := Not instring;
      End;
      If aText[i] = '''' Then Begin // Die String Erkennung
        instring2 := Not instring2;
      End;
      t := t + aText[i];
      If (Not instring) And (Not instring2) Then Begin
        If (aText[i] = ';') Then Begin
          t := trim(t);
          If (t <> '') And (t <> ';') Then Begin
            result := true; // Mindestens eine Query wurde ausgeführt -> nur dann ist das Ergebnis True
            SQLQuery.SQL.Text := t;
            SQLQuery.ExecSQL;
            If assigned(SQLTransaction) Then Begin
              SQLTransaction.Commit;
            End;
          End;
          t := '';
        End;
      End;
    End;
  Except
    On e: Exception Do Begin
      ShowMessage('Error invalid query.' + LineEnding + 'Errormessage:' + LineEnding + e.Message);
      If assigned(SQLTransaction) Then Begin
        SQLTransaction.Rollback;
      End;
      result := false;
    End;
  End;
  If assigned(SQLTransaction) Then Begin
    SQLQuery.Active := false; // den Kommunikationskanal wieder frei geben, darf aber nur wenn auch commited wurde !
  End;
End;

Function TableExists(Const SQLQuery: TSQLQuery; TableName: String): Boolean;
Begin
  result := false;
  If Not assigned(SQLQuery) Then exit;
  If (Not assigned(SQLQuery.SQLConnection)) Or (Not SQLQuery.SQLConnection.Connected) Then Begin
    ShowMessage('Error, not connected.');
    exit;
  End;
  StartSQLQuery(SQLQuery, 'Select name from sqlite_master where type=''table'' and name=''' + TableName + '''');
  result := Not SQLQuery.EOF;
End;

Function ColumnExistsInTable(Const SQLQuery: TSQLQuery; ColumnName,
  TableName: String): Boolean;
Var
  f: tfield;
Begin
  result := false;
  If Not assigned(SQLQuery) Then exit;
  If (Not assigned(SQLQuery.SQLConnection)) Or (Not SQLQuery.SQLConnection.Connected) Then Begin
    ShowMessage('Error, not connected.');
    exit;
  End;
  StartSQLQuery(SQLQuery, 'Pragma table_info(' + TableName + ')');
  f := SQLQuery.FieldByName('name');
  While (Not SQLQuery.EOF) Do Begin
    If lowercase(f.AsString) = lowercase(ColumnName) Then Begin
      result := true;
      exit;
    End;
    SQLQuery.Next;
  End;
End;

Function GetAllColumsFromTable(Const SQLQuery: TSQLQuery; TableName: String
  ): TStringlist;
Var
  f: TField;
Begin
  result := TStringList.Create;
  If Not assigned(SQLQuery) Then exit;
  If (Not assigned(SQLQuery.SQLConnection)) Or (Not SQLQuery.SQLConnection.Connected) Then Begin
    ShowMessage('Error, not connected.');
    exit;
  End;
  If Not StartSQLQuery(SQLQuery, 'PRAGMA table_info(' + TableName + ');') Then exit;
  f := SQLQuery.FieldByName('name');
  While (Not SQLQuery.EOF) Do Begin
    result.Add(F.AsString);
    SQLQuery.Next;
  End;
End;

Function GetPrimkeyFromTable(Const SQLQuery: TSQLQuery; TableName: String
  ): String;
Var
  pk, nf: TField;
Begin
  result := '';
  If Not assigned(SQLQuery) Then exit;
  If (Not assigned(SQLQuery.SQLConnection)) Or (Not SQLQuery.SQLConnection.Connected) Then Begin
    ShowMessage('Error, not connected.');
    exit;
  End;
  StartSQLQuery(SQLQuery, 'PRAGMA table_info( ' + TableName + ' ); ');
  If SQLQuery.EOF Then exit;
  nf := SQLQuery.FieldByName('Name');
  pk := SQLQuery.FieldByName('PK');
  If (Not assigned(nf)) Or (Not assigned(pk)) Then exit;
  While Not SQLQuery.EOF Do Begin
    If pk.AsString = '1' Then Begin
      result := nf.AsString;
      exit;
    End;
    SQLQuery.Next;
  End;
End;

Function GetTypeOfTableColumn(Const SQLQuery: TSQLQuery; ColumnName,
  TableName: String): TSQLColumnType;

  Function StrtoSQLColumnType(ct: String): TSQLColumnType;
  Begin
    ct := lowercase(trim(ct));
    Case ct Of
      'text': result := ctText;
      'numeric': result := ctNumeric;
      'integer': result := ctInteger;
      'real': result := ctReal;
      'blob': result := ctBlob;
      '': result := ctNull;
    Else Begin
        Raise Exception.Create('StrtoSQLColumnType: unknown type: ' + ct);
      End;
    End;
  End;

Var
  tf, nf: TField;
Begin
  If Not assigned(SQLQuery) Then Begin
    Raise Exception.Create('invalid SQLQuery');
  End;
  // Das geht leider nicht, da es nur funktioniert, wenn die Tabelle wenigstens eine Zeile beinhaltet
  // StartSQLQuery(SQLQuery, 'SELECT typeof(' + ColumnName + ') FROM ' + TableName);

  StartSQLQuery(SQLQuery, 'PRAGMA table_info(' + TableName + ')');
  If SQLQuery.EOF Then Begin
    Raise Exception.Create('No result');
  End;
  nf := SQLQuery.FieldByName('Name');
  tf := SQLQuery.FieldByName('type');
  While Not SQLQuery.EOF Do Begin
    If lowercase(trim(nf.AsString)) = lowercase(trim(ColumnName)) Then Begin
      result := StrtoSQLColumnType(tf.AsString);
      exit;
    End;
    SQLQuery.Next;
  End;
  Raise Exception.Create('Unable to find ' + ColumnName + ' in ' + TableName);
End;

Function ExportTableContent(Const SQLQuery: TSQLQuery; Const Filename: String;
  TableName: String): Boolean;
Var
  data, Prefix: String;
  i: Integer;
  sl, Res: TStringList;
  defformat: TFormatSettings;
Begin
  defformat := DefaultFormatSettings;
  defformat.DecimalSeparator := '.';
  result := false;
  StartSQLQuery(SQLQuery, 'Select * from ' + TableName);
  Prefix := 'INSERT INTO ' + TableName + ' (';
  If SQLQuery.Fields.Count = 0 Then exit; // Die Tabelle ist Leer
  sl := TStringList.Create;
  SQLQuery.Fields.GetFieldNames(sl);
  For i := 0 To sl.Count - 1 Do Begin
    If i <> 0 Then Prefix := Prefix + ', ';
    Prefix := Prefix + sl[i];
  End;
  sl.free;
  Prefix := Prefix + ') VALUES (';
  res := TStringList.Create;
  res.add('--');
  res.add('-- Content of ' + TableName);
  res.add('--');
  res.add('');
  While (Not SQLQuery.EOF) Do Begin
    data := '';
    For i := 0 To SQLQuery.Fields.Count - 1 Do Begin
      If i <> 0 Then data := data + ', ';
      // Fließkomma Zahlen müssen "englisch" exportiert werden !
      If SQLQuery.Fields[i].DataType = ftFloat Then Begin
        data := data + FloatToStr(SQLQuery.Fields[i].AsFloat, defformat);
      End
      Else Begin
        data := data + '''' + SQLQuery.Fields[i].AsString + '''';
      End;
    End;
    res.add(Prefix + data + ');');
    SQLQuery.Next;
  End;
  If res.count = 4 Then Begin
    showmessage('Table ' + TableName + ' has no content, will not be stored.');
  End
  Else Begin
    res.SaveToFile(Filename);
  End;
  result := true;
  res.free;
End;

End.

