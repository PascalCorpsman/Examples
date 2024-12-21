(******************************************************************************)
(* uneuralnetwork.pas                                              ??.??.2022 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Implementation of a multi layer neural network with bias     *)
(*               neurons.                                                     *)
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
Unit uneuralnetwork;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, uvectormath;

Type

  { TNeuralNetwork }

  TNeuralNetwork = Class
  private
    // -- Alles was gespeichert wird
    flayers: Array Of TMatrixNxM;
    fBias: Array Of TMatrixNxM;
    finputDim, fOutputDim: integer;
    fRecentAverageError: Single;
    // -- Arbeitsvariablen
    fModified: Boolean;
  public
    LearnRate: Single;
    Property Modified: Boolean read fModified;
    Constructor Create(Layers: Array Of Integer); // z.B. [2,4,1] -> 2 Eingang, 4 hidden, 1 Output
    Function Predict(input: TVectorN): TVectorN;
    Procedure Train(input, targets: TVectorN);
    Function getRecentAverageError(): Single;
    Function SaveToFile(Const Filename: String): Boolean;
    Function LoadFromFile(Const Filename: String): Boolean;
    Function Info(): String;
  End;

Implementation

Function Sigmoid(x: Single): Single;
Begin
  result := 1 / (1 + exp(-x));
End;

Function DerivSigmoid(x: Single): Single;
Var
  s: Single;
Begin
  //s := Sigmoid(x); // Da der Übergabe parameter bereits via Sigmoid bearbeitet wurde, kann das hier weg gelassen werden
  s := x;
  result := s * (1 - s);
End;

{ TNeuralNetwork }

Constructor TNeuralNetwork.Create(Layers: Array Of Integer);
Var
  i: Integer;
Begin
  fModified := false;
  LearnRate := 0.1; // Egal hauptsache Definiert, macht nachher eh die Kontrollierende Anwendung
  If length(Layers) < 2 Then Begin
    Raise Exception.Create('Error, you have to select at leas 2 layers.');
  End;
  fRecentAverageError := layers[high(Layers)]; // Wir gehen davon aus, dass jeder Ausgangsknoten Falsch ist !
  // Wir benötigen 1-Schicht weniger als Layers Angefragt sind
  setlength(fLayers, high(Layers));
  setlength(fBias, high(Layers));
  // Erstellen der ganzen Übergangsmatrizen
  For i := 0 To high(Layers) - 1 Do Begin
    flayers[i] := ZeroNxM(Layers[i], Layers[i + 1]);
    fBias[i] := ZeroNxM(1, Layers[i + 1]);
    RandomizeNxM(flayers[i]);
    RandomizeNxM(fBias[i]);
  End;
  // Für die Checks
  finputDim := Layers[0];
  fOutputDim := layers[high(layers)];
End;

Function TNeuralNetwork.Predict(input: TVectorN): TVectorN;
Var
  v: TMatrixNxM;
  i: Integer;
Begin
  If length(input) <> finputDim Then Begin
    Raise exception.Create('Error, input has invalid size.');
  End;
  // Input Conversion
  v := VNToNxM(input);
  // FeedForward
  For i := 0 To high(flayers) Do Begin
    v := flayers[i] * v;
    v := v + fBias[i];
    MapMatrix(v, @Sigmoid);
  End;
  // Output Conversion
  result := NxMToVN(v);
End;

Procedure TNeuralNetwork.Train(input, targets: TVectorN);
Var
  v: Array Of TMatrixNxM;
  i: Integer;
  delta, g, e: TMatrixNxM;
Begin
  If length(input) <> finputDim Then Begin
    Raise exception.Create('Error, input has invalid size.');
  End;
  If length(targets) <> fOutputDim Then Begin
    Raise exception.Create('Error, target has invalid size.');
  End;
  fModified := true;
  // 1. Feed Forward
  setlength(v, length(flayers) + 1);
  // Input Conversion
  v[0] := VNToNxM(input);
  // FeedForward
  For i := 0 To high(flayers) Do Begin
    v[i + 1] := flayers[i] * v[i];
    v[i + 1] := v[i + 1] + fBias[i];
    MapMatrix(v[i + 1], @Sigmoid);
  End;
  // Output stands in v[length(flayers)]
  // 2. Back Propagation
  // Calculate Error of Output
  e := VNToNxM(targets) - v[length(flayers)];
  fRecentAverageError := (fRecentAverageError + LenVN(targets - NxMToVN(v[length(flayers)]))) / 2; // Schleifender Mittelwert
  // Propagate through the layers
  For i := high(flayers) Downto 0 Do Begin
    // Calculate the Gradient
    g := MapMatrix2(v[i + 1], @DerivSigmoid);
    g := HadamardNxM(g, e);
    g := LearnRate * g;

    delta := g * TransposeMatrix(v[i]);

    // Adjust Weights and bias
    flayers[i] := flayers[i] + delta;
    fBias[i] := fBias[i] + g;

    // Calculate Error for next layer
    If i <> 0 Then Begin
      // i = 0 would calculate the Error from the input, this is not needed
      // anymore => not calculate it to preserve compution time
      e := TransposeMatrix(flayers[i]) * e;
    End;
  End;
End;

Function TNeuralNetwork.getRecentAverageError(): Single;
Begin
  result := fRecentAverageError;
End;

Function TNeuralNetwork.SaveToFile(Const Filename: String): Boolean;
Var
  fs: TFileStream;
  ui: uint32;
Begin
  result := false;
  fs := TFileStream.Create(Filename, fmCreate Or fmOpenWrite);
  // Speichern der Dimension
  fs.Write(finputDim, sizeof(finputDim));
  fs.Write(fOutputDim, sizeof(fOutputDim));
  fs.Write(fRecentAverageError, sizeof(fRecentAverageError));
  // Speichern der Matrizen
  ui := length(flayers);
  fs.Write(ui, sizeof(ui));
  For ui := 0 To high(flayers) Do Begin
    SaveMNxMToStream(fs, flayers[ui]);
  End;
  ui := length(fBias);
  fs.Write(ui, sizeof(ui));
  For ui := 0 To high(fBias) Do Begin
    SaveMNxMToStream(fs, fBias[ui]);
  End;
  fs.free;
End;

Function TNeuralNetwork.LoadFromFile(Const Filename: String): Boolean;
Var
  fs: TFileStream;
  ui: uint32;
  fiDim, fODim: integer;
Begin
  result := false;
  fs := TFileStream.Create(Filename, fmOpenRead);
  // Speichern der Dimension
  fiDim := 0;
  fODim := 0;
  fs.read(fiDim, sizeof(finputDim));
  fs.read(fODim, sizeof(fOutputDim));
  If (finputDim <> fiDim) Or (fOutputDim <> fODim) Then Begin
    fs.free;
    Raise exception.create('Error, the defined net has a different interface, than the net to load.');
    exit;
  End;
  fs.Read(fRecentAverageError, sizeof(fRecentAverageError));
  // Speichern der Matrizen
  ui := 0;
  fs.Read(ui, sizeof(ui));
  setlength(flayers, ui);
  For ui := 0 To high(flayers) Do Begin
    flayers[ui] := LoadMNxMFromStream(fs);
  End;

  ui := 0;
  fs.Read(ui, sizeof(ui));
  setlength(fBias, ui);
  If length(fBias) <> length(flayers) Then Begin
    Raise exception.create('Error, bias and layer dim is different, file invalid.');
    result := false;
    fs.free;
  End;
  For ui := 0 To high(fBias) Do Begin
    fBias[ui] := LoadMNxMFromStream(fs);
  End;
  fs.free;
  result := true;
End;

Function TNeuralNetwork.Info(): String;
Var
  i: Integer;
  ui64: uint64;
Begin
  // Layer Informationen
  result := 'Layers: [';
  ui64 := 0;
  For i := 0 To high(flayers) Do Begin
    result := result + inttostr(length(flayers[i])) + ', ';
    ui64 := ui64 + length(flayers[i]) * length(flayers[i, 0]);
    ui64 := ui64 + length(fBias[i]) * length(fBias[i, 0]);
  End;
  result := result + inttostr(length(flayers[high(flayers), 0])) + ']' + LineEnding;
  result := result + 'Trainable params: ' + IntToStr(ui64);

End;

End.

