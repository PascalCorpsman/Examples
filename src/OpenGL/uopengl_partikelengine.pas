(******************************************************************************)
(* uopengl_partikelengine                                          14.02.2013 *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Implementation of a simple and more complex particle engine  *)
(*               for use in OpenGL applications.                              *)
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
(*               0.02 - Added TExtendedPartikelEngine                         *)
(*                                                                            *)
(******************************************************************************)
Unit uopengl_partikelengine;

{$MODE objFPC}{$H+}

Interface

(*
 * If you get a compiler error with missing file
 * just create a file namend "uopengl_partikelengine.inc" in your project folder and
 * insert the following content:
 *
 * ---------- Content of file ----------

 // Activate/ Deactivate Extended mode

 {.$DEFINE USE_EXTENDED_PARTICLE_ENGINE}
   ---------- End content of file ----------
 *)

{$I uopengl_partikelengine.inc} // Einbinden der diversen Steuerdefines

Uses classes,
  uvectormath,
  dglOpenGL,
  sysutils,
  lclintf,
{$IFDEF USE_EXTENDED_PARTICLE_ENGINE}
  ugenmathcalc,
  utokenizer,
{$ENDIF}
  uopengl_graphikengine;

Type

  SimulationMode = (smStop, smStep, smContinual, smStopEmit);

  TPartikel = Record
    Position: Tvector3; // Die Aktuelle Position
    Speed: Tvector3; // Die Aktuelle Geschwindigkeit des Partikels
    Livetime: Single; // Zählt runter bis 0 dann stirbt das Partikel ( in ms )
    Age: Single; // Zählt hoch ( in ms )
    Color: TVector4; // Der Alpha Kanal ist wichtig , sonst gibts kein Blending
    tex: integer; // Wenn das Partikel keine Punktwolke sein soll, dann kann eine Textur angegeben werden
    Size: Tvector2; // Im Fall der Textur, kann hier die x,y Ausdehnung angegeben werden
  End;

  PPartikelList = ^TPartikelList;

  TPartikelList = Record
    Item: TPartikel;
    Next: PPartikelList;
  End;

  TForceAndLiveCallback = Procedure(Var Partikel: TPartikel; dt: Double) Of Object;

  { TOpenGLPartikelengine }

  TOpenGLPartikelengine = Class(TThread) // Attention you need to activate Threads in Linux Applications.
  protected
    Erstes, Letztes: PPartikelList;
    FLastTimeStep: QWord;
    FMode: SimulationMode;
    fdt: double;
    cnt: Integer;
    FForceAndLiveCallback: TForceAndLiveCallback; // Wird nach dem Reinrechnen der Gravity Aufgerufen,  Livetime <= 0 Bringt Partikel Um
    FEmissionTime: Dword;
    up, right: TVector3;
    Function FGetParticleCount: Integer;
    Procedure FStep(dt: Double);
    Procedure OnExecute; virtual;
    Procedure EnterBillBoardMode;
    Procedure LeaveBillBoardMode;
    Procedure RenderBillboard(Position: TVector3; Size: Tvector2; Tex: integer);
  public
    Gravity: Tvector3;
    PointSize: integer;
    Property ForceAndLiveCallback: TForceAndLiveCallback read FForceAndLiveCallback write FForceAndLiveCallback;
    Property Mode: SimulationMode read Fmode;
    Property Count: Integer read FGetParticleCount;
    Constructor Create;
    Destructor destroy; override;
    Procedure Execute; override;
    Procedure Render; // Rendert alle Partikel
    Procedure AddPartikel(Const Value: TPartikel); // Fügt einen Partikel Hinzu
    Procedure Reset; // Setzt den Zeitstempel Intern zurück
    Procedure Start; virtual;
    Procedure Stop; virtual;
    Procedure Clear; // Löscht alle Partikel die es gibt
    // Zeit in MS
    Procedure Step(dt: Double); // Muss eigentlich nicht aufgerufen werden, man kann anstatt Start und Stop auch selbst "schrittweise" simulieren.
  End;

{$IFDEF USE_EXTENDED_PARTICLE_ENGINE}

  { TExtendedPartikelEngine }

  TExtendedPartikelEngine = Class(TOpenGLPartikelengine) // Attention you need to activate Threads in Linux Applications.
  private
    fTimeStamp: QWord;
    FFormulaX: PCalcTree;
    FFormulaY: PCalcTree;
    FFormulaZ: PCalcTree;
    FFormulaR: PCalcTree;
    FFormulaG: PCalcTree;
    FFormulaB: PCalcTree;
    FFormulaW: PCalcTree;
    fTokenizer: TTokenizer;
    fCalculater: TGenMathCalc;
    FLiveTime: Single;
    FAge: Single; // Angabe in ms
    FVars: TVarlist;
    fForceAndLiveCallback2: TForceAndLiveCallback;
    Procedure FSetLifetime(Value: integer);
    Function fgetlifetime(): integer;
    Procedure ForceAndLifeCallback_(Var Partikel: TPartikel; dt: Double);
    Procedure CreateParticle();
  public
    Emitrate: integer; // Alle x ms soll eine neue Welle Emitiert werden
    ParticlesPerEmit: integer; // Die Anzahl an Partikel Pro Emitierung
    StartPos: TVector3; // Der Punkt an dem Alle Partikel erzeugt werden
    Size: TVector2; // Größe in OpenGL Coodrinaten ( gillt nur für Texture <> 0 )
    Texture: Integer; // Wenn 0 dann ist der partikel ein Punkt
    Property ForceAndLiveCallback: TForceAndLiveCallback read fForceAndLiveCallback2 write fForceAndLiveCallback2;
    Property Lifetime: integer read fgetLifetime write fsetlifetime; // Angabe in ms
    (*
     * Als Formeln Zulässig sind
     * /, *, +, - = Bindungsstärke von Links nach Rechts, Runde klammern Erlaubt
     * random     = Zufall [0..1[  ( Randomize nicht vergessen )
     * sin, cos   = in Gradmaß
     * age        = Alter des Partikels in ms
     * lifetime   = Die "Lebensdauer" des Partikels in ms
     *)
    Procedure SetDirectionFormula(x, y, z: String);
    Procedure SetColorFormula(r, g, b, w: String);
    Constructor create;
    Destructor destroy; override;
    Procedure Start; override; // Startet das Emitieren
    Procedure Stop; override; // Hört auf zu Emitieren
    Procedure Pause; // Versetzt die Engine in einen Schlafzustand
    Procedure Execute; override; // Was der Thread so ausführt
  End;
{$ENDIF}

Implementation

{$IFDEF USE_EXTENDED_PARTICLE_ENGINE}

{ TExtendedPartikelEngine }

Function Add_(value1, Value2: Pointer): Pointer;
Var
  res: ^single;
Begin
  new(res);
  res^ := single(value1^) + single(value2^);
  result := res;
End;

Function sub_(value1, Value2: Pointer): Pointer;
Var
  res: ^single;
Begin
  new(res);
  res^ := single(value1^) - single(value2^);
  result := res;
End;

Function mul_(value1, Value2: Pointer): Pointer;
Var
  res: ^single;
Begin
  new(res);
  res^ := single(value1^) * single(value2^);
  result := res;
End;

Function div_(value1, Value2: Pointer): Pointer;
Var
  res: ^single;
Begin
  new(res);
  If single(value2^) <> 0 Then Begin
    res^ := single(value1^) / single(value2^);
  End
  Else Begin
    res^ := 0;
  End;
  result := res;
End;

Function sin_(value: Pointer): Pointer;
Var
  res: ^single;
Begin
  new(res);
  res^ := sin(single(value^) * pi / 180);
  result := res;
End;

Function cos_(value: Pointer): Pointer;
Var
  res: ^single;
Begin
  new(res);
  res^ := cos(single(value^) * pi / 180);
  result := res;
End;

Function uminus_(value: Pointer): Pointer;
Var
  res: ^single;
Begin
  new(res);
  res^ := -single(value^);
  result := res;
End;

Function rnd(): Pointer;
Var
  res: ^single;
Begin
  new(res);
  res^ := random(); // Randomize beim Programmstart nicht vergessen !!
  result := res;
End;

Function OnCreateProc(Value: String): Pointer;
Var
  res: ^single;
Begin
  new(res);
  res^ := strtofloat(value);
  result := res;
End;

Procedure OnFreeProc(value: Pointer);
Var
  v: ^single;
Begin
  v := value;
  dispose(v);
End;

Procedure TExtendedPartikelEngine.FSetLifetime(Value: integer);
Begin
  FLiveTime := value; // Intern ist alles in Single, wegen GenMathCalc, deswegen hier der Wrapper
End;

Function TExtendedPartikelEngine.fgetlifetime: integer;
Begin
  result := round(FLiveTime); // Intern ist alles in Single, wegen GenMathCalc, deswegen hier der Wrapper
End;

Procedure TExtendedPartikelEngine.ForceAndLifeCallback_(Var Partikel: TPartikel;
  dt: Double);
Var
  s: ^Single;
  b: Boolean;
Begin
  // Für jedes Partikel die Farbe nach Berechnen
  FAge := partikel.Age;
  s := fCalculater.Calc(FFormulaW, b);
  Partikel.Color.w := s^;
  If Not b Then dispose(s);
  // Falls der User auch noch was machen will
  If assigned(fForceAndLiveCallback2) Then Begin
    fForceAndLiveCallback2(Partikel, dt);
  End;
End;

Procedure TExtendedPartikelEngine.CreateParticle;
Var
  p: TPartikel;
  s: ^single;
  b: Boolean;
Begin
  FAge := 0; // Falls jemand das in die Formel einbaut, bei der Geburt ist das Alter natürlich 0
  p.Position := StartPos;
  s := fCalculater.Calc(FFormulaX, b);
  p.Speed.x := s^;
  If Not b Then dispose(s);
  s := fCalculater.Calc(FFormulay, b);
  p.Speed.y := s^;
  If Not b Then dispose(s);
  s := fCalculater.Calc(FFormulaz, b);
  p.Speed.z := s^;
  If Not b Then dispose(s);
  p.Livetime := FLiveTime;
  p.age := 0;
  s := fCalculater.Calc(FFormulaR, b);
  p.Color.x := s^;
  If Not b Then dispose(s);
  s := fCalculater.Calc(FFormulaG, b);
  p.Color.y := s^;
  If Not b Then dispose(s);
  s := fCalculater.Calc(FFormulaB, b);
  p.Color.z := s^;
  If Not b Then dispose(s);
  s := fCalculater.Calc(FFormulaW, b);
  p.Color.w := s^;
  If Not b Then dispose(s);
  p.tex := Texture;
  p.Size := Size;
  AddPartikel(p); // Rein mit dem Partikel *g*
End;

Procedure TExtendedPartikelEngine.SetDirectionFormula(x, y, z: String);
Var
  tl: TTokenarray;
  mode_: SimulationMode;
Begin
  mode_ := FMode;
  fmode := smStop;
  If mode_ <> smStop Then
    sleep(20); // Warten so dass der Thread anhalten kann, ob das so auch richtig ist ??
  If assigned(FFormulaX) Then fCalculater.FreeCalcTree(FFormulaX);
  If assigned(FFormulay) Then fCalculater.FreeCalcTree(FFormulay);
  If assigned(FFormulaz) Then fCalculater.FreeCalcTree(FFormulaz);
  tl := fTokenizer.Scan(x);
  FFormulaX := fCalculater.Parse(tl, FVars);
  setlength(tl, 0);
  tl := fTokenizer.Scan(y);
  FFormulay := fCalculater.Parse(tl, FVars);
  setlength(tl, 0);
  tl := fTokenizer.Scan(z);
  FFormulaz := fCalculater.Parse(tl, FVars);
  setlength(tl, 0);
  fmode := mode_;
End;

Procedure TExtendedPartikelEngine.SetColorFormula(r, g, b, w: String);
Var
  tl: TTokenarray;
  mode_: SimulationMode;
Begin
  mode_ := FMode;
  fmode := smStop;
  If mode_ <> smStop Then
    sleep(20); // Warten so dass der Thread anhalten kann, ob das so auch richtig ist ??
  If assigned(FFormular) Then fCalculater.FreeCalcTree(FFormular);
  If assigned(FFormulag) Then fCalculater.FreeCalcTree(FFormulag);
  If assigned(FFormulab) Then fCalculater.FreeCalcTree(FFormulab);
  If assigned(FFormulaw) Then fCalculater.FreeCalcTree(FFormulaw);
  tl := fTokenizer.Scan(r);
  FFormular := fCalculater.Parse(tl, FVars);
  setlength(tl, 0);
  tl := fTokenizer.Scan(g);
  FFormulag := fCalculater.Parse(tl, FVars);
  setlength(tl, 0);
  tl := fTokenizer.Scan(b);
  FFormulab := fCalculater.Parse(tl, FVars);
  setlength(tl, 0);
  tl := fTokenizer.Scan(w);
  FFormulaw := fCalculater.Parse(tl, FVars);
  setlength(tl, 0);
  fmode := mode_;
End;

Constructor TExtendedPartikelEngine.create;
Begin
  Inherited create;
  Texture := 0;
  Emitrate := 20;
  ParticlesPerEmit := 1;
  Gravity := v3(0, -9.8, 0); // Die Gibt es so nicht, da die Richtung ja da ist
  FFormulaX := Nil;
  FFormulaY := Nil;
  FFormulaZ := Nil;
  FFormulaR := Nil;
  FFormulaG := Nil;
  FFormulaB := Nil;
  FFormulaW := Nil;
  fCalculater := TGenMathCalc.Create;
  fCalculater.OnCreateValue := @OnCreateProc;
  fCalculater.OnFreeValue := @OnFreeProc;
  fCalculater.AddUnOP('-', @uminus_);
  fCalculater.AddUnOP('sin', @sin_);
  fCalculater.AddUnOP('cos', @cos_);
  fCalculater.AddBinOP('/', @div_);
  fCalculater.AddBinOP('*', @mul_);
  fCalculater.AddBinOP('+', @add_);
  fCalculater.AddBinOP('-', @sub_);
  setlength(FVars, 3);
  FVars[0].Callback := true;
  FVars[0].Value := @rnd;
  FVars[0].Name := 'random';
  FVars[1].Callback := false;
  FVars[1].Value := @FLiveTime;
  FVars[1].Name := 'lifetime';
  FVars[2].Callback := false;
  FVars[2].Value := @FAge;
  FVars[2].Name := 'age';
  fTokenizer := TTokenizer.Create;
  fTokenizer.AddSeperator(' ');
  fTokenizer.AddOperator('+');
  fTokenizer.AddOperator('-');
  fTokenizer.AddOperator('*');
  fTokenizer.AddOperator('/');
  fTokenizer.AddOperator('(');
  fTokenizer.AddOperator(')');
  Inherited ForceAndLiveCallback := @ForceAndLifeCallback_;
End;

Destructor TExtendedPartikelEngine.destroy;
Begin
  fCalculater.FreeCalcTree(FFormulaX);
  fCalculater.FreeCalcTree(FFormulaY);
  fCalculater.FreeCalcTree(FFormulaZ);
  fCalculater.FreeCalcTree(FFormulaR);
  fCalculater.FreeCalcTree(FFormulaG);
  fCalculater.FreeCalcTree(FFormulaB);
  fCalculater.FreeCalcTree(FFormulaW);
  fCalculater.Free;
  fTokenizer.free;
  setlength(fvars, 0);
  Inherited destroy;
End;

Procedure TExtendedPartikelEngine.Start;
Begin
  If FMode = smStop Then Begin // wenn dat ding schon läuft brauchts nich noch mal gestartet werden.
    CreateParticle(); // Da der Start nur Geht wenn es Mindestens einen Partikel Gibt muss dieser auch Erzeugt werden !
    fTimeStamp := GetTickCount64;
    FEmissionTime := 0;
    Inherited start;
  End;
End;

Procedure TExtendedPartikelEngine.Stop;
Begin
  If (FMode = smContinual) Or (FMode = smStep) Then
    FMode := smStopEmit;
End;

Procedure TExtendedPartikelEngine.Pause;
Begin
  Inherited stop;
End;

Procedure TExtendedPartikelEngine.Execute;
Var
  i: Integer;
Begin
  While Not Terminated Do Begin
    (* Hier die Logik zum Emitieren *)
    If (fmode = smContinual) Then Begin // Achtung, wenn gar nichts emmitiert wird, liegt das daran, dass Zwischenzeitlich kein einziges Partikel mehr lebt.
      FEmissionTime := FEmissionTime + (GetTickCount64 - fTimeStamp);
      fTimeStamp := GetTickCount64;
      While FEmissionTime > Emitrate Do Begin
        For i := 0 To ParticlesPerEmit - 1 Do Begin
          CreateParticle();
        End;
        FEmissionTime := FEmissionTime - Emitrate;
      End;
    End
    Else Begin
      // Wenn wir nicht im Continual Mode sind und es keine Partikel mehr gibt
      If (Count = 0) Then Inherited stop; // Anhalten
    End;
    // Bewegen der Partikel
    OnExecute;
  End;
End;

{$ENDIF}

{ TPartikelengine }

Constructor TOpenGLPartikelengine.Create;
Begin
{$IFDEF Windows}
  Inherited create(false, 1024);
{$ELSE}
  Inherited create(false);
{$ENDIF}
  cnt := 0;
  Erstes := Nil;
  Letztes := Nil;
  ForceAndLiveCallback := Nil;
  FreeOnTerminate := True;
  FLastTimeStep := GetTickCount64;
  FMode := smStop;
  Gravity := v3(0, -9.8, 0);
  PointSize := 2;
End;

Destructor TOpenGLPartikelengine.destroy;
Var
  p1, p2: PPartikelList;
Begin
  p1 := erstes;
  While p1 <> Nil Do Begin
    p2 := p1;
    p1 := p1^.Next;
    dispose(p2);
  End;
End;

Procedure TOpenGLPartikelengine.AddPartikel(Const Value: TPartikel);
Var
  p: PPartikelList;
Begin
  If erstes = Nil Then Begin
    new(erstes);
    Letztes := erstes;
    erstes^.item := value;
    erstes^.Next := Nil;
  End
  Else Begin
    new(p);
    p^.item := value;
    p^.next := Nil;
    letztes^.next := p;
    letztes := letztes^.next;
  End;
  inc(cnt);
End;

Procedure TOpenGLPartikelengine.Execute;
Begin
  While Not Terminated Do Begin
    OnExecute; // Eine Kindklasse kann leider nicht so einfach ein inherited Execute aufrufen, deswegen ist der Execute Code entsprechend ausgelagert.
  End;
End;

Procedure TOpenGLPartikelengine.Render;
(*
 * Wenn man die Render Routine Aufruft und keine Partikel sehen kann, dann liegt
 * dies sicherlich daran dass der Tiefenbuffer zwar getestet aber nicht
 * geschrieben wird ( das muss so sein sonst kann man kein Alphablending mit
 * der Textur machen).
 *
 * Einzige Lösung des Problems, ist dass man die PartikelEngine als allerletztes
 * Rendern läst. Da alles nachfolgende Tendenziell die partikel wieder
 * "Übermalt".
 *)
Var
  lighting, blending: Boolean;
  p: PPartikelList;
  depthwritable: integer;
Begin
  lighting := glIsEnabled(GL_LIGHTING);
  If lighting Then Begin
    glDisable(GL_LIGHTING);
  End;
  blending := glIsEnabled(gl_blend);
  If Not blending Then Begin
    glcolor4f(1, 1, 1, 1);
    glEnable(GL_BLEND);
  End;
  // Der Tiefentest muss berücksichtigt werden, aber darf nicht beeinflußt werden.
  glGetIntegerv(GL_DEPTH_WRITEMASK, @depthwritable);
  If depthwritable <> 0 Then
    glDepthMask(FALSE);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  p := Erstes;
  If assigned(Erstes) Then Begin // Wenn das 1. eine Textur hat, dann haben es alle !!
    If Erstes^.Item.tex <> 0 Then Begin
      EnterBillBoardMode();
    End
    Else Begin
      glPointSize(PointSize);
      glBindTexture(GL_TEXTURE_2D, 0);
      glbegin(gl_points);
    End;
  End;
  While p <> Nil Do Begin
    glColor4fv(@p^.Item.color);
    If p^.Item.tex = 0 Then Begin
      glvertex3fv(@p^.Item.Position);
    End
    Else Begin
      RenderBillboard(p^.Item.Position, p^.Item.Size, p^.Item.tex);
    End;
    p := p^.Next;
  End;
  If assigned(Erstes) Then Begin
    If Erstes^.Item.tex <> 0 Then Begin
      LeaveBillBoardMode();
    End
    Else Begin
      glend;
    End;
  End;
  If depthwritable <> 0 Then
    glDepthMask(TRUE);
  If Not blending Then
    gldisable(GL_BLEND);
  If lighting Then Begin
    glenable(GL_LIGHTING);
  End;
End;

Procedure TOpenGLPartikelengine.Reset;
Begin
  FLastTimeStep := GetTickCount64;
End;

Procedure TOpenGLPartikelengine.FStep(dt: Double);
Var
  v, p, t: PPartikelList;
  dtt: Double;
Begin
  dtt := dt;
  dt := dt / 1000;
  p := Erstes;
  v := Erstes;
  While p <> Nil Do Begin
    // Die Erdbeschleunigung die sich auf Speed auswirkt
    // Speed( t + dt ) := dt /2 * Gravity + Speed( t )
    p^.Item.Speed := addv3(ScaleV3(dt {/ 2}, Gravity), p^.Item.Speed);
    // Pos( t + dt ) := dt * speed( t + dt ) + Pos( t )
    p^.Item.Position := addv3(p^.Item.Position, ScaleV3(dt, p^.Item.Speed));
    p^.Item.Livetime := p^.Item.Livetime - dtt;
    p^.Item.Age := p^.Item.Age + dtt;
    // Falls da noch mehr wie nur Gravity Rein soll, dann kann über diese Callback noch Nachträglich darauf einfluss genommen werden.
    If (p^.item.Livetime > 0) And assigned(ForceAndLiveCallback) Then
      ForceAndLiveCallback(p^.item, dt);
    // Ist die Livetime vorbei dann stirbt der Partikel
    If (p^.Item.Livetime <= 0) Then Begin
      If p = Erstes Then Begin
        t := erstes;
        erstes := erstes^.next;
        dispose(t);
        p := erstes;
      End
      Else Begin
        t := p;
        v^.Next := p^.next;
        If p = Letztes Then Begin
          Letztes := v;
          v^.next := Nil;
        End;
        p := p^.next;
        dispose(t);
      End;
      dec(cnt);
      If erstes = Nil Then FMode := smStop;
    End
    Else Begin
      v := p;
      p := p^.next;
    End;
  End;
End;

Procedure TOpenGLPartikelengine.OnExecute;
Var
  tmp: Double;
  t: qWord;
Begin
  Case Fmode Of
    smStop: Begin
        // Damit der Thread nicht unnötig CPU zeit verschwendet
        sleep(1);
      End;
    smStep: Begin
        // Wir Ziehen die Delta zeit so lange von fdt ab, bis wir den "Step" follständig gemacht haben
        // Dabei Rechnen wir alles in MS
        t := GetTickCount64;
        tmp := ((t - FLastTimeStep));
        FLastTimeStep := t;
        fdt := fdt - tmp;
        If fdt >= 0 Then Begin
          Fstep(tmp);
        End
        Else
          FMode := smStop;
        sleep(1);
      End;
    smStopEmit,
      smContinual: Begin
        // Wir Simulieren immer Mindestens eine MS
        t := GetTickCount64;
        tmp := ((t - FLastTimeStep));
        FLastTimeStep := t;
        fstep(tmp);
        sleep(1);
      End;
  End;
End;

Procedure TOpenGLPartikelengine.EnterBillBoardMode;
Var
  Matrix: TMatrix4x4;
Begin
  (*
   * Dieser Code muss theoretisch nur ein mal gerechnet werden ( so lange sich die Augposition nicht ändert )
   *)
  glGetFloatv(GL_MODELVIEW_MATRIX, @Matrix[0, 0]);
  // Auslesen des Right und Up Vektors ( das geht nur wenn man annimmt das die ModelView Matrix Orthogonal ist )
  Right := V3(Matrix[0, 0], Matrix[1, 0], Matrix[2, 0]);
  Up := V3(Matrix[0, 1], Matrix[1, 1], Matrix[2, 1]);
  // Das Billboard auf die gewünschte Größe Skalieren
  (*
   * Mit unterschiedlichen Positionen/ Dimensionen könnten nun mehrere Billboards gerendert werden.
   *)
  right := NormV3(right);
  up := NormV3(up);
End;

Procedure TOpenGLPartikelengine.LeaveBillBoardMode;
Begin
  // Es gibt nichts zum "Verlassen"
End;

Procedure TOpenGLPartikelengine.RenderBillboard(Position: TVector3; Size: Tvector2; Tex: integer);
Var
  rright, uup: TVector3;
Begin
  rright := right * (size.x / 2);
  uup := up * (size.y / 2);
  //Der eigentliche Renderschritt
  glBindTexture(GL_TEXTURE_2D, tex);
  glbegin(GL_QUADS);
  glTexCoord2d(1, 1);
  glVertex3f(Position.x + rRight.x + uUp.x, Position.y + rRight.y + uUp.y, Position.z + rRight.z + uUp.z);
  glTexCoord2d(0, 1);
  glVertex3f(Position.x - rRight.x + uUp.x, Position.y - rRight.y + uUp.y, Position.z - rRight.z + uUp.z);
  glTexCoord2d(0, 0);
  glVertex3f(Position.x - rRight.x - uUp.x, Position.y - rRight.y - uUp.y, Position.z - rRight.z - uUp.z);
  glTexCoord2d(1, 0);
  glVertex3f(Position.x + rRight.x - uUp.x, Position.y + rRight.y - uUp.y, Position.z + rRight.z - uUp.z);
  glend();
End;

Procedure TOpenGLPartikelengine.Start;
Begin
  If (Fmode = smstop) And assigned(erstes) Then Begin
    FLastTimeStep := Gettickcount64;
    fmode := smContinual;
  End;
End;

Procedure TOpenGLPartikelengine.Stop;
Begin
  fmode := smStop;
End;

Procedure TOpenGLPartikelengine.Clear;
Var
  p1, p2: PPartikelList;
Begin
  FMode := smStop;
  p1 := erstes;
  While p1 <> Nil Do Begin
    p2 := p1;
    p1 := p1^.Next;
    dispose(p2);
  End;
  erstes := Nil;
  Letztes := Nil;
  cnt := 0;
End;

Procedure TOpenGLPartikelengine.Step(dt: Double);
Begin
  If (Fmode = smstop) And assigned(erstes) Then Begin
    FLastTimeStep := Gettickcount64;
    fdt := dt;
    fmode := smStep;
  End;
End;

Function TOpenGLPartikelengine.FGetParticleCount: Integer;
Begin
  result := cnt;
End;

End.

