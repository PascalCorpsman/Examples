(******************************************************************************)
(* ulibxWiiMote                                                    2021.10.18 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description: This is the FPC Translation of                                *)
(*                                                                            *)
(*             https://github.com/dvdhrm/xwiimote/blob/master/lib/xwiimote.h  *)
(*             Written 2010-2013 by David Herrmann <dh.herrmann@gmail.com>    *)
(*                                                                            *)
(*          ! Attention !                                                     *)
(*            libxwiimote is Linux only.                                      *)
(*                                                                            *)
(* Preconditions:                                                             *)
(*        - sudo apt install xwiimote                                         *)
(*        - sudo apt install libxwiimote-dev                                  *)
(*                                                                            *)
(* Usage: - install driver                                                    *)
(*        - bind WIImote via os                                               *)
(*        - test by using commands                                            *)
(*            xwiishow list                                                   *)
(*            xwiishow 1                                                      *)
(*        - run demo                                                          *)
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
(* Known Issues: Not all yet ported, ...                                      *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*                                                                            *)
(******************************************************************************)
Unit ulibxwiimote;

{$MODE objfpc}{$H+}

Interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

Uses
  Classes, SysUtils, ctypes;

Const
  libxWII = 'libxwiimote.so';

Type
  xwii_iface = Pointer;
  xwii_monitor = Pointer;

  size_t = cuint;

Const
  (** Returncode if everyting is OK *)
  XWII_RETURN_OK = 0;

  (**
   * Interfaces
   *
   * Each constant describes a single interface. These are bit-masks that can be
   * binary-ORed. If an interface does not provide such a constant, it is static
   * and can be used without opening/closing it.
   *)

  (** Core interface *)
  XWII_IFACE_CORE = $000001;
  (** Accelerometer interface *)
  XWII_IFACE_ACCEL = $000002;
  (** IR interface *)
  XWII_IFACE_IR = $000004;

  (** MotionPlus extension interface *)
  XWII_IFACE_MOTION_PLUS = $000100;
  (** Nunchuk extension interface *)
  XWII_IFACE_NUNCHUK = $000200;
  (** ClassicController extension interface *)
  XWII_IFACE_CLASSIC_CONTROLLER = $000400;
  (** BalanceBoard extension interface *)
  XWII_IFACE_BALANCE_BOARD = $000800;
  (** ProController extension interface *)
  XWII_IFACE_PRO_CONTROLLER = $001000;
  (** Drums extension interface *)
  XWII_IFACE_DRUMS = $002000;
  (** Guitar extension interface *)
  XWII_IFACE_GUITAR = $004000;

  (** Special flag ORed with all valid interfaces *)
  XWII_IFACE_ALL = XWII_IFACE_CORE Or
    XWII_IFACE_ACCEL Or
    XWII_IFACE_IR Or
    XWII_IFACE_MOTION_PLUS Or
    XWII_IFACE_NUNCHUK Or
    XWII_IFACE_CLASSIC_CONTROLLER Or
    XWII_IFACE_BALANCE_BOARD Or
    XWII_IFACE_PRO_CONTROLLER Or
    XWII_IFACE_DRUMS Or
    XWII_IFACE_GUITAR;

  (** Special flag which causes the interfaces to be opened writable *)
  XWII_IFACE_WRITABLE = $010000;

  (**
   * LEDs
   *
   * One constant for each Player-LED.
   *)
  XWII_LED1 = 1;
  XWII_LED2 = 2;
  XWII_LED3 = 3;
  XWII_LED4 = 4;

  (* Event Types *)
  (**
   * Event Types
   *
   * Each event can be identified by the type field. New types might be added
   * at any time so unknown event-types must be ignored by applications. The
   * given payload of an event is described for each type. Unused payload-space
   * is zeroed by the library. However, the payload may be extended in new
   * revisions so applications must not depend on it being 0 or untouched.
   *)
  XWII_EVENT_KEY = 0;
  XWII_EVENT_ACCEL = 1;
  XWII_EVENT_IR = 2;
  XWII_EVENT_BALANCE_BOARD = 3;
  XWII_EVENT_MOTION_PLUS = 4;
  XWII_EVENT_PRO_CONTROLLER_KEY = 5;
  XWII_EVENT_PRO_CONTROLLER_MOVE = 6;
  XWII_EVENT_WATCH = 7;
  XWII_EVENT_CLASSIC_CONTROLLER_KEY = 8;
  XWII_EVENT_CLASSIC_CONTROLLER_MOVE = 9;
  XWII_EVENT_NUNCHUK_KEY = 10;
  XWII_EVENT_NUNCHUK_MOVE = 11;
  XWII_EVENT_DRUMS_KEY = 12;
  XWII_EVENT_DRUMS_MOVE = 13;
  XWII_EVENT_GUITAR_KEY = 14;
  XWII_EVENT_GUITAR_MOVE = 15;
  XWII_EVENT_GONE = 16;

  (* Zuordnung Key zu Nummer *)
  XWII_KEY_LEFT = 0;
  XWII_KEY_RIGHT = 1;
  XWII_KEY_UP = 2;
  XWII_KEY_DOWN = 3;
  XWII_KEY_A = 4;
  XWII_KEY_B = 5;
  XWII_KEY_PLUS = 6;
  XWII_KEY_MINUS = 7;
  XWII_KEY_HOME = 8;
  XWII_KEY_ONE = 9;
  XWII_KEY_TWO = 10;
  XWII_KEY_X = 11;
  XWII_KEY_Y = 12;
  XWII_KEY_TL = 13;
  XWII_KEY_TR = 14;
  XWII_KEY_ZL = 15;
  XWII_KEY_ZR = 16;

  (* Key State*)
  XWII_KEY_STATE_UP = 0;
  XWII_KEY_STATE_DOWN = 1;
  XWII_KEY_STATE_AUTO_REPEAT = 2;

  (*
   * https://dvdhrm.github.io/xwiimote/api/group__device.html
   *)

Function xwii_iface_new(Out dev: xwii_iface; syspath: PChar): cint{$IFDEF Linux} cdecl; external libxWII{$ENDIF};
Procedure xwii_iface_ref(dev: xwii_iface){$IFDEF Linux} cdecl; external libxWII{$ENDIF};
Procedure xwii_iface_unref(dev: xwii_iface){$IFDEF Linux} cdecl; external libxWII{$ENDIF};

Function xwii_iface_get_fd(dev: xwii_iface): cint{$IFDEF Linux} cdecl; external libxWII{$ENDIF};

Function xwii_iface_watch(dev: xwii_iface; watch: cbool): cint{$IFDEF Linux} cdecl; external libxWII{$ENDIF};

Function xwii_iface_open(dev: xwii_iface; ifaces: cuint): cint{$IFDEF Linux} cdecl; external libxWII{$ENDIF};

Procedure xwii_iface_close(dev: xwii_iface; ifaces: cuint){$IFDEF Linux} cdecl; external libxWII{$ENDIF};

Function xwii_iface_opened(dev: xwii_iface): cuint{$IFDEF Linux} cdecl; external libxWII{$ENDIF};

Function xwii_iface_available(dev: xwii_iface): cuint{$IFDEF Linux} cdecl; external libxWII{$ENDIF};

//XWII__DEPRECATED int 	xwii_iface_poll (struct xwii_iface *dev, struct xwii_event *ev) --> Deprecated c Function, not ported use dispatch instead !!

Function xwii_iface_dispatch(dev: xwii_iface; Var ev: Array Of Byte; size: size_t): cint{$IFDEF Linux} cdecl; external libxWII{$ENDIF};

Function xwii_iface_rumble(dev: xwii_iface; Value: cbool): cint{$IFDEF Linux} cdecl; external libxWII{$ENDIF};

Function xwii_iface_get_led(dev: xwii_iface; led: cuint; Out state: cbool): cint{$IFDEF Linux} cdecl; external libxWII{$ENDIF}; // TODO: Das macht nicht den Eindruck als das es geht
Function xwii_iface_set_led(dev: xwii_iface; led: cuint; state: cbool): cint{$IFDEF Linux} cdecl; external libxWII{$ENDIF}; // -- Das geht nur mit Root Rechten

Function xwii_iface_get_battery(dev: xwii_iface; Out capacity: cuint8): cint{$IFDEF Linux} cdecl; external libxWII{$ENDIF};

Function xwii_iface_get_devtype(dev: xwii_iface; Var devtype: Pchar): cint{$IFDEF Linux} cdecl; external libxWII{$ENDIF};
Function xwii_iface_get_extension(dev: xwii_iface; Var extension: PChar): cint{$IFDEF Linux} cdecl; external libxWII{$ENDIF};

Procedure xwii_iface_set_mp_normalization(dev: xwii_iface; x, y, z, factor: cint32){$IFDEF Linux} cdecl; external libxWII{$ENDIF}; // -- Not tested yet
Procedure xwii_iface_get_mp_normalization(dev: xwii_iface; Var x: cint32; Var y: cint32; Var z: cint32; Var factor: cint32){$IFDEF Linux} cdecl; external libxWII{$ENDIF}; // -- Not tested yet

(*
 * https://dvdhrm.github.io/xwiimote/api/group__monitor.html
 *)
Function xwii_monitor_new(poll, direct: cbool): xwii_monitor{$IFDEF Linux} cdecl; external libxWII{$ENDIF};
Procedure xwii_monitor_ref(mon: xwii_monitor){$IFDEF Linux} cdecl; external libxWII{$ENDIF};
Procedure xwii_monitor_unref(mon: xwii_monitor){$IFDEF Linux} cdecl; external libxWII{$ENDIF};
Function xwii_monitor_get_fd(mon: xwii_monitor; blocking: cbool): cint{$IFDEF Linux} cdecl; external libxWII{$ENDIF};
Function xwii_monitor_poll(monitor: xwii_monitor): PChar{$IFDEF Linux} cdecl; external libxWII{$ENDIF};

Implementation

{$IFDEF Windows}

{$WARNING Error this os is not supported by the library!}

Function xwii_iface_new(Out dev: xwii_iface; syspath: PChar): cint;
Begin
  dev := Nil;
  result := XWII_RETURN_OK + 1;
End;

Procedure xwii_iface_ref(dev: xwii_iface);
Begin

End;

Procedure xwii_iface_unref(dev: xwii_iface);
Begin

End;

Function xwii_iface_get_fd(dev: xwii_iface): cint;
Begin
  result := 0;
End;

Function xwii_iface_watch(dev: xwii_iface; watch: cbool): cint;
Begin
  result := XWII_RETURN_OK + 1;
End;

Function xwii_iface_open(dev: xwii_iface; ifaces: cuint): cint;
Begin
  result := XWII_RETURN_OK + 1;
End;

Procedure xwii_iface_close(dev: xwii_iface; ifaces: cuint);
Begin

End;

Function xwii_iface_opened(dev: xwii_iface): cuint;
Begin
  result := 0;
End;

Function xwii_iface_available(dev: xwii_iface): cuint;
Begin
  result := 0;
End;

Function xwii_iface_dispatch(dev: xwii_iface; Out ev: Array Of Byte; size: size_t): cint;
Begin
  result := 0;
End;

Function xwii_iface_rumble(dev: xwii_iface; Value: cbool): cint;
Begin
  result := XWII_RETURN_OK + 1;
End;

Function xwii_iface_get_led(dev: xwii_iface; led: cuint; Out state: cbool): cint;
Begin
  State := false;
  result := XWII_RETURN_OK + 1;
End;

Function xwii_iface_set_led(dev: xwii_iface; led: cuint; state: cbool): cint;
Begin
  result := XWII_RETURN_OK + 1;
End;

Function xwii_iface_get_battery(dev: xwii_iface; Out capacity: cuint8): cint;
Begin
  capacity := 0;
  result := XWII_RETURN_OK + 1;
End;

Function xwii_iface_get_devtype(dev: xwii_iface; Var devtype: Pchar): cint;
Begin
  devtype := Nil;
End;

Function xwii_iface_get_extension(dev: xwii_iface; Var extension: PChar): cint;
Begin
  extension := Nil;
End;

Procedure xwii_iface_set_mp_normalization(dev: xwii_iface; x, y, z, factor: cint32);
Begin

End;

Procedure xwii_iface_get_mp_normalization(dev: xwii_iface; Var x: cint32; Var y: cint32; Var z: cint32; Var factor: cint32);
Begin
  x := 0;
  y := 0;
  z := 0;
  factor := 0;
End;

Function xwii_monitor_new(poll, direct: cbool): xwii_monitor;
Begin
  result := Nil;
End;

Procedure xwii_monitor_ref(mon: xwii_monitor);
Begin

End;

Procedure xwii_monitor_unref(mon: xwii_monitor);
Begin

End;

Function xwii_monitor_get_fd(mon: xwii_monitor; blocking: cbool): cint;
Begin
  result := 0; // Das ist im Prinzip ein File-Handle da ist "0" der nicht definiert wert !
End;

Function xwii_monitor_poll(monitor: xwii_monitor): PChar;
Begin
  result := Nil;
End;

{$ENDIF}

End.

