{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit playingcard; 

interface

uses
  uplayingcard, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('uplayingcard', @uplayingcard.Register); 
end; 

initialization
  RegisterPackage('playingcard', @Register); 
end.
