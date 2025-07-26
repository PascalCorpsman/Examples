{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit lazcomment; 

interface

uses
  lazcommentsource, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('lazcommentsource', @lazcommentsource.Register); 
end; 

initialization
  RegisterPackage('lazcomment', @Register); 
end.
