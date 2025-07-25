{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit delforlazarus;

{$warn 5023 off : no warning about unused units}
interface

uses
  delforengine, delforinterf, delforsource, delfortypes, oobjects, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('delforsource', @delforsource.Register);
end;

initialization
  RegisterPackage('delforlazarus', @Register);
end.
