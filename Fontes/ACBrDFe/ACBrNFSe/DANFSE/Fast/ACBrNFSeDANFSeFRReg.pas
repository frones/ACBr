{$I ACBr.inc}

unit ACBrNFSeDANFSeFRReg;

interface

uses
  SysUtils, Classes, ACBrNFSeDANFSeFR, ACBrReg,
  {$IFDEF FPC}
     LResources
  {$ENDIF} ;

type
  { Editor de Proriedades de Componente para chamar OpenDialog dos Relatorios }

  { TACBrNFSeDANFSeFRFileNameProperty }

  TACBrNFSeDANFSeFRFileNameProperty = class(TACBrFileProperty)
  protected
    function GetFilter: String; override;
  end;

  procedure Register;

implementation

{$IFNDEF FPC}
   {$R ACBrNFSe.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrNFSe', [TACBrNFSeDANFSeFR]);
  
  RegisterPropertyEditor(TypeInfo(String), TACBrNFSeDANFSeFR, 'FastFile',
     TACBrNFSeDANFSeFRFileNameProperty);
end;

{ TACBrNFSeDANFSeFRFileNameProperty }

function TACBrNFSeDANFSeFRFileNameProperty.GetFilter: String;
begin
  Result := 'Arquivos do FastReport|*.fr3|Todos os arquivos|*.*';
end;

end.
