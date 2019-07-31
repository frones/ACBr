
{$I ACBr.inc}

unit ACBrCIOTReg;

interface

uses
  SysUtils, Classes, ACBrCIOT, pcnConversao,
  {$IFDEF FPC}
     LResources, LazarusPackageIntf, PropEdits, componenteditors
  {$ELSE}
     {$IFNDEF COMPILER6_UP}
        DsgnIntf
     {$ELSE}
        DesignIntf,
        DesignEditors
     {$ENDIF}
  {$ENDIF} ;

procedure Register;

implementation

uses
  ACBrReg, ACBrDFeRegUtil, ACBrDFeConfiguracoes, ACBrCIOTConfiguracoes;

{$IFNDEF FPC}
   {$R ACBrCIOT.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrCIOT', [TACBrCIOT]);

  RegisterPropertyEditor(TypeInfo(TCertificadosConf), TConfiguracoes, 'Certificados',
    TClassProperty);

  RegisterPropertyEditor(TypeInfo(TConfiguracoes), TACBrCIOT, 'Configuracoes',
    TClassProperty);

  RegisterPropertyEditor(TypeInfo(TWebServicesConf), TConfiguracoes, 'WebServices',
    TClassProperty);

  RegisterPropertyEditor(TypeInfo(String), TWebServicesConf, 'UF',
     TACBrUFProperty);

  RegisterPropertyEditor(TypeInfo(TGeralConfCIOT), TConfiguracoes, 'Geral',
    TClassProperty);

  RegisterPropertyEditor(TypeInfo(String), TGeralConfCIOT, 'PathSalvar',
     TACBrDirProperty);

  RegisterPropertyEditor(TypeInfo(TArquivosConfCIOT), TConfiguracoes, 'Arquivos',
    TClassProperty);

  RegisterPropertyEditor(TypeInfo(String), TArquivosConfCIOT, 'PathCIOT',
     TACBrDirProperty);
end;

{$IFDEF FPC}
initialization
   {$I ACBrCIOT.lrs}
{$ENDIF}

end.
