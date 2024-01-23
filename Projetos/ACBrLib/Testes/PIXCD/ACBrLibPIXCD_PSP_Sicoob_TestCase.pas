unit ACBrLibPIXCD_PSP_Sicoob_TestCase;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Dialogs;

type

  { TTestACBrPIXCDLib_PSP_Sicoob }

  TTestACBrPIXCDLib_PSP_Sicoob = class(TTestCase)
  private
    fCaminhoExec: String;
    procedure SetUp; override;
  published
    procedure Test_PSP_ConfigGravar_ChavePIX;
    procedure Test_PSP_ConfigGravar_ClientID;
    procedure Test_PSP_ConfigGravar_TokenSandbox;
    procedure Test_PSP_ConfigGravar_ArqChavePrivada;
    procedure Test_PSP_ConfigGravar_ArqCertificado;
    procedure Test_PSP_ConfigGravar_APIVersion;
  end;

implementation

uses
  ACBrLibPIXCDStaticImportMT, ACBrLibPIXCDConsts, ACBrLibConsts;

{ TTestACBrPIXCDLib_PSP_Sicoob }

procedure TTestACBrPIXCDLib_PSP_Sicoob.SetUp;
begin
  inherited SetUp;
  fCaminhoExec := ExtractFileDir(ParamStr(0));
end;

procedure TTestACBrPIXCDLib_PSP_Sicoob.Test_PSP_ConfigGravar_ChavePIX;
var
  Bufflen: Integer;
  AStr, Arq: String;
  Handle: THandle;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
  //Para evitar erro
  AssertEquals('Erro ao definir PSP', ErrOK, PIXCD_ConfigGravarValor(Handle, CSessaoPIXCDConfig, CChavePSP, '6'));
  AssertEquals('Erro ao definir ChavePIX', ErrOK, PIXCD_ConfigGravarValor(Handle, CSessaoPIXCDSicoobConfig, CChavePIXSicoob, 'meuemail@mail.com'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, PIXCD_ConfigLerValor(Handle, CSessaoPIXCDSicoobConfig, CChavePIXSicoob, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', 'meuemail@mail.com', AStr);
  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib_PSP_Sicoob.Test_PSP_ConfigGravar_ClientID;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
  //Para evitar erro
  AssertEquals('Erro ao definir PSP', ErrOK, PIXCD_ConfigGravarValor(Handle, CSessaoPIXCDConfig, CChavePSP, '6'));
  AssertEquals('Erro ao definir ClientID', ErrOK, PIXCD_ConfigGravarValor(Handle, CSessaoPIXCDSicoobConfig, CChaveClientIDSicoob, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, PIXCD_ConfigLerValor(Handle, CSessaoPIXCDSicoobConfig, CChaveClientIDSicoob, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx', AStr);
  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib_PSP_Sicoob.Test_PSP_ConfigGravar_TokenSandbox;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
  //Para evitar erro
  AssertEquals('Erro ao definir PSP', ErrOK, PIXCD_ConfigGravarValor(Handle, CSessaoPIXCDConfig, CChavePSP, '6'));
  AssertEquals('Erro ao definir TokenSandBox', ErrOK, PIXCD_ConfigGravarValor(Handle, CSessaoPIXCDSicoobConfig, CChaveTokenSandboxSicoob, 'xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, PIXCD_ConfigLerValor(Handle, CSessaoPIXCDSicoobConfig, CChaveTokenSandboxSicoob, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', 'xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx', AStr);
  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib_PSP_Sicoob.Test_PSP_ConfigGravar_ArqChavePrivada;
var
  Bufflen: Integer;
  AStr, Arq: String;
  Handle: THandle;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
  Arq := fCaminhoExec+'\Arq.key';
  AssertEquals('Erro ao definir ArqChavePrivada', ErrOK, PIXCD_ConfigGravarValor(Handle, CSessaoPIXCDSicoobConfig, CChaveArqChavePrivadaSicoob, PChar(Arq)));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, PIXCD_ConfigLerValor(Handle, CSessaoPIXCDSicoobConfig, CChaveArqChavePrivadaSicoob, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', fCaminhoExec+'\Arq.key', AStr);
  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib_PSP_Sicoob.Test_PSP_ConfigGravar_ArqCertificado;
var
  Bufflen: Integer;
  AStr, Arq: String;
  Handle: THandle;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
  Arq := fCaminhoExec+'\Arq.cert';
  AssertEquals('Erro ao definir ArqCertificado', ErrOK, PIXCD_ConfigGravarValor(Handle, CSessaoPIXCDSicoobConfig, CChaveArqCertificadoSicoob, PChar(Arq)));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, PIXCD_ConfigLerValor(Handle, CSessaoPIXCDSicoobConfig, CChaveArqCertificadoSicoob, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', fCaminhoExec+'\Arq.cert', AStr);
  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib_PSP_Sicoob.Test_PSP_ConfigGravar_APIVersion;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
  AssertEquals('Erro ao definir APIVersion', ErrOK, PIXCD_ConfigGravarValor(Handle, CSessaoPIXCDSicoobConfig, CChaveAPIVersionSicoob, '1'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, PIXCD_ConfigLerValor(Handle, CSessaoPIXCDSicoobConfig, CChaveAPIVersionSicoob, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '1', AStr);
  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

initialization
  RegisterTest(TTestACBrPIXCDLib_PSP_Sicoob);


end.

