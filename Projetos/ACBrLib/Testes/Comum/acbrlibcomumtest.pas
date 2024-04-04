{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit ACBrLibComumTest;

{$mode objfpc}{$H+}

interface

// Remover o ponto para descomentar a lib que desejar utilizar
{.$DEFINE TESTE_BAL}
{.$DEFINE TESTE_BOLETO}
{.$DEFINE TESTE_CEP}
{.$DEFINE TESTE_CNPJ}
{.$DEFINE TESTE_CTE}
{.$DEFINE TESTE_ESOCIAL}
{.$DEFINE TESTE_GTIN}
{.$DEFINE TESTE_IBGE}
{.$DEFINE TESTE_MAIL}
{.$DEFINE TESTE_MDFE}
{$DEFINE TESTE_NFE}
{.$DEFINE TESTE_NFSE}
{.$DEFINE TESTE_PIXCD}
{.$DEFINE TESTE_POS}
{.$DEFINE TESTE_REINF}
{.$DEFINE TESTE_SAT}

uses
  Classes, SysUtils, fpcunit, testregistry, Graphics, ACBrLibResposta;

type
  TTeste_Inicializar = function(var libHandle: TLibHandle; const eArqConfig, eChaveCrypt: PChar): longint;
                         {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

  TTeste_Finalizar = function(const libHandle: TLibHandle): longint;
                       {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

  TTeste_OpenSSLInfo = function(const libHandle: TLibHandle; const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
                         {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

  { TACbrLibRespostaDescendenteSimples }

  TACbrLibRespostaDescendenteSimples = class(TACBrLibRespostaBase)
  private
    FFonte: TFont;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo);
    destructor Destroy; override;
  published
    property Fonte: TFont read FFonte write FFonte;
  end;

  { TACBrLibResposta_Testes }

  TACBrLibResposta_Testes = class(TTestCase)
  published
    procedure GravarIni_TesteFonte;
    procedure Test_LIB_Inicializar;
    procedure Test_LIB_OpenSSLInfo;
  end;

  { TACBrLibComum_Testes }

  TACBrLibComum_Testes = class
    FACBrLibResposta_Testes: TACBrLibResposta_Testes;
    FTeste_Inicializar: TTeste_Inicializar;
    FTeste_Finalizar: TTeste_Finalizar;
    FTeste_OpenSSLInfo: TTeste_OpenSSLInfo;
  private
    function getTeste_Finalizar: TTeste_Finalizar;
    function getTeste_OpenSSLInfo: TTeste_OpenSSLInfo;
    function getTeste_Inicializar: TTeste_Inicializar;
  public
    constructor Create(AACBrLibResposta_Testes: TACBrLibResposta_Testes);

    property Teste_Inicializar: TTeste_Inicializar read getTeste_Inicializar;
    property Teste_Finalizar: TTeste_Finalizar read getTeste_Finalizar;
    property Teste_OpenSSLInfo: TTeste_OpenSSLInfo read getTeste_OpenSSLInfo;
  end;

implementation

uses
  IniFiles
  {$IFDEF TESTE_BAL},ACBrLibBALStaticImportMT{$ENDIF}
  {$IFDEF TESTE_BOLETO},ACBrLibBoletoStaticImportMT{$ENDIF}
  {$IFDEF TESTE_CEP},ACBrLibCEPStaticImportMT{$ENDIF}
  {$IFDEF TESTE_CNPJ},ACBrLibConsultaCNPJStaticImportMT{$ENDIF}
  {$IFDEF TESTE_CTE},ACBrLibCTeStaticImportMT{$ENDIF}
  {$IFDEF TESTE_ESOCIAL},ACBrLibeSocialStaticImportMT{$ENDIF}
  {$IFDEF TESTE_GTIN},ACBrLibGTINStaticImportMT{$ENDIF}
  {$IFDEF TESTE_IBGE},ACBrLibIBGEStaticImportMT{$ENDIF}
  {$IFDEF TESTE_MAIL},ACBrLibMailStaticImportMT{$ENDIF}
  {$IFDEF TESTE_MDFE},ACBrLibMDFeStaticImportMT{$ENDIF}
  {$IFDEF TESTE_NFE},ACBrLibNFeStaticImportMT{$ENDIF}
  {$IFDEF TESTE_NFSE},ACBrLibNFSeStaticImportMT{$ENDIF}
  {$IFDEF TESTE_PIXCD},ACBrLibPIXCDStaticImportMT{$ENDIF}
  {$IFDEF TESTE_POS},ACBrLibPosPrinterStaticImportMT{$ENDIF}
  {$IFDEF TESTE_REINF},ACBrLibReinfStaticImportMT{$ENDIF}
  {$IFDEF TESTE_SAT},ACBrLibSATStaticImportMT{$ENDIF}
  ;

{ TACBrLibComum_Testes }

constructor TACBrLibComum_Testes.Create(AACBrLibResposta_Testes: TACBrLibResposta_Testes);
begin
  FACBrLibResposta_Testes := AACBrLibResposta_Testes;
end;

function TACBrLibComum_Testes.getTeste_Inicializar: TTeste_Inicializar;
begin
  if not Assigned(FTeste_Inicializar) then
  begin
    {$IFDEF TESTE_BAL}FTeste_Inicializar := @BAL_Inicializar;{$ENDIF}
    {$IFDEF TESTE_BOLETO}FTeste_Inicializar := @BOLETO_Inicializar;{$ENDIF}
    {$IFDEF TESTE_CEP}FTeste_Inicializar := @CEP_Inicializar;{$ENDIF}
    {$IFDEF TESTE_CNPJ}FTeste_Inicializar := @CNPJ_Inicializar;{$ENDIF}
    {$IFDEF TESTE_CTE}FTeste_Inicializar := @CTE_Inicializar;{$ENDIF}
    {$IFDEF TESTE_ESOCIAL}FTeste_Inicializar := @ESOCIAL_Inicializar;{$ENDIF}
    {$IFDEF TESTE_GTIN}FTeste_Inicializar := @GTIN_Inicializar;{$ENDIF}
    {$IFDEF TESTE_IBGE}FTeste_Inicializar := @IBGE_Inicializar;{$ENDIF}
    {$IFDEF TESTE_MAIL}FTeste_Inicializar := @MAIL_Inicializar;{$ENDIF}
    {$IFDEF TESTE_MDFE}FTeste_Inicializar := @MDFE_Inicializar;{$ENDIF}
    {$IFDEF TESTE_NFE}FTeste_Inicializar := @NFE_Inicializar;{$ENDIF}
    {$IFDEF TESTE_NFSE}FTeste_Inicializar := @NFSE_Inicializar;{$ENDIF}
    {$IFDEF TESTE_PIXCD}FTeste_Inicializar := @PIXCD_Inicializar;{$ENDIF}
    {$IFDEF TESTE_POS}FTeste_Inicializar := @POS_Inicializar;{$ENDIF}
    {$IFDEF TESTE_REINF}FTeste_Inicializar := @REINF_Inicializar;{$ENDIF}
    {$IFDEF TESTE_SAT}FTeste_Inicializar := @SAT_Inicializar;{$ENDIF}
  end;

  FACBrLibResposta_Testes.AssertFalse('Lib não informada', not Assigned(FTeste_Inicializar));

  Result := FTeste_Inicializar;
end;

function TACBrLibComum_Testes.getTeste_Finalizar: TTeste_Finalizar;
begin
  if not Assigned(FTeste_Finalizar) then
  begin
    {$IFDEF TESTE_BAL}FTeste_Finalizar := @BAL_Finalizar;{$ENDIF}
    {$IFDEF TESTE_BOLETO}FTeste_Finalizar := @BOLETO_Finalizar;{$ENDIF}
    {$IFDEF TESTE_CEP}FTeste_Finalizar := @CEP_Finalizar;{$ENDIF}
    {$IFDEF TESTE_CNPJ}FTeste_Finalizar := @CNPJ_Finalizar;{$ENDIF}
    {$IFDEF TESTE_CTE}FTeste_Finalizar := @CTE_Finalizar;{$ENDIF}
    {$IFDEF TESTE_ESOCIAL}FTeste_Finalizar := @ESOCIAL_Finalizar;{$ENDIF}
    {$IFDEF TESTE_GTIN}FTeste_Finalizar := @GTIN_Finalizar;{$ENDIF}
    {$IFDEF TESTE_IBGE}FTeste_Finalizar := @IBGE_Finalizar;{$ENDIF}
    {$IFDEF TESTE_MAIL}FTeste_Finalizar := @MAIL_Finalizar;{$ENDIF}
    {$IFDEF TESTE_MDFE}FTeste_Finalizar := @MDFE_Finalizar;{$ENDIF}
    {$IFDEF TESTE_NFE}FTeste_Finalizar := @NFE_Finalizar;{$ENDIF}
    {$IFDEF TESTE_NFSE}FTeste_Finalizar := @NFSE_Finalizar;{$ENDIF}
    {$IFDEF TESTE_PIXCD}FTeste_Finalizar := @PIXCD_Finalizar;{$ENDIF}
    {$IFDEF TESTE_POS}FTeste_Finalizar := @POS_Finalizar;{$ENDIF}
    {$IFDEF TESTE_REINF}FTeste_Finalizar := @REINF_Finalizar;{$ENDIF}
    {$IFDEF TESTE_SAT}FTeste_Finalizar := @SAT_Finalizar;{$ENDIF}
  end;

  FACBrLibResposta_Testes.AssertFalse('Lib não informada', not Assigned(FTeste_Finalizar));

  Result := FTeste_Finalizar;
end;

function TACBrLibComum_Testes.getTeste_OpenSSLInfo: TTeste_OpenSSLInfo;
begin
  if not Assigned(FTeste_OpenSSLInfo) then
  begin
    {$IFDEF TESTE_BAL}FTeste_OpenSSLInfo := @BAL_OpenSSLInfo;{$ENDIF}
    {$IFDEF TESTE_BOLETO}FTeste_OpenSSLInfo := @BOLETO_OpenSSLInfo;{$ENDIF}
    {$IFDEF TESTE_CEP}FTeste_OpenSSLInfo := @CEP_OpenSSLInfo;{$ENDIF}
    {$IFDEF TESTE_CNPJ}FTeste_OpenSSLInfo := @CNPJ_OpenSSLInfo;{$ENDIF}
    {$IFDEF TESTE_CTE}FTeste_OpenSSLInfo := @CTE_OpenSSLInfo;{$ENDIF}
    {$IFDEF TESTE_ESOCIAL}FTeste_OpenSSLInfo := @ESOCIAL_OpenSSLInfo;{$ENDIF}
    {$IFDEF TESTE_GTIN}FTeste_OpenSSLInfo := @GTIN_OpenSSLInfo;{$ENDIF}
    {$IFDEF TESTE_IBGE}FTeste_OpenSSLInfo := @IBGE_OpenSSLInfo;{$ENDIF}
    {$IFDEF TESTE_MAIL}FTeste_OpenSSLInfo := @MAIL_OpenSSLInfo;{$ENDIF}
    {$IFDEF TESTE_MDFE}FTeste_OpenSSLInfo := @MDFE_OpenSSLInfo;{$ENDIF}
    {$IFDEF TESTE_NFE}FTeste_OpenSSLInfo := @NFE_OpenSSLInfo;{$ENDIF}
    {$IFDEF TESTE_NFSE}FTeste_OpenSSLInfo := @NFSE_OpenSSLInfo;{$ENDIF}
    {$IFDEF TESTE_PIXCD}FTeste_OpenSSLInfo := @PIXCD_OpenSSLInfo;{$ENDIF}
    {$IFDEF TESTE_POS}FTeste_OpenSSLInfo := @POS_OpenSSLInfo;{$ENDIF}
    {$IFDEF TESTE_REINF}FTeste_OpenSSLInfo := @REINF_OpenSSLInfo;{$ENDIF}
    {$IFDEF TESTE_SAT}FTeste_OpenSSLInfo := @SAT_OpenSSLInfo;{$ENDIF}
  end;

  FACBrLibResposta_Testes.AssertFalse('Lib não informada', not Assigned(FTeste_OpenSSLInfo));

  Result := FTeste_OpenSSLInfo;
end;

{ TACbrLibRespostaDescendenteSimples }

constructor TACbrLibRespostaDescendenteSimples.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo);
begin
  FFonte := TFont.Create;
end;

destructor TACbrLibRespostaDescendenteSimples.Destroy;
begin
  FFonte.Free;
end;

procedure TACBrLibResposta_Testes.GravarIni_TesteFonte;
var
  acrds: TACbrLibRespostaDescendenteSimples;
  Resultado, STeste: string;
  ITeste: Int64;
  AIni: TMemIniFile;
  Astr: TStringStream;
begin
  acrds := TACbrLibRespostaDescendenteSimples.Create('Sessao', resINI);
  try
    acrds.Fonte.Name := 'Arial';
    acrds.Fonte.Pitch := fpFixed;
    acrds.Fonte.Size := 8;
    Resultado := acrds.Gerar;
  finally
    acrds.Free;
  end;
  Astr := TStringStream.Create(Resultado);
  try
    AIni := TMemIniFile.Create(Astr);
    try
      STeste := AIni.ReadString('Fonte', 'Name', '');
      CheckEquals('Arial', STeste, 'Falhou Fonte.Name!');

      STeste := AIni.ReadString('Fonte', 'Pitch', '');
      CheckEquals('2', STeste, 'Falhou Fonte.Pitch (enumerado)!');

      ITeste := AIni.ReadInt64('Fonte', 'Size', -1);
      CheckEquals(8, ITeste, 'Falhou Fonte.Size !');
    finally
      AIni.Free;
    end;
  finally
    Astr.Free;
  end;
end;

procedure TACBrLibResposta_Testes.Test_LIB_Inicializar;
var
  Handle: THandle;
  ACBrLibComum_Testes: TACBrLibComum_Testes;
begin
  ACBrLibComum_Testes := TACBrLibComum_Testes.Create(Self);
  try
    AssertEquals(ErrOk, ACBrLibComum_Testes.Teste_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, ACBrLibComum_Testes.Teste_Finalizar(Handle));
  finally
    FreeAndNil(ACBrLibComum_Testes);
  end;
end;

procedure TACBrLibResposta_Testes.Test_LIB_OpenSSLInfo;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
  ACBrLibComum_Testes: TACBrLibComum_Testes;
begin
  ACBrLibComum_Testes := TACBrLibComum_Testes.Create(Self);
  try
    ACBrLibComum_Testes.Teste_Inicializar(Handle, '', '');

    Bufflen := 0;
    AssertEquals(ErrOk, ACBrLibComum_Testes.Teste_OpenSSLInfo(Handle, nil, Bufflen));

    AStr := Space(Bufflen);
    AssertEquals(ErrOk, ACBrLibComum_Testes.Teste_OpenSSLInfo(Handle, PChar(AStr), Bufflen));

    // Verificando se retornou as linhas com dados
    Bufflen := Pos(sLineBreak, AStr);
    AssertFalse('Erro ao ler OpenSSLInfo', Bufflen = 0);

    // Verificando se retornou a versao apos os dois pontos
    AStr := Trim(Copy(AStr, 1, Bufflen-1));
    AssertFalse('Erro ao ler OpenSSLInfo', 'OpenSSLVersion:' = AStr);

    AssertEquals(ErrOK, ACBrLibComum_Testes.Teste_Finalizar(Handle));
  finally
    FreeAndNil(ACBrLibComum_Testes);
  end;
end;


initialization

  RegisterTest(TACBrLibResposta_Testes);
end.

