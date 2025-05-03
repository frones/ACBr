// TODO
//- Nao precisa de diretorio de trabalho, usa o GetLibFullPath ?
//- Nao conseguiu carregar o certificado.. Sábado ?
//- Precisa do dposlocal.ini ?

{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

{$I ACBr.inc}

unit ACBrTEFPayKitAPI;

interface

uses
  Classes, SysUtils;

resourcestring
  sErrLibJaInicializada = 'Biblioteca DPOSDRV já foi inicializada';
  sErrLibNaoInicializada = 'Biblioteca DPOSDRV ainda NÃO foi carregada';
  sErrDirTrabalhoInvalido = 'Diretório de Trabalho não encontrado: %s';

const
  {$IFDEF MSWINDOWS}
   CPayKitLib = 'DPOSDRV.dll';
  {$ELSE}
   CPayKitLib = 'libDPOSDRV.so';
  {$ENDIF}

  CPayKitConf = 'dposlocal.ini';
  CPayKitCont = 'dposacbr.ini';

  CPayKitURLCertificado = 'https://tef.linxsaas.com.br/certificados/Gerenciador_Certificado.cgi';

  RET_OK = 0;

type
  EACBrTEFPayKitAPI = class(Exception);

  TACBrTEFPayKitGravarLog = procedure(const ALogLine: String; var Tratado: Boolean) of object ;

  { TACBrTEFPayKitAPI }

  TACBrTEFPayKitAPI = Class
  private
    fCarregada: Boolean;
    fCNPJEstabelecimento: String;
    fConfiguracaoIpPortaSsl: String;
    fDiretorioTrabalho: String;
    fEmTransacao: Boolean;
    fInicializada: Boolean;
    fModoDesfazimento: Byte;
    fNomeAutomacao: String;
    fNumeroEmpresa: Integer;
    fNumeroLoja: Integer;
    fNumeroPDV: Integer;
    fOnGravarLog: TACBrTEFPayKitGravarLog;
    fPathLib: String;
    fURLCertificado: String;
    fVersaoAutomacao: String;

    xTransacaoCheque: function(pValorTransacao, pNumeroCupomVenda, pNumeroControle,
      pQuantidadeCheques, pPeriodicidadeCheques, pDataPrimeiroCheque,
      pCarenciaPrimeiroCheque: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xTransacaoCartaoCredito: function(pValorTransacao, pNumeroCupomVenda,
      pNumeroControle: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    //xConfirmacaoCartaoCredito: function(pNumeroControle: PAnsiChar): LongInt;
    //  {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xTransacaoCartaoDebito: function(pValorTransacao, pNumeroCupomVenda,
      pNumeroControle: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    //xConfirmacaoCartaoDebito: function(pNumeroControle: PAnsiChar): LongInt;
    //  {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xTransacaoCartaoVoucher: function (pValorTransacao, pNumeroCupomVenda,
      pNumeroControle: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    //xConfirmacaoCartaoVoucher: function(pNumeroControle: PAnsiChar): LongInt;
    //  {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xTransacaoCancelamentoPagamento: function(pNumeroControle: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xTransacaoPreAutorizacaoCartaoCredito: function(pNumeroControle: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xTransacaoConsultaParcelas: function(pNumeroControle: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xTransacaoResumoVendas: function(pNumeroControle: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xTransacaoReimpressaoCupom: function: LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xConfirmaCartao: function(pNumeroControle: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xFinalizaTransacao: function: LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xObtemLogUltimaTransacao: procedure(oLogUltimaTransacao: PAnsiChar);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xVersaoDPOS: procedure(pVersao: PAnsiChar);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xInicializaDPOS: function: LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xFinalizaDPOS: function: LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xIdentificacaoAutomacaoComercial: function(pNomeAutomacao, pVersaoAutomacao,
      pReservado: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xConfiguraModoDesfazimento: function(iModoDesfazimento: LongInt): LongInt
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xConfiguraCNPJEstabelecimento: function(pCNPJEstabelecimento: PAnsiChar): LongInt
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xConfiguraEmpresaLojaPDV: function(pNumeroEmpresa, pNumeroLoja, pNumeroPDV: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xConfiguraComunicacaoDTEF: function(pConfiguracaoIpPortaSsl: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xBuscaCertificado: function(pURL, pPathCertificado: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    // Definicao das funcoes de transacao completa
    xTransacaoCartaoCreditoCompleta: function( pValorTransacao, pNumeroCupomVenda,
      pNumeroControle, pTipoOperacao, pNumeroParcelas, pValorParcela,
      pValorTaxaServico, pPermiteAlteracao, pReservado: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

     xTransacaoCartaoDebitoCompleta: function ( pValorTransacao, pNumeroCupomVenda,
       pNumeroControle, pTipoOperacao, pNumeroParcelas, pSequenciaParcela, pDataDebito,
       pValorParcela, pValorTaxaServico, pPermiteAlteracao, pReservado: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

     xTransacaoCartaoVoucherCompleta: function (pValorTransacao, pNumeroCupomVenda,
       pNumeroControle, pReservado: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

      xTransacaoManualPOSCompleta: function(pValorTransacao, pCodigoEstabelecimento,
        pData, pHora, pNumeroAutorizadora, pNumeroCartao, pTipoOperacao,
        pNumeroParcelas, pDataPreDatado, pNumeroControle: PAnsiChar): LongInt;
        {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    procedure SetCNPJEstabelecimento(AValue: String);
    procedure SetConfiguracaoIpPortaSsl(AValue: String);
    procedure SetURLCertificado(AValue: String);
    procedure SetInicializada(AValue: Boolean);
    procedure SetModoDesfazimento(AValue: Byte);
    procedure SetNomeAutomacao(AValue: String);
    procedure SetNumeroEmpresa(AValue: Integer);
    procedure SetNumeroLoja(AValue: Integer);
    procedure SetNumeroPDV(AValue: Integer);
    procedure SetVersaoAutomacao(AValue: String);
    procedure SetPathLib(const AValue: String);
    procedure SetDiretorioTrabalho(AValue: String);

  protected
    function GetLibFullPath: String;

    procedure LoadLibFunctions;
    procedure UnLoadLibFunctions;
    procedure ClearMethodPointers;
    procedure TratarErroPayKit(AErrorCode: LongInt);

    procedure DoException(const AErrorMsg: String );

    procedure VerificarCarregada;

    procedure VerificarDiretorioDeTrabalho;
    procedure VerificarEAjustarConf;

    procedure IdentificacaoAutomacaoComercial;
    procedure ConfiguraModoDesfazimento;
    procedure ConfiguraCNPJEstabelecimento;
    procedure ConfiguraEmpresaLojaPDV;
    procedure ConfiguraComunicacaoDTEF;
    procedure BuscaCertificado;
  public
    constructor Create;
    destructor Destroy; override;

    property PathLib: String read fPathLib write SetPathLib;
    property DiretorioTrabalho: String read fDiretorioTrabalho write SetDiretorioTrabalho;

    property Carregada: Boolean read fCarregada;
    property Inicializada: Boolean read fInicializada write SetInicializada;
    property EmTransacao: Boolean read fEmTransacao;

    property OnGravarLog: TACBrTEFPayKitGravarLog read fOnGravarLog write fOnGravarLog;

    procedure Inicializar;
    procedure DesInicializar;

    procedure GravarLog(const AString: AnsiString; Traduz: Boolean = False);

    function VersaoDPOS: String;
    procedure InicializaDPOS(Forcar: Boolean = False);
    procedure FinalizaDPOS(Forcar: Boolean = False);

    property NomeAutomacao: String read fNomeAutomacao write SetNomeAutomacao;
    property VersaoAutomacao: String read fVersaoAutomacao write SetVersaoAutomacao;
    property ModoDesfazimento: Byte read fModoDesfazimento write SetModoDesfazimento default 1;  // 0 - Automático, 1 - Explícito

    property CNPJEstabelecimento: String read fCNPJEstabelecimento write SetCNPJEstabelecimento;
    property NumeroEmpresa: Integer read fNumeroEmpresa write SetNumeroEmpresa;
    property NumeroLoja: Integer read fNumeroLoja write SetNumeroLoja;
    property NumeroPDV: Integer read fNumeroPDV write SetNumeroPDV;

    property ConfiguracaoIpPortaSsl: String read fConfiguracaoIpPortaSsl write SetConfiguracaoIpPortaSsl;
    property URLCertificado: String read fURLCertificado write SetURLCertificado;
  end;


implementation

uses
  IniFiles, Math,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO;

{ TACBrTEFPayKitAPI }

constructor TACBrTEFPayKitAPI.Create;
begin
  inherited;
  fCarregada := False;
  fDiretorioTrabalho := '';
  fPathLib := '';
  fEmTransacao := False;
  fInicializada := False;

  fNomeAutomacao := '';
  fVersaoAutomacao := '';
  fModoDesfazimento := 1;
  fOnGravarLog := Nil;
end;

destructor TACBrTEFPayKitAPI.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrTEFPayKitAPI.Inicializar;
begin
  if fInicializada then
    Exit;

  GravarLog('TACBrTEFPayKitAPI.Inicializar');

  VerificarDiretorioDeTrabalho;
  VerificarEAjustarConf;
  LoadLibFunctions;

  IdentificacaoAutomacaoComercial;
  ConfiguraModoDesfazimento;
  ConfiguraCNPJEstabelecimento;
  ConfiguraEmpresaLojaPDV;
  ConfiguraComunicacaoDTEF;
  BuscaCertificado;
  InicializaDPOS;

  fInicializada := True;
end;

procedure TACBrTEFPayKitAPI.DesInicializar;
begin
  if not fInicializada then
    Exit;

  GravarLog('TACBrTEFPayKitAPI.DesInicializar');
  UnLoadLibFunctions;
  fInicializada := False;
end;

procedure TACBrTEFPayKitAPI.GravarLog(const AString: AnsiString; Traduz: Boolean);
Var
  Tratado: Boolean;
  AStringLog: AnsiString;
begin
  if not Assigned(fOnGravarLog) then
    Exit;

  if Traduz then
    AStringLog := TranslateUnprintable(AString)
  else
    AStringLog := AString;

  Tratado := False;
  fOnGravarLog(AStringLog, Tratado);
end;

function TACBrTEFPayKitAPI.VersaoDPOS: String;
var
  pVersao: PAnsiChar;
begin
  Result := '';
  pVersao := AllocMem(100);
  try
    GravarLog('VersaoDPOS');
    xVersaoDPOS(pVersao);
    Result := String(pVersao);
    GravarLog('  Result: '+Result);
  finally
    Freemem(pVersao);
  end;
end;

procedure TACBrTEFPayKitAPI.InicializaDPOS(Forcar: Boolean);
var
  iRet: LongInt;
  ini: TMemIniFile;
  datamov: TDateTime;
  fechado: Boolean;
begin
  GravarLog('TACBrTEFPayKitAPI.InicializaDPOS');
  datamov := 0;
  fechado := False;
  ini := TMemIniFile.Create(DiretorioTrabalho + CPayKitCont);
  try
    if not Forcar then
    begin
      datamov := ini.ReadDateTime('Movimento','Data', 0);
      fechado := ini.ReadBool('Movimento','Fechado', False);
    end;

    if (datamov = Date) then
    begin
      if fechado then
        GravarLog('  Movimento já fechado')
      else
        GravarLog('  Movimento já aberto');

      Exit;
    end;

    GravarLog('InicializaDPOS');
    iRet := xInicializaDPOS;
    GravarLog('  ret: '+IntToStr(iRet));
    TratarErroPayKit(iRet);

    ini.WriteDateTime('Movimento','Data', Date);
    ini.WriteBool('Movimento','Fechado', False);
  finally
    ini.Free;
  end;
end;

procedure TACBrTEFPayKitAPI.FinalizaDPOS(Forcar: Boolean);
var
  iRet: LongInt;
  ini: TMemIniFile;
  fechado: Boolean;
  datamov: TDateTime;
begin
  GravarLog('TACBrTEFPayKitAPI.InicializaDPOS');
  fechado := False;
  datamov := 0;
  ini := TMemIniFile.Create(DiretorioTrabalho + CPayKitCont);
  try
    if not Forcar then
    begin
      datamov := ini.ReadDateTime('Movimento','Data', 0);
      fechado := ini.ReadBool('Movimento','Fechado', False);
    end;

    if fechado and (datamov = Date) then
    begin
      GravarLog('  Movimento já fechado');
      Exit;
    end;

    GravarLog('FinalizaDPOS');
    iRet := xFinalizaDPOS;
    GravarLog('  ret: '+IntToStr(iRet));
    TratarErroPayKit(iRet);

    ini.WriteDateTime('Movimento','Data', Date);
    ini.WriteBool('Movimento','Fechado', True);
  finally
    ini.Free;
  end;
end;

procedure TACBrTEFPayKitAPI.IdentificacaoAutomacaoComercial;
var
  iRet: LongInt;
  pNomeAutomacao, pVersaoAutomacao, pReservado: AnsiString;
begin
  pNomeAutomacao := PadRight(fNomeAutomacao, 20);
  pVersaoAutomacao := PadRight(fNomeAutomacao, 20);
  pReservado := PadRight('010', 256);   // O segundo byte informa se a automação está integrada com QR Code ('1' se sim, '0' se não)
  GravarLog('IdentificacaoAutomacaoComercial( '+pNomeAutomacao+', '+pVersaoAutomacao+', '+pReservado+ ')');
  iRet := xIdentificacaoAutomacaoComercial(PAnsiChar(pNomeAutomacao), PAnsiChar(pVersaoAutomacao), PAnsiChar(pReservado));
  GravarLog('  ret: '+IntToStr(iRet));
  TratarErroPayKit(iRet);
end;

procedure TACBrTEFPayKitAPI.ConfiguraModoDesfazimento;
var
  iRet: LongInt;
begin
  GravarLog('ConfiguraModoDesfazimento( '+IntToStr(fModoDesfazimento)+' )');
  iRet := xConfiguraModoDesfazimento(fModoDesfazimento);
  GravarLog('  ret: '+IntToStr(iRet));
  TratarErroPayKit(iRet);
end;

procedure TACBrTEFPayKitAPI.ConfiguraCNPJEstabelecimento;
var
  iRet: LongInt;
  pCNPJEstabelecimento: AnsiString;
begin
  pCNPJEstabelecimento := PadRight(fCNPJEstabelecimento, 14);
  GravarLog('ConfiguraCNPJEstabelecimento( '+pCNPJEstabelecimento+' )');
  iRet := xConfiguraCNPJEstabelecimento(PAnsiChar(pCNPJEstabelecimento));
  GravarLog('  ret: '+IntToStr(iRet));
  TratarErroPayKit(iRet);
end;

procedure TACBrTEFPayKitAPI.ConfiguraEmpresaLojaPDV;
var
  iRet: LongInt;
  pNumeroEmpresa, pNumeroLoja, pNumeroPDV: AnsiString;
begin
  pNumeroEmpresa := Format('%.4d',[fNumeroEmpresa]);
  pNumeroLoja := Format('%.4d',[fNumeroLoja]);
  pNumeroPDV := Format('%.4d',[fNumeroPDV]);
  GravarLog('ConfiguraEmpresaLojaPDV( '+pNumeroEmpresa+', '+pNumeroLoja+', '+pNumeroPDV+' )');
  iRet := xConfiguraEmpresaLojaPDV(PAnsiChar(pNumeroEmpresa), PAnsiChar(pNumeroLoja), PAnsiChar(pNumeroPDV));
  GravarLog('  ret: '+IntToStr(iRet));
  TratarErroPayKit(iRet);
end;

procedure TACBrTEFPayKitAPI.ConfiguraComunicacaoDTEF;
var
  iRet: LongInt;
  pConfiguracaoIpPortaSsl: AnsiString;
begin
  pConfiguracaoIpPortaSsl := Trim(fConfiguracaoIpPortaSsl);
  GravarLog('ConfiguraComunicacaoDTEF( '+pConfiguracaoIpPortaSsl+' )');
  iRet := xConfiguraComunicacaoDTEF(PAnsiChar(pConfiguracaoIpPortaSsl));
  GravarLog('  ret: '+IntToStr(iRet));
  TratarErroPayKit(iRet);
end;

procedure TACBrTEFPayKitAPI.BuscaCertificado;
var
  iRet: LongInt;
  pURL, pPathCertificado: AnsiString;
begin
  pURL := Trim(fURLCertificado);
  if (pURL = '') then
    pURL := CPayKitURLCertificado;

  pPathCertificado := ExtractFilePath(GetLibFullPath);

  GravarLog('BuscaCertificado( '+pURL+', '+pPathCertificado+' )');
  iRet := xBuscaCertificado(PAnsiChar(pURL), PAnsiChar(pPathCertificado));
  GravarLog('  ret: '+IntToStr(iRet));
  TratarErroPayKit(iRet);
end;

procedure TACBrTEFPayKitAPI.SetInicializada(AValue: Boolean);
begin
  if (fInicializada = AValue) then
    Exit;

  GravarLog('TACBrTEFPayKitAPI.SetInicializada( '+BoolToStr(AValue, True)+' )');

  if AValue then
    Inicializar
  else
    DesInicializar;
end;

procedure TACBrTEFPayKitAPI.SetCNPJEstabelecimento(AValue: String);
begin
  if fCNPJEstabelecimento = AValue then
    Exit;
  if fInicializada then
    DoException(sErrLibJaInicializada);
  fCNPJEstabelecimento := LeftStr(OnlyNumber(AValue), 14);
end;

procedure TACBrTEFPayKitAPI.SetConfiguracaoIpPortaSsl(AValue: String);
begin
  if fConfiguracaoIpPortaSsl = AValue then
    Exit;
  if fInicializada then
    DoException(sErrLibJaInicializada);
  fConfiguracaoIpPortaSsl := AValue;
end;

procedure TACBrTEFPayKitAPI.SetURLCertificado(AValue: String);
begin
  if fURLCertificado = AValue then
    Exit;
  if fInicializada then
    DoException(sErrLibJaInicializada);
  fURLCertificado := AValue;
end;

procedure TACBrTEFPayKitAPI.SetModoDesfazimento(AValue: Byte);
begin
  if fModoDesfazimento = AValue then
    Exit;
  if fInicializada then
    DoException(sErrLibJaInicializada);
  fModoDesfazimento := max(0, min(1, AValue));
end;

procedure TACBrTEFPayKitAPI.SetNomeAutomacao(AValue: String);
begin
  if fNomeAutomacao = AValue then
    Exit;
  if fInicializada then
    DoException(sErrLibJaInicializada);
  fNomeAutomacao := LeftStr(AValue, 20);
end;

procedure TACBrTEFPayKitAPI.SetNumeroEmpresa(AValue: Integer);
begin
  if fNumeroEmpresa = AValue then
    Exit;
  if fInicializada then
    DoException(sErrLibJaInicializada);
  fNumeroEmpresa := max(AValue, 0);
end;

procedure TACBrTEFPayKitAPI.SetNumeroLoja(AValue: Integer);
begin
  if fNumeroLoja = AValue then
    Exit;
  if fInicializada then
    DoException(sErrLibJaInicializada);
  fNumeroLoja := max(AValue, 0);
end;

procedure TACBrTEFPayKitAPI.SetNumeroPDV(AValue: Integer);
begin
  if fNumeroPDV = AValue then
    Exit;
  if fInicializada then
    DoException(sErrLibJaInicializada);
  fNumeroPDV := max(AValue, 0);
end;

procedure TACBrTEFPayKitAPI.SetVersaoAutomacao(AValue: String);
begin
  if fVersaoAutomacao = AValue then Exit;
  fVersaoAutomacao := LeftStr(AValue, 20);
end;

procedure TACBrTEFPayKitAPI.SetPathLib(const AValue: String);
begin
  if (fPathLib = AValue) then
    Exit;

  GravarLog('TACBrTEFPayKitAPI.SetPathLib( '+AValue+' )');

  if fInicializada then
    DoException(sErrLibJaInicializada);

  fPathLib := PathWithDelim(ExtractFilePath(AValue));
end;

procedure TACBrTEFPayKitAPI.SetDiretorioTrabalho(AValue: String);
begin
  if (fDiretorioTrabalho = AValue) then
    Exit;

  GravarLog('TACBrTEFPayKitAPI.SetDiretorioTrabalho( '+AValue+' )');

  if fInicializada then
    DoException(sErrLibJaInicializada);

  fDiretorioTrabalho := PathWithDelim(ExtractFilePath(AValue));
end;

function TACBrTEFPayKitAPI.GetLibFullPath: String;
var
  p, f: String;
begin
  if (PathLib <> '') then
  begin
    GravarLog('PathLib = '+PathLib);
    p := PathLib;
  end
  else
    p := ApplicationPath;

  Result := p + CPayKitLib;
  if not FileExists(Result) then
  begin
    f := p + 'Bin' + PathDelim + CPayKitLib;
    if FileExists(f) then
      Result := f;
  end;
end;

procedure TACBrTEFPayKitAPI.LoadLibFunctions;

  procedure PayKitFunctionDetect(LibName, FuncName: AnsiString; var LibPointer: Pointer;
    FuncIsRequired: Boolean = True) ;
  begin
    if not Assigned( LibPointer )  then
    begin
      GravarLog('   '+FuncName);
      if not FunctionDetect(LibName, FuncName, LibPointer) then
      begin
        LibPointer := NIL ;
        if FuncIsRequired then
          DoException(Format('Erro ao carregar a função: %s de: %s',[FuncName, LibName]))
        else
          GravarLog(Format('     Função não requerida: %s não encontrada em: %s',[FuncName, LibName]));
        end ;
    end ;
  end;

var
  sLibName: string;
begin
  if fCarregada then
    Exit;

  sLibName := GetLibFullPath;
  GravarLog('TACBrTEFPayKitAPI.LoadLibFunctions - '+sLibName);

  PayKitFunctionDetect(sLibName, 'TransacaoCheque', @xTransacaoCheque);
  PayKitFunctionDetect(sLibName, 'TransacaoCartaoCredito', @xTransacaoCartaoCredito);
  //PayKitFunctionDetect(sLibName, 'ConfirmacaoCartaoCredito', @xConfirmacaoCartaoCredito);
  PayKitFunctionDetect(sLibName, 'TransacaoCartaoDebito', @xTransacaoCartaoDebito);
  //PayKitFunctionDetect(sLibName, 'ConfirmacaoCartaoDebito', @xConfirmacaoCartaoDebito);
  PayKitFunctionDetect(sLibName, 'TransacaoCartaoVoucher', @xTransacaoCartaoVoucher);
  //PayKitFunctionDetect(sLibName, 'ConfirmacaoCartaoVoucher', @xConfirmacaoCartaoVoucher);
  PayKitFunctionDetect(sLibName, 'TransacaoCancelamentoPagamento', @xTransacaoCancelamentoPagamento);
  PayKitFunctionDetect(sLibName, 'TransacaoPreAutorizacaoCartaoCredito', @xTransacaoPreAutorizacaoCartaoCredito);
  PayKitFunctionDetect(sLibName, 'TransacaoConsultaParcelas', @xTransacaoConsultaParcelas);
  PayKitFunctionDetect(sLibName, 'TransacaoResumoVendas', @xTransacaoResumoVendas);
  PayKitFunctionDetect(sLibName, 'TransacaoReimpressaoCupom', @xTransacaoReimpressaoCupom);
  PayKitFunctionDetect(sLibName, 'ConfirmaCartao', @xConfirmaCartao);
  PayKitFunctionDetect(sLibName, 'FinalizaTransacao', @xFinalizaTransacao);
  PayKitFunctionDetect(sLibName, 'ObtemLogUltimaTransacao', @xObtemLogUltimaTransacao);
  PayKitFunctionDetect(sLibName, 'VersaoDPOS', @xVersaoDPOS);
  PayKitFunctionDetect(sLibName, 'InicializaDPOS', @xInicializaDPOS);
  PayKitFunctionDetect(sLibName, 'FinalizaDPOS', @xFinalizaDPOS);
  PayKitFunctionDetect(sLibName, 'IdentificacaoAutomacaoComercial', @xIdentificacaoAutomacaoComercial);
  PayKitFunctionDetect(sLibName, 'ConfiguraModoDesfazimento', @xConfiguraModoDesfazimento);
  PayKitFunctionDetect(sLibName, 'ConfiguraCNPJEstabelecimento', @xConfiguraCNPJEstabelecimento);
  PayKitFunctionDetect(sLibName, 'ConfiguraEmpresaLojaPDV', @xConfiguraEmpresaLojaPDV);
  PayKitFunctionDetect(sLibName, 'ConfiguraComunicacaoDTEF', @xConfiguraComunicacaoDTEF);
  PayKitFunctionDetect(sLibName, 'BuscaCertificado', @xBuscaCertificado);
  PayKitFunctionDetect(sLibName, 'TransacaoCartaoCreditoCompleta', @xTransacaoCartaoCreditoCompleta);
  PayKitFunctionDetect(sLibName, 'TransacaoCartaoDebitoCompleta', @xTransacaoCartaoDebitoCompleta);
  PayKitFunctionDetect(sLibName, 'TransacaoCartaoVoucherCompleta', @xTransacaoCartaoVoucherCompleta);
  PayKitFunctionDetect(sLibName, 'TransacaoManualPOSCompleta', @xTransacaoManualPOSCompleta);
  fCarregada := True;
end;

procedure TACBrTEFPayKitAPI.UnLoadLibFunctions;
var
  sLibName: String;
begin
  if not fCarregada then
    Exit;

  GravarLog('TACBrTEFPayKitAPI.UnLoadLibFunctions');

  sLibName := GetLibFullPath;
  UnLoadLibrary( sLibName );
  fCarregada := False;
  ClearMethodPointers;
end;

procedure TACBrTEFPayKitAPI.ClearMethodPointers;
begin
  xTransacaoCheque := Nil;
  xTransacaoCartaoCredito := Nil;
  //xConfirmacaoCartaoCredito := Nil;
  xTransacaoCartaoDebito := Nil;
  //xConfirmacaoCartaoDebito := Nil;
  xTransacaoCartaoVoucher := Nil;
  //xConfirmacaoCartaoVoucher := Nil;
  xTransacaoCancelamentoPagamento := Nil;
  xTransacaoPreAutorizacaoCartaoCredito := Nil;
  xTransacaoConsultaParcelas := Nil;
  xTransacaoResumoVendas := Nil;
  xTransacaoReimpressaoCupom := Nil;
  xConfirmaCartao := Nil;
  xFinalizaTransacao := Nil;
  xObtemLogUltimaTransacao := Nil;
  xVersaoDPOS := Nil;
  xInicializaDPOS := Nil;
  xFinalizaDPOS := Nil;
  xIdentificacaoAutomacaoComercial := Nil;
  xConfiguraModoDesfazimento := Nil;
  xConfiguraCNPJEstabelecimento := Nil;
  xConfiguraEmpresaLojaPDV := Nil;
  xConfiguraComunicacaoDTEF := Nil;
  xBuscaCertificado := Nil;
  xTransacaoCartaoCreditoCompleta := Nil;
  xTransacaoCartaoDebitoCompleta := Nil;
  xTransacaoCartaoVoucherCompleta := Nil;
  xTransacaoManualPOSCompleta := Nil;
end;

procedure TACBrTEFPayKitAPI.TratarErroPayKit(AErrorCode: LongInt);
var
  MsgErro: String;
begin
  case AErrorCode of
    RET_OK: MsgErro := '';
  else
    MsgErro := Format('Erro retornado: %d', [AErrorCode]);
  end;

  if (MsgErro <> '') then
    DoException(MsgErro);
end;

procedure TACBrTEFPayKitAPI.DoException(const AErrorMsg: String);
begin
  if (Trim(AErrorMsg) = '') then
    Exit;

  GravarLog('EACBrTEFPayKitAPI: '+AErrorMsg);
  raise EACBrTEFPayKitAPI.Create(ACBrStr(AErrorMsg));
end;

procedure TACBrTEFPayKitAPI.VerificarCarregada;
begin
  if not fCarregada then
    DoException(sErrLibNaoInicializada);
end;

procedure TACBrTEFPayKitAPI.VerificarDiretorioDeTrabalho;
begin
  if (fDiretorioTrabalho = '') then
    fDiretorioTrabalho := ApplicationPath + 'TEF' + PathDelim + 'PayKit';

  if not DirectoryExists(fDiretorioTrabalho) then
    ForceDirectories(fDiretorioTrabalho);

  if not DirectoryExists(fDiretorioTrabalho) then
    DoException(Format(sErrDirTrabalhoInvalido, [fDiretorioTrabalho]));
end;

procedure TACBrTEFPayKitAPI.VerificarEAjustarConf;
begin

end;

end.

