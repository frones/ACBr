{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  José M S Junior, Victor H Gonzales - Pandaaa   }
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
unit ACBrBoletoWS;

interface

uses
  Classes,
  SysUtils,
  ACBrBoleto,
  pcnGerador,
  pcnLeitor,
  ACBrUtil.Strings,
  pcnConversao,
  synacode,
  synautil,
  ACBrJSON,
  ACBrBoletoConversao,
  ACBrBoletoRetorno,
  ACBrDFeSSL,
  dateutils,
  strutils,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.XMLHTML,
  httpsend,
  ACBrBoletoWS.Rest.OAuth;

type

  EACBrBoletoWSException = class(Exception);

  TBoletoWS          = class;
  TRetornoEnvioClass = class;

    { TBoletoWSClass }
  TBoletoWSClass = class
  private
    FGerador     : TGerador;
    FBoleto      : TACBrBoleto;
    FTitulo      : TACBrTitulo;
    FDFeSSL      : TDFeSSL;
    FHTTPSend    : THTTPSend;
    FBoletoWS    : TBoletoWS;
    FRetornoBanco: TRetornoEnvioClass;
    FOAuth       : TOAuth;

  protected

    FRetornoWS   : String;
    FPDadosMsg   : String;
    FTipoRegistro: String;

    function GerarRemessa: String; virtual;
    function Enviar: Boolean; virtual;

    property DFeSSL: TDFeSSL read FDFeSSL write FDFeSSL;
    property httpsend: THTTPSend read FHTTPSend write FHTTPSend;
    property BoletoWS: TBoletoWS read FBoletoWS;
    property Gerador: TGerador read FGerador;
    property Boleto: TACBrBoleto read FBoleto;
    property ATitulo: TACBrTitulo read FTitulo;
    property RetornoBanco: TRetornoEnvioClass read FRetornoBanco write FRetornoBanco;
    property OAuth: TOAuth read FOAuth write FOAuth;

  public
    constructor Create(ABoletoWS: TBoletoWS); virtual;
    destructor Destroy; Override;

  end;

    { BoletoWS }
{$IFDEF RTL230_UP}
  [ ComponentPlatformsAttribute(pidWin32 or pidWin64) ]
{$ENDIF RTL230_UP}

  TBoletoWS = class(TACBrWebService)
  private
    FBanco        : TACBrTipoCobranca;
    FBoletoWSClass: TBoletoWSClass;
    FBoleto       : TACBrBoleto;
    FRetornoBanco : TRetornoEnvioClass;
    FRetornoWS    : String;
    FArqLOG       : String;

    procedure SetBanco(ABanco: TACBrTipoCobranca);
    procedure GravaLog(const AString: AnsiString);

    procedure Clear;

  protected

    property Banco: TACBrTipoCobranca read FBanco write SetBanco;

  public
    constructor Create(AOwner: TComponent); Override;
    destructor Destroy; override;
    procedure DoLog(const AString: String);
    function Enviar: Boolean; override;
    property RetornoBanco: TRetornoEnvioClass read FRetornoBanco;

  end;

    { TRetornoEnvioClass }
{$IFDEF RTL230_UP}
  [ ComponentPlatformsAttribute(pidWin32 or pidWin64) ]
{$ENDIF RTL230_UP}

  TRetornoEnvioClass = class
  private
    FACBrBoleto    : TACBrBoleto;
    FRetWS         : String;
    FEnvWS         : String;
    FCodRetorno    : Integer;
    FMsg           : String;
    FLeitor        : TLeitor;
    FHTTPResultCode: Integer;
  protected
    function LerListaRetorno: Boolean; virtual;
    function LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean; virtual;
    function RetornoEnvio(const AIndex: Integer): Boolean; virtual;

    property ACBrBoleto: TACBrBoleto read FACBrBoleto;
    property Leitor: TLeitor read FLeitor;
    property RetWS: String read FRetWS write FRetWS;
    property EnvWs: String read FEnvWS;

  public
    constructor Create(ABoletoWS: TACBrBoleto); virtual;
    destructor Destroy; Override;

    property Msg: String read FMsg write FMsg;
    property CodRetorno: Integer read FCodRetorno write FCodRetorno;
    property HTTPResultCode: Integer read FHTTPResultCode write FHTTPResultCode;

  end;

Const
  C_LER_RETORNO             = 'LerRetorno';
  C_LER_LISTA_RETORNO       = 'LerListaRetorno';
  C_RETORNO_ENVIO           = 'Retorno Envio';
  C_DFESSL                  = 'DFeSSL';
  C_OBTER_NOME_ARQUIVO      = 'Obter Nome Arquivo';
  C_DOLOG                   = 'DoLog';
  C_GERAR_REMESSA           = 'Gerar Remessa';
  C_ENVIAR                  = 'Enviar';
  C_REGISTRO_BOLETO         = 'registro_boleto';
  C_DEFINIR_SERVICO_EACTION = 'DefinirServicoEAction';
  C_DEFINIR_URL             = 'DefinirURL';
  c_DEFINIR_ENVELOPE_SOAP   = 'DefinirEnvelopeSoap';
  C_GERAR_HEADER            = 'GerarHeader';
  C_GERAR_DADOS             = 'GerarDados';
  C_DEFINIR_ROOT_ELEMENT    = 'RootElement';
  C_NO_CACHE                = 'no-cache';
  C_GRANT_TYPE              = 'grant_type';
  C_SCOPE                   = 'scope';
  C_CONTENT_TYPE            = 'Content-Type';
  C_CACHE_CONTROL           = 'Cache-Control';
  C_AUTHORIZATION           = 'Authorization';
  C_ACCESS_TOKEN            = 'access_token';
  C_ACCEPT                  = 'Accept';
  C_XML                     = 'xml';
  C_JSON                    = 'json';
  C_ID                      = 'id';

  C_ARQBOLETOWS_LOG  = 'ArqBoletoWS.log';
  C_RETORNO_REGISTRO = 'retorno_registro';
  C_ERRO             = 'erro';
  C_ERROR_CODE       = 'Error_Code ';
  C_HTTP_RESULT_CODE = 'HTTP_Result_Code ';

ResourceString
  S_METODO_NAO_IMPLEMENTADO = 'Metodo %s nao Implementado ';
  S_OPERACAO_NAO_IMPLEMENTADO = 'Operação %s nao Implementado para este Banco';
  S_ERRO_GERAR_TOKEN_AUTENTICACAO = 'Erro ao gerar token de Autenticação: %s';

implementation

uses
  ACBrBoletoW_Caixa,
  ACBrBoletoRet_Caixa,
  ACBrBoletoW_BancoBrasil,
  ACBrBoletoRet_BancoBrasil,
  ACBrBoletoW_BancoBrasil_API,
  ACBrBoletoRet_BancoBrasil_API,
  ACBrBoletoW_Itau,
  ACBrBoletoRet_Itau,
  ACBrBoletoW_Credisis,
  ACBrBoletoRet_Credisis,
  ACBrBoletoW_Sicredi_APIECOMM,
  ACBrBoletoRet_Sicredi_APIECOMM,
  ACBrBoletoW_Sicredi_APIV2,
  ACBrBoletoRet_Sicredi_APIV2,
  ACBrBoletoW_PenseBank_API,
  ACBrBoletoRet_PenseBank_API,
  ACBrBoletoW_Santander,
  ACBrBoletoRet_Santander,
  ACBrBoletoW_Santander_API,
  ACBrBoletoRet_Santander_API,
  ACBrBoletoW_Inter_API,
  ACBrBoletoRet_Inter_API,
  ACBrBoletoW_Bancoob,
  ACBrBoletoRet_Bancoob,
  ACBrBoletoW_Itau_API,
  ACBrBoletoRet_Itau_API,
  ACBrBoletoW_Safra,
  ACBrBoletoRet_Safra,
  ACBrBoletoW_Bancoob_APIV3,
  ACBrBoletoRet_Bancoob_APIV3;

  { TRetornoEnvioClass }

constructor TRetornoEnvioClass.Create(ABoletoWS: TACBrBoleto);
begin
  FRetWS      := '';
  FCodRetorno := 0;
  FMsg        := '';
  FLeitor     := TLeitor.Create;
  FACBrBoleto := ABoletoWS;

end;

destructor TRetornoEnvioClass.Destroy;
begin
  FLeitor.Free;
  inherited Destroy;
end;

function TRetornoEnvioClass.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
begin
  Result := False;
  raise EACBrBoletoWSException.Create(ACBrStr(ClassName + Format(S_METODO_NAO_IMPLEMENTADO, [ C_LER_RETORNO ])));

end;

function TRetornoEnvioClass.LerListaRetorno: Boolean;
begin
  Result := False;
  raise EACBrBoletoWSException.Create(ACBrStr(ClassName + Format(S_METODO_NAO_IMPLEMENTADO, [ C_LER_LISTA_RETORNO ])));

end;

function TRetornoEnvioClass.RetornoEnvio(const AIndex: Integer): Boolean;
begin
  Result := False;
  raise EACBrBoletoWSException.Create(ACBrStr(ClassName + Format(S_METODO_NAO_IMPLEMENTADO, [ C_RETORNO_ENVIO ])));

end;

  { TBoletoWSClass }

constructor TBoletoWSClass.Create(ABoletoWS: TBoletoWS);
begin
  FTipoRegistro := C_XML;
  FBoletoWS     := ABoletoWS;
  FGerador      := TGerador.Create;
  FHTTPSend     := THTTPSend.Create;
  FHTTPSend.Protocol := '1.1';
  FTitulo       := nil;

  if Assigned(ABoletoWS.FBoleto.Configuracoes.WebService) then
    FDFeSSL := TDFeSSL(ABoletoWS.FBoleto.Configuracoes.WebService);

  FOAuth := TOAuth.Create(FHTTPSend, ABoletoWS.FBoleto);

end;

destructor TBoletoWSClass.Destroy;
begin
  FGerador.Free;
  FOAuth.Free;
  FHTTPSend.Destroy;
  inherited Destroy;
end;

function TBoletoWSClass.GerarRemessa: String;
begin
  Result := '';
  raise EACBrBoletoWSException.Create(ACBrStr(ClassName + Format(S_METODO_NAO_IMPLEMENTADO, [ C_GERAR_REMESSA ])));
end;

function TBoletoWSClass.Enviar: Boolean;
begin
  Result := False;
  raise EACBrBoletoWSException.Create(ACBrStr(ClassName + Format(S_METODO_NAO_IMPLEMENTADO, [ C_ENVIAR ])));
end;

  { TBoletoWS }

procedure TBoletoWS.SetBanco(ABanco: TACBrTipoCobranca);
begin
  if ABanco = FBanco then
    exit;

  if Assigned(FBoletoWSClass) then
    FreeAndNil(FBoletoWSClass);

  if Assigned(FRetornoBanco) then
    FreeAndNil(FRetornoBanco);

  case ABanco of
    cobSicred:
      begin
        if UpperCase(FBoleto.Configuracoes.WebService.VersaoDF) = 'V2' then
        begin //API V2 (NOVA 2022)
          FBoletoWSClass := TBoletoW_Sicredi_APIV2.Create(Self);
          FRetornoBanco  := TRetornoEnvio_Sicredi_APIV2.Create(FBoleto);
        end
        else
        begin //API ECOMM
          FBoletoWSClass := TBoletoW_Sicredi_APIECOMM.Create(Self);
          FRetornoBanco  := TRetornoEnvio_Sicredi_APIECOMM.Create(FBoleto);
        end;
      end;
    cobCaixaEconomica:
      begin
        FBoletoWSClass := TBoletoW_Caixa.Create(Self);
        FRetornoBanco  := TRetornoEnvio_Caixa.Create(FBoleto);
      end;
    cobBancoDoBrasilWS:
      begin
        FBoletoWSClass := TBoletoW_BancoBrasil.Create(Self);
        FRetornoBanco  := TRetornoEnvio_BancoBrasil.Create(FBoleto);
      end;
    cobBancoDoBrasilAPI:
      begin
        FBoletoWSClass := TBoletoW_BancoBrasil_API.Create(Self);
        FRetornoBanco  := TRetornoEnvio_BancoBrasil_API.Create(FBoleto);
      end;
    cobItau:
      begin
        if UpperCase(FBoleto.Configuracoes.WebService.VersaoDF) = 'V2' then
        begin //API V2 (NOVA 2023)
          FBoletoWSClass := TBoletoW_Itau_API.Create(Self);
          FRetornoBanco  := TRetornoEnvio_Itau_API.Create(FBoleto);
        end
        else
        begin
          FBoletoWSClass := TBoletoW_Itau.Create(Self);
          FRetornoBanco  := TRetornoEnvio_Itau.Create(FBoleto);
        end;

      end;
    cobCrediSIS:
      begin
        FBoletoWSClass := TBoletoW_Credisis.Create(Self);
        FRetornoBanco  := TRetornoEnvio_Credisis.Create(FBoleto);
      end;
    cobPenseBankAPI:
      begin
        FBoletoWSClass := TBoletoW_PenseBank_API.Create(Self);
        FRetornoBanco  := TRetornoEnvio_PenseBank_API.Create(FBoleto);
      end;
    cobSantander:
      begin
        if UpperCase(FBoleto.Configuracoes.WebService.VersaoDF) = 'V1' then
        begin //API V1
          FBoletoWSClass := TBoletoW_Santander_API.Create(Self);
          FRetornoBanco  := TRetornoEnvio_Santander_API.Create(FBoleto);
        end
        else
        begin // WS
          FBoletoWSClass := TBoletoW_Santander.Create(Self);
          FRetornoBanco  := TRetornoEnvio_Santander.Create(FBoleto);
        end;
      end;
    cobBancoInter:
      begin
        FBoletoWSClass := TBoletoW_Inter_API.Create(Self);
        FRetornoBanco  := TRetornoEnvio_Inter_API.Create(FBoleto);
      end;
    cobBancoob:
      begin
        if UpperCase(FBoleto.Configuracoes.WebService.VersaoDF) = 'V3' then
        begin
        FBoletoWSClass := TBoletoW_Bancoob_APIV3.Create(Self);
        FRetornoBanco  := TRetornoEnvio_Bancoob_APIV3.Create(FBoleto);
        end
        else
        begin
        FBoletoWSClass := TBoletoW_Bancoob.Create(Self);
        FRetornoBanco  := TRetornoEnvio_Bancoob.Create(FBoleto);
        end
      end;
    cobBancoSafra:
      begin
        FBoletoWSClass := TBoletoW_Safra.Create(Self);
        FRetornoBanco  := TRetornoEnvio_Safra.Create(FBoleto);
      end;
    else
      FBoletoWSClass := TBoletoWSClass.Create(Self);
      FRetornoBanco  := TRetornoEnvioClass.Create(FBoleto);
  end;
  FBoletoWSClass.FBoleto := FBoleto;
  FBanco                 := ABanco;
end;

constructor TBoletoWS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if Assigned(AOwner) and (AOwner is TACBrBoleto) then
    FBoleto := TACBrBoleto(AOwner)
  else
    FBoleto := Nil;

  if (FBoleto.Configuracoes.Arquivos.LogNivel > logNenhum) and not DirectoryExists(FBoleto.Configuracoes.Arquivos.PathGravarRegistro) then
    ForceDirectories(FBoleto.Configuracoes.Arquivos.PathGravarRegistro);

  if FBoleto.Configuracoes.Arquivos.NomeArquivoLog = '' then
    FBoleto.Configuracoes.Arquivos.NomeArquivoLog := C_ARQBOLETOWS_LOG;

  if not (DirectoryExists(FBoleto.Configuracoes.Arquivos.PathGravarRegistro)) then
    FArqLOG := FBoleto.Configuracoes.Arquivos.NomeArquivoLog
  else
    FArqLOG := PathWithDelim(FBoleto.Configuracoes.Arquivos.PathGravarRegistro) + ExtractFileName(FBoleto.Configuracoes.Arquivos.NomeArquivoLog);

  Clear;
end;

procedure TBoletoWS.Clear;
begin
  if Assigned(FBoletoWSClass) then
    FBoletoWSClass.Free;

  FBoletoWSClass := TBoletoWSClass.Create(Self);
end;

procedure TBoletoWS.DoLog(const AString: String);
var
  Tratado: Boolean;
  LLog : String;
begin
  Tratado := False;
  LLog := NativeStringToAnsi(AString);
  if Assigned(FBoleto.Configuracoes.Arquivos.OnGravarLog) then
    FBoleto.Configuracoes.Arquivos.OnGravarLog(LLog, Tratado);

  if Tratado or (FBoleto.Configuracoes.Arquivos.LogNivel > logNenhum) then
    GravaLog(LLog);

end;

procedure TBoletoWS.GravaLog(const AString: AnsiString);
begin
  if (FArqLOG = '') then
    exit;

  WriteLog(FArqLOG, FormatDateTime('dd/mm/yy hh:nn:ss:zzz', now) + ' - ' + AString);
end;

destructor TBoletoWS.Destroy;
begin
  if Assigned(FBoletoWSClass) then
    FreeAndNil(FBoletoWSClass);

  if Assigned(FRetornoBanco) then
    FreeAndNil(FRetornoBanco);

  inherited Destroy;

end;

function TBoletoWS.Enviar: Boolean;
var
  indice    : Integer;
  LJsonEnvio: String;
begin
  Banco  := FBoleto.Banco.TipoCobranca;
  Result := False;

  try
    if FBoleto.ListadeBoletos.Count > 0 then
    begin
      for indice := 0 to Pred(FBoleto.ListadeBoletos.Count) do
      begin
        FBoletoWSClass.FTitulo := FBoleto.ListadeBoletos[ indice ];
        LJsonEnvio             := FBoletoWSClass.GerarRemessa;
        Result                 := FBoletoWSClass.Enviar;
        FRetornoWS             := FBoletoWSClass.FRetornoWS;

        RetornoBanco.RetWS  := FRetornoWS;
        RetornoBanco.FEnvWS := LJsonEnvio;
        RetornoBanco.RetornoEnvio(indice);
      end;
    end
    else
      if (FBoleto.Configuracoes.WebService.Operacao in [ tpConsulta ]) then //Apenas Consulta Genérica não precisa carregar Titulo na Lista
      begin
        FBoletoWSClass.GerarRemessa;
        Result             := FBoletoWSClass.Enviar;
        FRetornoWS         := FBoletoWSClass.FRetornoWS;
        RetornoBanco.RetWS := FRetornoWS;
        RetornoBanco.RetornoEnvio(0);
      end;
  except
    on E: Exception do
    begin
      if not (Assigned(FBoletoWSClass.RetornoBanco)) or ((FBoletoWSClass.RetornoBanco.CodRetorno = 0) and (Trim(FBoletoWSClass.RetornoBanco.Msg) = '')) then
        DoLog('Falha Envio: ' + ACBrStr(E.Message))
      else
        DoLog('Erro Envio: ' + ACBrStr(IntToStr(FBoletoWSClass.RetornoBanco.CodRetorno) + sLineBreak + FBoletoWSClass.RetornoBanco.Msg + sLineBreak + E.Message));
      raise;
    end;
  end;
end;

end.
