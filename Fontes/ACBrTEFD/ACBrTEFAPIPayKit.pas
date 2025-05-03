{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
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

unit ACBrTEFAPIPayKit;

interface

uses
  Classes, SysUtils,
  ACBrBase,
  ACBrTEFComum, ACBrTEFAPI, ACBrTEFAPIComum, ACBrTEFPayKitAPI;

const
  CSUBDIRETORIO_PAYKIT = 'paykit';

type

  { TACBrTEFRespPayKit }

  TACBrTEFRespPayKit = class( TACBrTEFResp )
  public
    procedure ConteudoToProperty; override;
  end;


  { TACBrTEFAPIClassPayKit }

  TACBrTEFAPIClassPayKit = class(TACBrTEFAPIClass)
  private
    fDiretorioTrabalho: String;
    fTEFPayKitAPI: TACBrTEFPayKitAPI;

    procedure QuandoGravarLogAPI(const ALogLine: String; var Tratado: Boolean);
    procedure SetDiretorioTrabalho(const AValue: String);

  protected
    procedure InterpretarRespostaAPI; override;

    function ExibirVersaoPayKit: Boolean;
  public
    constructor Create(AACBrTEFAPI: TACBrTEFAPIComum);
    destructor Destroy; override;

    procedure Inicializar; override;
    procedure DesInicializar; override;

    function EfetuarPagamento(
      ValorPagto: Currency;
      Modalidade: TACBrTEFModalidadePagamento = tefmpNaoDefinido;
      CartoesAceitos: TACBrTEFTiposCartao = [];
      Financiamento: TACBrTEFModalidadeFinanciamento = tefmfNaoDefinido;
      Parcelas: Byte = 0;
      DataPreDatado: TDateTime = 0;
      DadosAdicionais: String = ''): Boolean; override;

    function EfetuarAdministrativa(
      CodOperacaoAdm: TACBrTEFOperacao = tefopAdministrativo): Boolean; overload; override;
    function EfetuarAdministrativa(
      const CodOperacaoAdm: string = ''): Boolean; overload; override;

    function CancelarTransacao(
      const NSU, CodigoAutorizacaoTransacao: string;
      DataHoraTransacao: TDateTime;
      Valor: Double;
      const CodigoFinalizacao: string = '';
      const Rede: string = ''): Boolean; override;

    procedure FinalizarTransacao(
      const Rede, NSU, CodigoFinalizacao: String;
      AStatus: TACBrTEFStatusTransacao = tefstsSucessoAutomatico); override;

    procedure ResolverTransacaoPendente(AStatus: TACBrTEFStatusTransacao = tefstsSucessoManual); override;
    procedure AbortarTransacaoEmAndamento; override;

    property TEFPayKitAPI: TACBrTEFPayKitAPI read fTEFPayKitAPI;
    property DiretorioTrabalho: String read fDiretorioTrabalho write SetDiretorioTrabalho;
  end;

implementation

uses
  math, StrUtils,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO;

{ TACBrTEFRespPayKit }

procedure TACBrTEFRespPayKit.ConteudoToProperty;
begin
  inherited;
end;


{ TACBrTEFAPIClassPayKit }

constructor TACBrTEFAPIClassPayKit.Create(AACBrTEFAPI: TACBrTEFAPIComum);
begin
  inherited;

  fpTEFRespClass := TACBrTEFRespPayKit;

  fTEFPayKitAPI := TACBrTEFPayKitAPI.Create;
  fTEFPayKitAPI.OnGravarLog := QuandoGravarLogAPI;
end;

destructor TACBrTEFAPIClassPayKit.Destroy;
begin
  fTEFPayKitAPI.Free;
  inherited;
end;

procedure TACBrTEFAPIClassPayKit.Inicializar;
var
  P: Integer;
  ADir, IpStr, PortaStr: String;
begin
  if Inicializado then
    Exit;

  if (fDiretorioTrabalho = '') then
    ADir := PathWithDelim(fpACBrTEFAPI.DiretorioTrabalho) + CSUBDIRETORIO_PAYKIT + PathDelim
  else
    ADir := fDiretorioTrabalho;

  IpStr := fpACBrTEFAPI.DadosTerminal.EnderecoServidor;
  PortaStr := '';
  p := pos(':', IpStr);
  if (p > 0) then
  begin
    PortaStr := copy(IpStr, p+1, Length(IpStr));
    IpStr := copy(IpStr, 1, p-1);
  end;

  fTEFPayKitAPI.PathLib := PathDLL;
  fTEFPayKitAPI.DiretorioTrabalho := ADir;
  //fTEFPayKitAPI.EnderecoIP := IpStr;
  //fTEFPayKitAPI.PortaTCP := PortaStr;
  //fTEFPayKitAPI.VersaoAutomacao := fpACBrTEFAPI.DadosAutomacao.VersaoAplicacao;
  //fTEFPayKitAPI.Empresa := fpACBrTEFAPI.DadosTerminal.CodEmpresa;
  //fTEFPayKitAPI.Filial := fpACBrTEFAPI.DadosTerminal.CodFilial;
  //fTEFPayKitAPI.PDV := fpACBrTEFAPI.DadosTerminal.CodTerminal;
  //fTEFPayKitAPI.MsgPinPad := fpACBrTEFAPI.DadosAutomacao.NomeSoftwareHouse + '|' +
  //                          fpACBrTEFAPI.DadosAutomacao.NomeAplicacao + ' ' +
  //                          fpACBrTEFAPI.DadosAutomacao.VersaoAplicacao;
  //fTEFPayKitAPI.PortaPinPad := fpACBrTEFAPI.DadosTerminal.PortaPinPad;

  fTEFPayKitAPI.Inicializar;
  ExibirVersaoPayKit;

  inherited;
end;

procedure TACBrTEFAPIClassPayKit.DesInicializar;
begin
  fTEFPayKitAPI.DesInicializar;
  inherited;
end;

procedure TACBrTEFAPIClassPayKit.InterpretarRespostaAPI;
var
  i: Integer;
  AChave, AValue: String;
begin
  fpACBrTEFAPI.UltimaRespostaTEF.Clear;
  fpACBrTEFAPI.UltimaRespostaTEF.ViaClienteReduzida := fpACBrTEFAPI.DadosAutomacao.ImprimeViaClienteReduzida;

  //for i := 0 to fTEFPayKitAPI.DadosDaTransacao.Count-1 do
  //begin
  //  AChave := fTEFPayKitAPI.DadosDaTransacao.Names[i];
  //  AValue := fTEFPayKitAPI.DadosDaTransacao.ValueFromIndex[i];
  //
  //  fpACBrTEFAPI.UltimaRespostaTEF.Conteudo.GravaInformacao(AChave, AValue);
  //end;

  fpACBrTEFAPI.UltimaRespostaTEF.ConteudoToProperty;
end;

function TACBrTEFAPIClassPayKit.ExibirVersaoPayKit: Boolean;
var
  s: String;
begin
  s := Trim(fTEFPayKitAPI.VersaoDPOS);
  Result := (s <> '');
  if Result then
    TACBrTEFAPI(fpACBrTEFAPI).QuandoExibirMensagem(s, telaOperador, 0);
end;

procedure TACBrTEFAPIClassPayKit.QuandoGravarLogAPI(const ALogLine: String;
  var Tratado: Boolean);
begin
  fpACBrTEFAPI.GravarLog(ALogLine);
  Tratado := True;
end;

function TACBrTEFAPIClassPayKit.EfetuarAdministrativa(CodOperacaoAdm: TACBrTEFOperacao): Boolean;
var
  Param1: String;
begin
  //fTEFPayKitAPI.DadosDaTransacao.Clear;
  Result := False;
  //if CodOperacaoAdm = tefopAdministrativo then
  //  CodOperacaoAdm := PerguntarMenuAdmScope;

  Result := True;
  Param1 := '';

  case CodOperacaoAdm of
    tefopVersao:
      Result := ExibirVersaoPayKit();
  end;
end;

function TACBrTEFAPIClassPayKit.EfetuarAdministrativa(const CodOperacaoAdm: string): Boolean;
begin
  Result := EfetuarAdministrativa( TACBrTEFOperacao(StrToIntDef(CodOperacaoAdm, 0)) );
end;

function TACBrTEFAPIClassPayKit.CancelarTransacao(const NSU,
  CodigoAutorizacaoTransacao: string; DataHoraTransacao: TDateTime;
  Valor: Double; const CodigoFinalizacao: string; const Rede: string): Boolean;
begin
end;

function TACBrTEFAPIClassPayKit.EfetuarPagamento(ValorPagto: Currency;
  Modalidade: TACBrTEFModalidadePagamento; CartoesAceitos: TACBrTEFTiposCartao;
  Financiamento: TACBrTEFModalidadeFinanciamento; Parcelas: Byte;
  DataPreDatado: TDateTime; DadosAdicionais: String): Boolean;
begin
end;

procedure TACBrTEFAPIClassPayKit.FinalizarTransacao(const Rede, NSU,
  CodigoFinalizacao: String; AStatus: TACBrTEFStatusTransacao);
begin
end;

procedure TACBrTEFAPIClassPayKit.ResolverTransacaoPendente(AStatus: TACBrTEFStatusTransacao);
begin
  FinalizarTransacao( fpACBrTEFAPI.UltimaRespostaTEF.Rede,
                      fpACBrTEFAPI.UltimaRespostaTEF.NSU,
                      fpACBrTEFAPI.UltimaRespostaTEF.Finalizacao,
                      AStatus );
end;

procedure TACBrTEFAPIClassPayKit.AbortarTransacaoEmAndamento;
begin
  //fTEFPayKitAPI.AbortarTransacao;
end;

procedure TACBrTEFAPIClassPayKit.SetDiretorioTrabalho(const AValue: String);
begin
  if fDiretorioTrabalho = AValue then Exit;

  if Inicializado then
    fpACBrTEFAPI.DoException(Format(ACBrStr(sACBrTEFAPIComponenteInicializadoException),
                                     ['TACBrTEFAPIClassPayKit.DiretorioTrabalho']));

  fDiretorioTrabalho := AValue;
end;

end.
