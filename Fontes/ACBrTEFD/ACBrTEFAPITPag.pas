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

unit ACBrTEFAPITPag;

interface

uses
  Classes, SysUtils,
  ACBrBase,
  ACBrTEFComum, ACBrTEFAPI, ACBrTEFAPIComum, ACBrTEFTPagAPI;

type

  { TACBrTEFRespTPag }

  TACBrTEFRespTPag = class( TACBrTEFResp )
  public
    procedure ConteudoToProperty; override;
  end;


  { TACBrTEFAPIClassTPag }

  TACBrTEFAPIClassTPag = class(TACBrTEFAPIClass)
  private
    function GetTEFTPagAPI: TACBrTEFTPagAPI;
    procedure QuandoGravarLogAPI(const ALogLine: String; var Tratado: Boolean);
    procedure QuandoExibirMensagemAPI(const Mensagem: String);

  protected
    procedure InterpretarRespostaAPI; override;

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

    property TEFTPagAPI: TACBrTEFTPagAPI read GetTEFTPagAPI;
  end;

implementation

uses
  TypInfo,
  ACBrUtil.Strings;

{ TACBrTEFRespTPag }

procedure TACBrTEFRespTPag.ConteudoToProperty;
begin
  inherited ConteudoToProperty;
end;

{ TACBrTEFAPIClassTPag }

constructor TACBrTEFAPIClassTPag.Create(AACBrTEFAPI: TACBrTEFAPIComum);
begin
  inherited;

  fpTEFRespClass := TACBrTEFRespTPag;

  with GetTEFTPagAPI do
  begin
    OnGravarLog := QuandoGravarLogAPI;
    OnExibeMensagem := QuandoExibirMensagemAPI;
  end;
end;

destructor TACBrTEFAPIClassTPag.Destroy;
begin
  //fTEFTPagAPI.Free;
  inherited Destroy;
end;

procedure TACBrTEFAPIClassTPag.Inicializar;
begin
  if Inicializado then
    Exit;

  with GetTEFTPagAPI do
  begin
    PathLib := PathDLL;
    CNPJEmpresa := fpACBrTEFAPI.DadosEstabelecimento.CNPJ;
    Inicializar;
  end;

  inherited;
end;

procedure TACBrTEFAPIClassTPag.DesInicializar;
begin
  GetTEFTPagAPI.DesInicializar;
  inherited;
end;

function TACBrTEFAPIClassTPag.GetTEFTPagAPI: TACBrTEFTPagAPI;
begin
  Result := ACBrTEFTPagAPI.GetTEFTPagAPI;
end;

procedure TACBrTEFAPIClassTPag.QuandoGravarLogAPI(const ALogLine: String;
  var Tratado: Boolean);
begin
  fpACBrTEFAPI.GravarLog(ALogLine);
  Tratado := True;
end;

procedure TACBrTEFAPIClassTPag.QuandoExibirMensagemAPI(const Mensagem: String);
begin
  TACBrTEFAPI(fpACBrTEFAPI).QuandoExibirMensagem( Mensagem, telaTodas, -1);
end;

procedure TACBrTEFAPIClassTPag.InterpretarRespostaAPI;
begin
  inherited InterpretarRespostaAPI;
end;

function TACBrTEFAPIClassTPag.EfetuarPagamento(ValorPagto: Currency;
  Modalidade: TACBrTEFModalidadePagamento; CartoesAceitos: TACBrTEFTiposCartao;
  Financiamento: TACBrTEFModalidadeFinanciamento; Parcelas: Byte;
  DataPreDatado: TDateTime; DadosAdicionais: String): Boolean;
var
  Params: TACBrTEFTPagTransactionParams;
  ret: LongInt;
begin
  Params.amount := Trunc(ValorPagto * 100);
  if not (Modalidade in [tefmpNaoDefinido, tefmpCartao]) then
    fpACBrTEFAPI.DoException(Format(ACBrStr(sACBrTEFAPICapturaNaoSuportada),
      [GetEnumName(TypeInfo(TACBrTEFModalidadePagamento), integer(Modalidade) ), ClassName] ));

  Params.cardType := CardType_NONE;

  if (teftcCredito in CartoesAceitos) then
    Params.transactionType := TransactionType_CREDIT
  else if (teftcDebito in CartoesAceitos) then
    Params.transactionType := TransactionType_DEBIT
  else if (teftcVoucher in CartoesAceitos) then
    Params.transactionType := TransactionType_VOUCHER
  else
    Params.transactionType := TransactionType_CREDIT;

  if (Financiamento > tefmfAVista) then
    Params.creditType := CreditType_INSTALLMENT
  else
    Params.creditType := CreditType_NO_INSTALLMENT;

  Params.isTyped := 0;
  Params.installment := Parcelas;

  ret := GetTEFTPagAPI.ExecutarTransacao(Params);
end;

function TACBrTEFAPIClassTPag.EfetuarAdministrativa(
  CodOperacaoAdm: TACBrTEFOperacao): Boolean;
begin
  Result := inherited EfetuarAdministrativa(CodOperacaoAdm);
end;

function TACBrTEFAPIClassTPag.EfetuarAdministrativa(const CodOperacaoAdm: string
  ): Boolean;
begin
  Result := inherited EfetuarAdministrativa(CodOperacaoAdm);
end;

function TACBrTEFAPIClassTPag.CancelarTransacao(const NSU,
  CodigoAutorizacaoTransacao: string; DataHoraTransacao: TDateTime;
  Valor: Double; const CodigoFinalizacao: string; const Rede: string): Boolean;
begin
  Result := inherited CancelarTransacao(NSU, CodigoAutorizacaoTransacao,
    DataHoraTransacao, Valor, CodigoFinalizacao, Rede);
end;

procedure TACBrTEFAPIClassTPag.FinalizarTransacao(const Rede, NSU,
  CodigoFinalizacao: String; AStatus: TACBrTEFStatusTransacao);
begin
  inherited FinalizarTransacao(Rede, NSU, CodigoFinalizacao, AStatus);
end;

procedure TACBrTEFAPIClassTPag.ResolverTransacaoPendente(
  AStatus: TACBrTEFStatusTransacao);
begin
  inherited ResolverTransacaoPendente(AStatus);
end;

procedure TACBrTEFAPIClassTPag.AbortarTransacaoEmAndamento;
begin
  inherited AbortarTransacaoEmAndamento;
end;

end.

