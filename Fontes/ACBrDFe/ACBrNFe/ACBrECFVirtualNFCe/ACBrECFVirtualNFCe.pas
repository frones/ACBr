{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrECFVirtualNFCe;

interface

uses Classes, SysUtils,
{$IFDEF FPC}LResources, {$ENDIF}
  ACBrECFVirtual, ACBrECFVirtualPrinter, ACBrNFe, ACBrECF, ACBrDevice, ACBrBase,
  pcnNFe, pcnConversao;

const
  ACBrECFVirtualNFCe_VERSAO = '0.1.0a';
  cItemCancelado = '*cancelado*';
  estCupomAberto = [estVenda, estPagamento];

type

  TACBrECFVirtualNFCeQuandoAbrirDocumento = procedure(NFe: TNFe) of object;

  TACBrECFVirtualNFCeQuandoVenderItem = procedure(Det: TDetCollectionItem) of object;

  TACBrECFVirtualNFCeQuandoEfetuarPagamento = procedure(Det: TpagCollectionItem) of object;

  TACBrECFVirtualNFCeQuandoFecharDocumento = procedure(NFe: TNFe) of object;

  TACBrECFVirtualNFCeQuandoCancelarDocumento = procedure(Justificativa: string) of object;

  TACBrECFVirtualNFCeQuandoImprimirDocumento = procedure(var Tratado: Boolean) of object;

  { TACBrECFVirtualNFCe }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrECFVirtualNFCe = class(TACBrECFVirtualPrinter)
  private
    function GetACBrNFCe: TACBrNFe;
    function GetImprimir2ViaOffLine: Boolean;
    function GetTratarDescontoNoItem: Boolean;
    function GetQuandoAbrirDocumento: TACBrECFVirtualNFCeQuandoAbrirDocumento;
    function GetQuandoEfetuarPagamento: TACBrECFVirtualNFCeQuandoEfetuarPagamento;
    function GetQuandoVenderItem: TACBrECFVirtualNFCeQuandoVenderItem;
    function GetQuandoFecharDocumento: TACBrECFVirtualNFCeQuandoFecharDocumento;
    function GetQuandoCancelarDocumento: TACBrECFVirtualNFCeQuandoCancelarDocumento;
    function GetQuandoImprimirDocumento: TACBrECFVirtualNFCeQuandoImprimirDocumento;

    procedure SetQuandoAbrirDocumento(
      AValue: TACBrECFVirtualNFCeQuandoAbrirDocumento);
    procedure SetQuandoEfetuarPagamento(
      AValue: TACBrECFVirtualNFCeQuandoEfetuarPagamento);
    procedure SetQuandoVenderItem(AValue: TACBrECFVirtualNFCeQuandoVenderItem);
    procedure SetQuandoFecharDocumento(
      AValue: TACBrECFVirtualNFCeQuandoFecharDocumento);
    procedure SetQuandoCancelarDocumento(
      AValue: TACBrECFVirtualNFCeQuandoCancelarDocumento);
    procedure SetACBrNFCe(AValue: TACBrNFe);
    procedure SetImprimir2ViaOffLine(AValue: Boolean);
    procedure SetTratarDescontoNoItem(AValue: Boolean);
    procedure SetQuandoImprimirDocumento(const AValue: TACBrECFVirtualNFCeQuandoImprimirDocumento);
  protected
    procedure CreateVirtualClass; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property NomeArqINI;
    property ACBrNFCe: TACBrNFe read GetACBrNFCe write SetACBrNFCe;
    property Imprimir2ViaOffLine : Boolean read GetImprimir2ViaOffLine write SetImprimir2ViaOffLine default True;
    property TratarDescontoNoItem : Boolean read GetTratarDescontoNoItem write SetTratarDescontoNoItem default false;
    property QuandoAbrirDocumento: TACBrECFVirtualNFCeQuandoAbrirDocumento
      read GetQuandoAbrirDocumento write SetQuandoAbrirDocumento;
    property QuandoVenderItem: TACBrECFVirtualNFCeQuandoVenderItem
      read GetQuandoVenderItem write SetQuandoVenderItem;
    property QuandoEfetuarPagamento: TACBrECFVirtualNFCeQuandoEfetuarPagamento
      read GetQuandoEfetuarPagamento write SetQuandoEfetuarPagamento;
    property QuandoFecharDocumento: TACBrECFVirtualNFCeQuandoFecharDocumento
      read GetQuandoFecharDocumento write SetQuandoFecharDocumento;
    property QuandoCancelarDocumento: TACBrECFVirtualNFCeQuandoCancelarDocumento
      read GetQuandoCancelarDocumento write SetQuandoCancelarDocumento;
    property QuandoImprimirDocumento: TACBrECFVirtualNFCeQuandoImprimirDocumento
      read GetQuandoImprimirDocumento write SetQuandoImprimirDocumento;
  end;

  { TACBrECFVirtualNFCeClass }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrECFVirtualNFCeClass = class(TACBrECFVirtualPrinterClass)
  private
    function AdivinharFormaPagamento(const DescricaoPagto: string): TpcnFormaPagamento;
    procedure SomaTotais;
    procedure CancelarNFCe;
    function GetACBrECF: TACBrECF;
    procedure FazerImpressaoDocumento;
  protected
    fsACBrNFCe: TACBrNFe;
    fsECF: TACBrECF;
    fsNomeArqTempXML: string;
    fsQuandoAbrirDocumento: TACBrECFVirtualNFCeQuandoAbrirDocumento;
    fsQuandoEfetuarPagamento: TACBrECFVirtualNFCeQuandoEfetuarPagamento;
    fsQuandoVenderItem: TACBrECFVirtualNFCeQuandoVenderItem;
    fsQuandoFecharDocumento: TACBrECFVirtualNFCeQuandoFecharDocumento;
    fsQuandoCancelarDocumento: TACBrECFVirtualNFCeQuandoCancelarDocumento;
    fsQuandoImprimirDocumento: TACBrECFVirtualNFCeQuandoImprimirDocumento;
    fsImprimir2ViaOffLine : Boolean;
    fsTratarDescontoNoItem : Boolean;
    fsEhVenda: Boolean;
    fsDestCNPJ: string;
    fsDestNome: string;
    fsvTotTrib, fsvBC, fsvICMS, fsvBCST, fsvST, fsvProd, fsvFrete: Currency;
    fsvSeg, fsvDesc, fsvII, fsvIPI, fsvPIS, fsvCOFINS, fsvOutro, fsvNF: Currency;
    function GetSubModeloECF: string; override;
    function GetNumVersao: string; override;
    procedure AtivarVirtual; override;

    procedure AbreDocumentoVirtual; override;
    procedure EnviaConsumidorVirtual; override;
    procedure VendeItemVirtual(ItemCupom: TACBrECFVirtualClassItemCupom); override;
    procedure DescontoAcrescimoItemAnteriorVirtual(
      ItemCupom: TACBrECFVirtualClassItemCupom; PorcDesc: Double); override;
    procedure CancelaItemVendidoVirtual(NumItem: Integer); override;
    procedure SubtotalizaCupomVirtual(MensagemRodape: AnsiString = ''); override;
    procedure EfetuaPagamentoVirtual(Pagto: TACBrECFVirtualClassPagamentoCupom); override;
    procedure FechaCupomVirtual(Observacao: AnsiString = ''; IndiceBMP: Integer = 0); override;
    procedure VerificaPodeCancelarCupom(NumCOOCancelar: Integer = 0); override;
    procedure CancelaCupomVirtual; override;
    procedure CancelaDescontoAcrescimoItemVirtual(
      ItemCupom: TACBrECFVirtualClassItemCupom; TipoAcrescimoDesconto: String =
      'D'); override;

    procedure LeArqINIVirtual(ConteudoINI: TStrings); override;
    procedure GravaArqINIVirtual(ConteudoINI: TStrings); override;

    property ECF: TACBrECF read GetACBrECF;

  public
    constructor Create(AECFVirtualPrinter: TACBrECFVirtualPrinter); overload; override;
    property ACBrNFCe: TACBrNFe read fsACBrNFCe write fsACBrNFCe;
    property Imprimir2ViaOffLine : Boolean read fsImprimir2ViaOffLine write fsImprimir2ViaOffLine;
    property TratarDescontoNoItem : Boolean read fsTratarDescontoNoItem write fsTratarDescontoNoItem;
    property QuandoAbrirDocumento: TACBrECFVirtualNFCeQuandoAbrirDocumento
      read fsQuandoAbrirDocumento write fsQuandoAbrirDocumento;
    property QuandoVenderItem: TACBrECFVirtualNFCeQuandoVenderItem
      read fsQuandoVenderItem write fsQuandoVenderItem;
    property QuandoEfetuarPagamento: TACBrECFVirtualNFCeQuandoEfetuarPagamento
      read fsQuandoEfetuarPagamento write fsQuandoEfetuarPagamento;
    property QuandoFecharDocumento: TACBrECFVirtualNFCeQuandoFecharDocumento
      read fsQuandoFecharDocumento write fsQuandoFecharDocumento;
    property QuandoCancelarDocumento: TACBrECFVirtualNFCeQuandoCancelarDocumento
      read fsQuandoCancelarDocumento write fsQuandoCancelarDocumento;
    property QuandoImprimirDocumento: TACBrECFVirtualNFCeQuandoImprimirDocumento
      read fsQuandoImprimirDocumento write fsQuandoImprimirDocumento;
  end;

implementation

uses pcnConversaoNFe,
  ACBrConsts, ACBrECFClass, ACBrUtil.Strings, ACBrUtil.Math, ACBrUtil.FilesIO, ACBrUtil.Base, ACBrNFeDANFEClass;
	
{ TACBrECFVirtualNFCe }

procedure TACBrECFVirtualNFCe.CreateVirtualClass;
begin
  fpECFVirtualClass := TACBrECFVirtualNFCeClass.Create(self);
end;

procedure TACBrECFVirtualNFCe.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent is TACBrNFe) and (ACBrNFCe <> nil) then
      ACBrNFCe := nil;
  end;
end;

function TACBrECFVirtualNFCe.GetACBrNFCe: TACBrNFe;
begin
  Result := TACBrECFVirtualNFCeClass(fpECFVirtualClass).ACBrNFCe;
end;

function TACBrECFVirtualNFCe.GetImprimir2ViaOffLine: Boolean;
begin
  Result := TACBrECFVirtualNFCeClass(fpECFVirtualClass).fsImprimir2ViaOffLine;
end;

function TACBrECFVirtualNFCe.GetTratarDescontoNoItem: Boolean;
begin
  Result := TACBrECFVirtualNFCeClass(fpECFVirtualClass).fsTratarDescontoNoItem;
end;

procedure TACBrECFVirtualNFCe.SetACBrNFCe(AValue: TACBrNFe);
begin
  if AValue <> ACBrNFCe then
  begin
    if Assigned(ACBrNFCe) then
      ACBrNFCe.RemoveFreeNotification(Self);

    TACBrECFVirtualNFCeClass(fpECFVirtualClass).ACBrNFCe := AValue;

    if AValue <> nil then
      AValue.FreeNotification(Self);
  end;
end;

procedure TACBrECFVirtualNFCe.SetImprimir2ViaOffLine(AValue: Boolean);
begin
  TACBrECFVirtualNFCeClass(fpECFVirtualClass).fsImprimir2ViaOffLine := AValue;
end;

procedure TACBrECFVirtualNFCe.SetTratarDescontoNoItem(AValue: Boolean);
begin
  TACBrECFVirtualNFCeClass(fpECFVirtualClass).fsTratarDescontoNoItem := AValue;
end;

function TACBrECFVirtualNFCe.GetQuandoAbrirDocumento: TACBrECFVirtualNFCeQuandoAbrirDocumento;
begin
  Result := TACBrECFVirtualNFCeClass(fpECFVirtualClass).QuandoAbrirDocumento;
end;

procedure TACBrECFVirtualNFCe.SetQuandoAbrirDocumento(
  AValue: TACBrECFVirtualNFCeQuandoAbrirDocumento);
begin
  TACBrECFVirtualNFCeClass(fpECFVirtualClass).QuandoAbrirDocumento := AValue;
end;

function TACBrECFVirtualNFCe.GetQuandoEfetuarPagamento: TACBrECFVirtualNFCeQuandoEfetuarPagamento;
begin
  Result := TACBrECFVirtualNFCeClass(fpECFVirtualClass).QuandoEfetuarPagamento;
end;

procedure TACBrECFVirtualNFCe.SetQuandoEfetuarPagamento(
  AValue: TACBrECFVirtualNFCeQuandoEfetuarPagamento);
begin
  TACBrECFVirtualNFCeClass(fpECFVirtualClass).QuandoEfetuarPagamento := AValue;
end;

function TACBrECFVirtualNFCe.GetQuandoVenderItem: TACBrECFVirtualNFCeQuandoVenderItem;
begin
  Result := TACBrECFVirtualNFCeClass(fpECFVirtualClass).QuandoVenderItem;
end;

function TACBrECFVirtualNFCe.GetQuandoFecharDocumento: TACBrECFVirtualNFCeQuandoFecharDocumento;
begin
  Result := TACBrECFVirtualNFCeClass(fpECFVirtualClass).QuandoFecharDocumento;
end;

function TACBrECFVirtualNFCe.GetQuandoCancelarDocumento: TACBrECFVirtualNFCeQuandoCancelarDocumento;
begin
  Result := TACBrECFVirtualNFCeClass(fpECFVirtualClass).QuandoCancelarDocumento;
end;

procedure TACBrECFVirtualNFCe.SetQuandoVenderItem(
  AValue: TACBrECFVirtualNFCeQuandoVenderItem);
begin
  TACBrECFVirtualNFCeClass(fpECFVirtualClass).QuandoVenderItem := AValue;
end;

procedure TACBrECFVirtualNFCe.SetQuandoFecharDocumento(
  AValue: TACBrECFVirtualNFCeQuandoFecharDocumento);
begin
  TACBrECFVirtualNFCeClass(fpECFVirtualClass).QuandoFecharDocumento := AValue;
end;

procedure TACBrECFVirtualNFCe.SetQuandoCancelarDocumento(
  AValue: TACBrECFVirtualNFCeQuandoCancelarDocumento);
begin
  TACBrECFVirtualNFCeClass(fpECFVirtualClass).QuandoCancelarDocumento := AValue;
end;

function TACBrECFVirtualNFCe.GetQuandoImprimirDocumento: TACBrECFVirtualNFCeQuandoImprimirDocumento;
begin
  Result := TACBrECFVirtualNFCeClass(fpECFVirtualClass).QuandoImprimirDocumento;
end;

procedure TACBrECFVirtualNFCe.SetQuandoImprimirDocumento(
  const AValue: TACBrECFVirtualNFCeQuandoImprimirDocumento);
begin
  TACBrECFVirtualNFCeClass(fpECFVirtualClass).QuandoImprimirDocumento := AValue;
end;

{ TACBrECFVirtualNFCeClass }

constructor TACBrECFVirtualNFCeClass.Create(
  AECFVirtualPrinter: TACBrECFVirtualPrinter);
begin
  inherited Create(AECFVirtualPrinter);

  fsACBrNFCe := nil;
  fsECF := nil;
  fsQuandoAbrirDocumento    := nil;
  fsQuandoEfetuarPagamento  := nil;
  fsQuandoVenderItem        := nil;
  fsQuandoFecharDocumento   := nil;
  fsQuandoImprimirDocumento := nil;
  fsNomeArqTempXML := '';
  fsDestNome := '';
  fsDestCNPJ := '';
  fsEhVenda := False;
  fsImprimir2ViaOffLine := True;
  fsTratarDescontoNoItem:= False;
end;

procedure TACBrECFVirtualNFCeClass.FazerImpressaoDocumento;
var
  ImpressaoTratada: Boolean;
begin
  ImpressaoTratada := False;
  if Assigned(fsQuandoImprimirDocumento) then
    fsQuandoImprimirDocumento(ImpressaoTratada);
  if not ImpressaoTratada then
  begin
    fsACBrNFCe.NotasFiscais.Items[0].Imprimir;
  end;
end;

function TACBrECFVirtualNFCeClass.AdivinharFormaPagamento(const DescricaoPagto: string
  ): TpcnFormaPagamento;
var
  Descricao: string;
begin
  Descricao := TiraAcentos(LowerCase(DescricaoPagto));

  if Descricao = 'dinheiro' then
    Result := fpDinheiro
  else if Descricao = 'cheque' then
    Result := fpCheque
  else if Descricao = 'cartao de credito' then
    Result := fpCartaoCredito
  else if Descricao = 'cartao de debito' then
    Result := fpCartaoDebito
  else if Descricao = 'credito loja' then
    Result := fpCreditoLoja
  else if Descricao = 'vale alimentacao' then
    Result := fpValeAlimentacao
  else if Descricao = 'vale refeicao' then
    Result := fpValeRefeicao
  else if Descricao = 'vale presente' then
    Result := fpValePresente
  else if Descricao = 'vale combustivel' then
    Result := fpValeCombustivel
  else if Descricao = 'pix' then
    Result := fpPagamentoInstantaneo
  else if Descricao = 'carteira digital' then
    Result := fpTransfBancario
  else
  begin
    if pos('cartao', Descricao) > 0 then
    begin
      if pos('debito', Descricao) > 0 then
        Result := fpCartaoDebito
      else
        Result := fpCartaoCredito;
    end
    else
      Result := fpOutro
  end;
end;

procedure TACBrECFVirtualNFCeClass.SomaTotais;
var
  i: integer;
begin
  fsvTotTrib := 0;
  fsvBC := 0;
  fsvICMS := 0;
  fsvBCST := 0;
  fsvST := 0;
  fsvProd := 0;
  fsvFrete := 0;
  fsvSeg := 0;
  fsvDesc := 0;
  fsvII := 0;
  fsvIPI := 0;
  fsvPIS := 0;
  fsvCOFINS := 0;
  fsvOutro := 0;

  with fsACBrNFCe.NotasFiscais.Items[0].NFe do
  begin
    for i := 0 to Det.Count - 1 do
    begin
      if Det.Items[i].Prod.cProd <> cItemCancelado then
      begin
        fsvTotTrib := fsvTotTrib + Det.Items[i].Imposto.vTotTrib;
        fsvBC := fsvBC + Det.Items[i].Imposto.ICMS.vBC;
        fsvICMS := fsvICMS + Det.Items[i].Imposto.ICMS.vICMS;
        fsvBCST := fsvBCST + Det.Items[i].Imposto.ICMS.vBCST;
        fsvST := fsvST + Det.Items[i].Imposto.ICMS.vICMSST;
        fsvProd := fsvProd + Det.Items[i].Prod.vProd;
        fsvFrete := fsvFrete + Det.Items[i].Prod.vFrete;
        fsvSeg := fsvSeg + Det.Items[i].Prod.vSeg;
        fsvDesc := fsvDesc + Det.Items[i].Prod.vDesc;
        fsvII := fsvII + Det.Items[i].Imposto.II.vII;
        fsvIPI := fsvIPI + Det.Items[i].Imposto.IPI.vIPI;
        fsvPIS := fsvPIS + Det.Items[i].Imposto.PIS.vPIS;
        fsvCOFINS := fsvCOFINS + Det.Items[i].Imposto.COFINS.vCOFINS;
        fsvOutro := fsvOutro + Det.Items[i].Prod.vOutro;
      end;
    end;
  end;

  fsvNF := (fsvProd + fsvST + fsvFrete + fsvSeg + fsvOutro + fsvII + fsvIPI) - fsvDesc;

end;

procedure TACBrECFVirtualNFCeClass.CancelarNFCe;
var
  xJust: string;
  ChaveNFe: string;
begin
  xJust := ACBrStr('NFCe cancelado por erro na emissão');
  if Assigned(fsQuandoCancelarDocumento) then
    fsQuandoCancelarDocumento(xJust);

  with fsACBrNFCe do
  begin
    WebServices.Consulta.NFeChave := NotasFiscais.Items[0].NumID;

    if not WebServices.Consulta.Executar then
      raise Exception.Create(WebServices.Consulta.Msg);

    EventoNFe.Evento.Clear;
    with EventoNFe.Evento.New do
    begin
      ChaveNFe := OnlyNumber(WebServices.Consulta.NFeChave);
      infEvento.CNPJ := copy(ChaveNFe, 7, 14);
      infEvento.cOrgao := StrToIntDef(copy(ChaveNFe, 1, 2), 0);
      infEvento.dhEvento := now;
      infEvento.tpEvento := teCancelamento;
      infEvento.chNFe := WebServices.Consulta.NFeChave;
      infEvento.detEvento.nProt := WebServices.Consulta.Protocolo;
      infEvento.detEvento.xJust := xJust;
    end;

    EnviarEvento(NotasFiscais.Items[0].NFe.Ide.nNF);
  end;
end;

function TACBrECFVirtualNFCeClass.GetACBrECF: TACBrECF;
begin
  if not Assigned(fsECF) then
    fsECF := GetECFComponente(Self);

  Result := fsECF;
end;

function TACBrECFVirtualNFCeClass.GetSubModeloECF: string;
begin
  Result := 'VirtualNFCe';
end;

function TACBrECFVirtualNFCeClass.GetNumVersao: string;
begin
  Result := ACBrECFVirtualNFCe_VERSAO;
end;

procedure TACBrECFVirtualNFCeClass.AtivarVirtual;
begin
  if not (fpEstado in estCupomAberto) then
  begin
    fsACBrNFCe.NotasFiscais.Clear;
  end;

  inherited AtivarVirtual;

  fsACBrNFCe.Configuracoes.Geral.ModeloDF := moNFCe;

  fsNomeArqTempXML := ChangeFileExt(NomeArqINI, '.xml');
end;

procedure TACBrECFVirtualNFCeClass.AbreDocumentoVirtual;
begin
  fsEhVenda := False;

  if fpEstado = estVenda then
  begin
    fsEhVenda := True;

    with fsACBrNFCe do
    begin
      NotasFiscais.Clear;
      WebServices.EnvEvento.Clear;

      NotasFiscais.Add;
      Consumidor.Enviado := False;

      NotasFiscais.Items[0].NFe.Ide.modelo := 65;
      NotasFiscais.Items[0].NFe.Ide.serie := NumECF;
      NotasFiscais.Items[0].NFe.Ide.nNF := StrToInt(NumCupom);

      NotasFiscais.Items[0].NFe.Dest.CNPJCPF := fsDestCNPJ;
      NotasFiscais.Items[0].NFe.Dest.xNome := fsDestNome;
      fsDestNome := '';
      fsDestCNPJ := '';

      if Assigned(fsQuandoAbrirDocumento) then
        fsQuandoAbrirDocumento(NotasFiscais.Items[0].NFe);
    end;
  end
  else
    inherited AbreDocumentoVirtual;
end;

procedure TACBrECFVirtualNFCeClass.EnviaConsumidorVirtual;
begin
  fsDestCNPJ := OnlyNumber(Consumidor.Documento);
  fsDestNome := Consumidor.Nome;

  Consumidor.Enviado := True;
end;

procedure TACBrECFVirtualNFCeClass.VendeItemVirtual(
  ItemCupom: TACBrECFVirtualClassItemCupom);
var
  Det: TDetCollectionItem;
  AliqECF: TACBrECFAliquota;
begin

  with fsACBrNFCe do
  begin
    Det := NotasFiscais.Items[0].NFe.Det.New;

    Det.Prod.nItem := ItemCupom.Sequencia;
    Det.Prod.cProd := ItemCupom.Codigo;
    Det.Prod.xProd := ItemCupom.Descricao;
    Det.Prod.qCom := ItemCupom.Qtd;
    Det.Prod.vUnCom := ItemCupom.ValorUnit;
    Det.Prod.uCom := ItemCupom.Unidade;
    Det.Prod.qTrib := ItemCupom.Qtd;
    Det.Prod.vUnTrib := ItemCupom.ValorUnit;
    Det.Prod.uTrib := ItemCupom.Unidade;
    Det.Prod.vProd := RoundABNT(ItemCupom.Qtd * ItemCupom.ValorUnit, 2);

    if EAN13Valido(ItemCupom.Codigo) then
      Det.Prod.cEAN := ItemCupom.Codigo;

    AliqECF := fpAliquotas[ItemCupom.AliqPos];

    Det.Prod.CFOP := '5102';
    if (NotasFiscais.Items[0].NFe.Emit.CRT in [crtSimplesNacional, crtMEI]) then
    begin
      Det.Imposto.ICMS.CSOSN := csosn101;
      Det.Imposto.ICMS.vBC := 0;
      Det.Imposto.ICMS.pICMS := 0;
      Det.Imposto.ICMS.vICMS := 0;

      if AliqECF.Aliquota <= 0 then // Isento(Precisa tratar qdo for FF)
      begin
        Det.Imposto.ICMS.CSOSN := csosn400;
      end;
    end
    else
    begin
      Det.Imposto.ICMS.CST := cst00;
      Det.Imposto.ICMS.vBC := RoundABNT((ItemCupom.Qtd * ItemCupom.ValorUnit) + ItemCupom.DescAcres, 2);
      Det.Imposto.ICMS.pICMS := AliqECF.Aliquota;
      Det.Imposto.ICMS.vICMS := RoundABNT(Det.Imposto.ICMS.vBC * (Det.Imposto.ICMS.pICMS / 100), 2);

      if AliqECF.Aliquota <= 0 then // Isento(Precisa tratar qdo for FF)
      begin
        Det.Imposto.ICMS.CST := cst40;
      end;
    end;

    if Assigned(fsQuandoVenderItem) then
      fsQuandoVenderItem(Det);
  end;
end;

procedure TACBrECFVirtualNFCeClass.DescontoAcrescimoItemAnteriorVirtual(
  ItemCupom: TACBrECFVirtualClassItemCupom; PorcDesc: Double);
var
  Det: TDetCollectionItem;
  i:integer;
begin
  // Achando o Item anterior //
  if Estado <> estVenda then
     raise EACBrECFERRO.create(ACBrStr('O Estado da Impressora nao é "VENDA"')) ;

  if (ItemCupom.Sequencia < 1) or (ItemCupom.Sequencia > fsACBrNFCe.NotasFiscais.Items[0].NFe.Det.Count) then
     raise EACBrECFERRO.create(ACBrStr('Item ('+IntToStrZero(ItemCupom.Sequencia,3)+') fora da Faixa.')) ;

  for i:=0 to fsACBrNFCe.NotasFiscais.Items[0].NFe.Det.Count -1 do
   begin
     if fsACBrNFCe.NotasFiscais.Items[0].NFe.Det.Items[i].Prod.nItem=ItemCupom.Sequencia then
        Break;
   end;

  Det := fsACBrNFCe.NotasFiscais.Items[0].NFe.Det.Items[i];

  if ItemCupom.DescAcres > 0 then
    Det.Prod.vOutro := ItemCupom.DescAcres
  else
    Det.Prod.vDesc := -ItemCupom.DescAcres;

  if (not (fsACBrNFCe.NotasFiscais.Items[0].NFe.Emit.CRT in [crtSimplesNacional,crtMEI])) and (det.Imposto.ICMS.pICMS>0)  then
  begin
    Det.Imposto.ICMS.vBC := RoundABNT((ItemCupom.Qtd * ItemCupom.ValorUnit) + ItemCupom.DescAcres, 2);
    Det.Imposto.ICMS.vICMS := RoundABNT(Det.Imposto.ICMS.vBC * (Det.Imposto.ICMS.pICMS / 100), 2);
  end;
end;

procedure TACBrECFVirtualNFCeClass.CancelaItemVendidoVirtual(NumItem: Integer);
begin
  with fsACBrNFCe do
  begin
    if (NumItem > NotasFiscais.Items[0].NFe.Det.Count) or (NumItem < 1) then
      exit;

    NotasFiscais.Items[0].NFe.Det[NumItem - 1].Prod.cProd := cItemCancelado;
    NotasFiscais.Items[0].NFe.Det[NumItem - 1].Prod.vProd := 0;
    NotasFiscais.Items[0].NFe.Det[NumItem - 1].Prod.vDesc := 0;
    NotasFiscais.Items[0].NFe.Det[NumItem - 1].Prod.vOutro:= 0;
  end;
end;

procedure TACBrECFVirtualNFCeClass.SubtotalizaCupomVirtual(MensagemRodape: AnsiString);
var
  i, ItMaior: Integer;
  ItDescAcre: array of Extended;
  Total, VlDescAcres, TotDescAcre, VlItMaior, vlrDifeDescAcres: Extended;

  procedure ratearDescAcre(vlrRateioDescAcre : Extended);
  var
    iItens : integer;
  begin
    with fsACBrNFCe do
    begin
      for iItens := 0 to NotasFiscais.Items[0].NFe.Det.Count - 1 do
      begin
        if vlrRateioDescAcre <= 0 then //Caso não tenha mais valor para rateio então sai do laço para não cosumir processamento desnecessário
          Break;

        if (NotasFiscais.Items[0].NFe.Det[iItens].Prod.cProd <> cItemCancelado) then
        begin
          if ItDescAcre[iItens] > 0 then  //So decrementa em valores que obtiveram desconto
          begin
            ItDescAcre[iItens] := TruncTo(ItDescAcre[iItens], 2) - TruncTo(0.01, 2);
            vlrRateioDescAcre  := TruncTo(vlrRateioDescAcre, 2) - TruncTo(0.01, 2); //Decrementa o valor
          end;
        end;
      end;
    end;

    //Caso ainda fique valor para rateio efetuo uma recusividade para zerar ela
    if vlrRateioDescAcre > 0 then
      ratearDescAcre(vlrRateioDescAcre);
  end;
begin
  if fsEhVenda then
  begin
    with fsACBrNFCe do
    begin
      if (fsTratarDescontoNoItem) then
      begin
        //Verifica se já tem desconto no item , caso tenha não faz o rateio nos itens
        for i := 0 to NotasFiscais.Items[0].NFe.Det.Count - 1 do
        begin
          if (NotasFiscais.Items[0].NFe.Det[i].Prod.cProd <> cItemCancelado) and (NotasFiscais.Items[0].NFe.Det[i].Prod.vDesc > 0) then
            Exit;
        end;
      end;

      if fpCupom.DescAcresSubtotal > 0 then
        VlDescAcres := fpCupom.DescAcresSubtotal
      else
        VlDescAcres := -fpCupom.DescAcresSubtotal;

      if VlDescAcres <> 0 then
      begin
        Total := 0;
        ItMaior := -1;
        VlItMaior := 0;
        TotDescAcre := 0;
        vlrDifeDescAcres := 0;

        SetLength(ItDescAcre, NotasFiscais.Items[0].NFe.Det.Count);

        for i := 0 to NotasFiscais.Items[0].NFe.Det.Count - 1 do
        begin
          if NotasFiscais.Items[0].NFe.Det[i].Prod.cProd <> cItemCancelado then
            Total := Total + NotasFiscais.Items[0].NFe.Det[i].Prod.vProd;
        end;

        for i := 0 to NotasFiscais.Items[0].NFe.Det.Count - 1 do
        begin
          if NotasFiscais.Items[0].NFe.Det[i].Prod.cProd <> cItemCancelado then
          begin
            ItDescAcre[i] := RoundABNT(VlDescAcres * (NotasFiscais.Items[0].NFe.Det[i].Prod.vProd / Total), 2);
            TotDescAcre   := RoundABNT(TotDescAcre + ItDescAcre[i],2);

            if ItDescAcre[i] > VlItMaior then
            begin
              VlItMaior := ItDescAcre[i];
              ItMaior := i;
            end;
          end;
        end;

        if (VlDescAcres > 0) and (TotDescAcre = 0) then
          ItDescAcre[0] := VlDescAcres
        else
        if TotDescAcre <> VlDescAcres then
        begin
          vlrDifeDescAcres := TruncTo(TotDescAcre, 2) - TruncTo(VlDescAcres, 2);

          if ItDescAcre[ItMaior] >= vlrDifeDescAcres  then
            ItDescAcre[ItMaior] := ItDescAcre[ItMaior] - vlrDifeDescAcres
          else
          begin
            //Existe casos que o valor de desconto pode ficar negativo pois a vlrDifeDescAcres pode ser jogada em itens com descontos <= a vlrDifeDescAcres
            //Solução: ratear em todos os itens 0,01 para solucionar esse tipo raro de situação
            ratearDescAcre(vlrDifeDescAcres);
          end;
        end;

        for i := 0 to NotasFiscais.Items[0].NFe.Det.Count - 1 do
        begin
          if NotasFiscais.Items[0].NFe.Det[i].Prod.cProd <> cItemCancelado then
          begin
            if fpCupom.DescAcresSubtotal > 0 then
              NotasFiscais.Items[0].NFe.Det[i].Prod.vOutro :=
                NotasFiscais.Items[0].NFe.Det[i].Prod.vOutro + ItDescAcre[i]
            else
              NotasFiscais.Items[0].NFe.Det[i].Prod.vDesc :=
                NotasFiscais.Items[0].NFe.Det[i].Prod.vDesc + ItDescAcre[i];
          end;
        end;
      end;

      if fpCupom.DescAcresSubtotal > 0 then
        NotasFiscais.Items[0].NFe.Total.ICMSTot.vOutro :=
          NotasFiscais.Items[0].NFe.Total.ICMSTot.vOutro + VlDescAcres
      else
        NotasFiscais.Items[0].NFe.Total.ICMSTot.vDesc :=
          NotasFiscais.Items[0].NFe.Total.ICMSTot.vDesc + VlDescAcres;

    end;
  end
  else
    inherited;
end;

procedure TACBrECFVirtualNFCeClass.EfetuaPagamentoVirtual(
  Pagto: TACBrECFVirtualClassPagamentoCupom);
var
  NFCePagto: TpagCollectionItem;
begin
  if fsEhVenda then
  begin
    with fsACBrNFCe do
    begin

      NFCePagto := NotasFiscais.Items[0].NFe.pag.New;

      NFCePagto.vPag := Pagto.ValorPago;
      NFCePagto.tPag := AdivinharFormaPagamento(fpFormasPagamentos[Pagto.PosFPG].Descricao);

      if NFCePagto.tPag = fpOutro then
        NFCePagto.xPag := fpFormasPagamentos[Pagto.PosFPG].Descricao;

      if (NFCePagto.tPag in [fpCartaoCredito, fpCartaoDebito]) then
      begin
        NFCePagto.tpIntegra := tiPagNaoIntegrado;
      end;

      if Assigned(fsQuandoEfetuarPagamento) then
        fsQuandoEfetuarPagamento(NFCePagto);
    end;

  end;
end;

procedure TACBrECFVirtualNFCeClass.FechaCupomVirtual(Observacao: AnsiString;
  IndiceBMP: Integer);
var
  i: Integer;
  cStat, xMotivo: string;
begin
  if fsEhVenda then
  begin
    if (fsACBrNFCe.NotasFiscais.Items[0].Confirmada) AND (fsACBrNFCe.WebServices.Enviar.cStat = 100) and
       (fsACBrNFCe.NotasFiscais.Items[0].NFe.signature.DigestValue = fsACBrNFCe.NotasFiscais.Items[0].NFe.procNFe.digVal) then
    begin
      if fsACBrNFCe.DANFE is TACBrNFeDANFCEClass then
      begin
        //Caso o sistema recuperou de um travamento de impressão e a mesma ja estiver autorizada cStar = 100,
        //não será enviado a NFCe novamente, assim evitando o erro de duplicidade NFCe
        TACBrNFeDANFCEClass(fsACBrNFCe.DANFE).ViaConsumidor := True;
        FazerImpressaoDocumento;

        if (fsImprimir2ViaOffLine) and (fsACBrNFCe.Configuracoes.Geral.FormaEmissao = teOffLine) then
        begin
          TACBrNFeDANFCEClass(fsACBrNFCe.DANFE).ViaConsumidor := False;
          FazerImpressaoDocumento;
        end;
      end;
    end
    else
    begin
      i := 0;
      while i < fsACBrNFCe.NotasFiscais.Items[0].NFe.Det.Count do
      begin
        fsACBrNFCe.NotasFiscais.Items[0].NFe.Det[i].Prod.nItem := i + 1;
        if fsACBrNFCe.NotasFiscais.Items[0].NFe.Det[i].Prod.cProd = cItemCancelado then
          fsACBrNFCe.NotasFiscais.Items[0].NFe.Det.Delete(i)
        else
          Inc(i);
      end;

      SomaTotais;

      with fsACBrNFCe.NotasFiscais.Items[0] do
      begin
        NFe.Total.ICMSTot.vTotTrib := fsvTotTrib;
        NFe.Total.ICMSTot.vBC := fsvBC;
        NFe.Total.ICMSTot.vICMS := fsvICMS;
        NFe.Total.ICMSTot.vBCST := fsvBCST;
        NFe.Total.ICMSTot.vST := fsvST;
        NFe.Total.ICMSTot.vProd := fsvProd;
        NFe.Total.ICMSTot.vFrete := fsvFrete;
        NFe.Total.ICMSTot.vSeg := fsvSeg;
        NFe.Total.ICMSTot.vDesc := fsvDesc;
        NFe.Total.ICMSTot.vII := fsvII;
        NFe.Total.ICMSTot.vIPI := fsvIPI;
        NFe.Total.ICMSTot.vPIS := fsvPIS;
        NFe.Total.ICMSTot.vCOFINS := fsvCOFINS;
        NFe.Total.ICMSTot.vOutro := fsvOutro;
        NFe.Total.ICMSTot.vNF := fsvNF;

        if Assigned(fsQuandoFecharDocumento) then
          fsQuandoFecharDocumento(NFe);
      end;

      fsDestNome := '';
      fsDestCNPJ := '';

      with fsACBrNFCe do
      begin
        NotasFiscais.Items[0].NFe.InfAdic.infCpl := NotasFiscais.Items[0].NFe.InfAdic.infCpl + sLineBreak + Observacao;

        if Configuracoes.Geral.FormaEmissao = teOffLine then
        begin
          NotasFiscais.Assinar;

          if DANFE is TACBrNFeDANFCEClass then
          begin
            // imprimir obrigatoriamente duas vias quando em off-line
            // uma para consumidor e outra para o estabelecimento
            TACBrNFeDANFCEClass(DANFE).ViaConsumidor := True;
            FazerImpressaoDocumento;

            if fsImprimir2ViaOffLine then
            begin
              TACBrNFeDANFCEClass(DANFE).ViaConsumidor := False;
              FazerImpressaoDocumento;
            end;
          end;
        end
        else
        begin
          Enviar(NotasFiscais.Items[0].NFe.Ide.nNF, false, true);

          if WebServices.Enviar.cStat <> 100 then
          begin
            cStat := IntToStr(WebServices.Enviar.cStat);
            xMotivo := ACBrStrToAnsi(WebServices.Enviar.xMotivo);
            ChaveCupom := NotasFiscais.Items[0].NFe.infNFe.ID;

            raise EACBrNFeException.Create('Erro ao enviar Dados da Venda:' + sLineBreak +
              'cStat: ' + cStat + sLineBreak +
              'xMotivo: ' + xMotivo);
          end;
        end;

        ChaveCupom := NotasFiscais.Items[0].NFe.infNFe.ID;

        if NotasFiscais.Items[0].Confirmada then
          FazerImpressaoDocumento;
      end;
    end;
  end
  else
    inherited FechaCupomVirtual(Observacao, IndiceBMP);

  fsEhVenda := False;
end;

procedure TACBrECFVirtualNFCeClass.VerificaPodeCancelarCupom(
  NumCOOCancelar: Integer);
begin
  { nada aqui }
end;

procedure TACBrECFVirtualNFCeClass.CancelaCupomVirtual;
var
  NomeNFCe, cStat, xMotivo: string;
begin
  if Estado = estNaoFiscal then
    exit;

  with fsACBrNFCe do
  begin
    if (Estado in estCupomAberto) then // Não precisa cancelar se ainda não enviou...d
    begin
      NotasFiscais.Clear;
      exit;
    end;

    if (NotasFiscais.Count > 0) and
      (ChaveCupom = NotasFiscais.Items[0].NFe.infNFe.ID) then // Está na memória ?
      CancelarNFCe
    else
    begin
      NomeNFCe := PathWithDelim(Configuracoes.Arquivos.GetPathNFe(Now, CNPJ, IE, StrToInt(ModeloDFToStr(moNFCe))))+OnlyNumber(ChaveCupom)+'-nfe.xml';

      if not FileExists(NomeNFCe) then
        NomeNFCe := PathWithDelim(Configuracoes.Arquivos.GetPathNFe(Now-1, CNPJ, IE, StrToInt(ModeloDFToStr(moNFCe))))+OnlyNumber(ChaveCupom)+'-nfe.xml';

      if FileExists(NomeNFCe) then
      begin
        NotasFiscais.Clear;
        NotasFiscais.LoadFromFile(NomeNFCe);
        CancelarNFCe;
      end
      else
        raise EACBrNFeException.Create('NFCe não encontrada: ' + NomeNFCe);
    end;

    if (not (WebServices.EnvEvento.EventoRetorno.cStat in [128, 135, 136])) or
      (not (WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat in [135, 136])) then
    begin
      if WebServices.EnvEvento.EventoRetorno.retEvento.Count > 0 then
      begin
        cStat := IntToStr(WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat);
        xMotivo := WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo;
      end
      else
      begin
        cStat := IntToStr(WebServices.EnvEvento.EventoRetorno.cStat);
        xMotivo := WebServices.EnvEvento.EventoRetorno.xMotivo;
      end;

      raise EACBrNFeException.Create('Erro ao enviar cancelamento:' + sLineBreak +
        'cStat: ' + cStat + sLineBreak +
        'xMotivo: ' + xMotivo);
    end;

    ImprimirEvento;
  end;
end;

procedure TACBrECFVirtualNFCeClass.CancelaDescontoAcrescimoItemVirtual(
  ItemCupom: TACBrECFVirtualClassItemCupom; TipoAcrescimoDesconto: String);
begin
  {}
end;

procedure TACBrECFVirtualNFCeClass.LeArqINIVirtual(ConteudoINI: TStrings);
begin
  // Se o cupom está aberto, ou inicializando o ECF, deve ler conteudo temporário do XML
  if (fpEstado in [estVenda, estPagamento, estNaoInicializada] ) then
    if (fsNomeArqTempXML <> '') and FileExists(fsNomeArqTempXML) then
    begin
      fsACBrNFCe.NotasFiscais.Clear;
      fsACBrNFCe.NotasFiscais.LoadFromFile(fsNomeArqTempXML);
      fsEhVenda := True;
    end;

  inherited LeArqINIVirtual(ConteudoINI);
end;

procedure TACBrECFVirtualNFCeClass.GravaArqINIVirtual(ConteudoINI: TStrings);
begin
  // Se cupom está aberto, deve persistir o CFe //
  if (fsEhVenda) and (fpEstado in estCupomAberto) then
  begin
    // Forçar geração do XML para persistir itens
    fsACBrNFCe.NotasFiscais[0].GerarXML; 
    fsACBrNFCe.NotasFiscais.GravarXML(fsNomeArqTempXML);
  end
  else if (fsNomeArqTempXML <> '') and FileExists(fsNomeArqTempXML) then
    DeleteFile(fsNomeArqTempXML);

  inherited GravaArqINIVirtual(ConteudoINI);
end;

{$IFDEF FPC}
{$IFNDEF NOGUI}
initialization
{$I ACBrECFVirtualNFCe.lrs}
{$ENDIF}
{$ENDIF}

end.

