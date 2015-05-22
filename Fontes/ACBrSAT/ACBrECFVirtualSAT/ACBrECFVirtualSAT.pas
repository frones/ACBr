{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
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
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrECFVirtualSAT;

interface

uses ACBrECFVirtual, ACBrECFVirtualPrinter, ACBrSAT, ACBrUtil, ACBrConsts,
  Classes, SysUtils, pcnCFe, pcnConversao, ACBrECF, ACBrDevice
  {$IFDEF FPC}, LResources {$ENDIF};

const
  ACBrECFVirtualSAT_VERSAO = '0.1.0a';
  estCupomAberto = [estVenda, estPagamento];

type

  TACBrECFVirtualSATQuandoAbrirDocumento = procedure(CFe: TCFe) of object;

  TACBrECFVirtualSATQuandoVenderItem = procedure(Det: TDetCollectionItem) of object;

  TACBrECFVirtualSATQuandoEfetuarPagamento = procedure(Det: TMPCollectionItem) of object;

  { TACBrECFVirtualSAT }

  TACBrECFVirtualSAT = class(TACBrECFVirtualPrinter)
  private
    function GetACBrSAT: TACBrSAT;
    function GetQuandoAbrirDocumento: TACBrECFVirtualSATQuandoAbrirDocumento;

      function GetQuandoEfetuarPagamento: TACBrECFVirtualSATQuandoEfetuarPagamento;
    function GetQuandoVenderItem: TACBrECFVirtualSATQuandoVenderItem;
    procedure SetQuandoAbrirDocumento(
      AValue: TACBrECFVirtualSATQuandoAbrirDocumento);
    procedure SetQuandoEfetuarPagamento(
      AValue: TACBrECFVirtualSATQuandoEfetuarPagamento);
    procedure SetQuandoVenderItem(AValue: TACBrECFVirtualSATQuandoVenderItem);
    procedure SetSAT(AValue: TACBrSAT);
  protected
    procedure CreateVirtualClass; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property ACBrSAT: TACBrSAT read GetACBrSAT write SetSAT;

    property QuandoAbrirDocumento : TACBrECFVirtualSATQuandoAbrirDocumento
      read GetQuandoAbrirDocumento write SetQuandoAbrirDocumento ;
    property QuandoVenderItem : TACBrECFVirtualSATQuandoVenderItem
      read GetQuandoVenderItem write SetQuandoVenderItem ;
    property QuandoEfetuarPagamento : TACBrECFVirtualSATQuandoEfetuarPagamento
      read GetQuandoEfetuarPagamento write SetQuandoEfetuarPagamento ;
  end;

  { TACBrECFVirtualSATClass }

  TACBrECFVirtualSATClass = class(TACBrECFVirtualPrinterClass)
  private
    fsACBrSAT: TACBrSAT;
    fsQuandoAbrirDocumento: TACBrECFVirtualSATQuandoAbrirDocumento;
    fsQuandoEfetuarPagamento: TACBrECFVirtualSATQuandoEfetuarPagamento;
    fsQuandoVenderItem: TACBrECFVirtualSATQuandoVenderItem;
    fsNomeArqTempXML: String;
    fsECF: TACBrECF;

    function AdivinharCodigoMP( const DescricaoPagto: String ): TpcnCodigoMP;
    function GetACBrECF: TACBrECF;
  protected
    function GetSubModeloECF: string; override;
    function GetNumVersao: string; override;
    procedure AtivarVirtual; override;

    procedure AbreDocumentoVirtual ; override;
    Procedure EnviaConsumidorVirtual ; override;
    procedure VendeItemVirtual( ItemCupom: TACBrECFVirtualClassItemCupom); override;
    Procedure CancelaItemVendidoVirtual( NumItem : Integer ) ; override ;
    Procedure SubtotalizaCupomVirtual( DescontoAcrescimo : Double = 0;
       MensagemRodape : AnsiString  = '' ) ; override ;
    Procedure EfetuaPagamentoVirtual( Pagto: TACBrECFVirtualClassPagamentoCupom) ; override ;
    procedure FechaCupomVirtual(Observacao: AnsiString; IndiceBMP: Integer);
      override;
    Procedure CancelaCupomVirtual ; override ;

    procedure LeArqINIVirtual( ConteudoINI: TStrings ) ; override;
    procedure GravaArqINIVirtual( ConteudoINI: TStrings ) ; override;

    property ECF: TACBrECF read GetACBrECF;

  public
    Constructor Create( AECFVirtualPrinter : TACBrECFVirtualPrinter ); override;
    property ACBrSAT: TACBrSAT read fsACBrSAT write fsACBrSAT;

    property QuandoAbrirDocumento : TACBrECFVirtualSATQuandoAbrirDocumento
      read fsQuandoAbrirDocumento write fsQuandoAbrirDocumento ;
    property QuandoVenderItem : TACBrECFVirtualSATQuandoVenderItem
      read fsQuandoVenderItem write fsQuandoVenderItem ;
    property QuandoEfetuarPagamento : TACBrECFVirtualSATQuandoEfetuarPagamento
      read fsQuandoEfetuarPagamento write fsQuandoEfetuarPagamento ;
  end;


procedure Register;

implementation

uses ACBrECFClass, ACBrSATClass;

{$IFNDEF FPC}
   {$R ACBrECFVirtualSAT.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrSAT', [TACBrECFVirtualSAT]);
end;

{ TACBrECFVirtualSAT }

procedure TACBrECFVirtualSAT.CreateVirtualClass;
begin
  fpECFVirtualClass := TACBrECFVirtualSATClass.Create(self);
end;

procedure TACBrECFVirtualSAT.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent is TACBrSAT) and (ACBrSAT <> nil) then
      ACBrSAT := nil;
  end;
end;

function TACBrECFVirtualSAT.GetACBrSAT: TACBrSAT;
begin
  Result := TACBrECFVirtualSATClass( fpECFVirtualClass ).ACBrSAT;
end;

procedure TACBrECFVirtualSAT.SetSAT(AValue: TACBrSAT);
begin
  if AValue <> ACBrSAT then
  begin
    if Assigned(ACBrSAT) then
      ACBrSAT.RemoveFreeNotification(Self);

    TACBrECFVirtualSATClass(fpECFVirtualClass).ACBrSAT := AValue;

    if AValue <> nil then
      AValue.FreeNotification(Self);
  end;
end;

function TACBrECFVirtualSAT.GetQuandoAbrirDocumento: TACBrECFVirtualSATQuandoAbrirDocumento;
begin
  Result := TACBrECFVirtualSATClass( fpECFVirtualClass ).QuandoAbrirDocumento ;
end;

procedure TACBrECFVirtualSAT.SetQuandoAbrirDocumento(
  AValue: TACBrECFVirtualSATQuandoAbrirDocumento);
begin
  TACBrECFVirtualSATClass( fpECFVirtualClass ).QuandoAbrirDocumento := AValue ;
end;

function TACBrECFVirtualSAT.GetQuandoEfetuarPagamento: TACBrECFVirtualSATQuandoEfetuarPagamento;
begin
  Result := TACBrECFVirtualSATClass( fpECFVirtualClass ).QuandoEfetuarPagamento ;
end;

procedure TACBrECFVirtualSAT.SetQuandoEfetuarPagamento(
  AValue: TACBrECFVirtualSATQuandoEfetuarPagamento);
begin
  TACBrECFVirtualSATClass( fpECFVirtualClass ).QuandoEfetuarPagamento := AValue ;
end;

function TACBrECFVirtualSAT.GetQuandoVenderItem: TACBrECFVirtualSATQuandoVenderItem;
begin
  Result := TACBrECFVirtualSATClass( fpECFVirtualClass ).QuandoVenderItem ;
end;

procedure TACBrECFVirtualSAT.SetQuandoVenderItem(
  AValue: TACBrECFVirtualSATQuandoVenderItem);
begin
  TACBrECFVirtualSATClass( fpECFVirtualClass ).QuandoVenderItem := AValue ;
end;

{ TACBrECFVirtualSATClass }

constructor TACBrECFVirtualSATClass.Create(
  AECFVirtualPrinter: TACBrECFVirtualPrinter);
begin
  inherited Create(AECFVirtualPrinter);

  fsACBrSAT := Nil;
  fsECF     := Nil;
  fsQuandoAbrirDocumento   := Nil;
  fsQuandoEfetuarPagamento := Nil;
  fsQuandoVenderItem       := Nil;
  fsNomeArqTempXML := '';
end;

function TACBrECFVirtualSATClass.AdivinharCodigoMP(const DescricaoPagto: String
  ): TpcnCodigoMP;
var
  Descricao: String;
begin
  Descricao := TiraAcentos( LowerCase( DescricaoPagto ) );

  if Descricao = 'dinheiro' then
    Result := mpDinheiro
  else if Descricao = 'cheque' then
    Result := mpCheque
  else if Descricao = 'cartao de credito' then
    Result := mpCartaodeCredito
  else if Descricao = 'cartao de debito' then
    Result := mpCartaodeDebito
  else if Descricao = 'credito loja' then
    Result := mpCreditoLoja
  else if Descricao = 'vale alimentacao' then
    Result := mpValeAlimentacao
  else if Descricao = 'vale refeicao' then
    Result := mpValeRefeicao
  else if Descricao = 'vale presente' then
    Result := mpValePresente
  else if Descricao = 'vale combustivel' then
    Result := mpValeCombustivel
  else
  begin
    if pos('cartao', Descricao) > 0 then
    begin
      if pos('debito', Descricao) > 0 then
        Result := mpCartaodeDebito
      else
        Result := mpCartaodeCredito;
    end
    else
      Result := mpOutros
  end;
end;

function TACBrECFVirtualSATClass.GetACBrECF: TACBrECF;
begin
  if not Assigned(fsECF) then
    fsECF := GetECFComponente(Self);

  Result := fsECF;
end;

function TACBrECFVirtualSATClass.GetSubModeloECF: string;
begin
  Result := 'VirtualSAT';
end;

function TACBrECFVirtualSATClass.GetNumVersao: string;
begin
  Result := ACBrECFVirtualSAT_VERSAO;
end;

procedure TACBrECFVirtualSATClass.AtivarVirtual;
begin
  fsACBrSAT.Inicializar;
  inherited AtivarVirtual;

  fsNomeArqTempXML := ChangeFileExt( NomeArqINI, '.xml' );
end;

procedure TACBrECFVirtualSATClass.AbreDocumentoVirtual;
begin
  if fpEstado = estVenda then
  begin
    with fsACBrSAT do
    begin
      InicializaCFe();
      Consumidor.Enviado := False ;

      CFe.ide.numeroCaixa := NumECF;

      if Assigned( fsQuandoAbrirDocumento ) then
        fsQuandoAbrirDocumento( CFe );
    end;

    exit; // Não Imprime
  end;

  inherited AbreDocumentoVirtual;
end;

procedure TACBrECFVirtualSATClass.EnviaConsumidorVirtual;
begin
  with fsACBrSAT.CFe do
  begin
    Dest.CNPJCPF := OnlyNumber(Consumidor.Documento);
    Dest.xNome   := Consumidor.Nome;
  end;

  Consumidor.Enviado := True ;
end;

procedure TACBrECFVirtualSATClass.VendeItemVirtual(
  ItemCupom: TACBrECFVirtualClassItemCupom);
var
  Det: TDetCollectionItem;
  Acres: Double;
  AliqECF: TACBrECFAliquota;
begin

  with fsACBrSAT do
  begin
    Det := CFe.Det.Add;

    Det.nItem := ItemCupom.Sequencia;
    Det.Prod.cProd    := ItemCupom.Codigo;
    Det.Prod.xProd    := ItemCupom.Descricao;
    Det.Prod.qCom     := ItemCupom.Qtd;
    Det.Prod.vUnCom   := ItemCupom.ValorUnit;
    Det.Prod.uCom     := ItemCupom.Unidade;

    if ItemCupom.DescAcres > 0 then
    begin
      // Não há campo para Acréscimo... somando o Acréscimo no preço Unitário
      Acres := RoundABNT(ItemCupom.DescAcres/ItemCupom.Qtd, ECF.DecimaisPreco);
      Det.Prod.vUnCom := Det.Prod.vUnCom + Acres;
    end
    else
      Det.Prod.vDesc := ItemCupom.DescAcres;

    if ECF.Arredonda then
      Det.Prod.indRegra := irArredondamento
    else
      Det.Prod.indRegra := irTruncamento;

    if EAN13Valido(ItemCupom.Codigo) then
      Det.Prod.cEAN := ItemCupom.Codigo;

    AliqECF := fpAliquotas[ ItemCupom.PosAliq ];

    Det.Prod.CFOP          := '5102';
    Det.Imposto.ICMS.CST   := cst00;
    Det.Imposto.ICMS.pICMS := AliqECF.Aliquota;

    if ItemCupom.PosAliq = 0 then       // FF
    begin
      Det.Prod.CFOP        := '5405';
      Det.Imposto.ICMS.CST := cst60;
    end
    else if ItemCupom.PosAliq = 1 then  // II
    begin
      Det.Imposto.ICMS.CST := cst40;
    end
    else if ItemCupom.PosAliq = 2 then  // NN
    begin
      Det.Imposto.ICMS.CST := cst41;
    end
    else if AliqECF.Tipo = 'S' then     // Serviços
    begin
      Det.Prod.CFOP           := '5949';
      Det.Imposto.ICMS.CST    := cst90;
      Det.Imposto.ICMS.pICMS  := 0;
      Det.Imposto.ISSQN.vAliq := AliqECF.Aliquota;
    end;

    if Assigned( fsQuandoVenderItem ) then
      fsQuandoVenderItem( Det );
  end;
end;

procedure TACBrECFVirtualSATClass.CancelaItemVendidoVirtual(NumItem: Integer);
begin
  with fsACBrSAT do
  begin
    if (NumItem > CFe.Det.Count) or (NumItem < 1) then
      exit;

    CFe.Det.Delete(NumItem-1);
  end;
end;

procedure TACBrECFVirtualSATClass.SubtotalizaCupomVirtual(
  DescontoAcrescimo: Double; MensagemRodape: AnsiString);
begin
  with fsACBrSAT do
  begin
    if DescontoAcrescimo > 0 then
      CFe.Total.DescAcrEntr.vAcresSubtot := DescontoAcrescimo
    else
      CFe.Total.DescAcrEntr.vDescSubtot  := DescontoAcrescimo;

    CFe.InfAdic.infCpl := MensagemRodape;
  end;
end;

procedure TACBrECFVirtualSATClass.EfetuaPagamentoVirtual(
  Pagto: TACBrECFVirtualClassPagamentoCupom);
var
  CfePagto: TMPCollectionItem;
begin
  with fsACBrSAT do
  begin
    CfePagto := CFe.Pagto.Add;

    CfePagto.vMP := Pagto.ValorPago;
    CfePagto.cMP := AdivinharCodigoMP( fpFormasPagamentos[ Pagto.PosFPG ].Descricao );

    if Assigned( fsQuandoEfetuarPagamento ) then
      fsQuandoEfetuarPagamento( CfePagto );
  end;
end;

procedure TACBrECFVirtualSATClass.FechaCupomVirtual(Observacao: AnsiString;
  IndiceBMP: Integer);
var
  codigoDeRejeicao, mensagem: String;
begin
  with fsACBrSAT do
  begin
    CFe.InfAdic.infCpl := Observacao;

    EnviarDadosVenda;

    if Resposta.codigoDeRetorno <> 6000 then
    begin
      codigoDeRejeicao := Resposta.RetornoLst[2];
      mensagem         := ACBrStrToAnsi( Resposta.RetornoLst[3] );

      raise EACBrSATErro.Create( 'Erro ao enviar Dados da Venda:' + sLineBreak +
        'Cod.Retorno: '+IntToStr( Resposta.codigoDeRetorno ) +
        ', Cod.Rejeição: '+codigoDeRejeicao + sLineBreak +
        mensagem );
    end;

    ChaveCupom := CFe.infCFe.ID;

    ImprimirExtrato;
  end;
end;

procedure TACBrECFVirtualSATClass.CancelaCupomVirtual;
var
  NomeCFe, codigoDeRejeicao, mensagem: String;
begin
  with fsACBrSAT do
  begin
    if (Estado in estCupomAberto) then    // Não precisa cancelar se ainda não enviou...d
    begin
      CFe.Clear;
      CFeCanc.Clear;
      exit ;
    end;

    if ChaveCupom = CFe.infCFe.ID then    // Está na memória ?
      CancelarUltimaVenda
    else
    begin
      NomeCFe := PastaCFeVenda + PathDelim + CPREFIXO_CFe + ChaveCupom + '.xml';

      if FileExists( NomeCFe ) then
      begin
        CFe.LoadFromFile( NomeCFe);
        CancelarUltimaVenda;
      end
      else
        raise EACBrSATErro.Create( 'CFe não encontrado: '+NomeCFe);
    end;

    if Resposta.codigoDeRetorno <> 7000 then
    begin
      codigoDeRejeicao := Resposta.RetornoLst[2];
      mensagem         := ACBrStrToAnsi( Resposta.RetornoLst[3] );

      raise EACBrSATErro.Create( 'Erro ao Cancelar Venda: ' + ChaveCupom + sLineBreak +
        'Cod.Retorno: '+IntToStr( Resposta.codigoDeRetorno ) +
        ', Cod.Rejeição: '+codigoDeRejeicao + sLineBreak +
        mensagem );
    end;

    ImprimirExtratoCancelamento;
  end;
end;

procedure TACBrECFVirtualSATClass.LeArqINIVirtual(ConteudoINI: TStrings);
begin
  // Se o cupom está aberto, deve ler conteudo temporário do XML
  if (fpEstado in estCupomAberto) then
    if (fsNomeArqTempXML <> '') and FileExists( fsNomeArqTempXML ) then
      fsACBrSAT.CFe.LoadFromFile( fsNomeArqTempXML );

  inherited LeArqINIVirtual(ConteudoINI);
end;

procedure TACBrECFVirtualSATClass.GravaArqINIVirtual(ConteudoINI: TStrings);
begin
  // Se cupom está aberto, deve persistir o CFe //
  if (fpEstado in estCupomAberto) then
    WriteToTXT( fsNomeArqTempXML, fsACBrSAT.CFe.GerarXML(True), False, False )
  else
  begin
    if (fsNomeArqTempXML <> '') and FileExists( fsNomeArqTempXML ) then
      DeleteFile( fsNomeArqTempXML );
  end;

  inherited GravaArqINIVirtual(ConteudoINI);
end;

{$IFDEF FPC}
{$IFNDEF NOGUI}
initialization
   {$I ACBrECFVirtualSAT.lrs}
{$ENDIF}
{$ENDIF}

end.

// TODO:
// Buferizar o cancelamento, para não perder a sequencia
// Criar formas de pagamento padrão (de acordo com as da lei)
