{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Wemerson Souto                                  }
{                              Daniel Simoes de Almeida                        }
{                              André Ferreira de Moraes                        }
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

unit ACBrNFeDANFeRLSimplificado;

interface

uses
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, Qt,
  {$ELSE}
  Graphics, Controls, Forms,
  {$ENDIF}
  RLReport, RLBarcode, ACBrNFeDANFeRL, RLFilters, RLPDFFilter, math;

type

  { TfrlDANFeRLSimplificado }

  TfrlDANFeRLSimplificado = class(TfrlDANFeRL)
    RLb02_Emitente: TRLBand;
    RLb03_DadosGerais: TRLBand;
    RLb04_Destinatario: TRLBand;
    RLb05c_Lin_Itens: TRLBand;
    RLiLogo: TRLImage;
    RLLabel1: TRLLabel;
    RLLabel27: TRLLabel;
    RLLabel9: TRLLabel;
    RLlChave: TRLLabel;
    RLlDescricao: TRLLabel;
    RLlEmissao: TRLLabel;
    RLlEntradaSaida: TRLLabel;
    RLlMsgTipoEmissao: TRLLabel;
    RLlProtocolo: TRLLabel;
    RLlTipoEmissao: TRLLabel;
    RLmDestinatario: TRLMemo;
    RLmEmitente: TRLMemo;
    RLShape102: TRLDraw;
    RLShape68: TRLDraw;
    rlb01_Chave: TRLBand;
    RLBarcode1: TRLBarcode;
    RLLabel17: TRLLabel;
    lblNumero: TRLLabel;
    subItens: TRLSubDetail;
    rlb05a_Cab_Itens: TRLBand;
    RLLabel2: TRLLabel;
    RLLabel4: TRLLabel;
    RLLabel5: TRLLabel;
    RLLabel6: TRLLabel;
    RLLabel7: TRLLabel;
    RLLabel8: TRLLabel;
    RLLabel3: TRLLabel;
    rlb05b_Desc_Itens: TRLBand;
    rlmProdutoCodigo: TRLLabel;
    rlmProdutoDescricao: TRLLabel;
    rlmProdutoUnidade: TRLLabel;
    rlmProdutoQTDE: TRLLabel;
    rlmProdutoValor: TRLLabel;
    rlmProdutoTotal: TRLLabel;
    rlmProdutoItem: TRLLabel;
    rlb06a_Totais: TRLBand;
    rlmPagDesc: TRLMemo;
    rlmPagValor: TRLMemo;
    rlb06b_Tributos: TRLBand;
    rllTributos: TRLLabel;
    rlcfop: TRLLabel;
    rlmProdutoCFOP: TRLLabel;
    rlmProdutoCST: TRLLabel;
    rlCST: TRLLabel;
    rlbFaturaReal: TRLBand;
    RLLabel10: TRLLabel;
    rlbFatura: TRLBand;
    RLLabel19: TRLLabel;
    rllFatNum1: TRLLabel;
    rllFatNum2: TRLLabel;
    rllFatNum3: TRLLabel;
    rllFatData1: TRLLabel;
    rllFatData2: TRLLabel;
    rllFatData3: TRLLabel;
    rllFatValor1: TRLLabel;
    rllFatValor2: TRLLabel;
    rllFatValor3: TRLLabel;
    rllFatNum4: TRLLabel;
    rllFatData4: TRLLabel;
    rllFatValor4: TRLLabel;
    rllFatNum5: TRLLabel;
    rllFatData5: TRLLabel;
    rllFatValor5: TRLLabel;
    rllFatNum6: TRLLabel;
    rllFatData6: TRLLabel;
    rllFatValor6: TRLLabel;
    rllFatNum7: TRLLabel;
    rllFatData7: TRLLabel;
    rllFatValor7: TRLLabel;
    rllFatNum8: TRLLabel;
    rllFatData8: TRLLabel;
    rllFatValor8: TRLLabel;
    rllFatNum9: TRLLabel;
    rllFatData9: TRLLabel;
    rllFatValor9: TRLLabel;
    rllFatNum10: TRLLabel;
    rllFatData10: TRLLabel;
    rllFatValor10: TRLLabel;
    rllFatNum11: TRLLabel;
    rllFatData11: TRLLabel;
    rllFatValor11: TRLLabel;
    rllFatNum12: TRLLabel;
    rllFatData12: TRLLabel;
    rllFatValor12: TRLLabel;
    rllFatNum13: TRLLabel;
    rllFatData13: TRLLabel;
    rllFatValor13: TRLLabel;
    rllFatNum14: TRLLabel;
    rllFatData14: TRLLabel;
    rllFatValor14: TRLLabel;
    rllFatNum15: TRLLabel;
    rllFatData15: TRLLabel;
    rllFatValor15: TRLLabel;
    rllCabFatura1: TRLLabel;
    rllCabFatura2: TRLLabel;
    rllCabFatura3: TRLLabel;
    RLDraw69: TRLDraw;
    RlbDadoPagamento: TRLLabel;
    RlbDadoNumero: TRLLabel;
    RlbDadoValorOriginal: TRLLabel;
    RlbDadoValorDesconto: TRLLabel;
    RlbDadoValorLiquido: TRLLabel;
    RLLabelPag: TRLLabel;
    RLLabelNUmero: TRLLabel;
    RLLabelValor: TRLLabel;
    C: TRLLabel;
    RLLabelLIQ: TRLLabel;
    procedure RLb02_EmitenteBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLb03_DadosGeraisBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLb04_DestinatarioBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLb06a_TotaisBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLb06b_TributosBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLNFeBeforePrint(Sender: TObject; var PrintReport: Boolean);
    procedure rlb01_ChaveBeforePrint(Sender: TObject; var PrintBand: Boolean);
    procedure subItensDataRecord(Sender: TObject; RecNo, CopyNo: Integer; var EOF: Boolean; var RecordAction: TRLRecordAction);
    procedure rlb05b_Desc_ItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLNFeDataRecord(Sender: TObject; RecNo, CopyNo: Integer; var EOF: Boolean; var RecordAction: TRLRecordAction);
  private
    FNumItem: Integer;
    FTotalPages: Integer;
    procedure InicializarDados;
    procedure AdicionarFaturaReal;
    procedure AdicionarFatura;
    function ManterDuplicatas: Integer;
  public
    procedure ProtocoloNFE(const sProtocolo: String);
  end;

implementation

uses
  StrUtils, DateUtils,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime,
  ACBrValidador, ACBrDFeUtil,
  ACBrDFeReportFortes, pcnNFe, pcnConversao;

{$IfNDef FPC}
  {$R *.dfm}
{$Else}
  {$R *.lfm}
{$ENDIF}

const
  _NUM_ITEMS_PAGE1 = 18;
  _NUM_ITEMS_OTHERPAGES = 50;

procedure TfrlDANFeRLSimplificado.RLNFeBeforePrint(Sender: TObject; var PrintReport: Boolean);
var
  nRestItens: Integer;
begin
  inherited;

  FTotalPages := 1;

  if (fpNFe.Det.Count > _NUM_ITEMS_PAGE1) then
  begin
    nRestItens := fpNFe.Det.Count - _NUM_ITEMS_PAGE1;
    if (nRestItens <= _NUM_ITEMS_OTHERPAGES) then
      Inc(FTotalPages)
    else
    begin
      Inc(FTotalPages, nRestItens div _NUM_ITEMS_OTHERPAGES);
      if ((nRestItens mod _NUM_ITEMS_OTHERPAGES) > 0) then
        Inc(FTotalPages);
    end;
  end;

  RLNFe.Title := 'NF-e: ' + FormatFloat('000,000,000', fpNFe.Ide.nNF);
  TDFeReportFortes.AjustarMargem(RLNFe, fpDANFe);
  InicializarDados;
end;

procedure TfrlDANFeRLSimplificado.RLb02_EmitenteBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  if fpDANFe.ExpandeLogoMarca then
  begin
    rliLogo.top := 13;
    rliLogo.Left := 2;
    rliLogo.Height := 108;
    rliLogo.Width := 284;

    TDFeReportFortes.AjustarLogo(rliLogo, fpDANFe.ExpandeLogoMarcaConfig);

    rlmEmitente.Enabled := False;
    RLb02_Emitente.Height:= 188;
    RLmEmitente.Top:= rlilogo.Top + rlilogo.Height + 3;
  end;

  if not TDFeReportFortes.CarregarLogo(rliLogo, fpDANFe.Logo) then
  begin
    //TODO: implementar algum tratamento para logo vazio? Ex.: Veja: TfrlDANFeRLRetrato.InicializarDados
     RLb02_Emitente.Height:= 80;
     RLmEmitente.Top:= rlilogo.Top;
  end;


//  if not fpDANFe.ExpandeLogoMarca then
//  begin
    rlmEmitente.Enabled := True;
    rlmEmitente.Lines.Clear;

    with fpNFe.Emit do
    begin
      rlmEmitente.Lines.Add(fpDANFe.ManterNomeImpresso(XNome, XFant));
      with EnderEmit do
      begin
        rlmEmitente.Lines.Add(XLgr + IfThen(Nro = '0', '', ', ' + Nro) +
          IfThen(EstaVazio(XCpl), '', ', ' + XCpl) +
          IfThen(EstaVazio(XBairro), '', ', ' + XBairro) +
          ', ' + XMun + '/ ' + UF);
      end;

      rlmEmitente.Lines.Add('CNPJ: ' + FormatarCNPJouCPF(CNPJCPF) + ' IE: ' + IE);
    end;
 // end;
end;

procedure TfrlDANFeRLSimplificado.RLb03_DadosGeraisBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  // Contingencia ********************************************************
  if fpNFe.Ide.tpEmis in [teContingencia, teFSDA] then
    rllTipoEmissao.Caption := 'CONTINGENCIA FS-DA';

  rllEntradaSaida.Caption := tpNFToStr(fpNFe.Ide.tpNF);

  lblNumero.Caption := ACBrStr('Número: ' + FormatFloat('000,000,000', fpNFe.Ide.nNF) +
    ' - Série: ' + FormatFloat('000', fpNFe.Ide.serie));

  rllEmissao.Caption := ACBrStr('Emissão: ' + FormatDateTimeBr(fpNFe.Ide.dEmi));
end;

procedure TfrlDANFeRLSimplificado.RLb04_DestinatarioBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  rlmDestinatario.Lines.Clear;
  with fpNFe.Dest do
  begin
    rlmDestinatario.Lines.Add(XNome);
    with EnderDest do
    begin
      rlmDestinatario.Lines.Add(XLgr + IfThen(Nro = '0', '', ', ' + Nro) +
        IfThen(EstaVazio(XCpl), '', ', ' + XCpl) +
        IfThen(EstaVazio(XBairro), '', ', ' + XBairro) +
        ', ' + XMun + '/ ' + UF);
    end;

    rlmDestinatario.Lines.Add(ACBrStr('CPF/CNPJ: ' + FormatarCNPJouCPF(CNPJCPF) + ' IE: ' + IE));
  end;

  rllMsgTipoEmissao.Visible := False;
  if (fpNFe.Ide.tpAmb = taHomologacao) then
  begin
    rllMsgTipoEmissao.Caption := ACBrStr('HOMOLOGAÇÂO - SEM VALOR FISCAL');
    rllMsgTipoEmissao.Enabled := True;
    rllMsgTipoEmissao.Visible := True;
  end;

  if (fpNFe.procNFe.cStat > 0) then
  begin
    if (fpDANFe.Cancelada or (fpNFe.procNFe.cStat in [101, 151, 155])) then
    begin
      rllMsgTipoEmissao.Caption := 'NF-e CANCELADA';
      rllMsgTipoEmissao.Visible := True;
      rllMsgTipoEmissao.Enabled := True;
    end;

    if (fpNFe.procNFe.cStat = 110) then
    begin
      rllMsgTipoEmissao.Caption := 'NF-e DENEGADA';
      rllMsgTipoEmissao.Visible := True;
      rllMsgTipoEmissao.Enabled := True;
    end;

    if not (fpNFe.procNFe.cStat in [100, 101, 110, 151, 155]) then
    begin
      rllMsgTipoEmissao.Caption := fpNFe.procNFe.xMotivo;
      rllMsgTipoEmissao.Visible := True;
      rllMsgTipoEmissao.Enabled := True;
    end;
  end;

  case fpNFe.Ide.tpEmis of
    teContingencia:
    begin
      rllMsgTipoEmissao.Caption := ACBrStr('DANFE em Contingencia - impresso em decorrencia de problemas tecnicos');
      rllMsgTipoEmissao.Visible := True;
      rllMsgTipoEmissao.Enabled := True;
    end;

    teFSDA:
    begin
      rllMsgTipoEmissao.Caption := ACBrStr('DANFE em Contingencia - impresso em decorrencia de problemas tecnicos');
      rllMsgTipoEmissao.Visible := True;
      rllMsgTipoEmissao.Enabled := True;
    end;
  end;

  rllMsgTipoEmissao.Repaint;
end;

procedure TfrlDANFeRLSimplificado.RLb06a_TotaisBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  rlmPagDesc.Lines.Clear;
  rlmPagValor.Lines.Clear;
  rlmPagDesc.Lines.Add('Qtde Total de Itens');
  rlmPagValor.Lines.Add(IntToStr(fpNFe.Det.Count));
  rlmPagDesc.Lines.Add('Valor Total');
  rlmPagValor.Lines.Add(FormatFloatBr(fpNFe.Total.ICMSTot.vNF));
end;

procedure TfrlDANFeRLSimplificado.RLb06b_TributosBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  Perc: Double;
begin
  inherited;

  Perc := 0;
  if (fpNFe.Total.ICMSTot.vNF > 0) then
    Perc := (fpNFe.Total.ICMSTot.vTotTrib / fpNFe.Total.ICMSTot.vNF) * 100;

  rllTributos.Caption := ACBrStr('Valor aprox. dos tributos: ') +
    FormatFloatBr(fpNFe.Total.ICMSTot.vTotTrib) +
    '(' + FormatFloatBr(Perc) + '%)(Fonte: IBPT)';
end;

procedure TfrlDANFeRLSimplificado.AdicionarFatura;
var
  x, iQuantDup: Integer;
begin
  rlbFatura.Visible := (fpNFe.Cobr.Dup.Count > 0);


  if (fpNFe.Cobr.Dup.Count > 0) then
  begin
    for x := 1 to 15 do
    begin
      TRLLabel(FindComponent('rllFatNum' + IntToStr(x))).Caption := '';
      TRLLabel(FindComponent('rllFatData' + IntToStr(x))).Caption := '';
      TRLLabel(FindComponent('rllFatValor' + IntToStr(x))).Caption := '';
    end;

    TRLLabel(FindComponent('rllFatNum1')).AutoSize := True;

    iQuantDup := ManterDuplicatas;

    {=============== Ajusta o tamanho do quadro das faturas ===============}

    rlbFatura.Height := iQuantDup * 22;
  end;

end;

procedure TfrlDANFeRLSimplificado.AdicionarFaturaReal;
begin
     rlbFaturaReal.Visible := fpDANFe.ExibeCampoFatura;


  if (fpNFe.infNFe.Versao >= 4) then
  begin
    RlbDadoPagamento.Caption := ACBrStr('Fatura');
    rlbFaturaReal.Visible := NaoEstaVazio(fpNFe.Cobr.Fat.nFat) and fpDANFe.ExibeCampoFatura;
  end
  else
  begin
    case fpNFe.Ide.indPag of
      ipVista:
        RlbDadoPagamento.Caption := ACBrStr('PAGAMENTO A VISTA');
      ipPrazo:
        RlbDadoPagamento.Caption := ACBrStr('PAGAMENTO A PRAZO');
      ipOutras:
      begin
        RlbDadoPagamento.Caption := 'OUTROS';
        rlbFaturaReal.Visible := NaoEstaVazio(fpNFe.Cobr.Fat.nFat) and fpDANFe.ExibeCampoFatura;
      end;
    end;
  end;

  if NaoEstaVazio(fpNFe.Cobr.Fat.nFat) then
  begin
//    RLLabelNUmero.Caption := ACBrStr('NÚMERO');
//    RLLabelValor.Caption := ACBrStr('VALOR ORIGINAL');
//    RLLabelDupl.Caption := ACBrStr('VALOR DESCONTO');
//    RLLabelLIQ.Caption := ACBrStr('VALOR LÍQUIDO');
//
//    // Define a Coluna dos label's
//    RLLabelNUmero.Left := 264;
//    RLLabelValor.Left := 439;
//    RLLabelDupl.Left := 541;
//    RLLabelLIQ.Left := 652;
    with fpNFe.Cobr.Fat do
    begin
      RlbDadoNumero.Caption := nFat;
      RlbDadoValorOriginal.Caption := FormatFloatBr(vOrig);
      RlbDadoValorDesconto.Caption := FormatFloatBr(vDesc);
      RlbDadoValorLiquido.Caption := FormatFloatBr(vLiq);
    end;
  end
  else
  begin
    RLLabelNUmero.Caption := '';
    RLLabelValor.Caption := '';
//    RLLabelDupl.Caption := '';
    RLLabelLIQ.Caption := '';
    RlbDadoNumero.Caption := '';
    RlbDadoValorOriginal.Caption := '';
    RlbDadoValorDesconto.Caption := '';
    RlbDadoValorLiquido.Caption := '';
  end;
end;

procedure TfrlDANFeRLSimplificado.InicializarDados;
begin
  rlmProdutoCodigo.Width    := fpDANFe.LarguraCodProd;
  rlmProdutoDescricao.Left  := rlmProdutoCodigo.Left + rlmProdutoCodigo.Width + 2;
  RLLabel4.Left             := rlmProdutoDescricao.Left;
  AdicionarFaturaReal;
  AdicionarFatura;
end;

function TfrlDANFeRLSimplificado.ManterDuplicatas: Integer;
var
  x: Integer;
begin
  with fpNFe.Cobr do
  begin
    Result := min(Dup.Count, 15);

    for x := 0 to (Result - 1) do
    begin
      with Dup[x] do
      begin
        TRLLabel(FindComponent('rllFatNum' + IntToStr(x + 1))).Caption := NDup;
        TRLLabel(FindComponent('rllFatData' + IntToStr(x + 1))).Caption := FormatDateBr(DVenc);
        TRLLabel(FindComponent('rllFatValor' + IntToStr(x + 1))).Caption := FormatFloatBr(VDup);
      end;
    end;
  end;
end;

procedure TfrlDANFeRLSimplificado.ProtocoloNFE(const sProtocolo: String);
begin
  fpDANFe.Protocolo := sProtocolo;
end;

procedure TfrlDANFeRLSimplificado.rlb01_ChaveBeforePrint(Sender: TObject; var PrintBand: Boolean);
begin
  inherited;

  PrintBand := (RLNFe.PageNumber = 1);
  RLBarcode1.Caption := OnlyNumber(fpNFe.InfNFe.Id);
  rllChave.Caption := FormatarChaveAcesso(fpNFe.InfNFe.Id);

  // Normal **************************************************************
  if (fpNFe.Ide.tpEmis in [teNormal, teSCAN]) then
  begin
    if (fpNFe.procNFe.cStat = 100) then
      rllDescricao.Caption := ACBrStr('Protocolo de Autorização');

    if (fpNFe.procNFe.cStat in [101, 151, 155]) then
      rllDescricao.Caption := ACBrStr('Protocolo de Homologação de Cancelamento');

    if (fpNFe.procNFe.cStat = 110) then
      rllDescricao.Caption := ACBrStr('Protocolo de Denegação de Uso');
  end;

  if NaoEstaVazio(fpDANFe.Protocolo) then
    rllProtocolo.Caption := fpDANFe.Protocolo
  else
    rllProtocolo.Caption := fpNFe.procNFe.nProt + ' ' +
      IfThen(fpNFe.procNFe.dhRecbto <> 0, FormatDateTimeBr(fpNFe.procNFe.dhRecbto), '');
end;

procedure TfrlDANFeRLSimplificado.rlb05b_Desc_ItensBeforePrint(Sender: TObject; var PrintIt: Boolean);

  function ManterinfAdProd(sXProd: String; sinfAdProd: String): String;
  begin
    Result := sXProd;
    if NaoEstaVazio(sinfAdProd) then
      Result := Result + sLineBreak + sLineBreak + 'InfAd: ' + sinfAdProd;
  end;

begin
  inherited;

  with fpNFe.Det.Items[FNumItem] do
  begin
    rlmProdutoItem.Caption := FormatFloat('000', FNumItem + 1);
    rlmProdutoCodigo.Caption := fpDANFe.ManterCodigo(Prod.cEAN, Prod.CProd);
    rlmProdutoDescricao.Caption := ManterinfAdProd(Prod.XProd, infAdProd);
    rlmProdutoQTDE.Caption := fpDANFe.FormatarQuantidade(Prod.qCom);
    rlmProdutoCFOP.Caption := prod.cfop;
    case fpNFe.Emit.CRT of
      crtRegimeNormal, crtSimplesExcessoReceita: begin
        rlmProdutoCST.Caption := OrigToStr(Imposto.ICMS.orig) + CSTICMSToStr(Imposto.ICMS.CST);
        rlCST.Caption:= 'Cst';
        end;
      crtSimplesNacional: begin
        rlmProdutoCST.Caption := OrigToStr(Imposto.ICMS.orig) + CSOSNIcmsToStr(Imposto.ICMS.CSOSN);
        rlCST.Caption:= 'Csosn/Cst';
        end;
    end;
    rlmProdutoValor.Caption := fpDANFe.FormatarValorUnitario(Prod.vUnCom);
    rlmProdutoUnidade.Caption := Prod.UCom;
    rlmProdutoTotal.Caption := FormatFloatBr(Prod.vProd);
  end;
end;

procedure TfrlDANFeRLSimplificado.RLNFeDataRecord(Sender: TObject; RecNo, CopyNo: Integer; var EOF: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;

  EOF := (RecNo > 1);
  RecordAction := raUseIt;
end;

procedure TfrlDANFeRLSimplificado.subItensDataRecord(Sender: TObject; RecNo, CopyNo: Integer; var EOF: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;

  FNumItem := RecNo - 1;
  EOF := (RecNo > fpNFe.Det.Count);
  RecordAction := raUseIt;
end;

end.
