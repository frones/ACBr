{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrNF3e.DANF3ERLRetrato;

interface

uses
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, Qt, QStdCtrls,
  {$ELSE}
  Graphics, Controls, Forms,
  {$ENDIF}
  RLReport,
  {$IFDEF BORLAND}
   {$IF CompilerVersion > 22}
    Vcl.Imaging.jpeg,
   {$ELSE}
    jpeg,
   {$IFEND}
  {$ENDIF}
  ACBrNF3e.DANF3ERL, RLBarcode, RLFilters, RLPDFFilter, Data.DB;

type

  { TfrmDANF3eRLRetrato }

  TfrmDANF3eRLRetrato = class(TfrmDANF3eRL)
    RLDivisao_04: TRLBand;
    RLDivisao_05: TRLBand;
    rllUsuario: TRLLabel;
    rllSistema: TRLLabel;
    RLDivisao_03: TRLBand;
    RLDivisao_01: TRLBand;
    rliLogo: TRLImage;
    RLLabel8: TRLLabel;
    rlmEnderecoDist: TRLMemo;
    RLDraw11: TRLDraw;
    RLDraw12: TRLDraw;
    rlmEnderecamento: TRLMemo;
    RLDivisao_02: TRLBand;
    RLDraw13: TRLDraw;
    RLDraw14: TRLDraw;
    RLDraw17: TRLDraw;
    RLDraw18: TRLDraw;
    RLDraw20: TRLDraw;
    RLDraw21: TRLDraw;
    imgQRCode: TRLImage;
    RLDadosCliente: TRLMemo;
    RLDadosAutorizacao: TRLMemo;
    RLLabel15: TRLLabel;
    RLLabel16: TRLLabel;
    RLDraw22: TRLDraw;
    RLDraw23: TRLDraw;
    RLDraw24: TRLDraw;
    RLDraw28: TRLDraw;
    RLDraw29: TRLDraw;
    RLLabel17: TRLLabel;
    RLLabel18: TRLLabel;
    RLLabel19: TRLLabel;
    RLLabel20: TRLLabel;
    RLLabel21: TRLLabel;
    RLLabel22: TRLLabel;
    rllClassificacao: TRLLabel;
    rllTipoFornecimento: TRLLabel;
    rllLeituraAnterior: TRLLabel;
    rllLeituraAtual: TRLLabel;
    rllNDias: TRLLabel;
    rllProxLeitura: TRLLabel;
    RLLabel25: TRLLabel;
    RLLabel26: TRLLabel;
    RLLabel27: TRLLabel;
    RLLabel30: TRLLabel;
    RLLabel35: TRLLabel;
    rllMesAno: TRLLabel;
    rllDataVenc: TRLLabel;
    rllTotalPagar: TRLLabel;
    RLDraw30: TRLDraw;
    RLDraw32: TRLDraw;
    rllCodInst: TRLLabel;
    rllCodCliente: TRLLabel;
    rllChave: TRLLabel;
    RLDraw33: TRLDraw;
    rlmMensagens: TRLMemo;
    rllXmotivo: TRLLabel;
    rllHomologacao: TRLLabel;
    rlbCodigoBarras: TRLBarcode;
    RLDraw1: TRLDraw;
    RLDraw2: TRLDraw;
    RLDraw3: TRLDraw;
    rlmItemDesc: TRLMemo;
    rlmGrandContr: TRLMemo;
    RLLabel1: TRLLabel;
    RLDraw4: TRLDraw;
    RLDraw5: TRLDraw;
    RLLabel2: TRLLabel;
    RLLabel3: TRLLabel;
    RLLabel4: TRLLabel;
    RLLabel5: TRLLabel;
    RLLabel6: TRLLabel;
    RLLabel7: TRLLabel;
    RLLabel9: TRLLabel;
    RLLabel10: TRLLabel;
    RLLabel11: TRLLabel;
    RLLabel12: TRLLabel;
    RLLabel13: TRLLabel;
    RLLabel14: TRLLabel;
    RLLabel23: TRLLabel;
    RLLabel24: TRLLabel;
    RLLabel28: TRLLabel;
    RLDraw6: TRLDraw;
    RLDraw7: TRLDraw;
    rlmItemUnid: TRLMemo;
    rlmItemQtde: TRLMemo;
    rlmItemPreco: TRLMemo;
    rlmItemValor: TRLMemo;
    rlmItemPIS: TRLMemo;
    rlmItemBase: TRLMemo;
    rmlItemAliquota: TRLMemo;
    rmlItemICMS: TRLMemo;
    rlmItemTarifa: TRLMemo;
    RLLabel29: TRLLabel;
    rllValor: TRLLabel;
    rllPIS: TRLLabel;
    rllBaseCalc: TRLLabel;
    rllAliquota: TRLLabel;
    rllICMS: TRLLabel;
    RLDraw8: TRLDraw;
    RLDraw9: TRLDraw;
    RLDraw10: TRLDraw;
    RLDraw15: TRLDraw;
    RLDraw16: TRLDraw;
    RLDraw19: TRLDraw;
    RLDraw25: TRLDraw;
    RLDraw26: TRLDraw;
    RLDraw27: TRLDraw;
    RLLabel31: TRLLabel;
    rlmFisco: TRLMemo;
    RLDraw31: TRLDraw;
    RLDraw34: TRLDraw;
    RLLabel32: TRLLabel;
    RLLabel33: TRLLabel;
    rlmCol1: TRLMemo;
    rlmCol2: TRLMemo;
    rlmCol3: TRLMemo;
    rlmCol4: TRLMemo;
    RLDraw35: TRLDraw;
    RLDraw36: TRLDraw;
    RLDraw37: TRLDraw;
    RLDraw38: TRLDraw;
    RLDraw39: TRLDraw;
    RLDraw40: TRLDraw;
    RLDraw41: TRLDraw;
    RLDraw42: TRLDraw;
    RLDraw43: TRLDraw;
    RLDraw44: TRLDraw;
    RLLabel34: TRLLabel;
    RLLabel36: TRLLabel;
    RLLabel37: TRLLabel;
    RLLabel38: TRLLabel;
    RLLabel39: TRLLabel;
    RLLabel40: TRLLabel;
    RLLabel41: TRLLabel;
    RLLabel42: TRLLabel;
    RLLabel43: TRLLabel;
    RLDraw45: TRLDraw;
    rlmMedidor: TRLMemo;
    rlmGrandezas: TRLMemo;
    rlm_Col3: TRLMemo;
    rlm_Col4: TRLMemo;
    rlm_Col5: TRLMemo;
    rlm_Col6: TRLMemo;
    rlm_Col7: TRLMemo;

//    procedure rlbItensAfterPrint(Sender: TObject);
//    procedure rlbItensBeforePrint(Sender: TObject; var PrintIt: Boolean);

//    procedure subItensDataRecord(Sender: TObject; RecNo, CopyNo: Integer;
//      var EOF: Boolean; var RecordAction: TRLRecordAction);

//    procedure RLNF3eDataRecord(Sender: TObject; RecNo, CopyNo: Integer;
//      var Eof: Boolean; var RecordAction: TRLRecordAction);

    procedure RLNF3eBeforePrint(Sender: TObject; var PrintIt: Boolean);

    procedure RLDivisao_01BeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLDivisao_02BeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLDivisao_03BeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLDivisao_04BeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLDivisao_05BeforePrint(Sender: TObject; var PrintIt: Boolean);

  private
    FNumItem: Integer;
  end;

implementation

uses
  DateUtils, StrUtils, Math,
  ACBrNF3eDANF3eClass, ACBrNF3e.DANF3ERLClass,
  ACBrDFeUtil, ACBrValidador,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime,
  ACBrDFeReportFortes, ACBrImage, ACBrDelphiZXingQRCode,
  ACBrXmlBase,
  ACBrNF3eClass, ACBrNF3eConversao, pcnConversao, ACBrNF3e;

{$IfNDef FPC}
 {$R *.dfm}
{$Else}
 {$R *.lfm}
{$EndIf}

procedure TfrmDANF3eRLRetrato.RLNF3eBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  TDFeReportFortes.AjustarMargem(RLNF3e, fpDANF3e);

  RLNF3e.Title := OnlyNumber(fpNF3e.InfNF3e.Id);
end;

procedure TfrmDANF3eRLRetrato.RLDivisao_01BeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  TDFeReportFortes.CarregarLogo(rliLogo, fpDANF3e.Logo);

  rlmEnderecoDist.Lines.Clear;
  rlmEnderecoDist.Lines.Add(fpNF3e.Emit.xNome);
  rlmEnderecoDist.Lines.Add(fpNF3e.Emit.EnderEmit.xLgr + ', ' +
                            fpNF3e.Emit.EnderEmit.nro + ' ' +
                            fpNF3e.Emit.EnderEmit.xMun + '/' +
                            fpNF3e.Emit.EnderEmit.UF);
  rlmEnderecoDist.Lines.Add('Inscrição Estadual: ' +
                            FormatarIE(fpNF3e.Emit.IE, fpNF3e.Emit.EnderEmit.UF) +
                            ' ' + 'CNPJ: ' + FormatarCNPJ(fpNF3e.Emit.CNPJ));

  rlmEnderecamento.Lines.Clear;
  rlmEnderecamento.Lines.Add('...');
end;

procedure TfrmDANF3eRLRetrato.RLDivisao_02BeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  Identificacao, xURL: string;
begin
  rllClassificacao.Caption := tpGrpTensaoToDesc(fpNF3e.acessante.tpGrpTensao);
  rllTipoFornecimento.Caption := tpFaseToDesc(fpNF3e.acessante.tpFase);

  if fpNF3e.gMed.Count > 0 then
  begin
    rllLeituraAnterior.Caption := DateToStr(fpNF3e.gMed[0].dMedAnt);
    rllLeituraAtual.Caption := DateToStr(fpNF3e.gMed[0].dMedAtu);
  end;

  rllNDias.Caption := IntToStr(fpNF3e.gANEEL.gHistFat[0].gGrandFat[0].qtdDias);

  rllProxLeitura.Caption := DateToStr(fpNF3e.gFat.dProxLeitura);

  RLDadosCliente.Lines.Clear;
  RLDadosCliente.Lines.Add(fpNF3e.Dest.xNome);
  RLDadosCliente.Lines.Add(fpNF3e.Dest.EnderDest.xLgr + ', ' +
                            fpNF3e.Dest.EnderDest.nro + ' ' +
                            fpNF3e.Dest.EnderDest.xCpl);
  RLDadosCliente.Lines.Add(fpNF3e.Dest.EnderDest.xBairro + ' ' +
                            fpNF3e.Dest.EnderDest.xMun + '/' +
                            fpNF3e.Dest.EnderDest.UF);
  RLDadosCliente.Lines.Add('CEP: ' + FormatarCEP(IntToStr(fpNF3e.Dest.EnderDest.CEP)));

  if fpNF3e.Dest.idOutros <> '' then
    Identificacao := 'Identificação: ' + fpNF3e.Dest.idOutros
  else
  begin
    if Length(fpNF3e.Dest.CNPJCPF) = 11 then
      Identificacao := 'CPF: '
    else
      Identificacao := 'CNPJ: ';

    Identificacao := Identificacao + FormatarCNPJouCPF(fpNF3e.Dest.CNPJCPF);
  end;

  RLDadosCliente.Lines.Add(Identificacao);

  if fpNF3e.Dest.IE <> '' then
    RLDadosCliente.Lines.Add('Insc. Est.: ' +
                          FormatarIE(fpNF3e.Dest.IE, fpNF3e.Dest.EnderDest.UF));

  rllCodInst.Caption := fpNF3e.acessante.idAcesso;
  rllCodCliente.Caption := fpNF3e.acessante.idCodCliente;
  rllMesAno.Caption := Copy(DateToStr(fpNF3e.gFat.CompetFat), 4, 7);
  rllDataVenc.Caption := DateToStr(fpNF3e.gFat.dVencFat);
  rllTotalPagar.Caption := 'R$ ' + fpDANF3e.FormatarQuantidade(fpNF3e.Total.vNF);

  PintarQRCode(fpNF3e.infNF3eSupl.qrCodNF3e, imgQRCode.Picture.Bitmap, qrUTF8NoBOM);

  RLDadosAutorizacao.Lines.Clear;
  RLDadosAutorizacao.Lines.Add('NOTA FISCAL N. ' + IntToStr(fpNF3e.ide.nNF) + ' ' +
                               'SÉRIE: ' + IntToStr(fpNF3e.ide.serie));
  RLDadosAutorizacao.Lines.Add('DATA DE EMISSÃO: ' + DateToStr(fpNF3e.ide.dhEmi));
  RLDadosAutorizacao.Lines.Add(' ');
  RLDadosAutorizacao.Lines.Add('Consulte pela Chave de Acesso em:');
  xURL := TACBrNF3e(fpDANF3e.ACBrNF3e).GetURLConsultaNF3e(fpNF3e.ide.cUF, fpNF3e.ide.tpAmb, 1.0);
  RLDadosAutorizacao.Lines.Add(xURL);
  RLDadosAutorizacao.Lines.Add(' ');
  RLDadosAutorizacao.Lines.Add('Protocolo de Autorização: ' + fpNF3e.procNF3e.nProt);
  RLDadosAutorizacao.Lines.Add('Data: ' + DateTimeToStr(fpNF3e.procNF3e.dhRecbto));

  rllChave.Caption := FormatarChaveAcesso(fpNF3e.infNF3e.ID);

  rlmMensagens.Lines.Clear;
  rlmMensagens.Lines.Add(StringReplace(fpNF3e.infAdic.infCpl, ';', slineBreak, [rfReplaceAll]));

  rllXmotivo.Visible := True;
  {
  rlbCanceladaDenegada.Visible := fpDANF3e.Cancelada;
  if rlbCanceladaDenegada.Visible then
  begin
//    rllDadosVariaveis3_Descricao.Caption := ACBrStr('PROTOCOLO DE HOMOLOGAÇÃO DE CANCELAMENTO');
    rllXmotivo.Caption := 'NF-e CANCELADA';
    RLLCanceladaDenegada.Caption := 'NF-e CANCELADA';
  end
  else
  begin
    if (fpNF3e.procNF3e.cStat > 0) then
    begin
      case fpNF3e.procNF3e.cStat of
        100:
        begin
          rlbCodigoBarras.Visible := True;
          rllXMotivo.Visible := False;
//          rllDadosVariaveis3_Descricao.Caption := ACBrStr('PROTOCOLO DE AUTORIZAÇÃO DE USO');
        end;

        101, 135, 151, 155:
        begin
          rllXmotivo.Caption := 'NF-e CANCELADA';
//          rllDadosVariaveis3_Descricao.Caption := ACBrStr('PROTOCOLO DE HOMOLOGAÇÃO DE CANCELAMENTO');
          rlbCanceladaDenegada.Visible := True;
          RLLCanceladaDenegada.Caption := 'NF-e CANCELADA';
        end;

        110, 205, 301, 302:
        begin
          rllXmotivo.Caption := 'NF-e DENEGADA';
//          rllDadosVariaveis3_Descricao.Caption := ACBrStr('PROTOCOLO DE DENEGAÇÃO DE USO');
          rlbCanceladaDenegada.Visible := True;
          RLLCanceladaDenegada.Caption := 'NF-e DENEGADA';
        end;

        else
        begin
          rllXmotivo.Caption := fpNF3e.procNF3e.xMotivo;
//          rllDadosVariaveis3_Descricao.Visible := False;
//          rllDadosVariaveis3.Visible := False;
        end;
      end;
    end
    else
    begin
      if (fpNF3e.Ide.tpEmis = TACBrTipoEmissao.teNormal) then
      begin
        rllXmotivo.Caption := ACBrStr('NF-E NÃO ENVIADA PARA SEFAZ');
//        rllDadosVariaveis3_Descricao.Visible := False;
//        rllDadosVariaveis3.Visible := False;
        rllHomologacao.Visible := true;
        rllHomologacao.Caption := ACBrStr('NF-e NÃO PROTOCOLADA NA SEFAZ - SEM VALOR FISCAL')
      end;
    end;
  end;
  }
end;

procedure TfrmDANF3eRLRetrato.RLDivisao_03BeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  i, j: Integer;
  xLinha: string;
  Valor, Percentual: Double;
begin
  rlmItemDesc.Lines.Clear;
  rlmItemUnid.Lines.Clear;
  rlmItemQtde.Lines.Clear;
  rlmItemPreco.Lines.Clear;
  rlmItemValor.Lines.Clear;
  rlmItemPIS.Lines.Clear;
  rlmItemBase.Lines.Clear;
  rmlItemAliquota.Lines.Clear;
  rmlItemICMS.Lines.Clear;
  rlmItemTarifa.Lines.Clear;
  Percentual := 0;

  for i := 0 to fpNF3e.NFDet.Count - 1 do
  begin
    for j := 0 to fpNF3e.NFDet[i].Det.Count - 1 do
    begin
      rlmItemDesc.Lines.Add(fpNF3e.NFDet[i].Det[j].detItem.Prod.xProd);
      rlmItemUnid.Lines.Add(uMedFatToDesc(fpNF3e.NFDet[i].Det[j].detItem.Prod.uMed));

      Valor := fpNF3e.NFDet[i].Det[j].detItem.Prod.qFaturada;
      rlmItemQtde.Lines.Add(fpDANF3e.FormatarQuantidade(Valor));

      Valor := fpNF3e.NFDet[i].Det[j].detItem.Prod.vItem;
      rlmItemPreco.Lines.Add(fpDANF3e.FormatarQuantidade(Valor));

      Valor := fpNF3e.NFDet[i].Det[j].detItem.Prod.vProd;
      rlmItemValor.Lines.Add(fpDANF3e.FormatarQuantidade(Valor));

      Valor := fpNF3e.NFDet[i].Det[j].detItem.Imposto.PIS.vPIS +
               fpNF3e.NFDet[i].Det[j].detItem.Imposto.PISEfet.vPISEfet +
               fpNF3e.NFDet[i].Det[j].detItem.Imposto.COFINS.vCOFINS +
               fpNF3e.NFDet[i].Det[j].detItem.Imposto.COFINSEfet.vCOFINSEfet;
      if Valor > 0 then
        rlmItemPIS.Lines.Add(fpDANF3e.FormatarQuantidade(Valor))
      else
        rlmItemPIS.Lines.Add(' ');

      Valor := fpNF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vBC +
               fpNF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vBCST;
      if Valor > 0 then
        rlmItemBase.Lines.Add(fpDANF3e.FormatarQuantidade(Valor))
      else
        rlmItemBase.Lines.Add(' ');

      Valor := fpNF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pICMS +
               fpNF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pICMSST +
               fpNF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pFCP +
               fpNF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.pFCPST;
      Percentual := Percentual + Valor;

      if Valor > 0 then
        rmlItemAliquota.Lines.Add(fpDANF3e.FormatarAliquota(Valor))
      else
        rmlItemAliquota.Lines.Add(' ');

      Valor := fpNF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vICMS +
               fpNF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vICMSST +
               fpNF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vFCP +
               fpNF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vFCPST;
      if Valor > 0 then
        rmlItemICMS.Lines.Add(fpDANF3e.FormatarQuantidade(Valor))
      else
        rmlItemICMS.Lines.Add(' ');

//      rlmItemTarifa.Lines.Add(fpDANF3e.FormatarQuantidade(fpNF3e.NFDet[i].Det[j].detItem.Imposto.ICMS.vICMS));
    end;
  end;

  rllValor.Caption := fpDANF3e.FormatarQuantidade(fpNF3e.Total.vProd);
  rllPIS.Caption := '';
  rllBaseCalc.Caption := '';
  rllAliquota.Caption := '';
  rllICMS.Caption := '';

  Valor := fpNF3e.Total.vPIS +
           fpNF3e.Total.vPISEfet +
           fpNF3e.Total.vCOFINS +
           fpNF3e.Total.vCOFINSEfet;
  if Valor > 0 then
    rllPIS.Caption := fpDANF3e.FormatarQuantidade(Valor);

  Valor := fpNF3e.Total.vBC +
           fpNF3e.Total.vBCST;
  if Valor > 0 then
    rllBaseCalc.Caption := fpDANF3e.FormatarQuantidade(Valor);

  if Percentual > 0 then
    rllAliquota.Caption := fpDANF3e.FormatarAliquota(Percentual);

  Valor := fpNF3e.Total.vICMS +
//             fpNF3e.Total.vICMSST +
           fpNF3e.Total.vFCP +
           fpNF3e.Total.vFCPST;
  if Valor > 0 then
    rllICMS.Caption := fpDANF3e.FormatarQuantidade(Valor);

  rlmGrandContr.Lines.Clear;
  for i := 0 to fpNF3e.gGrContrat.Count - 1 do
  begin
    xLinha := tpGrContratToDesc(fpNF3e.gGrContrat[i].tpGrContrat) + ' ' +
              tpPosTarDescToStr(fpNF3e.gGrContrat[i].tpPosTar) + ' kW ' +
              fpDANF3e.FormatarQuantidade(fpNF3e.gGrContrat[i].qUnidContrat, False);
    rlmGrandContr.Lines.Add(xLinha);
  end;
end;

procedure TfrmDANF3eRLRetrato.RLDivisao_04BeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  rlmFisco.Lines.Clear;
  rlmFisco.Lines.Text := StringReplace(fpNF3e.infAdic.infAdFisco, ';', slineBreak, [rfReplaceAll]);
end;

procedure TfrmDANF3eRLRetrato.RLDivisao_05BeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  rllSistema.Visible := NaoEstaVazio(fpDANF3e.Sistema);
  rllSistema.Caption := fpDANF3e.Sistema;

  rllUsuario.Visible := NaoEstaVazio(fpDANF3e.Usuario);
  rllUsuario.Caption := ACBrStr('DATA / HORA DA IMPRESSÃO: ') + FormatDateTimeBr(Now) +
    ' - ' + fpDANF3e.Usuario;

  rllHomologacao.Visible := (fpNF3e.Ide.tpAmb = TACBrTipoAmbiente.taHomologacao);
  rllHomologacao.Caption := ACBrStr('AMBIENTE DE HOMOLOGAÇÃO - NF-E SEM VALOR FISCAL');



  rlbCodigoBarras.Visible := True;
  rlbCodigoBarras.Caption := OnlyNumber(fpNF3e.gFat.codBarras);
end;

{
procedure TfrmDANF3eRLRetrato.RLNF3eDataRecord(Sender: TObject; RecNo,
  CopyNo: Integer; var Eof: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;

  EOF := (RecNo > 1);
  RecordAction := raUseIt;
end;
}
{
procedure TfrmDANF3eRLRetrato.subItensDataRecord(Sender: TObject; RecNo, CopyNo: Integer; var EOF: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;

  FNumItem := RecNo - 1;
//  EOF := (RecNo > fpNF3e.Det.Count);
  RecordAction := raUseIt;
end;
}
{
procedure TfrmDANF3eRLRetrato.rlbItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  with fpNF3e.Det.Items[FNumItem] do
  begin
    txtCodigo.Lines.Text := IntToStr(Prod.nItem);
    rlmDescricao.Lines.Text := Prod.xProd;
    txtQuantidade.Caption := fpDANF3e.FormatarQuantidade(Prod.qCom);
    txtValor.Caption := fpDANF3e.FormatarQuantidade(Prod.vProd);
  end;
end;
}
{
procedure TfrmDANF3eRLRetrato.rlbItensAfterPrint(Sender: TObject);
begin
  //Código removido
end;
}
end.
