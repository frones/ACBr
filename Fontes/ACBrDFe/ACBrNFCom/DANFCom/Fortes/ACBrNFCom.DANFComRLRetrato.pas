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

unit ACBrNFCom.DANFComRLRetrato;

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
  ACBrNFCom.DANFComRL, RLBarcode, RLFilters, RLPDFFilter, DB;

type

  { TfrlDANFComRLRetrato }

  TfrlDANFComRLRetrato = class(TfrmDANFComRL)
    rlbDivisao01: TRLBand;
    rliEmitente: TRLDraw;
    rllDocumento1: TRLLabel;
    rllNumNF1: TRLLabel;
    rllSERIE1: TRLLabel;
    rlbDivisao02: TRLBand;
    rlbDivisao05: TRLBand;
    rlsRectProdutos: TRLDraw;
    RLDraw50: TRLDraw;
    RLLabel23: TRLLabel;
    rlsDivProd2: TRLDraw;
    rlsDivProd5: TRLDraw;
    rlsDivProd6: TRLDraw;
    rlsDivProd7: TRLDraw;
    rlsDivProd8: TRLDraw;
    rlsDivProd9: TRLDraw;
    rlsDivProd10: TRLDraw;
    rlsDivProd12: TRLDraw;
    rllDadosVariaveis1b: TRLLabel;
    rllDadosVariaveis3_Descricao: TRLLabel;
    RLLabel77: TRLLabel;
    lblUnidade: TRLLabel;
    lblValorTotal: TRLLabel;
    lblValorUnitarioSup: TRLLabel;
    lblValorUnitarioInf: TRLLabel;
    lblBaseSup: TRLLabel;
    lblBaseInf: TRLLabel;
    lblQuantidade: TRLLabel;
    lblValICMSSup: TRLLabel;
    lblValICMSinf: TRLLabel;
    lblAliquota: TRLLabel;
    rlmEmitente: TRLMemo;
    rliLogo: TRLImage;
    rllDadosVariaveis3: TRLLabel;
    rlmDadosAdicionais: TRLMemo;
    rllChave: TRLLabel;
    rllXmotivo: TRLLabel;
    RLLabel26: TRLLabel;
    rllPageNumber: TRLSystemInfo;
    rllLastPage: TRLSystemInfo;
    rlbDivisao04: TRLBand;
    rllHomologacao: TRLLabel;
    rlmDescricaoProduto: TRLMemo;
    lblPIS: TRLLabel;
    lblCOFINS: TRLLabel;
    RLDraw4: TRLDraw;
    lblValorTotalSup: TRLLabel;
    rlbDivisao03: TRLSubDetail;
    rlbItens: TRLBand;
    LinhaUnidade: TRLDraw;
    LinhaQuantidade: TRLDraw;
    LinhaValorUnitario: TRLDraw;
    LinhaValorTotal: TRLDraw;
    LinhaBaseICMS: TRLDraw;
    LinhaValorICMS: TRLDraw;
    LinhaAliqICMS: TRLDraw;
    LinhaNCM: TRLDraw;
    LinhaFimItens: TRLDraw;
    rlmDescricao: TRLMemo;
    txtUnidade: TRLMemo;
    txtQuantidade: TRLMemo;
    txtValorUnitario: TRLMemo;
    txtValorTotal: TRLLabel;
    txtValorPISCOFINS: TRLLabel;
    txtBaseICMS: TRLLabel;
    txtValorICMS: TRLLabel;
    txtAliqICMS: TRLLabel;
    FundoItem: TRLLabel;
    LinhaCodigo: TRLDraw;
    LinhaFinal: TRLDraw;
    LinhaItem: TRLDraw;
    rlmDestinatario: TRLMemo;
    imgQRCode: TRLImage;
    RLLabel2: TRLLabel;
    rllDataEmissao: TRLLabel;
    RLLabel3: TRLLabel;
    RLDraw1: TRLDraw;
    rldrw1: TRLDraw;
    RLLabel4: TRLLabel;
    RLLabel5: TRLLabel;
    RLDraw6: TRLDraw;
    RLLabel6: TRLLabel;
    RLDraw8: TRLDraw;
    rllReferencia: TRLLabel;
    rllVencimento: TRLLabel;
    rllTotalPagar: TRLLabel;
    RLDraw2: TRLDraw;
    RLLabel54: TRLLabel;
    rllTotalNF: TRLLabel;
    RLLabel1: TRLLabel;
    RLLabel7: TRLLabel;
    RLLabel8: TRLLabel;
    RLLabel9: TRLLabel;
    rllValorICMS: TRLLabel;
    rllBaseICMS: TRLLabel;
    rllDescontos: TRLLabel;
    rllAcessorias: TRLLabel;
    RLDraw7: TRLDraw;
    RLDraw9: TRLDraw;
    RLDraw10: TRLDraw;
    RLDraw11: TRLDraw;
    RLDraw12: TRLDraw;
    RLDraw13: TRLDraw;
    RLLabel10: TRLLabel;
    RLLabel11: TRLLabel;
    RLLabel12: TRLLabel;
    RLDraw14: TRLDraw;
    RLDraw15: TRLDraw;
    RLDraw16: TRLDraw;
    RLDraw18: TRLDraw;
    RLDraw19: TRLDraw;
    RLLabel13: TRLLabel;
    RLLabel14: TRLLabel;
    RLLabel15: TRLLabel;
    RLLabel16: TRLLabel;
    RLDraw17: TRLDraw;
    rllValorPIS: TRLLabel;
    rllValorCOFINS: TRLLabel;
    rllValorFUST: TRLLabel;
    rllValorFUNTTEL: TRLLabel;
    RLDraw20: TRLDraw;
    RLLabel17: TRLLabel;
    RLDraw21: TRLDraw;
    rlmDadosFisco: TRLMemo;
    rlmAreaContribuinte: TRLMemo;
    rlbDivisao06: TRLBand;
    RLLabel18: TRLLabel;
    RLLabel19: TRLLabel;
    RLDraw45: TRLDraw;
    rlbCodBarLinhaDig: TRLBarcode;
    RLDraw46: TRLDraw;
    RLLabel49: TRLLabel;
    RLLabel50: TRLLabel;
    RLLabel51: TRLLabel;
    RLLabel52: TRLLabel;
    rllMesRef: TRLLabel;
    rllVencimento2: TRLLabel;
    rllTotPagar: TRLLabel;
    rllIdentDebAut: TRLLabel;
    RLDraw47: TRLDraw;
    RLDraw48: TRLDraw;
    RLDraw49: TRLDraw;
    RLDraw3: TRLDraw;
    rllLinhaDig: TRLLabel;
    rllNumFat: TRLLabel;
    rllCodAgencia: TRLLabel;
    rllCodBanco: TRLLabel;
    rlbDivisao07: TRLBand;
    rllUsuario: TRLLabel;
    rllSistema: TRLLabel;
    rlbDivisao08: TRLBand;
    rlmAnatel: TRLMemo;
    RLDraw5: TRLDraw;

    procedure rlbDivisao01BeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbDivisao03DataRecord(Sender: TObject; RecNo, CopyNo: Integer;
      var EOF: Boolean; var RecordAction: TRLRecordAction);
    procedure rlbDivisao04BeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbDivisao05BeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure rlbDivisao06BeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbDivisao07BeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure rlbDivisao08BeforePrint(Sender: TObject; var PrintIt: Boolean);

    procedure RLNFComBeforePrint(Sender: TObject; var PrintIt: Boolean);

    procedure rlbItensAfterPrint(Sender: TObject);
    procedure rlbItensBeforePrint(Sender: TObject; var PrintIt: Boolean);

    procedure RLNFComDataRecord(Sender: TObject; RecNo, CopyNo: Integer;
      var EOF: Boolean; var RecordAction: TRLRecordAction);

  private
    FNumItem: Integer;
  end;

implementation

uses
  DateUtils, StrUtils, Math,
  ACBrImage,
  ACBrDelphiZXingQRCode,
  ACBrXmlBase,
  ACBrDFeUtil, 
  ACBrDFeReportFortes,
  ACBrUtil.Base, 
  ACBrUtil.Strings, 
  ACBrUtil.DateTime,
  ACBrValidador,
  pcnConversao, 
  ACBrNFCom, 
  ACBrNFComClass, 
  ACBrNFComConversao,
  ACBrNFCom.DANFComRLClass;

{$IfNDef FPC}
 {$R *.dfm}
{$Else}
 {$R *.lfm}
{$EndIf}

procedure TfrlDANFComRLRetrato.rlbDivisao01BeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  sTemp: string;
begin
  TDFeReportFortes.AjustarMargem(RLNFCom, fpDANFCom);
  TDFeReportFortes.CarregarLogo(rliLogo, fpDANFCom.Logo);

  rlmEmitente.AutoSize := False;

  with fpNFCom.Emit do
  begin
    rlmEmitente.Lines.Add(xNome);

    sTemp := Trim(EnderEmit.XLgr) +
      IfThen(EnderEmit.Nro = '0', '', ', ' + EnderEmit.Nro) + ' ' +
      IfThen(NaoEstaVazio(EnderEmit.xCpl), Trim(EnderEmit.XCpl), '') + ' - ' +
      Trim(EnderEmit.XBairro);

    rlmEmitente.Lines.Add(sTemp);

    rlmEmitente.Lines.Add('CNPJ: ' + FormatarCNPJouCPF(CNPJ));
    rlmEmitente.Lines.Add('INSCRIÇÃO ESTADUAL: ' + IE);
    {
    sTemp := sTemp + ' - CEP:' + FormatarCEP(EnderEmit.CEP) + ' - ' +
      EnderEmit.XMun + ' - ' + EnderEmit.UF;

    sTemp := 'TEL: ' + FormatarFone(EnderEmit.Fone);
    }
  end;

  with fpNFCom.Dest do
  begin
    rlmDestinatario.Lines.Add(xNome);

    sTemp := Trim(EnderDest.XLgr) +
      IfThen(EnderDest.Nro = '0', '', ', ' + EnderDest.Nro) + ' ' +
      IfThen(NaoEstaVazio(EnderDest.xCpl), Trim(EnderDest.XCpl), '') + ' - ' +
      Trim(EnderDest.XBairro);

    rlmDestinatario.Lines.Add(sTemp);

    if idOutros = '' then
    begin
      rlmDestinatario.Lines.Add('CNPJ/CPF: ' + FormatarCNPJouCPF(CNPJCPF));
      rlmDestinatario.Lines.Add('INSCRIÇÃO ESTADUAL: ' + IE);
    end
    else
      rlmDestinatario.Lines.Add('idOutros: ' + idOutros);

    rlmDestinatario.Lines.Add('CÓDIGO CLIENTE: ' + fpNFCom.assinante.iCodAssinante);
    rlmDestinatario.Lines.Add('N. TELEFONE: ' + FormatarFone(EnderDest.Fone));

    rlmDestinatario.Lines.Add('PERÍODO: ' + '');
  end;

  PintarQRCode(fpNFCom.infNFComSupl.qrCodNFCom, imgQRCode.Picture.Bitmap, qrUTF8NoBOM);

  rllNumNF1.Caption := ACBrStr('NOTA FISCAL FATURA No. ') +
                       FormatarNumeroDocumentoFiscal(IntToStr(fpNFCom.Ide.nNF));

  rllSERIE1.Caption := ACBrStr('SÉRIE: ') +
                       PadLeft(IntToStr(fpNFCom.Ide.Serie), 3, '0');

  rllDataEmissao.Caption := ACBrStr('DATA DE EMISSÃO: ') +
                            FormatDateBr(fpNFCom.Ide.dhEmi);

  rllDadosVariaveis1b.Caption := 'http://dfe-portal.sefazvirtual.rs.gov.br/NFCom';

  rllChave.Caption := FormatarChaveAcesso(fpNFCom.InfNFCom.Id);
  rllChave.AutoSize := True;

  rllDadosVariaveis3_Descricao.Visible := True;
  rllDadosVariaveis3.Visible := True;
  rllXMotivo.Visible := True;

  if fpDANFCom.Cancelada then
  begin
    rllXmotivo.Caption := 'NFCom CANCELADA';

    rllDadosVariaveis3_Descricao.Visible := False;
    rllDadosVariaveis3.Visible := False;
  end
  else
  begin
    if (fpNFCom.procNFCom.cStat > 0) then
    begin
      case fpNFCom.procNFCom.cStat of
        100:
        begin
          rllDadosVariaveis3_Descricao.Caption := ACBrStr('PROTOCOLO DE AUTORIZAÇÃO DE USO');
          rllDadosVariaveis3.Caption := fpNFCom.procNFCom.nProt + ' ' +
                                   FormatDateTimeBr(fpNFCom.procNFCom.dhRecbto);

          rllXMotivo.Visible := False;
        end;

        101, 135, 151, 155:
        begin
          rllXmotivo.Caption := 'NFCom CANCELADA';
          rllDadosVariaveis3_Descricao.Caption := ACBrStr('PROTOCOLO DE HOMOLOGAÇÃO DE CANCELAMENTO');
        end;
      else
        begin
          rllXmotivo.Caption := fpNFCom.procNFCom.xMotivo;

          rllDadosVariaveis3_Descricao.Visible := False;
          rllDadosVariaveis3.Visible := False;
        end;
      end;
    end
    else
    begin
      if (fpNFCom.Ide.tpEmis = TACBrTipoEmissao(teNormal)) then
      begin
        rllXmotivo.Caption := ACBrStr('NFCom NÃO ENVIADA PARA SEFAZ');
        rllDadosVariaveis3_Descricao.Visible := False;
        rllDadosVariaveis3.Visible := False;
        rllHomologacao.Visible := true;
        rllHomologacao.Caption := ACBrStr('NFCom NÃO PROTOCOLADA NA SEFAZ - SEM VALOR FISCAL')
      end;
    end;
  end;

  sTemp := FormatDateBr(fpNFCom.gFat.CompetFat);
  sTemp := RightStr(sTemp, 4) + '/' + Copy(sTemp, 4, 2);
  rllReferencia.Caption := sTemp;
  rllVencimento.Caption := FormatDateBr(fpNFCom.gFat.dVencFat);
  rllTotalPagar.Caption := 'R$ ' + FormatFloatBr(fpNFCom.Total.vNF);
end;

procedure TfrlDANFComRLRetrato.rlbDivisao03DataRecord(Sender: TObject;
  RecNo, CopyNo: Integer; var EOF: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;

  FNumItem := RecNo - 1;
  EOF := (RecNo > fpNFCom.Det.Count);
  RecordAction := raUseIt;
end;

procedure TfrlDANFComRLRetrato.rlbDivisao04BeforePrint(
  Sender: TObject; var PrintIt: Boolean);
begin
  with fpNFCom.Total do
  begin
    rllTotalNF.Caption := FormatFloatBr(VNF);
    rllBaseICMS.Caption := FormatFloatBr(VBC);
    rllValorICMS.Caption := FormatFloatBr(VICMS);
    rllDescontos.Caption := FormatFloatBr(VDesc);
    rllAcessorias.Caption := FormatFloatBr(VOutro);

    rllValorPIS.Caption := FormatFloatBr(vPIS);
    rllValorCOFINS.Caption := FormatFloatBr(vCOFINS);
    rllValorFUST.Caption := FormatFloatBr(vFUST);
    rllValorFUNTTEL.Caption := FormatFloatBr(vFUNTTEL);
  end;
end;

procedure TfrlDANFComRLRetrato.rlbDivisao05BeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  rlmDadosAdicionais.Lines.Text := fpNFCom.infAdic.infCpl;
end;

procedure TfrlDANFComRLRetrato.rlbDivisao06BeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  sTemp: string;
begin
  rllIdentDebAut.Caption := fpNFCom.gFat.codDebAuto;
  sTemp := FormatDateBr(fpNFCom.gFat.CompetFat);
  sTemp := RightStr(sTemp, 4) + '/' + Copy(sTemp, 4, 2);
  rllMesRef.Caption := sTemp;
  rllVencimento2.Caption := FormatDateBr(fpNFCom.gFat.dVencFat);
  rllTotPagar.Caption := 'R$ ' + FormatFloatBr(fpNFCom.Total.vNF);

  rllNumFat.Caption := 'Número da Fatura: ' +
                       FormatarNumeroDocumentoFiscal(IntToStr(fpNFCom.Ide.nNF));

  if fpNFCom.gFat.codAgencia <> '' then
  begin
    rllCodAgencia.Caption := 'Agência: ' + fpNFCom.gFat.codAgencia;
    rllCodBanco.Caption := 'Banco: ' + fpNFCom.gFat.codBanco;
  end;

  sTemp := fpNFCom.gFat.codBarras;
  rllLinhaDig.Caption := Copy(sTemp, 01, 12) + ' ' + Copy(sTemp, 13, 12) + ' ' +
                         Copy(sTemp, 25, 12) + ' ' + Copy(sTemp, 37, 12);
  rlbCodBarLinhaDig.Visible := True;
  rlbCodBarLinhaDig.Caption := OnlyNumber(fpNFCom.gFat.codBarras);
end;

procedure TfrlDANFComRLRetrato.rlbDivisao07BeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  rllSistema.Visible := NaoEstaVazio(fpDANFCom.Sistema);
  rllSistema.Caption := fpDANFCom.Sistema;

  rllUsuario.Visible := NaoEstaVazio(fpDANFCom.Usuario);
  rllUsuario.Caption := ACBrStr('DATA / HORA DA IMPRESSÃO: ') + FormatDateTimeBr(Now) +
    ' - ' + fpDANFCom.Usuario;
end;

procedure TfrlDANFComRLRetrato.rlbDivisao08BeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  rlmAnatel.Lines.Clear;

end;

procedure TfrlDANFComRLRetrato.RLNFComBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  RLNFCom.Title := OnlyNumber(fpNFCom.InfNFCom.Id);
end;

procedure TfrlDANFComRLRetrato.RLNFComDataRecord(Sender: TObject;
  RecNo, CopyNo: Integer; var EOF: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;

  EOF := (RecNo > 1);
  RecordAction := raUseIt;
end;

procedure TfrlDANFComRLRetrato.rlbItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  with fpNFCom.Det[FNumItem] do
  begin
    rlmDescricao.Lines.Text := Prod.xProd;
    txtUnidade.Lines.Text := uMedToStr(Prod.uMed);
    txtQuantidade.Lines.Text := fpDANFCom.FormatarQuantidade(Prod.qFaturada);
    txtValorUnitario.Lines.Text := fpDANFCom.FormatarValorUnitario(Prod.vItem);
    txtValorTotal.Caption := FormatFloatBr(Prod.vProd);
    txtValorPISCOFINS.Caption := FormatFloatBr(Imposto.PIS.vPIS + Imposto.COFINS.vCOFINS);
    txtBaseICMS.Caption := FormatFloatBr(Imposto.ICMS.VBC);
    txtAliqICMS.Caption := FormatFloatBr(Imposto.ICMS.PICMS + Imposto.ICMS.pFCP);
    txtValorICMS.Caption := FormatFloatBr(Imposto.ICMS.VICMS + Imposto.ICMS.vFCP);

{
Deverá ser apresentada uma linha totalizadora “TOTAL” ao final do quadro dos itens.
}
  end;
end;

procedure TfrlDANFComRLRetrato.rlbItensAfterPrint(Sender: TObject);
begin
  //Código removido
end;

end.
