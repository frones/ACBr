{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit ACBrMDFeDAMDFeRLRetrato;

interface

uses
  SysUtils, 
  Variants, 
  Classes, 
  Graphics, 
  Controls, 
  Forms, 
  ExtCtrls,
  RLReport, 
  RLBarcode, 
  RLPDFFilter, 
  RLFilters,
  pcnConversao, 
  pmdfeConversaoMDFe, 
  ACBrMDFeDAMDFeRL, 
  ACBrMDFeDAMDFeClass;

type

  { TfrlDAMDFeRLRetrato }

  TfrlDAMDFeRLRetrato = class(TfrlDAMDFeRL)
    rlb_2_Rodo: TRLBand;
    rlb_3_Aereo: TRLBand;
    rlb_4_Aquav: TRLBand;
    rlb_5_Ferrov: TRLBand;
    rlb_6_Observacao: TRLBand;
    rlLabel22: TRLLabel;
    rllMsg1: TRLLabel;
    rllDataHoraImpressao: TRLLabel;
    rllSistema: TRLLabel;
    rlb_7_Documentos_Titulos: TRLBand;
    rlLabel141: TRLLabel;
    rlLabel91: TRLLabel;
    rlLabel92: TRLLabel;
    rlLabel96: TRLLabel;
    rlLabel109: TRLLabel;
    rlLabel106: TRLLabel;
    rlLabel100: TRLLabel;
    rlmChave1: TRLMemo;
    rlmObservacao: TRLMemo;
    rlb_2: TRLBand;
    rlLabel12: TRLLabel;
    rllPesoTotal: TRLLabel;
    rllqNFeMDFe: TRLLabel;
    rllqCTe: TRLLabel;
    lblQTDENFeMDFe: TRLLabel;
    rlLabel5: TRLLabel;
    subItens: TRLSubDetail;
    rlbItens: TRLBand;
    rlmChave2: TRLMemo;
    rlb_1_DadosManifesto: TRLBand;
    rliLogo: TRLImage;
    rlmEmitente: TRLMemo;
    rlmDadosEmitente: TRLMemo;
    RLPanel7: TRLPanel;
    rlLabel35: TRLLabel;
    rlLabel9: TRLLabel;
    rlLabel13: TRLLabel;
    rlLabel14: TRLLabel;
    rlLabel15: TRLLabel;
    rlLabel16: TRLLabel;
    rlmCPF: TRLMemo;
    rlmCondutor: TRLMemo;
    RLDraw7: TRLDraw;
    rlmPlaca: TRLMemo;
    rlmRNTRC: TRLMemo;
    RLLabel7: TRLLabel;
    rlLabel19: TRLLabel;
    rlLabel20: TRLLabel;
    rlLabel21: TRLLabel;
    rlmRespCNPJ: TRLMemo;
    rlmFornCNPJ: TRLMemo;
    rlmNumComprovante: TRLMemo;
    RLPanel8: TRLPanel;
    RLPanel9: TRLPanel;
    rlLabel24: TRLLabel;
    rllCodEmbar: TRLLabel;
    rlLabel26: TRLLabel;
    rllNomeEmbar: TRLLabel;
    RLLabel8: TRLLabel;
    RLLabel11: TRLLabel;
    RLLabel18: TRLLabel;
    RLLabel31: TRLLabel;
    rlmCodCarreg: TRLMemo;
    rlmNomeCarreg: TRLMemo;
    rlmCodDescarreg: TRLMemo;
    rlmNomeDescarreg: TRLMemo;
    rlbMunicipio: TRLLabel;
    RLLabel28: TRLLabel;
    RLLabel29: TRLLabel;
    rlmRespSeguradora: TRLMemo;
    RLLabel30: TRLLabel;
    rlmRespApolice: TRLMemo;
    rlmRespSeguro: TRLLabel;
    rlLabel17: TRLLabel;
    RLMemo1: TRLMemo;
    imgQRCode: TRLImage;
    RLPanel4: TRLPanel;
    RLBarcode1: TRLBarcode;
    RLPanel3: TRLPanel;
    rllChave: TRLLabel;
    rlLabel1: TRLLabel;
    rllDescricao: TRLLabel;
    rllModal: TRLLabel;
    rllProtocolo: TRLMemo;
    rllMsg2: TRLLabel;
    RLPanel6: TRLPanel;
    rlLabel2: TRLLabel;
    rllModelo: TRLLabel;
    rlLabel3: TRLLabel;
    rllSerie: TRLLabel;
    rlLabel4: TRLLabel;
    rllNumMDFe: TRLLabel;
    rlLabel25: TRLLabel;
    RLSystemInfo1: TRLSystemInfo;
    rlLabel33: TRLLabel;
    rllEmissao: TRLLabel;
    rlLabel77: TRLLabel;
    rllUFCarrega: TRLLabel;
    RLLabel6: TRLLabel;
    rllUFDescarrega: TRLLabel;
    RLPanel2: TRLPanel;
    RLPanel5: TRLPanel;
    RLPanel10: TRLPanel;
    RLPanel11: TRLPanel;
    RLPanel1: TRLPanel;
    RLPanel12: TRLPanel;
    RLPanel13: TRLPanel;
    RLPanel14: TRLPanel;
    RLLabel10: TRLLabel;
    RLLabel23: TRLLabel;
    RLDraw1: TRLDraw;
    RLDraw2: TRLDraw;
    RLDraw3: TRLDraw;
    RLDraw4: TRLDraw;
    RLDraw5: TRLDraw;
    RLDraw6: TRLDraw;
    RLDraw8: TRLDraw;
    RLDraw9: TRLDraw;
    rlLabel74: TRLLabel;
    RLPanel_Contingencia: TRLPanel;
    procedure rlb_1_DadosManifestoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_2_RodoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_3_AereoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_4_AquavBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_5_FerrovBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_6_ObservacaoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLMDFeBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure subItensDataRecord(Sender: TObject; RecNo, CopyNo: Integer;
      var Eof: Boolean; var RecordAction: TRLRecordAction);
    procedure RLMDFeDataRecord(Sender: TObject; RecNo, CopyNo: Integer;
      var Eof: Boolean; var RecordAction: TRLRecordAction);
    procedure rlbItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbItensAfterPrint(Sender: TObject);
    procedure rlb_7_Documentos_TitulosBeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure subItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
  private
    FNumItem: Integer;
  end;

implementation

uses
  StrUtils,
  Dialogs,
  DateUtils,
  pmdfeMDFe,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrUtil.Base,
  ACBrDFeUtil,
  ACBrValidador,
  ACBrImage,
  ACBrDelphiZXingQRCode,
  ACBrDFeReportFortes;

{$ifdef FPC}
 {$R *.lfm}
{$else}
 {$R *.dfm}
{$endif}

procedure TfrlDAMDFeRLRetrato.rlb_1_DadosManifestoBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  CarregouLogo: Boolean;
begin
  inherited;

  if (RLMDFe.PageNumber <> 1) then
  begin
    rlb_2_Rodo.Visible := False;
    rlb_2.Visible      := False;
  end;

  {$IFNDEF BORLAND}
    RLMemo1.Layout    := tlCenter;
    RLMemo1.Font.Size := 6;
  {$ENDIF}

  CarregouLogo := TDFeReportFortes.CarregarLogo(rliLogo, fpDAMDFe.Logo);

  if not CarregouLogo then
  begin
    rlmDadosEmitente.Left := rlmEmitente.Left;
    rlmDadosEmitente.Width := rlmEmitente.Width;
  end;

  if fpDAMDFe.ExpandeLogoMarca then
  begin
    rliLogo.top     := 3;
    rliLogo.Left    := 2;
    rliLogo.Height  := 163;
    rliLogo.Width   := 317;

    TDFeReportFortes.AjustarLogo(rliLogo, fpDAMDFe.ExpandeLogoMarcaConfig);

    rlmEmitente.visible := False;
    rlmDadosEmitente.visible := False;
  end
  else
  begin
    rlmEmitente.Enabled := True;
    rlmDadosEmitente.Enabled := True;
    // Emitente
    with fpMDFe.Emit do
    begin
      rlmEmitente.Lines.Text := XNome;

      rlmDadosEmitente.Lines.Clear;
      with EnderEmit do
      begin
        rlmDadosEmitente.Lines.Add(XLgr + ifthen(XBairro <> '', ', ' + XBairro, '') +  IfThen(Nro = '0', '', ', ' + Nro));

        if XCpl <> '' then
          rlmDadosEmitente.Lines.Add(XCpl);

        rlmDadosEmitente.Lines.Add(XMun + ' - ' + UF + '   CEP: ' + FormatarCEP(CEP));
      end;

      rlmDadosEmitente.Lines.Add(IfThen(Length(OnlyNumber(CNPJCPF)) > 11, 'CNPJ: ', 'CPF: ') + FormatarCNPJouCPF(CNPJCPF) + ACBrStr('   IE: ') + IE);
      rlmDadosEmitente.Lines.Add('TEL.: ' + FormatarFone(EnderEmit.Fone));
      rlmDadosEmitente.Lines.Add(ifthen(fpDAMDFe.Site <> '', fpDAMDFe.Site, '')
                                 + '  ' + ifthen(fpDAMDFe.Email <> '', fpDAMDFe.Email, ''));

    end;
  end;

  RLBarcode1.Caption := Copy ( fpMDFe.InfMDFe.Id, 5, 44 );
  rllChave.Caption   := FormatarChaveAcesso(Copy(fpMDFe.InfMDFe.Id, 5, 44));

  rllProtocolo.Lines.Clear;
  rllProtocolo.Font.Size  := 8;
  rllProtocolo.Font.Style := [fsBold];

  RLPanel_Contingencia.Visible := false;
  rllProtocolo.Color := clWhite;
  rllProtocolo.Font.Color := clWindowText;

  if fpMDFe.ide.tpEmis = teNormal then
  begin

    if fpDAMDFe.Protocolo <> '' then
      rllProtocolo.Lines.Add(fpDAMDFe.Protocolo)
    else
      rllProtocolo.Lines.Add(fpMDFe.procMDFe.nProt + '   ' +
        IfThen(fpMDFe.procMDFe.dhRecbto <> 0,
        DateTimeToStr(fpMDFe.procMDFe.dhRecbto), ''));
  end
  else
  begin
    if fpMDFe.procMDFe.nProt <> '' then
      rllProtocolo.Lines.Add(fpMDFe.procMDFe.nProt + '   ' +
        IfThen(fpMDFe.procMDFe.dhRecbto <> 0,
        DateTimeToStr(fpMDFe.procMDFe.dhRecbto), ''))
    else
	begin
      RLPanel_Contingencia.Visible := true;
      rllProtocolo.Color := clBlack;
      rllProtocolo.Font.Color := clBtnFace;

      rllProtocolo.Lines.Add(ACBrStr('EMISSÃO EM CONTINGÊNCIA. Obrigatória a autorização em 168 horas' +
        ' após esta Emissão (') + FormatDateTime('dd/mm/yyyy hh:nn', fpMDFe.Ide.dhEmi) + ')');
	end;
  end;

  rllModelo.Caption       := fpMDFe.Ide.modelo;
  rllSerie.Caption        := Poem_Zeros(fpMDFe.Ide.serie, 3);
  rllNumMDFe.Caption      := FormatarNumeroDocumentoFiscal(IntToStr(fpMDFe.Ide.nMDF));
  rllEmissao.Caption      := FormatDateTimeBr(fpMDFe.Ide.dhEmi);
  rllUFCarrega.Caption    := fpMDFe.Ide.UFIni;
  rllUFDescarrega.Caption := fpMDFe.Ide.UFFim;

  rlb_2_Rodo.Enabled   := False;
  rlb_3_Aereo.Enabled  := false;
  rlb_4_Aquav.Enabled  := false;
  rlb_5_Ferrov.Enabled := false;

  case fpMDFe.Ide.modal of
    moRodoviario  : rllModal.Caption := ACBrStr('MODAL RODOVIÁRIO DE CARGA');
    moAereo       : rllModal.Caption := ACBrStr('MODAL AÉREO DE CARGA');
    moAquaviario  : rllModal.Caption := ACBrStr('MODAL AQUAVIÁRIO DE CARGA');
    moFerroviario : rllModal.Caption := ACBrStr('MODAL FERROVIÁRIO DE CARGA');
  end;

  rllqCTe.Caption  := FormatFloatBr(fpMDFe.tot.qCTe,  '#0');
  lblQTDENFeMDFe.Caption := 'QTDE NF-e';
  rllqNFeMDFe.Caption := FormatFloatBr(fpMDFe.tot.qNFe, '#0');

  if fpMDFe.tot.qMDFe > 0 then
  begin
    lblQTDENFeMDFe.Caption := 'QTDE MDF-e';
    rllqNFeMDFe.Caption := FormatFloatBr(fpMDFe.tot.qMDFe, '#0');
  end;

  if fpMDFe.tot.cUnid = uTON then
    rlLabel12.Caption := 'PESO TOTAL (Ton)'
  else
    rlLabel12.Caption := 'PESO TOTAL (Kg)';

  rllPesoTotal.Caption := FormatFloatBr(fpMDFe.tot.qCarga, ',#0.0000');
end;

procedure TfrlDAMDFeRLRetrato.rlb_2_RodoBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  i: integer;
begin
  inherited;

  rlb_2_Rodo.Enabled := (fpMDFe.Ide.modal = moRodoviario);

  if rlb_2_Rodo.Enabled then
    rlb_2_Rodo.Height := 208
  else
    rlb_2_Rodo.Height := 0;

  rlmPlaca.Lines.Clear;
  rlmPlaca.Lines.Add(FormatarPlaca(fpMDFe.rodo.veicTracao.placa) + ' - ' +
                     fpMDFe.rodo.veicTracao.UF );

  rlmRNTRC.Lines.Clear;

  if fpMDFe.rodo.veicTracao.prop.RNTRC <> '' then
    rlmRNTRC.Lines.Add(fpMDFe.rodo.veicTracao.prop.RNTRC)
  else
    if (fpMDFe.infMDFe.versao >= 3) then
      rlmRNTRC.Lines.Add(fpMDFe.rodo.infANTT.RNTRC)
    else
      rlmRNTRC.Lines.Add(fpMDFe.rodo.RNTRC);

  for i := 0 to fpMDFe.rodo.veicReboque.Count - 1 do
  begin
    rlmPlaca.Lines.Add(FormatarPlaca(fpMDFe.rodo.veicReboque.Items[i].placa) + ' - ' +
                     fpMDFe.rodo.veicReboque.Items[i].UF );

    if fpMDFe.rodo.veicReboque.Items[i].prop.RNTRC <> '' then
      rlmRNTRC.Lines.Add(fpMDFe.rodo.veicReboque.Items[i].prop.RNTRC)
    else
      if (fpMDFe.infMDFe.versao >= 3) then
        rlmRNTRC.Lines.Add(fpMDFe.rodo.infANTT.RNTRC)
      else
        rlmRNTRC.Lines.Add(fpMDFe.rodo.RNTRC);
  end;

  rlmCPF.Lines.Clear;
  rlmCondutor.Lines.Clear;

  for i := 0 to fpMDFe.rodo.veicTracao.condutor.Count - 1 do
  begin
    rlmCPF.Lines.Add(FormatarCPF(fpMDFe.rodo.veicTracao.condutor.Items[i].CPF));
    rlmCondutor.Lines.Add(fpMDFe.rodo.veicTracao.condutor.Items[i].xNome);
  end;

  rlmRespCNPJ.Lines.Clear;
  rlmFornCNPJ.Lines.Clear;
  rlmNumComprovante.Lines.Clear;

  if fpMDFe.rodo.valePed.disp.Count > 0 then
  begin
    for i := 0 to fpMDFe.rodo.valePed.disp.Count - 1 do
    begin
      rlmRespCNPJ.Lines.Add(FormatarCNPJ(fpMDFe.rodo.valePed.disp.Items[i].CNPJPg));
      rlmFornCNPJ.Lines.Add(FormatarCNPJ(fpMDFe.rodo.valePed.disp.Items[i].CNPJForn));
      rlmNumComprovante.Lines.Add(fpMDFe.rodo.valePed.disp.Items[i].nCompra);
    end;
  end
  else
  if fpMDFe.rodo.infANTT.valePed.disp.Count > 0 then
  begin
    for i := 0 to fpMDFe.rodo.infANTT.valePed.disp.Count - 1 do
    begin
      rlmRespCNPJ.Lines.Add(FormatarCNPJ(fpMDFe.rodo.infANTT.valePed.disp.Items[i].CNPJPg));
      rlmFornCNPJ.Lines.Add(FormatarCNPJ(fpMDFe.rodo.infANTT.valePed.disp.Items[i].CNPJForn));
      rlmNumComprovante.Lines.Add(fpMDFe.rodo.infANTT.valePed.disp.Items[i].nCompra);
    end;
  end;

  rlmRespSeguradora.Lines.Clear;
  rlmRespApolice.Lines.Clear;


  rlmRespSeguro.Caption:= '';

  if fpMDFe.seg.Count > 0 then
    rlmRespSeguro.Caption := RspSeguroMDFeToStrText(fpMDFe.seg.Items[0].respSeg);

  for i := 0 to fpMDFe.seg.Count - 1 do
  begin
    rlmRespSeguradora.Lines.Add(fpMDFe.seg.Items[i].xSeg);
    rlmRespApolice.Lines.Add(fpMDFe.seg.Items[i].nApol);

  end;
end;

procedure TfrlDAMDFeRLRetrato.rlb_3_AereoBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  rlb_3_Aereo.Enabled := (fpMDFe.Ide.modal = moAereo);

  if rlb_3_Aereo.Enabled then
    rlb_3_Aereo.Height := 54
  else
    rlb_3_Aereo.Height := 0;
end;

procedure TfrlDAMDFeRLRetrato.rlb_4_AquavBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  i: integer;
begin
  inherited;

  rlb_4_Aquav.Enabled := (fpMDFe.Ide.modal = moAquaviario);

  if rlb_4_Aquav.Enabled then
    rlb_4_Aquav.Height := 121
  else
    rlb_4_Aquav.Height := 0;

  rllCodEmbar.Caption  := fpMDFe.aquav.cEmbar;
  rllNomeEmbar.Caption := fpMDFe.aquav.xEmbar;

  rlmCodCarreg.Lines.Clear;
  rlmNomeCarreg.Lines.Clear;
  rlmCodDescarreg.Lines.Clear;
  rlmNomeDescarreg.Lines.Clear;

  for i := 0 to fpMDFe.aquav.infTermCarreg.Count - 1 do
  begin
    rlmCodCarreg.Lines.Add(fpMDFe.aquav.infTermCarreg.Items[i].cTermCarreg);
    rlmNomeCarreg.Lines.Add(fpMDFe.aquav.infTermCarreg.Items[i].xTermCarreg);
  end;

  for i := 0 to fpMDFe.aquav.infTermDescarreg.Count - 1 do
  begin
    rlmCodDescarreg.Lines.Add(fpMDFe.aquav.infTermDescarreg.Items[i].cTermDescarreg);
    rlmNomeDescarreg.Lines.Add(fpMDFe.aquav.infTermDescarreg.Items[i].xTermDescarreg);
  end;
end;

procedure TfrlDAMDFeRLRetrato.rlb_5_FerrovBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  rlb_5_Ferrov.Enabled := (fpMDFe.Ide.modal = moFerroviario);

  if rlb_5_Ferrov.Enabled then
    rlb_5_Ferrov.Height := 60
  else
    rlb_5_Ferrov.Height := 0;
end;

procedure TfrlDAMDFeRLRetrato.rlb_6_ObservacaoBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  rlmObservacao.Lines.BeginUpdate;
  rlmObservacao.Lines.Clear;
  rlmObservacao.Lines.Add(StringReplace(fpMDFe.infAdic.infCpl, '&lt;BR&gt;', #13#10, [rfReplaceAll, rfIgnoreCase]));
  rlmObservacao.Lines.Text := StringReplace(rlmObservacao.Lines.Text, ';', #13, [rfReplaceAll]);
  rlmObservacao.Lines.EndUpdate;

  // Mensagem para modo Homologacao.

  rllMsg1.Caption := '';
  rllMsg1.Enabled := False;
  rllMsg1.Visible := False;
  rllMsg2.Caption := '';
  rllMsg2.Enabled := False;
  rllMsg2.Visible := False;

  if fpMDFe.procMDFe.cStat > 0 then
  begin
    if fpMDFe.Ide.tpAmb = taHomologacao then
    begin
      rllMsg1.Caption := ACBrStr('AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL');
      rllMsg1.Enabled := True;
      rllMsg1.Visible := True;
    end;

    if (fpMDFe.procMDFe.cStat = 101) or (fpDAMDFe.Cancelada) then
    begin
      rllMsg2.Caption := 'MDF-e CANCELADO';
      rllMsg2.Visible := True;
      rllMsg2.Enabled := True;
    end;

    if (fpMDFe.procMDFe.cStat = 100) and (fpDAMDFe.Encerrado) then
    begin
      rllMsg2.Caption := 'MDF-e ENCERRADO';
      rllMsg2.Visible := True;
      rllMsg2.Enabled := True;
    end;

    if fpMDFe.procMDFe.cStat = 110 then
    begin
      rllMsg2.Caption := 'MDF-e DENEGADO';
      rllMsg2.Visible := True;
      rllMsg2.Enabled := True;
    end;

    if not fpMDFe.procMDFe.cStat in [100, 101, 110] then
    begin
      rllMsg2.Caption := fpMDFe.procMDFe.xMotivo;
      rllMsg2.Visible := True;
      rllMsg2.Enabled := True;
    end;
  end
  else
  begin
    rllMsg1.Caption := ACBrStr('MDF-E NÃO ENVIADO PARA SEFAZ');
    rllMsg1.Visible := True;
    rllMsg1.Enabled := True;
  end;

  rllMsg1.Repaint;
  rllMsg2.Repaint;

  // imprime data e hora da impressao
  rllDataHoraImpressao.Caption := ACBrStr('DATA / HORA DA IMPRESSÃO: ') +
    FormatDateTime('dd/mm/yyyy hh:nn:ss', Now);

  // imprime usuario
  if fpDAMDFe.Usuario <> '' then
    rllDataHoraImpressao.Caption := rllDataHoraImpressao.Caption + ' - ' + fpDAMDFe.Usuario;
//    rllDataHoraImpressao.Caption := rllDataHoraImpressao.Caption + ACBrStr('   USUÁRIO: ') + fpDAMDFe.Usuario;

  // imprime sistema
  if fpDAMDFe.Sistema <> '' then
    rllSistema.Caption := fpDAMDFe.Sistema
//    rllSistema.Caption := 'Desenvolvido por ' + fpDAMDFe.Sistema
  else
    rllSistema.Caption := '';
end;

procedure TfrlDAMDFeRLRetrato.RLMDFeBeforePrint(Sender: TObject; var PrintIt: boolean);
begin
  inherited;


  RLMDFe.FirstPageNumber := 1;
  RLMDFe.Title  := ACBrStr('Manifesto Eletrônico de Documentos Fiscais - MDF-e');

  TDFeReportFortes.AjustarMargem(RLMDFe, fpDAMDFe);

  with RLMDFe do
  begin
    Title              := ACBrStr('Manifesto Eletrônico de Documentos Fiscais - MDF-e');
    Borders.DrawTop    := False;
    Borders.DrawLeft   := False;
    Borders.DrawRight  := False;
    Borders.DrawBottom := False;
  end;

  with rlb_1_DadosManifesto do
  begin
    Borders.DrawTop    := False;
    Borders.DrawLeft   := False;
    Borders.DrawRight  := False;
    Borders.DrawBottom := False;
  end;

  with rlb_2_Rodo do
  begin
    Borders.DrawTop    := False;
    Borders.DrawLeft   := False;
    Borders.DrawRight  := False;
    Borders.DrawBottom := False;
  end;

  with rlb_2 do
  begin
    Borders.DrawTop    := False;
    Borders.DrawLeft   := False;
    Borders.DrawRight  := False;
    Borders.DrawBottom := False;
  end;

  with rlb_7_Documentos_Titulos do
  begin
    Borders.DrawTop    := False;
    Borders.DrawLeft   := False;
    Borders.DrawRight  := False;
    Borders.DrawBottom := False;
  end;

  with rlb_6_Observacao do
  begin
    BandType           := btFooter;
    Borders.DrawTop    := False;
    Borders.DrawLeft   := False;
    Borders.DrawRight  := False;
    Borders.DrawBottom := False;
  end;

  subItens.Borders.DrawLeft  := False;
  subItens.Borders.DrawRight := False;

  with rlbItens do
  begin
    AutoSize           := True;
    IntegralHeight     := False;
    Borders.DrawTop    := False;
    Borders.DrawLeft   := False;
    Borders.DrawRight  := False;
    Borders.DrawBottom := False;
    rlbMunicipio.Visible:= fPDAMDFE.ExibirMunicipioDescarregamento;
  end;

  rlmChave1.Lines.Clear;
  rlmChave1.Align := faWidth;
//  rlmChave2.Lines := rlmChave1.Lines;
  rlmChave2.Visible:= false;

  rlmChave1.AutoSize := True;
//  rlmChave2.AutoSize := rlmChave1.AutoSize;

  if not EstaVazio(Trim(fpMDFe.infMDFeSupl.qrCodMDFe)) then
    PintarQRCode( fpMDFe.infMDFeSupl.qrCodMDFe, imgQRCode.Picture.Bitmap, qrUTF8NoBOM )
  else
    imgQRCode.Visible := False;
end;

procedure TfrlDAMDFeRLRetrato.RLMDFeDataRecord(Sender: TObject; RecNo,
  CopyNo: Integer; var Eof: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;

  Eof := (RecNo > 1);
  RecordAction := raUseIt;
end;

procedure TfrlDAMDFeRLRetrato.subItensDataRecord(Sender: TObject; RecNo,
  CopyNo: Integer; var Eof: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;

  FNumItem     := RecNo - 1;
  Eof          := (RecNo > fpMDFe.infDoc.infMunDescarga.Count);
  RecordAction := raUseIt;
end;

procedure TfrlDAMDFeRLRetrato.rlbItensAfterPrint(Sender: TObject);
begin
  inherited;
  rlb_6_Observacao.Visible := RLMDFe.PageNumber = 1 ;
  rlmChave1.Lines.Clear;
//  rlmChave2.Lines.Clear;
end;

procedure TfrlDAMDFeRLRetrato.rlbItensBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  J, nItem: integer;
  LDocumentos1, LDocumentos2 :TStrings;
  LDocumento1, LDocumento2 : String;


  procedure Printar( sTemp : String; nItem : Integer );
  begin
    if (nItem mod 2) = 0 then
      LDocumentos1.Add(sTemp)
    else
      LDocumentos2.Add( sTemp);
  end;
begin
  nItem := 0;
  rlmChave1.Lines.Clear;
//  rlmChave2.Lines.Clear;

  LDocumentos1 := TStringList.Create;
  try
    LDocumentos2 := TStringList.Create;
    try

      with fpMDFe.infDoc.infMunDescarga.Items[FNumItem] do
      begin
        if rlbMunicipio.Visible then
          rlbMunicipio.Caption := ACBrStr(Format('Município de Descarregamento: %s ',[ fpMDFe.infDoc.infMunDescarga.Items[FNumItem].xMunDescarga]));

       // Lista de CT-e
        for J := 0 to ( infCTe.Count - 1) do
        begin
          Printar( 'CT-e          ' + FormatarChaveAcesso(infCTe.Items[J].chCTe), nItem);
          Inc(nItem);
        end;

       // Lista de CT
        for J := 0 to (infCT.Count - 1) do
        begin
          Printar( 'CT            ' + FormatarCNPJouCPF(fpMDFe.emit.CNPJCPF) + ' - '
                                    + IntToStr(infCT.Items[J].serie)     + '-'
                                    + infCT.Items[J].nCT , nItem );
          Inc(nItem);
        end;

        // Lista de NF-e
        for J := 0 to (infNFe.Count - 1) do
        begin
          Printar( 'NF-e          ' + FormatarChaveAcesso(infNFe.Items[J].chNFe),nItem );
          Inc(nItem);
        end;

        // Lista de NF
        for J := 0 to ( infNF.Count - 1) do
        begin
          Printar( 'NF            ' + FormatarCNPJouCPF(infNF.Items[J].CNPJ) + ' - '
                                    + IntToStr(infNF.Items[J].serie) + '-'
                                    + IntToStr(infNF.Items[J].nNF),nItem);
          Inc(nItem);
        end;

        // Lista de MDF-e
        for J := 0 to ( infMDFeTransp.Count - 1) do
        begin
          Printar( 'MDF-e         ' + FormatarChaveAcesso( infMDFeTransp.Items[J].chMDFe),nItem);
          Inc(nItem);
        end;
      end;
      for j := 0 to LDocumentos1.Count -1 do
      begin
        LDocumento1:= EmptyStr;
        LDocumento2:= EmptyStr;
        if LDocumentos1.Count -1 >= j  then
          LDocumento1:= LDocumentos1[j];
        if LDocumentos2.Count -1 >= j  then
          LDocumento2:= LDocumentos2[j];
        rlmChave1.Lines.Add(Format('%s         %s', [LDocumento1,LDocumento2])   );
      end;
      rlmChave1.Lines.Add(' ');
    finally
      LDocumentos2.free;
    end;
  finally
    LDocumentos1.free;
  end;
  inherited;
end;

procedure TfrlDAMDFeRLRetrato.rlb_7_Documentos_TitulosBeforePrint(
  Sender: TObject; var PrintIt: Boolean);
begin
  PrintIt := (deRelacaoDFe in fPDAMDFE.ImprimeDadosExtras);
  inherited;
end;

procedure TfrlDAMDFeRLRetrato.subItensBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  PrintIt := (deRelacaoDFe in fPDAMDFE.ImprimeDadosExtras);
  inherited;
end;

end.
