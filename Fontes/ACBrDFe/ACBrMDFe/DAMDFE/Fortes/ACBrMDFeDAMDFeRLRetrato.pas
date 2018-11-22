{******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
******************************************************************************}

{$I ACBr.inc}

unit ACBrMDFeDAMDFeRLRetrato;

interface

uses
  Messages, SysUtils, Variants, Classes, db, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, RLReport, RLBarcode, RLPDFFilter, pcnConversao,
  pmdfeConversaoMDFe, ACBrMDFeDAMDFeRL, ACBrMDFeDAMDFeClass, ACBrMDFeDAMDFeRLClass,
  RLFilters;

type

  { TfrlDAMDFeRLRetrato }

  TfrlDAMDFeRLRetrato = class(TfrlDAMDFeRL)
    rlb_2_Rodo: TRLBand;
    rlb_3_Aereo: TRLBand;
    rlb_4_Aquav: TRLBand;
    rlb_5_Ferrov: TRLBand;
    rlb_6_Observacao: TRLBand;
    rlLabel22: TRLLabel;
    rllMsgTeste: TRLLabel;
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
    rlmObservacao: TRLMemo;
    RLBand1: TRLBand;
    RLDraw2: TRLDraw;
    RLDraw3: TRLDraw;
    RLDraw4: TRLDraw;
    rlLabel12: TRLLabel;
    rllPesoTotal: TRLLabel;
    rllqMDFe: TRLLabel;
    rllqNFe: TRLLabel;
    rllqCTe: TRLLabel;
    rlLabel23: TRLLabel;
    rlLabel10: TRLLabel;
    rlLabel5: TRLLabel;
    RLBand2: TRLBand;
    rllModal: TRLLabel;
    subItens: TRLSubDetail;
    rlbItens: TRLBand;
    LinhaQuantidade: TRLDraw;
    rlmChave1: TRLMemo;
    rlmChave2: TRLMemo;
    rlb_1_DadosManifesto: TRLBand;
    rliLogo: TRLImage;
    rlmEmitente: TRLMemo;
    rlmDadosEmitente: TRLMemo;
    RLPanel1: TRLPanel;
    RLPanel2: TRLPanel;
    rllProtocolo: TRLLabel;
    rllDescricao: TRLLabel;
    RLPanel3: TRLPanel;
    rllChave: TRLLabel;
    rlLabel1: TRLLabel;
    RLPanel4: TRLPanel;
    RLBarcode1: TRLBarcode;
    rlLabel74: TRLLabel;
    rlLabel17: TRLLabel;
    RLMemo1: TRLMemo;
    RLPanel5: TRLPanel;
    RLPanel6: TRLPanel;
    rllModelo: TRLLabel;
    rllSerie: TRLLabel;
    rllNumMDFe: TRLLabel;
    RLSystemInfo1: TRLSystemInfo;
    rllEmissao: TRLLabel;
    rllUFCarrega: TRLLabel;
    rllUFDescarrega: TRLLabel;
    rlLabel2: TRLLabel;
    rlLabel3: TRLLabel;
    rlLabel4: TRLLabel;
    rlLabel25: TRLLabel;
    rlLabel33: TRLLabel;
    rlLabel77: TRLLabel;
    RLLabel6: TRLLabel;
    rlsLinhaV05: TRLDraw;
    rlsLinhaV06: TRLDraw;
    rlsLinhaV07: TRLDraw;
    rlsLinhaV08: TRLDraw;
    rlsLinhaV09: TRLDraw;
    RLDraw1: TRLDraw;
    RLPanel7: TRLPanel;
    rlShape10: TRLDraw;
    rlLabel35: TRLLabel;
    rlLabel9: TRLLabel;
    rlLabel13: TRLLabel;
    rlLabel14: TRLLabel;
    rlLabel15: TRLLabel;
    rlLabel16: TRLLabel;
    RLDraw5: TRLDraw;
    RLDraw6: TRLDraw;
    rlmCPF: TRLMemo;
    rlmCondutor: TRLMemo;
    RLDraw7: TRLDraw;
    RLDraw8: TRLDraw;
    rlmPlaca: TRLMemo;
    rlmRNTRC: TRLMemo;
    RLDraw9: TRLDraw;
    RLLabel7: TRLLabel;
    RLDraw10: TRLDraw;
    rlLabel19: TRLLabel;
    rlLabel20: TRLLabel;
    rlLabel21: TRLLabel;
    rlShape16: TRLDraw;
    rlShape17: TRLDraw;
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
    RLDraw11: TRLDraw;
    RLDraw12: TRLDraw;
    RLDraw13: TRLDraw;
    RLDraw14: TRLDraw;
    rlbNumcipio: TRLLabel;
    RLDraw15: TRLDraw;
    RLLabel27: TRLLabel;
    rllValorMercadoria: TRLLabel;
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
  private
    { Private declarations }
    FNumItem: Integer;
    FTotalPages: integer;

  public
    { Public declarations }
    procedure ProtocoloMDFe(const sProtocolo: string);
  end;


implementation

uses
  StrUtils, DateUtils, pmdfeMDFe, ACBrUtil, ACBrDFeUtil, ACBrValidador;

{$ifdef FPC}
 {$R *.lfm}
{$else}
 {$R *.dfm}
{$endif}

const
  _NUM_ITEMS_PAGE1 = 18;
  _NUM_ITEMS_OTHERPAGES = 50;

var
  FProtocoloMDFe: string;
  nItemControle: integer;

procedure TfrlDAMDFeRLRetrato.ProtocoloMDFe(const sProtocolo: string);
begin
  FProtocoloMDFe := sProtocolo;
end;

procedure TfrlDAMDFeRLRetrato.rlb_1_DadosManifestoBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  vStringStream: TStringStream;
begin
  inherited;
  if  (RLMDFe.PageNumber <> 1) then
  begin
    rlb_2_Rodo.Visible        := False;
    RLBand2.Visible           := False;
    RLBand1.Visible           := False;
  end;

  {$IFNDEF BORLAND}
    RLMemo1.Layout := tlCenter;
    RLMemo1.Font.Size := 6;
  {$ENDIF}

  if (FLogo <> '') then
  begin
    if FilesExists(FLogo) then
      rliLogo.Picture.LoadFromFile(FLogo)
    else
    begin
      vStringStream := TStringStream.Create(FLogo);
      try
        try
          rliLogo.Picture.Bitmap.LoadFromStream(vStringStream);
        except
        end;
      finally
        vStringStream.Free;
      end;
    end;
  end;

  if FExpandirLogoMarca then
  begin
    rliLogo.top := 2;
    rliLogo.Left := 2;
    rliLogo.Height := 142;
    rliLogo.Width := 330;
    rliLogo.Stretch := True;
    rlmEmitente.visible := False;
    rlmDadosEmitente.visible := False;
  end;

  if not FExpandirLogoMarca then
  begin
    rliLogo.Stretch := true;
    rlmEmitente.Enabled := True;
    rlmDadosEmitente.Enabled := True;
    // Emitente
    with FMDFe.Emit do
    begin
      rlmEmitente.Lines.Text := XNome;

      rlmDadosEmitente.Lines.Clear;
      with EnderEmit do
      begin
        rlmDadosEmitente.Lines.Add(XLgr + IfThen(Nro = '0', '', ', ' + Nro));
        if XCpl <> '' then
          rlmDadosEmitente.Lines.Add(XCpl);
        if XBairro <> '' then
          rlmDadosEmitente.Lines.Add(XBairro);
        rlmDadosEmitente.Lines.Add('CEP: ' + FormatarCEP(CEP) +
          ' - ' + XMun + ' - ' + UF);
      end;
      rlmDadosEmitente.Lines.Add('CNPJ: ' + FormatarCNPJouCPF(CNPJCPF));
      rlmDadosEmitente.Lines.Add(ACBrStr('INSCRIÇÃO ESTADUAL: ') + IE);
      rlmDadosEmitente.Lines.Add('TELEFONE: ' + FormatarFone(EnderEmit.Fone));

      if Trim(FSite) <> '' then
        rlmDadosEmitente.Lines.Add('SITE: ' + FSite);
      if Trim(FEmail) <> '' then
        rlmDadosEmitente.Lines.Add('E-MAIL: ' + FEmail);
    end;
  end;

  RLBarcode1.Caption  := Copy ( FMDFe.InfMDFe.Id, 5, 44 );
  rllChave.Caption    := FormatarChaveAcesso(Copy(FMDFe.InfMDFe.Id, 5, 44));

  if FMDFe.ide.tpEmis = teNormal then
  begin
    rllProtocolo.Font.Size := 8;
    rllProtocolo.Font.Style := [fsBold];
    if FProtocoloMDFE <> '' then
      rllProtocolo.Caption := FProtocoloMDFE
    else
      rllProtocolo.Caption := FMDFe.procMDFe.nProt + '   ' +
        IfThen(FMDFe.procMDFe.dhRecbto <> 0,
        DateTimeToStr(FMDFe.procMDFe.dhRecbto), '');
  end
  else
  begin
    rllProtocolo.Font.Size := 5;
    rllProtocolo.Font.Style := [];
    rllProtocolo.Caption := ACBrStr('Impressão em contingência. Obrigatória a autorização em 168 horas' +
      ' após esta Emissão (') + FormatDateTime('dd/mm/yyyy hh:nn', Now) + ')';
  end;

  rllModelo.Caption       := FMDFe.Ide.modelo;
  rllSerie.Caption        := Poem_Zeros(FMDFe.Ide.serie, 3);
  rllNumMDFe.Caption      := FormatarNumeroDocumentoFiscal(IntToStr(FMDFe.Ide.nMDF));
  rllEmissao.Caption      := FormatDateTimeBr(FMDFe.Ide.dhEmi);
  rllUFCarrega.Caption    := FMDFe.Ide.UFIni;
  rllUFDescarrega.Caption := FMDFe.Ide.UFFim;
  rlb_3_Aereo.Visible     := false;
  rlb_4_Aquav.Visible     := false;
  rlb_5_Ferrov.Visible    := false;

  case FMDFe.Ide.modal of
    moRodoviario  : rllModal.Caption := ACBrStr('MODAL RODOVIÁRIO DE CARGA');
    moAereo       : rllModal.Caption := ACBrStr('MODAL AÉREO DE CARGA');
    moAquaviario  : rllModal.Caption := ACBrStr('MODAL AQUAVIÁRIO DE CARGA');
    moFerroviario : rllModal.Caption := ACBrStr('MODAL FERROVIÁRIO DE CARGA');
  end;

  rllqCTe.Caption  := FormatFloatBr(FMDFe.tot.qCTe,  '#0');
  rllqNFe.Caption  := FormatFloatBr(FMDFe.tot.qNFe,  '#0');
  rllqMDFe.Caption := FormatFloatBr(FMDFe.tot.qMDFe, '#0');

  if FMDFe.tot.cUnid = uTON then
    rlLabel12.Caption := 'PESO TOTAL (Ton)'
  else
    rlLabel12.Caption := 'PESO TOTAL (Kg)';

  rllPesoTotal.Caption := FormatFloatBr(FMDFe.tot.qCarga, ',#0.0000');
  rllValorMercadoria.Caption := FormatFloatBr(FMDFe.tot.vCarga, ',#0.00');

end;

procedure TfrlDAMDFeRLRetrato.rlb_2_RodoBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  i: integer;
begin
  inherited;
  rlb_2_Rodo.Enabled := (FMDFe.Ide.modal = moRodoviario);

  rlmPlaca.Lines.Clear;
  rlmPlaca.Lines.Add(FormatarPlaca(FMDFe.rodo.veicTracao.placa) + ' - ' +
                     FMDFe.rodo.veicTracao.UF );

  rlmRNTRC.Lines.Clear;
  if FMDFe.rodo.veicTracao.prop.RNTRC <> '' then
    rlmRNTRC.Lines.Add(FMDFe.rodo.veicTracao.prop.RNTRC)
  else
    if (FMDFe.infMDFe.versao >= 3) then
      rlmRNTRC.Lines.Add(FMDFe.rodo.infANTT.RNTRC)
    ELSE
      rlmRNTRC.Lines.Add(FMDFe.rodo.RNTRC);

  for i := 0 to FMDFe.rodo.veicReboque.Count - 1 do
  begin
    rlmPlaca.Lines.Add(FormatarPlaca(FMDFe.rodo.veicReboque.Items[i].placa) + ' - ' +
                     FMDFe.rodo.veicReboque.Items[i].UF );
    if FMDFe.rodo.veicReboque.Items[i].prop.RNTRC <> '' then
      rlmRNTRC.Lines.Add(FMDFe.rodo.veicReboque.Items[i].prop.RNTRC)
    else
      rlmRNTRC.Lines.Add(FMDFe.rodo.RNTRC);
  end;

  rlmCPF.Lines.Clear;
  rlmCondutor.Lines.Clear;

  for i := 0 to FMDFe.rodo.veicTracao.condutor.Count - 1 do
  begin
    rlmCPF.Lines.Add(FormatarCPF(FMDFe.rodo.veicTracao.condutor.Items[i].CPF));
    rlmCondutor.Lines.Add(FMDFe.rodo.veicTracao.condutor.Items[i].xNome);
  end;

  rlmRespCNPJ.Lines.Clear;
  rlmFornCNPJ.Lines.Clear;
  rlmNumComprovante.Lines.Clear;
  
  if FMDFe.rodo.valePed.disp.Count > 0 then
  begin
    for i := 0 to FMDFe.rodo.valePed.disp.Count - 1 do
    begin
      rlmRespCNPJ.Lines.Add(FormatarCNPJ(FMDFe.rodo.valePed.disp.Items[i].CNPJPg));
      rlmFornCNPJ.Lines.Add(FormatarCNPJ(FMDFe.rodo.valePed.disp.Items[i].CNPJForn));
      rlmNumComprovante.Lines.Add(FMDFe.rodo.valePed.disp.Items[i].nCompra);
    end;
  end
  else
  if FMDFe.rodo.infANTT.valePed.disp.Count > 0 then
  begin
    for i := 0 to FMDFe.rodo.infANTT.valePed.disp.Count - 1 do
    begin
      rlmRespCNPJ.Lines.Add(FormatarCNPJ(FMDFe.rodo.infANTT.valePed.disp.Items[i].CNPJPg));
      rlmFornCNPJ.Lines.Add(FormatarCNPJ(FMDFe.rodo.infANTT.valePed.disp.Items[i].CNPJForn));
      rlmNumComprovante.Lines.Add(FMDFe.rodo.infANTT.valePed.disp.Items[i].nCompra);
    end;
  end;
end;

procedure TfrlDAMDFeRLRetrato.rlb_3_AereoBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;
   rlb_3_Aereo.Enabled := (FMDFe.Ide.modal = moAereo);
end;

procedure TfrlDAMDFeRLRetrato.rlb_4_AquavBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  i: integer;
begin
  inherited;
  rlb_4_Aquav.Enabled   := (FMDFe.Ide.modal = moAquaviario);
  rllCodEmbar.Caption   := FMDFe.aquav.cEmbar;
  rllNomeEmbar.Caption  := FMDFe.aquav.xEmbar;

  rlmCodCarreg.Lines.Clear;
  rlmNomeCarreg.Lines.Clear;
  rlmCodDescarreg.Lines.Clear;
  rlmNomeDescarreg.Lines.Clear;

  for i := 0 to FMDFe.aquav.infTermCarreg.Count - 1 do
  begin
    rlmCodCarreg.Lines.Add(FMDFe.aquav.infTermCarreg.Items[i].cTermCarreg);
    rlmNomeCarreg.Lines.Add(FMDFe.aquav.infTermCarreg.Items[i].xTermCarreg);
  end;

  for i := 0 to FMDFe.aquav.infTermDescarreg.Count - 1 do
  begin
    rlmCodDescarreg.Lines.Add(FMDFe.aquav.infTermDescarreg.Items[i].cTermDescarreg);
    rlmNomeDescarreg.Lines.Add(FMDFe.aquav.infTermDescarreg.Items[i].xTermDescarreg);
  end;
end;

procedure TfrlDAMDFeRLRetrato.rlb_5_FerrovBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;
  rlb_5_Ferrov.Enabled := (FMDFe.Ide.modal = moFerroviario);
end;

procedure TfrlDAMDFeRLRetrato.rlb_6_ObservacaoBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  rlmObservacao.Lines.BeginUpdate;
  rlmObservacao.Lines.Clear;
  rlmObservacao.Lines.Add(StringReplace(FMDFe.infAdic.infCpl, '&lt;BR&gt;', #13#10, [rfReplaceAll, rfIgnoreCase]));
  rlmObservacao.Lines.Text := StringReplace(rlmObservacao.Lines.Text, ';', #13, [rfReplaceAll]);
  rlmObservacao.Lines.EndUpdate;

  // Mensagem para modo Homologacao.

  rllMsgTeste.Enabled := False;
  rllMsgTeste.Visible := False;

  if FMDFe.Ide.tpAmb = taHomologacao then
  begin
    rllMsgTeste.Caption := ACBrStr('AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL');
    rllMsgTeste.Enabled := True;
    rllMsgTeste.Visible := True;
  end
  else
  begin
    if FMDFe.procMDFe.cStat > 0 then
    begin
      if (FMDFe.procMDFe.cStat = 101) or (FMDFeCancelada) then
      begin
        rllMsgTeste.Caption := 'MDF-e CANCELADO';
        rllMsgTeste.Visible := True;
        rllMsgTeste.Enabled := True;
      end;

      if (FMDFe.procMDFe.cStat = 100) and (FMDFeEncerrado) then
      begin
        rllMsgTeste.Caption := 'MDF-e ENCERRADO';
        rllMsgTeste.Visible := True;
        rllMsgTeste.Enabled := True;
      end;

      if FMDFe.procMDFe.cStat = 110 then
      begin
        rllMsgTeste.Caption := 'MDF-e DENEGADO';
        rllMsgTeste.Visible := True;
        rllMsgTeste.Enabled := True;
      end;

      if not FMDFe.procMDFe.cStat in [101, 110, 100] then
      begin
        rllMsgTeste.Caption := FMDFe.procMDFe.xMotivo;
        rllMsgTeste.Visible := True;
        rllMsgTeste.Enabled := True;
      end;
    end
    else
    begin
      rllMsgTeste.Caption := ACBrStr('MDF-E NÃO ENVIADO PARA SEFAZ');
      rllMsgTeste.Visible := True;
      rllMsgTeste.Enabled := True;
    end;
  end;

  rllMsgTeste.Repaint;

  // imprime data e hora da impressao
  rllDataHoraImpressao.Caption := ACBrStr('DATA E HORA DA IMPRESSÃO: ') + FormatDateTime('dd/mm/yyyy hh:nn', Now);

  // imprime usuario
  if FUsuario <> '' then
    rllDataHoraImpressao.Caption := rllDataHoraImpressao.Caption + ACBrStr('   USUÁRIO: ') + FUsuario;

  // imprime sistema
  if FSistema <> '' then
    rllSistema.Caption := 'Desenvolvido por ' + FSistema
  else
    rllSistema.Caption := '';
end;

procedure TfrlDAMDFeRLRetrato.RLMDFeBeforePrint(Sender: TObject; var PrintIt: boolean);
begin
  inherited;
  nItemControle := 0;
  FTotalPages   := 1;
  RLMDFe.Title  := ACBrStr('Manifesto Eletrônico de Documentos Fiscais - MDF-e');

  with RLMDFe do
  begin
    Title                 := ACBrStr('Manifesto Eletrônico de Documentos Fiscais - MDF-e');

    Margins.TopMargin     := FMargemSuperior * 10;
    Margins.BottomMargin  := FMargemInferior * 10;
    Margins.LeftMargin    := FMargemEsquerda * 10;
    Margins.RightMargin   := FMargemDireita * 10;

    Borders.DrawTop       := true;
    Borders.DrawLeft      := true;
    Borders.DrawRight     := true;
    Borders.DrawBottom    := true;
  end;

  with rlb_1_DadosManifesto do
  begin
    Borders.DrawTop     := False;
    Borders.DrawLeft    := False;
    Borders.DrawRight   := False;
    Borders.DrawBottom  := False;
  end;

  with rlb_2_Rodo do
  begin
    Borders.DrawTop     := True;
    Borders.DrawLeft    := False;
    Borders.DrawRight   := False;
    Borders.DrawBottom  := False;
  end;

  with RLBand2 do
  begin
    Borders.DrawTop     := False;
    Borders.DrawLeft    := False;
    Borders.DrawRight   := False;
    Borders.DrawBottom  := False;
  end;

  with RLBand1 do
  begin
    Borders.DrawTop     := True;
    Borders.DrawLeft    := False;
    Borders.DrawRight   := False;
    Borders.DrawBottom  := False;
  end;

  with rlb_7_Documentos_Titulos do
  begin
    Borders.DrawTop     := True;
    Borders.DrawLeft    := False;
    Borders.DrawRight   := False;
    Borders.DrawBottom  := True;
  end;

  with rlb_6_Observacao do
  begin
    BandType            := btFooter;
    Borders.DrawTop     := True;
    Borders.DrawLeft    := False;
    Borders.DrawRight   := False;
    Borders.DrawBottom  := False;
  end;

  subItens.Borders.DrawLeft := False;
  subItens.Borders.DrawRight:= False;

  with rlbItens do
  begin
    AutoSize             := True;
    Borders.DrawTop      := False;
    Borders.DrawLeft     := False;
    Borders.DrawRight    := False;
    Borders.DrawBottom   := true;
  end;

  rlmChave1.Lines.Clear;
  rlmChave2.Lines     := rlmChave1.Lines;

  rlmChave1.AutoSize  := True;
  rlmChave2.AutoSize  := rlmChave1.AutoSize;
end;

procedure TfrlDAMDFeRLRetrato.RLMDFeDataRecord(Sender: TObject; RecNo,
  CopyNo: Integer; var Eof: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;
  Eof := (RecNo > 1);
  RecordAction := raUseIt;
  rlb_6_Observacao.Visible := Eof;
end;



procedure TfrlDAMDFeRLRetrato.subItensDataRecord(Sender: TObject; RecNo,
  CopyNo: Integer; var Eof: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;
  FNumItem      := RecNo - 1 ;
  Eof           := (RecNo > FMDFe.infDoc.infMunDescarga.Count) ;
  RecordAction  := raUseIt ;
end;

procedure TfrlDAMDFeRLRetrato.rlbItensAfterPrint(Sender: TObject);
begin
  inherited;
  rlmChave1.Lines.Clear;
  rlmChave2.Lines.Clear;
end;

procedure TfrlDAMDFeRLRetrato.rlbItensBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
   J, nItem : integer;
  Procedure Printar( sTemp : String; nItem : Integer );
  begin
    if (nItem mod 2) = 0 then
      rlmChave1.Lines.Add(sTemp)
    else
      rlmChave2.Lines.Add( sTemp);
  end;
begin
  nItem := 0;
  with FMDFe.infDoc.infMunDescarga.Items[FNumItem] do
  begin
    rlbNumcipio.Caption   := ACBrStr(Format('Município %s ',[ FMDFe.infDoc.infMunDescarga.Items[FNumItem].xMunDescarga]));
   // Lista de CT-e
    for J := 0 to ( infCTe.Count - 1) do
    begin
      Printar( 'CT-e          ' + FormatarChaveAcesso(infCTe.Items[J].chCTe), nItem) ;
      Inc(nItem);
    end;

   // Lista de CT
    for J := 0 to (infCT.Count - 1) do
    begin
      Printar( 'CT            ' + FormatarCNPJouCPF(FMDFe.emit.CNPJCPF) + ' - '
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
  inherited;
end;

end.
