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

{*******************************************************************************
|* Historico
|*
|* 01/08/2012: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit ACBrMDFeDAMDFEQRRetrato;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, QuickRpt, QRCtrls,  XMLIntf, XMLDoc, JPEG,
  pcnConversao, pmdfeConversaoMDFe, DB, DBClient,
  ACBrMDFeDAMDFEQR, ACBrDFeQRCodeBar, ACBrMDFeDAMDFEClass,
  ACBrMDFeDAMDFEQRClass;

type

  TfqrDAMDFEQRRetrato = class(TfqrDAMDFEQR)
    qrb_1_DadosManifesto: TQRBand;
    qrsQuadro4: TQRShape;
    qrsQuadro3: TQRShape;
    qrsQuadro2: TQRShape;
    qrsQuadro1: TQRShape;
    qrsHorizontal1: TQRShape;
    QRLabel8: TQRLabel;
    QRLabel17: TQRLabel;
    qriLogo: TQRImage;
    qrmEmitente: TQRMemo;
    qrmDadosEmitente: TQRMemo;
    qriBarCode: TQRImage;
    QRShape1: TQRShape;
    QRLabel74: TQRLabel;
    QRShape2: TQRShape;
    QRLabel1: TQRLabel;
    qrlChave: TQRLabel;
    QRLabel2: TQRLabel;
    qrlModelo: TQRLabel;
    QRLabel3: TQRLabel;
    qrlSerie: TQRLabel;
    QRLabel4: TQRLabel;
    qrlNumMDFe: TQRLabel;
    QRLabel25: TQRLabel;
    qrlPageNumber: TQRLabel;
    QRLabel33: TQRLabel;
    qrlEmissao: TQRLabel;
    QRLabel77: TQRLabel;
    qrlUFCarrega: TQRLabel;
    qrsLinhaV09: TQRShape;
    qrsLinhaV08: TQRShape;
    qrsLinhaV07: TQRShape;
    qrsLinhaV06: TQRShape;
    qrsLinhaV05: TQRShape;
    qrsLinhaV10: TQRShape;
    qrlDescricao: TQRLabel;
    qrlProtocolo: TQRLabel;
    qrlModal: TQRLabel;
    QRLabel6: TQRLabel;
    QRLabel5: TQRLabel;
    QRLabel10: TQRLabel;
    QRLabel11: TQRLabel;
    QRLabel12: TQRLabel;
    QRShape3: TQRShape;
    QRShape5: TQRShape;
    QRShape6: TQRShape;
    QRShape7: TQRShape;
    qrlCIOT: TQRLabel;
    qrlqCTe: TQRLabel;
    qrlqNFe: TQRLabel;
    qrlqNF: TQRLabel;
    qrlPesoTotal: TQRLabel;
    qrb_2_Rodo: TQRChildBand;
    QRShape8: TQRShape;
    QRLabel35: TQRLabel;
    QRLabel9: TQRLabel;
    QRShape9: TQRShape;
    QRLabel13: TQRLabel;
    QRLabel14: TQRLabel;
    QRLabel15: TQRLabel;
    QRLabel16: TQRLabel;
    QRShape10: TQRShape;
    QRShape11: TQRShape;
    QRShape12: TQRShape;
    qrmPlaca: TQRMemo;
    qrmRNTRC: TQRMemo;
    qrmCPF: TQRMemo;
    QRShape13: TQRShape;
    qrmCondutor: TQRMemo;
    QRShape14: TQRShape;
    QRLabel18: TQRLabel;
    QRShape15: TQRShape;
    QRLabel19: TQRLabel;
    QRLabel20: TQRLabel;
    QRLabel21: TQRLabel;
    QRShape16: TQRShape;
    QRShape17: TQRShape;
    qrmRespCNPJ: TQRMemo;
    qrmFornCNPJ: TQRMemo;
    qrmNumComprovante: TQRMemo;
    qrb_3_Aereo: TQRChildBand;
    qrb_4_Aquav: TQRChildBand;
    qrb_5_Ferrov: TQRChildBand;
    qrb_6_Observacao: TQRChildBand;
    QRShape18: TQRShape;
    QRLabel22: TQRLabel;
    qrmObservacao: TQRMemo;
    qrlMsgTeste: TQRLabel;
    qrb_8_Documentos_Lista: TQRBand;
    cdsItens: TClientDataSet;
    cdsItensChave_1: TStringField;
    cdsItensChave_2: TStringField;
    qrlDataHoraImpressao: TQRLabel;
    qrlSistema: TQRLabel;
    QRLabel23: TQRLabel;
    qrlqMDFe: TQRLabel;
    QRShape19: TQRShape;
    qrb_7_Documentos_Titulos: TQRBand;
    qrsQuadrado5: TQRShape;
    QRLabel141: TQRLabel;
    qrmChave1: TQRDBText;
    qrs2: TQRShape;
    qrmChave2: TQRDBText;
    QRLabel91: TQRLabel;
    QRLabel92: TQRLabel;
    QRLabel96: TQRLabel;
    QRLabel109: TQRLabel;
    QRLabel106: TQRLabel;
    QRLabel100: TQRLabel;
    QRShape20: TQRShape;
    QRLabel24: TQRLabel;
    QRLabel26: TQRLabel;
    QRShape21: TQRShape;
    QRShape22: TQRShape;
    qrlCodEmbar: TQRLabel;
    qrlNomeEmbar: TQRLabel;
    QRShape23: TQRShape;
    QRShape24: TQRShape;
    QRShape25: TQRShape;
    QRShape26: TQRShape;
    QRLabel27: TQRLabel;
    QRLabel28: TQRLabel;
    QRLabel29: TQRLabel;
    QRLabel30: TQRLabel;
    qrmCodCarreg: TQRMemo;
    qrmCodDescarreg: TQRMemo;
    qrmNomeCarreg: TQRMemo;
    qrmNomeDescarreg: TQRMemo;
    QRShape27: TQRShape;
    QRLabel31: TQRLabel;
    qrlUFDescarrega: TQRLabel;
    procedure QRMDFeBeforePrint(Sender: TCustomQuickRep;
      var PrintReport: Boolean);
    procedure qrb_1_DadosManifestoBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb_2_RodoBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb_3_AereoBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb_4_AquavBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb_5_FerrovBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb_6_ObservacaoBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
  private
    { Private declarations }
//    FTotalPages : Integer;
    procedure Itens;
  public
    { Public declarations }
    procedure ProtocoloMDFe(const sProtocolo: String);
  end;


implementation

uses
  StrUtils, DateUtils, ACBrValidador,
  pmdfeMDFe, ACBrUtil, ACBrDFeUtil;

{$R *.dfm}

const
   _NUM_ITEMS_PAGE1      = 18;
   _NUM_ITEMS_OTHERPAGES = 50;

var
  FProtocoloMDFe: String;
  nItemControle: Integer;

procedure TfqrDAMDFEQRRetrato.ProtocoloMDFe(const sProtocolo: String);
begin
   FProtocoloMDFe := sProtocolo;
end;

procedure TfqrDAMDFEQRRetrato.QRMDFeBeforePrint(Sender: TCustomQuickRep;
  var PrintReport: Boolean);
begin
  inherited;
   Itens;
   nItemControle := 0;
//   FTotalPages   := 1;

   QRMDFe.ReportTitle := 'Manifesto Eletrônico de Documentos Fiscais - MDF-e';

   QRMDFe.Page.TopMargin    := FMargemSuperior * 100;
   QRMDFe.Page.BottomMargin := FMargemInferior * 100;
   QRMDFe.Page.LeftMargin   := FMargemEsquerda * 100;
   QRMDFe.Page.RightMargin  := FMargemDireita  * 100;
end;

procedure TfqrDAMDFEQRRetrato.qrb_1_DadosManifestoBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
  vStringStream: TStringStream;
begin
  inherited;

  // Alterado por Italo em 15/01/2014
  if (FLogo <> '') then
    begin
      if FilesExists(FLogo) then
        qriLogo.Picture.LoadFromFile(FLogo)
      else
        begin
          vStringStream := TStringStream.Create(FLogo);
          try
            try
              qriLogo.Picture.Bitmap.LoadFromStream(vStringStream);
            except
            end;
          finally
            vStringStream.Free;
          end;
        end;
    end;

  if FExpandirLogoMarca then
   begin
    qriLogo.top              := 2;
    qriLogo.Left             := 2;
    qriLogo.Height           := 142;
    qriLogo.Width            := 330;
    qriLogo.Stretch          := True;
    qrmEmitente.Enabled      := False;
    qrmDadosEmitente.Enabled := False;
   end;

  if not FExpandirLogoMarca then
   begin
    qrmEmitente.Enabled      := True;
    qrmDadosEmitente.Enabled := True;
    // Emitente
    with FMDFe.Emit do
    begin
      qrmEmitente.Lines.Text := XNome;

      qrmDadosEmitente.Lines.Clear;
      with EnderEmit do
      begin
        qrmDadosEmitente.Lines.Add(XLgr + IfThen(Nro = '0', '', ', ' + Nro));
        if XCpl <> '' then qrmDadosEmitente.Lines.Add(XCpl);
        if XBairro <> '' then qrmDadosEmitente.Lines.Add(XBairro);
        qrmDadosEmitente.Lines.Add('CEP: ' + FormatarCEP(FormatFloat('00000000', CEP)) + ' - ' + XMun + ' - ' + UF);
      end;
      qrmDadosEmitente.Lines.Add('CNPJ: ' + FormatarCNPJ(CNPJ));
      qrmDadosEmitente.Lines.Add('INSCRIÇÃO ESTADUAL: ' + IE);
      qrmDadosEmitente.Lines.Add('TELEFONE: ' + FormatarFone(EnderEmit.Fone));

      if Trim(FSite) <> '' then
        qrmDadosEmitente.Lines.Add('SITE: ' + FSite);
      if Trim(FEmail) <> '' then
        qrmDadosEmitente.Lines.Add('E-MAIL: ' + FEmail);
    end;
   end;

  SetBarCodeImage(Copy(FMDFe.InfMDFe.Id, 5, 44), qriBarCode);

  qrlChave.Caption := FormatarChaveAcesso(Copy(FMDFe.InfMDFe.Id, 5, 44));

  if (FMDFe.ide.tpEmis = teNormal) or (FProtocoloMDFE <> '') or
     (FMDFe.procMDFe.nProt <> '')
   then begin
    qrlProtocolo.Font.Size  := 8;
    qrlProtocolo.Font.Style := [fsBold];
    if FProtocoloMDFE <> ''
     then qrlProtocolo.Caption := FProtocoloMDFE
     else qrlProtocolo.Caption := FMDFe.procMDFe.nProt + '   ' +
                                  ifThen(FMDFe.procMDFe.dhRecbto <> 0,
                                      DateTimeToStr(FMDFe.procMDFe.dhRecbto), '');
   end
   else begin
    qrlProtocolo.Font.Size  := 5;
    qrlProtocolo.Font.Style := [];
    qrlProtocolo.Caption := 'Impressão em contingência. Obrigatória a autorização em 24 horas' +
     ' após esta impressão (' + FormatDateTime('dd/mm/yyyy hh:nn', Now) + ')';
   end;

  qrlModelo.Caption       := FMDFe.Ide.modelo;
  qrlSerie.Caption        := FormatFloat('000', FMDFe.Ide.serie);
  qrlNumMDFe.Caption      := FormatFloat('000,000,000', FMDFe.Ide.nMDF);
  qrlPageNumber.Caption   := format('%2.2d', [QRMDFe.PageNumber]) + '/' + format('%2.2d', [FTotalPages]);
  qrlEmissao.Caption      := FormatDateTime('dd/mm/yyyy hh:nn', FMDFe.Ide.dhEmi);
  qrlUFCarrega.Caption    := FMDFe.Ide.UFIni;
  qrlUFDescarrega.Caption := FMDFe.Ide.UFFim;

  qrlCIOT.Caption := '';
  // TMDFeModal = ( moRodoviario, moAereo, moAquaviario, moFerroviario );
  case FMDFe.Ide.modal of
   moRodoviario:  begin
                   qrlModal.Caption := 'MODAL RODOVIÁRIO DE CARGA';
                   qrlCIOT.Caption := FMDFe.rodo.CIOT;
                  end;
   moAereo:       begin
                   qrlModal.Caption := 'MODAL AÉREO DE CARGA';
                  end;
   moAquaviario:  begin
                   qrlModal.Caption := 'MODAL AQUAVIÁRIO DE CARGA';
                  end;
   moFerroviario: begin
                   qrlModal.Caption := 'MODAL FERROVIÁRIO DE CARGA';
                  end;
  end;

  qrlqCTe.Caption  := FormatFloat('##0', FMDFe.tot.qCTe);
//  qrlqCT.Caption   := FormatFloat('##0', FMDFe.tot.qCT);
  qrlqNFe.Caption  := FormatFloat('##0', FMDFe.tot.qNFe);
  qrlqNF.Caption   := FormatFloat('##0', FMDFe.tot.qNF);
  qrlqMDFe.Caption := FormatFloat('##0', FMDFe.tot.qMDFe);

  // UnidMed = (uM3,uKG, uTON, uUNIDADE, uLITROS, uMMBTU);
  if FMDFe.tot.cUnid = uKG
   then qrlPesoTotal.Caption := FormatFloat('#,##0.###', FMDFe.tot.qCarga)
   else qrlPesoTotal.Caption := FormatFloat('#,##0.###', FMDFe.tot.qCarga * 1000);
end;

procedure TfqrDAMDFEQRRetrato.qrb_2_RodoBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
  i: Integer;
begin
  inherited;

//  PrintBand := QRMDFe.PageNumber = 1;
  qrb_2_Rodo.Enabled := (FMDFe.Ide.modal = moRodoviario);

  qrmPlaca.Lines.Clear;
  qrmPlaca.Lines.Add(FormatarPlaca(FMDFe.rodo.veicTracao.placa));

  qrmRNTRC.Lines.Clear;
  if FMDFe.rodo.veicTracao.prop.RNTRC <> ''
   then qrmRNTRC.Lines.Add(FMDFe.rodo.veicTracao.prop.RNTRC)
   else qrmRNTRC.Lines.Add(FMDFe.rodo.RNTRC);

  for i := 0 to FMDFe.rodo.veicReboque.Count -1 do
   begin
    qrmPlaca.Lines.Add(FormatarPlaca(FMDFe.rodo.veicReboque.Items[i].placa));
    if FMDFe.rodo.veicReboque.Items[i].prop.RNTRC <> ''
     then qrmRNTRC.Lines.Add(FMDFe.rodo.veicReboque.Items[i].prop.RNTRC)
     else qrmRNTRC.Lines.Add(FMDFe.rodo.RNTRC);
   end;

  qrmCPF.Lines.Clear;
  qrmCondutor.Lines.Clear;

  for i := 0 to FMDFe.rodo.veicTracao.condutor.Count -1 do
   begin
    qrmCPF.Lines.Add(FormatarCPF(FMDFe.rodo.veicTracao.condutor.Items[i].CPF));
    qrmCondutor.Lines.Add(FMDFe.rodo.veicTracao.condutor.Items[i].xNome);
   end;

  qrmRespCNPJ.Lines.Clear;
  qrmFornCNPJ.Lines.Clear;
  qrmNumComprovante.Lines.Clear;

  for i := 0 to FMDFe.rodo.valePed.disp.Count -1 do
   begin
    qrmRespCNPJ.Lines.Add(FormatarCNPJ(FMDFe.rodo.valePed.disp.Items[i].CNPJPg));
    qrmFornCNPJ.Lines.Add(FormatarCNPJ(FMDFe.rodo.valePed.disp.Items[i].CNPJForn));
    qrmNumComprovante.Lines.Add(FMDFe.rodo.valePed.disp.Items[i].nCompra);
   end;
end;

procedure TfqrDAMDFEQRRetrato.qrb_3_AereoBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;

//  PrintBand := QRMDFe.PageNumber = 1;
  qrb_3_Aereo.Enabled := (FMDFe.Ide.modal = moAereo);
end;

procedure TfqrDAMDFEQRRetrato.qrb_4_AquavBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
 i: Integer;
begin
  inherited;

//  PrintBand := QRMDFe.PageNumber = 1;
  qrb_4_Aquav.Enabled := (FMDFe.Ide.modal = moAquaviario);

  qrlCodEmbar.Caption  := FMDFe.aquav.cEmbar;
  qrlNomeEmbar.Caption := FMDFe.aquav.xEmbar;

  qrmCodCarreg.Lines.Clear;
  qrmNomeCarreg.Lines.Clear;
  qrmCodDescarreg.Lines.Clear;
  qrmNomeDescarreg.Lines.Clear;

  for i := 0 to FMDFe.aquav.infTermCarreg.Count -1 do
   begin
    qrmCodCarreg.Lines.Add(FMDFe.aquav.infTermCarreg.Items[i].cTermCarreg);
    qrmNomeCarreg.Lines.Add(FMDFe.aquav.infTermCarreg.Items[i].xTermCarreg);
   end;

  for i := 0 to FMDFe.aquav.infTermDescarreg.Count -1 do
   begin
    qrmCodDescarreg.Lines.Add(FMDFe.aquav.infTermDescarreg.Items[i].cTermDescarreg);
    qrmNomeDescarreg.Lines.Add(FMDFe.aquav.infTermDescarreg.Items[i].xTermDescarreg);
   end;
end;

procedure TfqrDAMDFEQRRetrato.qrb_5_FerrovBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

//  PrintBand := QRMDFe.PageNumber = 1;
  qrb_5_Ferrov.Enabled := (FMDFe.Ide.modal = moFerroviario);
end;

procedure TfqrDAMDFEQRRetrato.qrb_6_ObservacaoBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  qrmObservacao.Lines.BeginUpdate;
  qrmObservacao.Lines.Clear;
  qrmObservacao.Lines.Add(StringReplace(FMDFe.infAdic.infCpl, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase]));
  qrmObservacao.Lines.Text:=StringReplace(qrmObservacao.Lines.Text,';',#13,[rfReplaceAll]);
  qrmObservacao.Lines.EndUpdate;

  // Mensagem para modo Homologacao.

  if FMDFe.Ide.tpAmb = taHomologacao then
   begin
    qrlMsgTeste.Caption := 'AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL';
    qrlMsgTeste.Enabled := True;
    qrlMsgTeste.Visible := True;
   end
   else begin
    if FMDFe.procMDFe.cStat > 0 then
     begin
      if (FMDFe.procMDFe.cStat = 101) or (FMDFeCancelada) then
       begin
        qrlMsgTeste.Caption := 'MDF-e CANCELADO';
        qrlMsgTeste.Visible := True;
        qrlMsgTeste.Enabled := True;
       end;

      if (FMDFe.procMDFe.cStat = 100) and (FMDFeEncerrado) then
       begin
        qrlMsgTeste.Caption := 'MDF-e ENCERRADO';
        qrlMsgTeste.Visible := True;
        qrlMsgTeste.Enabled := True;
       end;

      if FMDFe.procMDFe.cStat = 110 then
       begin
        qrlMsgTeste.Caption := 'MDF-e DENEGADO';
        qrlMsgTeste.Visible := True;
        qrlMsgTeste.Enabled := True;
       end;

      if not FMDFe.procMDFe.cStat in [101, 110, 100] then
       begin
        qrlMsgTeste.Caption := FMDFe.procMDFe.xMotivo;
        qrlMsgTeste.Visible := True;
        qrlMsgTeste.Enabled := True;
       end;
     end else
     begin
      qrlMsgTeste.Caption := 'MDF-E NÃO ENVIADO PARA SEFAZ';
      qrlMsgTeste.Visible := True;
      qrlMsgTeste.Enabled := True;
     end;
   end;

  qrlMsgTeste.Repaint;

  // imprime data e hora da impressao
  QrlDataHoraImpressao.Caption:= 'DATA E HORA DA IMPRESSÃO: ' + FormatDateTime('dd/mm/yyyy hh:nn', Now);

  // imprime usuario
  if FUsuario <> ''
   then QrlDataHoraImpressao.Caption:= QrlDataHoraImpressao.Caption + '   USUÁRIO: ' + FUsuario;

  // imprime sistema
  if FSistema <> ''
   then qrlSistema.Caption:= 'Desenvolvido por ' + FSistema
   else qrlSistema.Caption:= '';
end;

procedure TfqrDAMDFEQRRetrato.Itens;
var
  I, J, nItem: Integer;
begin
  cdsItens.Close;
  cdsItens.CreateDataSet;
  cdsItens.Open;
  nItem := 0;

  for I := 0 to (FMDFe.infDoc.infMunDescarga.Count - 1) do
   begin

    // Lista de CT-e
    for J := 0 to (FMDFe.infDoc.infMunDescarga.Items[I].infCTe.Count - 1) do
     begin
      with FMDFe.infDoc.infMunDescarga.Items[I].infCTe.Items[J] do
       begin
        if (nItem mod 2) = 0
         then begin
          cdsItens.Append;
          cdsItensChave_1.AsString := 'CT-e          ' +
                                      FormatarChaveAcesso(chCTe);
         end
         else begin
          cdsItensChave_2.AsString := 'CT-e          ' +
                                      FormatarChaveAcesso(chCTe);
          cdsItens.Post;
         end;
        inc(nItem);
       end;
     end;

    // Lista de CT
    for J := 0 to (FMDFe.infDoc.infMunDescarga.Items[I].infCT.Count - 1) do
     begin
      with FMDFe.infDoc.infMunDescarga.Items[I].infCT.Items[J] do
       begin
        if (nItem mod 2) = 0
         then begin
          cdsItens.Append;
          cdsItensChave_1.AsString := 'CT            ' +
                              FormatarCNPJouCPF(FMDFe.emit.CNPJ) + ' - ' +
                              IntToStr(serie) + '-' + nCT;
         end
         else begin
          cdsItensChave_2.AsString := 'CT            ' +
                              FormatarCNPJouCPF(FMDFe.emit.CNPJ) + ' - ' +
                              IntToStr(serie) + '-' + nCT;
          cdsItens.Post;
         end;
        inc(nItem);
       end;
     end;

    // Lista de NF-e
    for J := 0 to (FMDFe.infDoc.infMunDescarga.Items[I].infNFe.Count - 1) do
     begin
      with FMDFe.infDoc.infMunDescarga.Items[I].infNFe.Items[J] do
       begin
        if (nItem mod 2) = 0
         then begin
          cdsItens.Append;
          cdsItensChave_1.AsString := 'NF-e          ' +
                                      FormatarChaveAcesso(chNFe);
         end
         else begin
          cdsItensChave_2.AsString := 'NF-e          ' +
                                      FormatarChaveAcesso(chNFe);
          cdsItens.Post;
         end;
        inc(nItem);
       end;
     end;

    // Lista de NF
    for J := 0 to (FMDFe.infDoc.infMunDescarga.Items[I].infNF.Count - 1) do
     begin
      with FMDFe.infDoc.infMunDescarga.Items[I].infNF.Items[J] do
       begin
        if (nItem mod 2) = 0
         then begin
          cdsItens.Append;
          cdsItensChave_1.AsString := 'NF            ' +
                                      FormatarCNPJouCPF(CNPJ) + ' - ' +
                                      IntToStr(serie) + '-' + IntToStr(nNF);
         end
         else begin
          cdsItensChave_2.AsString := 'NF            ' +
                                      FormatarCNPJouCPF(CNPJ) + ' - ' +
                                      IntToStr(serie) + '-' + IntToStr(nNF);
          cdsItens.Post;
         end;
        inc(nItem);
       end;
     end;

    // Lista de MDF-e
    for J := 0 to (FMDFe.infDoc.infMunDescarga.Items[I].infMDFeTransp.Count - 1) do
     begin
      with FMDFe.infDoc.infMunDescarga.Items[I].infMDFeTransp.Items[J] do
       begin
        if (nItem mod 2) = 0
         then begin
          cdsItens.Append;
          cdsItensChave_1.AsString := 'MDF-e         ' +
                                     FormatarChaveAcesso(chMDFe);
         end
         else begin
          cdsItensChave_2.AsString := 'MDF-e         ' +
                                     FormatarChaveAcesso(chMDFe);
          cdsItens.Post;
         end;
        inc(nItem);
       end;
     end;

   end;

  cdsItens.First;
end;

end.
