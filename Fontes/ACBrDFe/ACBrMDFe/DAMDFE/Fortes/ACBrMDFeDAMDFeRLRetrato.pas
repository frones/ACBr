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
    RLBarcode1: TRLBarcode;
    rlb_1_DadosManifesto: TRLBand;
    rlsQuadro4: TRLDraw;
    rlsQuadro3: TRLDraw;
    rlsQuadro2: TRLDraw;
    rlsQuadro1: TRLDraw;
    rlsHorizontal1: TRLDraw;
    rlLabel17: TRLLabel;
    rliLogo: TRLImage;
    rlmEmitente: TRLMemo;
    rlmDadosEmitente: TRLMemo;
    rlShape1: TRLDraw;
    rlLabel74: TRLLabel;
    rlShape2: TRLDraw;
    rlLabel1: TRLLabel;
    rllChave: TRLLabel;
    rlLabel2: TRLLabel;
    rllModelo: TRLLabel;
    rlLabel3: TRLLabel;
    rllSerie: TRLLabel;
    rlLabel4: TRLLabel;
    rllNumMDFe: TRLLabel;
    rlLabel25: TRLLabel;
    rlLabel33: TRLLabel;
    rllEmissao: TRLLabel;
    rlLabel77: TRLLabel;
    rllUFCarrega: TRLLabel;
    rlsLinhaV09: TRLDraw;
    rlsLinhaV08: TRLDraw;
    rlsLinhaV07: TRLDraw;
    rlsLinhaV06: TRLDraw;
    rlsLinhaV05: TRLDraw;
    rlsLinhaV10: TRLDraw;
    rllDescricao: TRLLabel;
    rllProtocolo: TRLLabel;
    rllModal: TRLLabel;
    rlLabel5: TRLLabel;
    rlLabel10: TRLLabel;
    rlLabel12: TRLLabel;
    rlShape4: TRLDraw;
    rlShape7: TRLDraw;
    rllqCTe: TRLLabel;
    rllqNFe: TRLLabel;
    rllPesoTotal: TRLLabel;
    rlb_2_Rodo: TRLBand;
    rlShape8: TRLDraw;
    rlLabel35: TRLLabel;
    rlLabel9: TRLLabel;
    rlShape9: TRLDraw;
    rlLabel13: TRLLabel;
    rlLabel14: TRLLabel;
    rlLabel15: TRLLabel;
    rlLabel16: TRLLabel;
    rlShape10: TRLDraw;
    rlShape11: TRLDraw;
    rlShape12: TRLDraw;
    rlmPlaca: TRLMemo;
    rlmRNTRC: TRLMemo;
    rlmCPF: TRLMemo;
    rlShape13: TRLDraw;
    rlmCondutor: TRLMemo;
    rlShape14: TRLDraw;
    rlLabel18: TRLLabel;
    rlShape15: TRLDraw;
    rlLabel19: TRLLabel;
    rlLabel20: TRLLabel;
    rlLabel21: TRLLabel;
    rlShape16: TRLDraw;
    rlShape17: TRLDraw;
    rlmRespCNPJ: TRLMemo;
    rlmFornCNPJ: TRLMemo;
    rlmNumComprovante: TRLMemo;
    rlb_3_Aereo: TRLBand;
    rlb_4_Aquav: TRLBand;
    rlb_5_Ferrov: TRLBand;
    rlb_6_Observacao: TRLBand;
    rlShape18: TRLDraw;
    rlLabel22: TRLLabel;
    rlmObservacao: TRLMemo;
    rllMsgTeste: TRLLabel;
    rlb_8_Documentos_Lista: TRLBand;
    rllDataHoraImpressao: TRLLabel;
    rllSistema: TRLLabel;
    rlLabel23: TRLLabel;
    rllqMDFe: TRLLabel;
    rlShape19: TRLDraw;
    rlb_7_Documentos_Titulos: TRLBand;
    rlsQuadrado5: TRLDraw;
    rlLabel141: TRLLabel;
    rlmChave1: TRLDBText;
    rls2: TRLDraw;
    rlmChave2: TRLDBText;
    rlLabel91: TRLLabel;
    rlLabel92: TRLLabel;
    rlLabel96: TRLLabel;
    rlLabel109: TRLLabel;
    rlLabel106: TRLLabel;
    rlLabel100: TRLLabel;
    rlShape20: TRLDraw;
    rlLabel24: TRLLabel;
    rlLabel26: TRLLabel;
    rlShape21: TRLDraw;
    rlShape22: TRLDraw;
    rllCodEmbar: TRLLabel;
    rllNomeEmbar: TRLLabel;
    rlShape23: TRLDraw;
    rlShape24: TRLDraw;
    rlShape25: TRLDraw;
    rlShape26: TRLDraw;
    rlLabel27: TRLLabel;
    rlLabel28: TRLLabel;
    rlLabel29: TRLLabel;
    rlLabel30: TRLLabel;
    rlmCodCarreg: TRLMemo;
    rlmCodDescarreg: TRLMemo;
    rlmNomeCarreg: TRLMemo;
    rlmNomeDescarreg: TRLMemo;
    RLDraw1: TRLDraw;
    RLDraw2: TRLDraw;
    RLDraw3: TRLDraw;
    rllUFDescarrega: TRLLabel;
    RLLabel11: TRLLabel;
    RLSystemInfo1: TRLSystemInfo;
    RLMemo1: TRLMemo;
    procedure rlb_1_DadosManifestoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_2_RodoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_3_AereoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_4_AquavBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_5_FerrovBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_6_ObservacaoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLMDFeBeforePrint(Sender: TObject; var PrintIt: boolean);
  private
    { Private declarations }
    FTotalPages: integer;
    procedure Itens;
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
  ImprimePagComp : Boolean;
begin
  inherited;

  ImprimePagComp                        := (RLMDFe.PageNumber = 1);
  if ImprimePagComp then
    rlb_1_DadosManifesto.Height         := 267
  else
    rlb_1_DadosManifesto.Height         := 200;

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
      rlmDadosEmitente.Lines.Add('CNPJ: ' + FormatarCNPJ(CNPJ));
      rlmDadosEmitente.Lines.Add(ACBrStr('INSCRIÇÃO ESTADUAL: ') + IE);
      rlmDadosEmitente.Lines.Add('TELEFONE: ' + FormatarFone(EnderEmit.Fone));

      if Trim(FSite) <> '' then
        rlmDadosEmitente.Lines.Add('SITE: ' + FSite);
      if Trim(FEmail) <> '' then
        rlmDadosEmitente.Lines.Add('E-MAIL: ' + FEmail);
    end;
  end;

  RLBarcode1.Caption := Copy ( FMDFe.InfMDFe.Id, 5, 44 );
  //SetBarCodeImage( Copy ( FMDFe.InfMDFe.Id, 5, 44 ), rliBarCode );
  rllChave.Caption := FormatarChaveAcesso(Copy(FMDFe.InfMDFe.Id, 5, 44));

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

  // TMDFeModal = ( moRodoviario, moAereo, moAquaviario, moFerroviario );
  case FMDFe.Ide.modal of
    moRodoviario:
      begin
        rllModal.Caption := ACBrStr('MODAL RODOVIÁRIO DE CARGA');
        rlb_3_Aereo.Visible := false;
        rlb_4_Aquav.Visible := false;
        rlb_5_Ferrov.Visible := false;
      end;

    moAereo:
      begin
        rllModal.Caption := ACBrStr('MODAL AÉREO DE CARGA');
      end;

    moAquaviario:
      begin
        rllModal.Caption := ACBrStr('MODAL AQUAVIÁRIO DE CARGA');
      end;

    moFerroviario:
      begin
        rllModal.Caption := ACBrStr('MODAL FERROVIÁRIO DE CARGA');
      end;
  end;

  rllqCTe.Caption  := FormatFloatBr(FMDFe.tot.qCTe,  '#0');
  rllqNFe.Caption  := FormatFloatBr(FMDFe.tot.qNFe,  '#0');
  rllqMDFe.Caption := FormatFloatBr(FMDFe.tot.qMDFe, '#0');

  if Frac(FMDFe.tot.qCarga) > 0 then
    rllPesoTotal.Caption := FormatFloatBr(FMDFe.tot.qCarga, ',#0.000')
  else
    rllPesoTotal.Caption := FormatFloatBr(FMDFe.tot.qCarga, ',#0');
end;

procedure TfrlDAMDFeRLRetrato.rlb_2_RodoBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  i: integer;
begin
  inherited;

  //  PrintBand := RLMDFe.PageNumber = 1;
  rlb_2_Rodo.Enabled := (FMDFe.Ide.modal = moRodoviario);

  rlmPlaca.Lines.Clear;
  rlmPlaca.Lines.Add(FormatarPlaca(FMDFe.rodo.veicTracao.placa));

  rlmRNTRC.Lines.Clear;
  if FMDFe.rodo.veicTracao.prop.RNTRC <> '' then
    rlmRNTRC.Lines.Add(FMDFe.rodo.veicTracao.prop.RNTRC)
  else
    rlmRNTRC.Lines.Add(FMDFe.rodo.RNTRC);

  for i := 0 to FMDFe.rodo.veicReboque.Count - 1 do
  begin
    rlmPlaca.Lines.Add(FormatarPlaca(FMDFe.rodo.veicReboque.Items[i].placa));
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

  for i := 0 to FMDFe.rodo.valePed.disp.Count - 1 do
  begin
    rlmRespCNPJ.Lines.Add(FormatarCNPJ(FMDFe.rodo.valePed.disp.Items[i].CNPJPg));
    rlmFornCNPJ.Lines.Add(FormatarCNPJ(FMDFe.rodo.valePed.disp.Items[i].CNPJForn));
    rlmNumComprovante.Lines.Add(FMDFe.rodo.valePed.disp.Items[i].nCompra);
  end;
end;

procedure TfrlDAMDFeRLRetrato.rlb_3_AereoBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  //  PrintBand := RLMDFe.PageNumber = 1;
  rlb_3_Aereo.Enabled := (FMDFe.Ide.modal = moAereo);
end;

procedure TfrlDAMDFeRLRetrato.rlb_4_AquavBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  i: integer;
begin
  inherited;

  //  PrintBand := RLMDFe.PageNumber = 1;
  rlb_4_Aquav.Enabled := (FMDFe.Ide.modal = moAquaviario);

  rllCodEmbar.Caption := FMDFe.aquav.cEmbar;
  rllNomeEmbar.Caption := FMDFe.aquav.xEmbar;

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

  //  PrintBand := RLMDFe.PageNumber = 1;
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
  Itens;
  nItemControle := 0;
  FTotalPages := 1;

  RLMDFe.Title := ACBrStr('Manifesto Eletrônico de Documentos Fiscais - MDF-e');

  with RLMDFe.Margins do
    begin
      TopMargin := FMargemSuperior * 10;
      BottomMargin := FMargemInferior * 10;
      LeftMargin := FMargemEsquerda * 10;
      RightMargin := FMargemDireita * 10;
    end;
end;

procedure TfrlDAMDFeRLRetrato.Itens;
var
  I, J, nItem: integer;
begin
  //cdsItens.Close;
  //cdsItens.CreateDataSet;
  //cdsItens.Open;
  nItem := 0;

  for I := 0 to (FMDFe.infDoc.infMunDescarga.Count - 1) do
  begin

    // Lista de CT-e
    for J := 0 to (FMDFe.infDoc.infMunDescarga.Items[I].infCTe.Count - 1) do
    begin
      with FMDFe.infDoc.infMunDescarga.Items[I].infCTe.Items[J] do
      begin
        if (nItem mod 2) = 0 then
        begin
          cdsItens.Append;
          cdsItens.FieldByName('CHAVE1').AsString := 'CT-e          ' +
            FormatarChaveAcesso(chCTe);
        end
        else
        begin
          cdsItens.FieldByName('CHAVE2').AsString := 'CT-e          ' +
            FormatarChaveAcesso(chCTe);
          cdsItens.Post;
        end;
        Inc(nItem);
      end;
    end;

    // Lista de CT
    for J := 0 to (FMDFe.infDoc.infMunDescarga.Items[I].infCT.Count - 1) do
    begin
      with FMDFe.infDoc.infMunDescarga.Items[I].infCT.Items[J] do
      begin
        if (nItem mod 2) = 0 then
        begin
          cdsItens.Append;
          cdsItens.FieldByName('CHAVE1').AsString := 'CT            ' +
            FormatarCNPJouCPF(FMDFe.emit.CNPJ) + ' - ' +
            IntToStr(serie) + '-' + nCT;
        end
        else
        begin
          cdsItens.FieldByName('CHAVE2').AsString := 'CT            ' +
            FormatarCNPJouCPF(FMDFe.emit.CNPJ) + ' - ' +
            IntToStr(serie) + '-' + nCT;
          cdsItens.Post;
        end;
        Inc(nItem);
      end;
    end;

    // Lista de NF-e
    for J := 0 to (FMDFe.infDoc.infMunDescarga.Items[I].infNFe.Count - 1) do
    begin
      with FMDFe.infDoc.infMunDescarga.Items[I].infNFe.Items[J] do
      begin
        if (nItem mod 2) = 0 then
        begin
          cdsItens.Append;
          cdsItens.FieldByName('CHAVE1').AsString := 'NF-e          ' +
            FormatarChaveAcesso(chNFe);
        end
        else
        begin
          cdsItens.FieldByName('CHAVE2').AsString := 'NF-e          ' +
            FormatarChaveAcesso(chNFe);
          cdsItens.Post;
        end;
        Inc(nItem);
      end;
    end;

    // Lista de NF
    for J := 0 to (FMDFe.infDoc.infMunDescarga.Items[I].infNF.Count - 1) do
    begin
      with FMDFe.infDoc.infMunDescarga.Items[I].infNF.Items[J] do
      begin
        if (nItem mod 2) = 0 then
        begin
          cdsItens.Append;
          cdsItens.FieldByName('CHAVE1').AsString := 'NF            ' +
            FormatarCNPJouCPF(CNPJ) + ' - ' +
            IntToStr(serie) + '-' + IntToStr(nNF);
        end
        else
        begin
          cdsItens.FieldByName('CHAVE2').AsString := 'NF            ' +
            FormatarCNPJouCPF(CNPJ) + ' - ' +
            IntToStr(serie) + '-' + IntToStr(nNF);
          cdsItens.Post;
        end;
        Inc(nItem);
      end;
    end;

    // Lista de MDF-e
    for J := 0 to (FMDFe.infDoc.infMunDescarga.Items[I].infMDFeTransp.Count - 1) do
    begin
      with FMDFe.infDoc.infMunDescarga.Items[I].infMDFeTransp.Items[J] do
      begin
        if (nItem mod 2) = 0 then
        begin
          cdsItens.Append;
          cdsItens.FieldByName('CHAVE1').AsString := 'MDF-e         ' +
            FormatarChaveAcesso(chMDFe);
        end
        else
        begin
          cdsItens.FieldByName('CHAVE1').AsString := 'MDF-e         ' +
            FormatarChaveAcesso(chMDFe);
          cdsItens.Post;
        end;
        Inc(nItem);
      end;
    end;

  end;

  cdsItens.First;
end;

end.
