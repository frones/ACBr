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

unit ACBrNFeDANFeRLEtiqueta;

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

  { TfrlDANFeRLEtiqueta }

  TfrlDANFeRLEtiqueta = class(TfrlDANFeRL)
    RLb02_Emitente: TRLBand;
    RLb04_Destinatario: TRLBand;
    RLb05c_Lin_Itens: TRLBand;
    RLiLogo: TRLImage;
    RLlChave: TRLLabel;
    RLlDescricao: TRLLabel;
    RLlMsgTipoEmissao: TRLLabel;
    RLlProtocolo: TRLLabel;
    RLmDestinatario: TRLMemo;
    RLmEmitente: TRLMemo;
    RLShape68: TRLDraw;
    rlb01_Chave: TRLBand;
    RLBarcode1: TRLBarcode;
    RLBand1: TRLBand;
    rlmDadosAdicionais: TRLMemo;
    RLLabel17: TRLLabel;
    rllTipoEmissao: TRLLabel;
    lblNumero: TRLLabel;
    rllEmissao: TRLLabel;
    RLLabel27: TRLLabel;
    RLShape102: TRLDraw;
    rllEntradaSaida: TRLLabel;
    RLLabel12: TRLLabel;
    RLLabel1: TRLLabel;
    RLLabel2: TRLLabel;
    RLLabel3: TRLLabel;
    procedure RLb02_EmitenteBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLb03_DadosGeraisBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLb04_DestinatarioBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLNFeBeforePrint(Sender: TObject; var PrintReport: Boolean);
    procedure rlb01_ChaveBeforePrint(Sender: TObject; var PrintBand: Boolean);
    procedure subItensDataRecord(Sender: TObject; RecNo, CopyNo: Integer; var EOF: Boolean; var RecordAction: TRLRecordAction);
    procedure RLNFeDataRecord(Sender: TObject; RecNo, CopyNo: Integer; var EOF: Boolean; var RecordAction: TRLRecordAction);
    procedure RLBand1BeforePrint(Sender: TObject; var PrintIt: Boolean);
  private
    FNumItem: Integer;
    FTotalPages: Integer;
    procedure InicializarDados;
  public
    procedure ProtocoloNFE(const sProtocolo: String);
  end;

implementation

uses
  StrUtils, DateUtils,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime,
  ACBrValidador, ACBrDFeUtil,
  ACBrDFeReportFortes, ACBrNFe.Classes, pcnConversao, pcnConversaoNFe;

{$IfNDef FPC}
  {$R *.dfm}
{$Else}
  {$R *.lfm}
{$ENDIF}


procedure TfrlDANFeRLEtiqueta.RLNFeBeforePrint(Sender: TObject; var PrintReport: Boolean);
var
  nRestItens: Integer;
begin
  inherited;

  FTotalPages := 1;

  if fpDANFe.FormatarNumeroDocumento then
    RLNFe.Title := 'NF-e: ' + FormatFloat('000,000,000', fpNFe.Ide.nNF)
  else
    RLNFe.Title := 'NF-e: ' + IntToStr(fpNFe.Ide.nNF);

  TDFeReportFortes.AjustarMargem(RLNFe, fpDANFe);
  InicializarDados;
end;

procedure TfrlDANFeRLEtiqueta.RLb02_EmitenteBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

    rliLogo.top := 3;
    rliLogo.Left := 5;
    RLILogo.Width:=  fpDANFe.TamanhoLogoWidth;
    RLILogo.Height:= fpDANFe.TamanhoLogoHeight;

    TDFeReportFortes.AjustarLogo(rliLogo, fpDANFe.ExpandeLogoMarcaConfig);

    rlmEmitente.Enabled := False;

  if not TDFeReportFortes.CarregarLogo(rliLogo, fpDANFe.Logo) then
  begin
    //TODO: implementar algum tratamento para logo vazio? Ex.: Veja: TfrlDANFeRLRetrato.InicializarDados
     RLb02_Emitente.Height:= 80;
     RLmEmitente.Top:= rlilogo.Top;
  end;

  rlmEmitente.Enabled := True;
  rlmEmitente.Lines.Clear;

  with fpNFe.Emit do
  begin
    rlmEmitente.Lines.Add(fpDANFe.ManterNomeImpresso(XNome, XFant));

    rlmEmitente.Lines.Add(EnderEmit.XLgr +
      IfThen(EnderEmit.Nro = '0', '', ', ' + EnderEmit.Nro) +
      IfThen(EstaVazio(EnderEmit.XCpl), '', ', ' + EnderEmit.XCpl) +
      IfThen(EstaVazio(EnderEmit.XBairro), '', ', ' + EnderEmit.XBairro) +
      ', ' + EnderEmit.XMun + '/ ' + EnderEmit.UF);

    rlmEmitente.Lines.Add('CNPJ: ' + FormatarCNPJouCPF(CNPJCPF) + ' IE: ' + IE);
  end;

  // Contingencia ********************************************************
  if fpNFe.Ide.tpEmis in [teContingencia, teFSDA] then
    rllTipoEmissao.Caption := 'CONTINGENCIA FS-DA';

  rllEntradaSaida.Caption := tpNFToStr(fpNFe.Ide.tpNF);

  if fpDANFe.FormatarNumeroDocumento then
    lblNumero.Caption := ACBrStr('Número: ' + FormatFloat('000,000,000', fpNFe.Ide.nNF))
  else
    lblNumero.Caption := ACBrStr('Número: ' + IntToStr(fpNFe.Ide.nNF));

  lblNumero.Caption := lblNumero.Caption + ' - Série: ' + FormatFloat('000', fpNFe.Ide.serie);

  rllEmissao.Caption := ACBrStr('Emissão: ' + FormatDateTimeBr(fpNFe.Ide.dEmi));

end;

procedure TfrlDANFeRLEtiqueta.RLb03_DadosGeraisBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  // Contingencia ********************************************************
  if fpNFe.Ide.tpEmis in [teContingencia, teFSDA] then
    rllTipoEmissao.Caption := 'CONTINGENCIA FS-DA';

  rllEntradaSaida.Caption := tpNFToStr(fpNFe.Ide.tpNF);

  if fpDANFe.FormatarNumeroDocumento then
    lblNumero.Caption := ACBrStr('Número: ' + FormatFloat('000,000,000', fpNFe.Ide.nNF))
  else
    lblNumero.Caption := ACBrStr('Número: ' + IntToStr(fpNFe.Ide.nNF));

  lblNumero.Caption := lblNumero.Caption + ' - Série: ' + FormatFloat('000', fpNFe.Ide.serie);

  rllEmissao.Caption := ACBrStr('Emissão: ' + FormatDateTimeBr(fpNFe.Ide.dEmi));
end;

procedure TfrlDANFeRLEtiqueta.RLb04_DestinatarioBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  rlmDestinatario.Lines.Clear;
  with fpNFe.Dest do
  begin
    rlmDestinatario.Lines.Add(XNome);

    rlmDestinatario.Lines.Add(EnderDest.XLgr +
      IfThen(EnderDest.Nro = '0', '', ', ' + EnderDest.Nro) +
      IfThen(EstaVazio(EnderDest.XCpl), '', ', ' + EnderDest.XCpl) +
      IfThen(EstaVazio(EnderDest.XBairro), '', ', ' + EnderDest.XBairro) +
      ', ' + EnderDest.XMun + '/ ' + EnderDest.UF);

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


procedure TfrlDANFeRLEtiqueta.InicializarDados;
begin
  if fpDANFe.Etiqueta then
  begin
    RLLabel17.Caption := 'DANFE Etiqueta - Etiqueta';
  end
  else
  begin
  //
  end;
end;

procedure TfrlDANFeRLEtiqueta.ProtocoloNFE(const sProtocolo: String);
begin
  fpDANFe.Protocolo := sProtocolo;
end;

procedure TfrlDANFeRLEtiqueta.rlb01_ChaveBeforePrint(Sender: TObject; var PrintBand: Boolean);
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

procedure TfrlDANFeRLEtiqueta.RLBand1BeforePrint(Sender: TObject; var
    PrintIt: Boolean);
begin
  inherited;
  rlmDadosAdicionais.Lines.Clear;
  rlmDadosAdicionais.Lines.Add('Informações Adicionais:');
  rlmDadosAdicionais.Lines.Add(fpNFe.infAdic.infCpl);
end;

procedure TfrlDANFeRLEtiqueta.RLNFeDataRecord(Sender: TObject; RecNo, CopyNo: Integer; var EOF: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;

  EOF := (RecNo > 1);
  RecordAction := raUseIt;
end;

procedure TfrlDANFeRLEtiqueta.subItensDataRecord(Sender: TObject; RecNo, CopyNo: Integer; var EOF: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;

  FNumItem := RecNo - 1;
  EOF := (RecNo > fpNFe.Det.Count);
  RecordAction := raUseIt;
end;

end.
