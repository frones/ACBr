{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
|* 14/03/2013: Peterson de Cerqueira Matos
|*  - Início da impressão dos eventos em Fortes Report
******************************************************************************}


{$I ACBr.inc}
unit ACBrNFeDANFeEventoRLRetrato;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QExtCtrls, Qt, QStdCtrls,
  {$ELSE}
    Controls, Forms, Dialogs, ExtCtrls,
  {$ENDIF}
  RLReport, RLFilters, RLPDFFilter, {$IFDEF BORLAND} XMLIntf, XMLDoc, {$ENDIF}
  pcnConversao, RLBarcode, ACBrNFeDANFeEventoRL;

type
  TfrlDANFeEventoRLRetrato = class(TfrlDANFeEventoRL)
    rlbUsuario: TRLBand;
    rlbEvento: TRLBand;
    rllNomeEvento: TRLLabel;
    RLLabel32: TRLLabel;
    rllOrgao: TRLLabel;
    RLLabel35: TRLLabel;
    rllEvento: TRLLabel;
    RLLabel39: TRLLabel;
    rllStatusEvento: TRLLabel;
    RLLabel40: TRLLabel;
    rllProtocoloEvento: TRLLabel;
    RLLabel36: TRLLabel;
    rllDescrEvento: TRLLabel;
    RLLabel33: TRLLabel;
    rllAmbiente: TRLLabel;
    RLLabel37: TRLLabel;
    rllSeqEvento: TRLLabel;
    RLLabel42: TRLLabel;
    rllDataHoraRegistro: TRLLabel;
    RLLabel38: TRLLabel;
    rllVersaoEvento: TRLLabel;
    RLDraw16: TRLDraw;
    RLDraw17: TRLDraw;
    RLDraw22: TRLDraw;
    RLDraw20: TRLDraw;
    RLDraw19: TRLDraw;
    RLDraw24: TRLDraw;
    RLDraw21: TRLDraw;
    RLDraw15: TRLDraw;
    rlbNFe: TRLBand;
    RLLabel20: TRLLabel;
    RLLabel44: TRLLabel;
    rllModeloNF: TRLLabel;
    RLLabel45: TRLLabel;
    rllSerieNF: TRLLabel;
    RLLabel46: TRLLabel;
    rllMesAnoEmissaoNF: TRLLabel;
    RLDraw30: TRLDraw;
    RLDraw32: TRLDraw;
    RLDraw31: TRLDraw;
    RLDraw29: TRLDraw;
    RLDraw37: TRLDraw;
    rlbCondUso: TRLBand;
    RLLabel21: TRLLabel;
    rlbCabecalho: TRLBand;
    RLDraw3: TRLDraw;
    rliLogo: TRLImage;
    rllTitulo: TRLLabel;
    rllCabecalhoLinha1: TRLLabel;
    rllCabecalhoLinha2: TRLLabel;
    rlbCodigoBarras: TRLBarcode;
    rllChaveNFe: TRLLabel;
    RLLabel4: TRLLabel;
    RLDraw4: TRLDraw;
    rllDataHoraEvento: TRLLabel;
    RLLabel5: TRLLabel;
    RLDraw5: TRLDraw;
    RLDraw50: TRLDraw;
    rlmCondUso: TRLMemo;
    rllUsuario: TRLLabel;
    rllSistema: TRLLabel;
    rlbJustificativa: TRLBand;
    RLLabel3: TRLLabel;
    rllJustificativa: TRLLabel;
    RLDraw1: TRLDraw;
    RLDraw2: TRLDraw;
    RLDraw6: TRLDraw;
    rlbCorrecao: TRLBand;
    RLLabel6: TRLLabel;
    RLDraw7: TRLDraw;
    rlmCorrecao: TRLMemo;
    RLLabel1: TRLLabel;
    RLDraw8: TRLDraw;
    rllNumNF: TRLLabel;
    rlbDestinatario: TRLBand;
    RLDraw9: TRLDraw;
    RLLabel18: TRLLabel;
    RLLabel2: TRLLabel;
    rllDestNome: TRLLabel;
    RLLabel7: TRLLabel;
    rllDestEndereco: TRLLabel;
    RLLabel8: TRLLabel;
    rllDestCidade: TRLLabel;
    RLLabel9: TRLLabel;
    rllDestFone: TRLLabel;
    RLLabel10: TRLLabel;
    rllDestBairro: TRLLabel;
    RLLabel41: TRLLabel;
    rllDestUF: TRLLabel;
    RLLabel11: TRLLabel;
    rllDestCNPJ: TRLLabel;
    RLLabel12: TRLLabel;
    rllDestCEP: TRLLabel;
    RLLabel13: TRLLabel;
    rllDestIE: TRLLabel;
    RLDraw10: TRLDraw;
    RLDraw11: TRLDraw;
    RLDraw12: TRLDraw;
    RLDraw23: TRLDraw;
    RLDraw13: TRLDraw;
    RLDraw14: TRLDraw;
    RLDraw18: TRLDraw;
    RLDraw25: TRLDraw;
    rlbEmitente: TRLBand;
    RLDraw26: TRLDraw;
    RLLabel14: TRLLabel;
    RLLabel15: TRLLabel;
    rllEmitNome: TRLLabel;
    RLLabel17: TRLLabel;
    rllEmitEndereco: TRLLabel;
    RLLabel22: TRLLabel;
    rllEmitCidade: TRLLabel;
    RLLabel24: TRLLabel;
    rllEmitFone: TRLLabel;
    RLLabel26: TRLLabel;
    rllEmitBairro: TRLLabel;
    RLLabel28: TRLLabel;
    rllEmitUF: TRLLabel;
    RLLabel30: TRLLabel;
    rllEmitCNPJ: TRLLabel;
    RLLabel34: TRLLabel;
    rllEmitCEP: TRLLabel;
    RLLabel47: TRLLabel;
    rllEmitIE: TRLLabel;
    RLDraw27: TRLDraw;
    RLDraw28: TRLDraw;
    RLDraw33: TRLDraw;
    RLDraw34: TRLDraw;
    RLDraw35: TRLDraw;
    RLDraw36: TRLDraw;
    RLDraw38: TRLDraw;
    RLDraw39: TRLDraw;
    rliMarcaDagua1: TRLImage;
    procedure RLEventoBeforePrint(Sender: TObject; var PrintIt: Boolean);
  private
    procedure InitDados;
    procedure AjustaLayout;
  public

  end;

implementation

uses
  ACBrDFeUtil, ACBrNFeDANFeRLClass, ACBrValidador, ACBrUtil;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TfrlDANFeEventoRLRetrato.InitDados;
begin
  // Carrega logomarca
  if (FLogo <> '') and FileExists(FLogo) { *Converted from FileExists* } then
     rliLogo.Picture.LoadFromFile(FLogo);

  // Carrega marca d'água
  if (FMarcaDagua <> '') and FileExists(FMarcaDagua) { *Converted from FileExists* } then
    begin
      rliMarcaDagua1.Picture.LoadFromFile(FMarcaDagua);
    end;

  with FEventoNFe do
    begin
      // Preeche os campos - Quadro NF-e
      rllModeloNF.Caption := Copy(InfEvento.chNFe, 21, 2);
      rllSerieNF.Caption := Copy(InfEvento.chNFe, 23, 3);
      rllNumNF.Caption := Copy(InfEvento.chNFe, 26, 3) + '.' +
                          Copy(InfEvento.chNFe, 29, 3) + '.' +
                          Copy(InfEvento.chNFe, 32, 3);
      rllMesAnoEmissaoNF.Caption := Copy(InfEvento.chNFe, 5, 2) + '/' +
                                    Copy(InfEvento.chNFe, 3, 2);
      rllChaveNFe.Caption := FormatarChaveAcesso(InfEvento.chNFe);
      rlbCodigoBarras.Caption := InfEvento.chNFe;

      // Preenche os campos - Quadro Evento
      rllOrgao.Caption := IntToStr(InfEvento.cOrgao);
      case InfEvento.tpAmb of
        taProducao:    rllAmbiente.Caption := ACBrStr('PRODUÇÃO');
        taHomologacao: rllAmbiente.Caption := ACBrStr('HOMOLOGAÇÃO - SEM VALOR FISCAL');
      end;
      rllDataHoraEvento.Caption := DateTimeToStr(InfEvento.dhEvento);
      rllEvento.Caption := InfEvento.TipoEvento;
      rllDescrEvento.Caption := ACBrStr(InfEvento.DescEvento);
      rllSeqEvento.Caption := IntToStr(InfEvento.nSeqEvento);
      rllVersaoEvento.Caption := InfEvento.versaoEvento;
      rllStatusEvento.Caption := IntToStr(RetInfEvento.cStat) + ' - ' +
                                          RetInfEvento.xMotivo;
      rllProtocoloEvento.Caption := RetInfEvento.nProt;
      rllDataHoraRegistro.Caption := DateTimeToStr(RetInfEvento.dhRegEvento);

      // Se o XML da NF-e foi carregado, preenche os campos
      // do Emitente e do Destinatário
      if FNFe <> nil then
        begin
          with FNFe do
            begin
              // 1.) Preenche os campos do Emitente
              rllEmitNome.Caption := Emit.xNome;
              rllEmitCNPJ.Caption := FormatarCNPJouCPF(Emit.CNPJCPF);
              if Emit.EnderEmit.xCpl > '' then
                rllEmitEndereco.Caption := Emit.EnderEmit.xLgr + ', ' + Emit.EnderEmit.nro + ' ' + Emit.EnderEmit.xCpl
              else
                rllEmitEndereco.Caption := Emit.EnderEmit.xLgr + ', ' + Emit.EnderEmit.nro;
              rllEmitBairro.Caption := Emit.EnderEmit.xBairro;
              rllEmitCEP.Caption := FormatarCEP(Emit.EnderEmit.CEP);
              rllEmitCidade.Caption := Emit.EnderEmit.xMun;
              rllEmitFone.Caption := FormatarFone(Emit.EnderEmit.fone);
              rllEmitUF.Caption := Emit.EnderEmit.UF;
              rllEmitIE.Caption := Emit.IE;

              // 2.) Preenche os campos do Destinatário
              rllDestNome.Caption := Dest.xNome;
              rllDestCNPJ.Caption := FormatarCNPJouCPF(Dest.CNPJCPF);
              if Dest.EnderDest.xCpl > '' then
                rllDestEndereco.Caption := Dest.EnderDest.xLgr + ', ' + Dest.EnderDest.nro + ' ' + Dest.EnderDest.xCpl
              else
                rllDestEndereco.Caption := Dest.EnderDest.xLgr + ', ' + Dest.EnderDest.nro;
              rllDestBairro.Caption := Dest.EnderDest.xBairro;
              rllDestCEP.Caption := FormatarCEP(Dest.EnderDest.CEP);
              rllDestCidade.Caption := Dest.EnderDest.xMun;
              rllDestFone.Caption := FormatarFone(Dest.EnderDest.fone);
              rllDestUF.Caption := Dest.EnderDest.UF;
              rllDestIE.Caption := Dest.IE;
            end; // with NFe
        end; // if FNFe <> nil

      // Preenche os campos específicos de acordo com o evento
      case InfEvento.tpEvento of
        teCCe:
          begin
            rllTitulo.Caption := ACBrStr('CARTA DE CORREÇÃO ELETRÔNICA');

            // Prrenche os campos - "Condições de uso"
            rlmCondUso.Lines.Add(StringReplace(InfEvento.detEvento.xCondUso,
                                  ';', #13#10, [rfReplaceAll, rfIgnoreCase]));
            rlmCondUso.Lines.Text := StringReplace(rlmCondUso.Lines.Text,
                                  ': I', ':'#13#10'I', [rfReplaceAll, rfIgnoreCase]);

            // Prrenche os campos - "Correção"
            rlmCorrecao.Lines.Add(StringReplace(InfEvento.detEvento.xCorrecao,
                                  ';', #13#10, [rfReplaceAll, rfIgnoreCase]));
          end;

        teCancelamento:
          begin
            rllTitulo.Caption := 'CANCELAMENTO DE NF-E';
            rllJustificativa.Caption := InfEvento.detEvento.xJust;
          end;
        teManifDestConfirmacao:
          begin
            rllTitulo.Caption := ACBrStr('CONFIRMAÇÃO DA OPERAÇÃO');
          end;
        teManifDestCiencia:
          begin
            rllTitulo.Caption := ACBrStr('CIÊNCIA DA EMISSÃO/OPERAÇÃO');
          end;
        teManifDestDesconhecimento:
          begin
            rllTitulo.Caption := ACBrStr('DESCONHECIMENTO DA OPERAÇÃO');
          end;
        teManifDestOperNaoRealizada:
          begin
            rllTitulo.Caption := ACBrStr('OPERAÇÃO NÃO REALIZADA');
            rllJustificativa.Caption := InfEvento.detEvento.xJust;
          end;
      end; // case InfEvento.tpEvento

      rllNomeEvento.Caption := rllTitulo.Caption;
    end; // with FEventoNFe.Evento.Items[FItemAtual].InfEvento


  // Exibe o desenvolvedor do sistema
  if FSsitema <> '' then
    begin
      rllSistema.Caption := FSsitema;
      rllSistema.Visible := True;
    end
  else
    rllSistema.Visible := False;

  // Exibe o nome do usuário
  if FUsuario <> '' then
    begin
      rllUsuario.Caption := ACBrStr('DATA / HORA DA IMPRESSÃO: ') +
                            DateTimeToStr(Now) + ' - ' + FUsuario;
      rllUsuario.Visible := True;
    end
  else
    rllUsuario.Visible := False;

end;

procedure TfrlDANFeEventoRLRetrato.AjustaLayout;
var b, i: Integer;
begin
  // Ajuste das margens
  with RLEvento.Margins do
    begin
      TopMargin := FMargemSuperior * 10;
      BottomMargin := FMargemInferior * 10;
      LeftMargin := FMargemEsquerda * 10;
      RightMargin := FMargemDireita * 10;
    end;

  // Ajuste da fonte
  case FNomeFonte of
    nfArial:
      for b := 0 to (RLEvento.ControlCount - 1) do
        for i := 0 to ((TRLBand(RLEvento.Controls[b]).ControlCount) - 1) do
          begin
            TRLLabel((TRLBand(RLEvento.Controls[b])).Controls[i]).Font.Name :=
                                                                      'Arial';
          end;

    nfCourierNew:
      begin
        for b := 0 to (RLEvento.ControlCount - 1) do
          for i := 0 to ((TRLBand(RLEvento.Controls[b]).ControlCount) - 1) do
            begin
              TRLLabel((TRLBand(RLEvento.Controls[b])).Controls[i]).Font.Name :=
                                                                'Courier New';
                TRLLabel((TRLBand(RLEvento.Controls[b])).Controls[i]).Font.Size :=
               (TRLLabel((TRLBand(RLEvento.Controls[b])).Controls[i]).Font.Size) - 1;

            end;
      end;

    nfTimesNewRoman:
      for b := 0 to (RLEvento.ControlCount - 1) do
        for i := 0 to ((TRLBand(RLEvento.Controls[b]).ControlCount) - 1) do
          begin
            TRLLabel((TRLBand(RLEvento.Controls[b])).Controls[i]).Font.Name :=
                                                             'Times New Roman';
          end;
  end;

  // Dados em negrito
  if FNegrito then
    begin
      for b := 0 to (RLEvento.ControlCount - 1) do
        for i := 0 to ((TRLBand(RLEvento.Controls[b]).ControlCount) - 1) do
          begin
            if TRLLabel((TRLBand(RLEvento.Controls[b])).Controls[i]).Tag = 70 then
              TRLLabel((TRLBand(RLEvento.Controls[b])).Controls[i]).Font.Style := [fsBold];
          end;
    end
  else
    begin
      for b := 0 to (RLEvento.ControlCount - 1) do
        for i := 0 to ((TRLBand(RLEvento.Controls[b]).ControlCount) - 1) do
          begin
            if TRLLabel((TRLBand(RLEvento.Controls[b])).Controls[i]).Tag = 70 then
              TRLLabel((TRLBand(RLEvento.Controls[b])).Controls[i]).Font.Style := [];
          end;
    end;

  // Centraliza as linhas do cabeçalho caso o logo não seja informado
  if (FLogo <> '') and FileExists(FLogo) { *Converted from FileExists* } then
    begin
      rllTitulo.Left := 88;
      rllCabecalhoLinha1.Left := 88;
      rllCabecalhoLinha2.Left := 88;

      rllTitulo.Width := 650;
      rllCabecalhoLinha1.Width := 650;
      rllCabecalhoLinha2.Width := 650;
    end
  else
    begin
      rllTitulo.Left := 8;
      rllCabecalhoLinha1.Left := 8;
      rllCabecalhoLinha2.Left := 8;

      rllTitulo.Width := 730;
      rllCabecalhoLinha1.Width := 730;
      rllCabecalhoLinha2.Width := 730;
    end;

  // Se o XML da NF-e foi carregado também, exibe os quadros
  // "Emitente" e "Destinatário"
  if FNFe <> nil then
    begin
      rlbEmitente.Visible := True;
      rlbDestinatario.Visible := True;
    end
  else
    begin
      rlbEmitente.Visible := False;
      rlbDestinatario.Visible := False;
    end;

  // Exibe os itens específicos de cada evento e ajusta a posição
  // da marca dágua, de acordo com o evento
  case FEventoNFe.InfEvento.tpEvento of
    teCCe:
      begin
        rlbJustificativa.Visible := False;
        rlbCondUso.Visible := True;
        rlbCorrecao.Visible := True;
        rliMarcaDagua1.Top := ((rlbCorrecao.Top + rlbCorrecao.Height) div 2) - (rliMarcaDagua1.Height div 2);
      end;

    teCancelamento,teManifDestConfirmacao,teManifDestCiencia, teManifDestDesconhecimento, teManifDestOperNaoRealizada:
      begin
        rlbJustificativa.Visible := True;
        rlbCondUso.Visible := False;
        rlbCorrecao.Visible := False;
        rliMarcaDagua1.Top := ((rlbDestinatario.Top + rlbDestinatario.Height) div 2) - (rliMarcaDagua1.Height div 2);
      end;
  end; // case FEventoNFe.Evento.Items[0].InfEvento.tpEvento

  rliMarcaDagua1.Left := (RLEvento.Width div 2) - (rliMarcaDagua1.Width div 2);
end;

procedure TfrlDANFeEventoRLRetrato.RLEventoBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  AjustaLayout;
  InitDados;
end;

end.
