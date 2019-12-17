{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Conhecimen-}
{ to de Transporte eletrônico - NFe - http://www.nfe.fazenda.gov.br            }

{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrNFeDAInutRLRetrato;

interface

uses
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, Qt,
  {$ELSE}
  Graphics, Controls, Forms,
  {$ENDIF}
  ACBrNFeDAInutRL,
  RLReport, RLFilters, RLPDFFilter;

type

  { TfrmNFeDAInutRLRetrato }

  TfrmNFeDAInutRLRetrato = class(TfrmNFeDAInutRL)
    rlb_01_Titulo: TRLBand;
    rllProtocolo: TRLLabel;
    rllOrgao: TRLLabel;
    rllDescricao: TRLLabel;
    rlLabel2: TRLLabel;
    rlLabel78: TRLLabel;
    rllModelo: TRLLabel;
    rlb_07_Rodape: TRLBand;
    rlb_03_Inutilizacao: TRLBand;
    rlsLinhaV10: TRLDraw;
    rlsLinhaV09: TRLDraw;
    rlsLinhaH04: TRLDraw;
    rlsLinhaV01: TRLDraw;
    rllLinha3: TRLLabel;
    rllLinha2: TRLLabel;
    rllLinha1: TRLLabel;
    rlShape88: TRLDraw;
    rllTituloEvento: TRLLabel;
    rlShape48: TRLDraw;
    rlLabel9: TRLLabel;
    rllTipoAmbiente: TRLLabel;
    rlLabel6: TRLLabel;
    rllSerie: TRLLabel;
    rlLabel28: TRLLabel;
    rllAno: TRLLabel;
    rlLabel17: TRLLabel;
    rllNumeracao: TRLLabel;
    rlShape49: TRLDraw;
    rlShape50: TRLDraw;
    rlLabel18: TRLLabel;
    rllStatus: TRLLabel;
    rlb_02_Emitente: TRLBand;
    rlsLinhaH07: TRLDraw;
    rlsLinhaH06: TRLDraw;
    rllRazaoEmitente: TRLLabel;
    rllMunEmitente: TRLLabel;
    rllInscEstEmitente: TRLLabel;
    rllEnderecoEmitente: TRLLabel;
    rllCNPJEmitente: TRLLabel;
    rllCEPEmitente: TRLLabel;
    rlLabel98: TRLLabel;
    rlLabel93: TRLLabel;
    rlLabel24: TRLLabel;
    rlLabel22: TRLLabel;
    rlLabel16: TRLLabel;
    rlLabel13: TRLLabel;
    rlLabel12: TRLLabel;
    rlShape51: TRLDraw;
    rlShape53: TRLDraw;
    rlShape82: TRLDraw;
    rlShape99: TRLDraw;
    rlLabel4: TRLLabel;
    rllBairroEmitente: TRLLabel;
    rlShape108: TRLDraw;
    rlLabel5: TRLLabel;
    rllFoneEmitente: TRLLabel;
    rlShape109: TRLDraw;
    rllblSistema: TRLLabel;
    rlShape1: TRLDraw;
    rllDataHoraImpressao: TRLLabel;
    rlShape2: TRLDraw;
    rlLabel1: TRLLabel;
    rllJustificativa: TRLMemo;
    procedure RLInutBeforePrint(Sender: TObject; var PrintReport: Boolean);
  private
    procedure InicializaDados;
    procedure AjustarLayout;
  public
  end;

implementation

uses
  DateUtils,
  pcnConversao,
  ACBrUtil, ACBrDFeReportFortes, ACBrNFeDANFeRLClass, ACBrValidador;

{$IfNDef FPC}
  {$R *.dfm}
{$Else}
  {$R *.lfm}
{$EndIf}

procedure TfrmNFeDAInutRLRetrato.RLInutBeforePrint(Sender: TObject; var PrintReport: Boolean);
begin
  inherited;
  AjustarLayout;
  InicializaDados;

end;

procedure TfrmNFeDAInutRLRetrato.InicializaDados;
begin
  RLNFeInut.Title := ACBrStr('Inutilização');

  if (fpNFe <> nil) then
  begin
    with fpNFe do
    begin
      // 1.) Preenche os campos do Emitente
      rllRazaoEmitente.Caption := Emit.xNome;
      rllCNPJEmitente.Caption  := FormatarCNPJouCPF(Emit.CNPJCPF);
      if NaoEstaVazio(Emit.EnderEmit.xCpl) then
        rllEnderecoEmitente.Caption := Emit.EnderEmit.xLgr + ', ' + Emit.EnderEmit.nro + ' ' + Emit.EnderEmit.xCpl
      else
        rllEnderecoEmitente.Caption := Emit.EnderEmit.xLgr + ', ' + Emit.EnderEmit.nro;

      rllBairroEmitente.Caption  := Emit.EnderEmit.xBairro;
      rllCEPEmitente.Caption     := FormatarCEP(Emit.EnderEmit.CEP);
      rllMunEmitente.Caption     := Emit.EnderEmit.xMun;
      rllFoneEmitente.Caption    := FormatarFone(Emit.EnderEmit.fone);
//      rllEmitUF.Caption          := Emit.EnderEmit.UF;
      rllInscEstEmitente.Caption := Emit.IE;
    end; // with NFe
  end; // if fpNFe <> nil


  // Preeche os campos - Quadro Inutilizacao
  rllOrgao.Caption := IntToStr(fpInutNFe.RetInutNFe.cUF);
  case fpInutNFe.RetInutNFe.tpAmb of
    taProducao:
      rllTipoAmbiente.Caption := ACBrStr('PRODUÇÃO');
    taHomologacao:
      rllTipoAmbiente.Caption := ACBrStr('HOMOLOGAÇÃO - SEM VALOR FISCAL');
  end;

  rllAno.Caption              := IntToStr(fpInutNFe.RetInutNFe.ano);
  rllModelo.Caption           := IntToStr(fpInutNFe.RetInutNFe.modelo);
  rllSerie.Caption            := IntToStr(fpInutNFe.RetInutNFe.serie);
  rllNumeracao.Caption        := IntToStr(fpInutNFe.RetInutNFe.nNFIni) + ACBrStr(' a ') + IntToStr(fpInutNFe.RetInutNFe.nNFFin);
  rllStatus.Caption           := IntToStr(fpInutNFe.RetInutNFe.cStat) + ' - ' + fpInutNFe.RetInutNFe.xMotivo;
  rllProtocolo.Caption        := fpInutNFe.RetInutNFe.nProt + ' ' + FormatDateTimeBr(fpInutNFe.RetInutNFe.dhRecbto);
  rllJustificativa.Lines.Text := ACBrStr(fpInutNFe.RetInutNFe.xJust);

  rllDataHoraImpressao.Caption := ACBrStr('DATA E HORA DA IMPRESSÃO: ') + FormatDateTimeBr(Now);

  rllblSistema.Caption := Trim(fpDANFe.Sistema);
  if NaoEstaVazio(fpDANFe.Usuario) then
  begin
    if NaoEstaVazio(rllblSistema.Caption) then
    begin
      rllblSistema.Caption := rllblSistema.Caption + ' - ' + fpDANFe.Usuario;
    end
    else
    begin
      rllblSistema.Caption := fpDANFe.Usuario;
    end;
  end;

  if rllblSistema.Caption = '' then
  begin
    rllblSistema.Caption := ACBrStr('Desenvolvido por Projeto ACBr - http://www.projetoacbr.com.br');
  end;

end;

procedure TfrmNFeDAInutRLRetrato.AjustarLayout;
var
  b, i: Integer;
begin
  TDFeReportFortes.AjustarMargem(RLNFeInut, fpDANFe);

  // Ajuste da fonte
  case fpDANFe.Fonte.Nome of
    nfArial:
      for b := 0 to (RLNFeInut.ControlCount - 1) do
        for i := 0 to (TRLBand(RLNFeInut.Controls[b]).ControlCount - 1) do
          TRLLabel(TRLBand(RLNFeInut.Controls[b]).Controls[i]).Font.Name := 'Arial';

    nfCourierNew:
    begin
      for b := 0 to (RLNFeInut.ControlCount - 1) do
        for i := 0 to (TRLBand(RLNFeInut.Controls[b]).ControlCount - 1) do
        begin
          TRLLabel(TRLBand(RLNFeInut.Controls[b]).Controls[i]).Font.Name := 'Courier New';
          TRLLabel(TRLBand(RLNFeInut.Controls[b]).Controls[i]).Font.Size :=
            TRLLabel((TRLBand(RLNFeInut.Controls[b])).Controls[i]).Font.Size - 1;
        end;
    end;

    nfTimesNewRoman:
      for b := 0 to (RLNFeInut.ControlCount - 1) do
        for i := 0 to (TRLBand(RLNFeInut.Controls[b]).ControlCount - 1) do
          TRLLabel((TRLBand(RLNFeInut.Controls[b])).Controls[i]).Font.Name := 'Times New Roman';
  end;

end;

end.

