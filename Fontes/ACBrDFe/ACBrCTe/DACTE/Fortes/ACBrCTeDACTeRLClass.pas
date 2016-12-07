{******************************************************************************}
{ Projeto: Componente ACBrCTe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Conhecimen-}
{ to de Transporte eletrônico - CTe - http://www.cte.fazenda.gov.br            }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014 Mark dos Santos Gonçalves              }
{                                        Juliomar Marchetti                    }
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
******************************************************************************}

{$I ACBr.inc}

unit ACBrCTeDACTeRLClass;

interface

{$H+}

uses
  Forms, SysUtils, Classes,
  pcnConversao, pcteCTe, ACBrCTeDACTEClass, RLTypes ,
  ACBrCTeDACTeRL, ACBrCTeDACTeRLRetrato, ACBrCTeDACTeRLRetratoA5,
  ACBrCTeDAEventoRL, ACBrCTeDAEventoRLRetrato;

type

  { TACBrCTeDACTeRL }

  TACBrCTeDACTeRL = class(TACBrCTeDACTeClass)
  private
		protected
     FPrintDialog: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImprimirDACTe(CTe: TCTe = nil); override;
    procedure ImprimirDACTePDF(CTe: TCTe = nil); override;
    procedure ImprimirEVENTO(CTe: TCTe = nil); override;
    procedure ImprimirEVENTOPDF(CTe: TCTe = nil); override;
    procedure ImprimirINUTILIZACAO(CTe: TCTe = nil); override;
    procedure ImprimirINUTILIZACAOPDF(CTe: TCTe = nil); override;
  published
    property PrintDialog: Boolean read FPrintDialog write FPrintDialog;
  end;

implementation

uses
  StrUtils, Dialogs, ACBrUtil, ACBrCTe,
  ACBrCTeDAInutRL, ACBrCTeDAInutRLRetrato;

var
  frmCTeDAEventoRL: TfrmCTeDAEventoRL;
  frmCTeDAInutRL : TfrmCTeDAInutRL;

constructor TACBrCTeDACTeRL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrintDialog := True;
end;

destructor TACBrCTeDACTeRL.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrCTeDACTeRL.ImprimirDACTe(CTe: TCTe = nil);
var
  i: integer;
  sProt: string;

  frmDACTeRLRetrato: TfrmDACTeRL;
begin
  case TamanhoPapel of
    tpA5:
    begin
      frmDACTeRLRetrato := TfrmDACTeRLRetratoA5.Create(Self);
      frmDACTeRLRetrato.RLCTe.PageSetup.PaperSize := fpA5;
      frmDACTeRLRetrato.RLCTe.PageSetup.PaperHeight := 148.0;
      frmDACTeRLRetrato.RLCTe.PageSetup.PaperWidth := 210.0;
    end;
    else
    begin // tpA4
      frmDACTeRLRetrato := TfrmDACTeRLRetrato.Create(Self);
      frmDACTeRLRetrato.RLCTe.PageSetup.PaperSize := fpA4;
      frmDACTeRLRetrato.RLCTe.PageSetup.PaperHeight := 297.0;
      frmDACTeRLRetrato.RLCTe.PageSetup.PaperWidth := 210.0;
    end;
  end;

  sProt := TACBrCTe(ACBrCTe).DACTe.ProtocoloCTe;

  if CTe = nil then
  begin
    for i := 0 to TACBrCTe(ACBrCTe).Conhecimentos.Count - 1 do
      frmDACTeRLRetrato.Imprimir(Self, TACBrCTe(ACBrCTe).Conhecimentos.Items[i].CTe
        , Logo
        , Email
        , ImprimirHoraSaida
        , ExpandirLogoMarca
        , ImprimirHoraSaida_Hora
        , FResumoCanhoto
        , Fax
        , NumCopias
        , Sistema
        , Site
        , Usuario
        , MostrarPreview
        , MostrarStatus
        , MargemSuperior
        , MargemInferior
        , MargemEsquerda
        , MargemDireita
        , Impressora
        , PosCanhoto
        , CTeCancelada
        , EPECEnviado
		    , PrintDialog);
  end
  else
    frmDACTeRLRetrato.Imprimir(Self,
       CTe
      , Logo
      , Email
      , ImprimirHoraSaida
      , ExpandirLogoMarca
      , ImprimirHoraSaida_Hora
      , FResumoCanhoto
      , Fax
      , NumCopias
      , Sistema
      , Site
      , Usuario
      , MostrarPreview
      , MostrarStatus
      , MargemSuperior
      , MargemInferior
      , MargemEsquerda
      , MargemDireita
      , Impressora
      , PosCanhoto
      , CTeCancelada
      , EPECEnviado
	    , PrintDialog);

  if frmDACTeRLRetrato.RLCTe <> nil then
    frmDACTeRLRetrato.Free;
end;

procedure TACBrCTeDACTeRL.ImprimirDACTePDF(CTe: TCTe = nil);
var
  i: integer;
  sProt: string;
  NomeArq: string;

  frmDACTeRLRetrato: TfrmDACTeRL;
begin
  case TamanhoPapel of
    tpA5:
    begin
      frmDACTeRLRetrato := TfrmDACTeRLRetratoA5.Create(Self);
      frmDACTeRLRetrato.RLCTe.PageSetup.PaperSize := fpA5;
      frmDACTeRLRetrato.RLCTe.PageSetup.PaperHeight := 148.0;
      frmDACTeRLRetrato.RLCTe.PageSetup.PaperWidth := 210.0;
    end;
    else
    begin // tpA4
      frmDACTeRLRetrato := TfrmDACTeRLRetrato.Create(Self);
      frmDACTeRLRetrato.RLCTe.PageSetup.PaperSize := fpA4;
      frmDACTeRLRetrato.RLCTe.PageSetup.PaperHeight := 297.0;
      frmDACTeRLRetrato.RLCTe.PageSetup.PaperWidth := 210.0;
    end;
  end;

  sProt := TACBrCTe(ACBrCTe).DACTe.ProtocoloCTe;

  if CTe = nil then
  begin
    for i := 0 to TACBrCTe(ACBrCTe).Conhecimentos.Count - 1 do
    begin
      NomeArq := OnlyNumber(TACBrCTe(ACBrCTe).Conhecimentos.Items[i].CTe.infCTe.ID);
      NomeArq := PathWithDelim(Self.PathPDF) + NomeArq + '-cte.pdf';

      frmDACTeRLRetrato.SavePDF(Self,
        NomeArq
        , TACBrCTe(ACBrCTe).Conhecimentos.Items[i].CTe
        , Logo
        , Email
        , ImprimirHoraSaida
        , ExpandirLogoMarca
        , ImprimirHoraSaida_Hora
        , FResumoCanhoto
        , Fax
        , MostrarStatus
        , NumCopias
        , Sistema
        , Site
        , Usuario
        , MargemSuperior
        , MargemInferior
        , MargemEsquerda
        , MargemDireita
        , PosCanhoto
        , CTeCancelada
        , EPECEnviado);
    end;
  end
  else
  begin
    NomeArq := OnlyNumber(CTe.infCTe.ID);
    NomeArq := PathWithDelim(Self.PathPDF) + NomeArq + '-cte.pdf';
    frmDACTeRLRetrato.SavePDF(Self,
      NomeArq
      , CTe
      , Logo
      , Email
      , ImprimirHoraSaida
      , ExpandirLogoMarca
      , ImprimirHoraSaida_Hora
      , FResumoCanhoto
      , Fax
      , MostrarStatus
      , NumCopias
      , Sistema
      , Site
      , Usuario
      , MargemSuperior
      , MargemInferior
      , MargemEsquerda
      , MargemDireita
      , PosCanhoto
      , CTeCancelada
      , EPECEnviado);
  end;

  if frmDACTeRLRetrato.RLCTe <> nil then
    frmDACTeRLRetrato.Free;
end;

procedure TACBrCTeDACTeRL.ImprimirEVENTO(CTe: TCTe);
var
  i, j: integer;
  Impresso: boolean;
begin
  frmCTeDAEventoRL := TfrmCTeDAEventoRLRetrato.Create(Self);

  if TACBrCTe(ACBrCTe).Conhecimentos.Count > 0 then
  begin
    for i := 0 to (TACBrCTe(ACBrCTe).EventoCTe.Evento.Count - 1) do
    begin
      Impresso := False;
      for j := 0 to (TACBrCTe(ACBrCTe).Conhecimentos.Count - 1) do
      begin
        if OnlyNumber(TACBrCTe(ACBrCTe).Conhecimentos.Items[j].CTe.infCTe.Id) = TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.chCTe then
        begin
          frmCTeDAEventoRL.Imprimir(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i],
            FLogo,
            FNumCopias,
            FSistema,
            FUsuario,
            FMostrarPreview,
            FMargemSuperior,
            FMargemInferior,
            FMargemEsquerda,
            FMargemDireita,
            FImpressora,
            TACBrCTe(ACBrCTe).Conhecimentos.Items[j].CTe);
          Impresso := True;
          Break;
        end;
      end;

      if Impresso = False then
      begin
        frmCTeDAEventoRL.Imprimir(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i],
          FLogo,
          FNumCopias,
          FSistema,
          FUsuario,
          FMostrarPreview,
          FMargemSuperior,
          FMargemInferior,
          FMargemEsquerda,
          FMargemDireita,
          FImpressora);
      end;
    end;
  end
  else
  begin
    for i := 0 to (TACBrCTe(ACBrCTe).EventoCTe.Evento.Count - 1) do
    begin
      frmCTeDAEventoRL.Imprimir(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i],
        FLogo,
        FNumCopias,
        FSistema,
        FUsuario,
        FMostrarPreview,
        FMargemSuperior,
        FMargemInferior,
        FMargemEsquerda,
        FMargemDireita,
        FImpressora);
    end;
  end;

  FreeAndNil(frmCTeDAEventoRL);
end;

procedure TACBrCTeDACTeRL.ImprimirEVENTOPDF(CTe: TCTe);
var
  i, j: integer;
  sFile: string;
  Impresso: boolean;
begin
  frmCTeDAEventoRL := TfrmCTeDAEventoRLRetrato.Create(Self);

  if TACBrCTe(ACBrCTe).Conhecimentos.Count > 0 then
  begin
    for i := 0 to (TACBrCTe(ACBrCTe).EventoCTe.Evento.Count - 1) do
    begin
      sFile := TACBrCTe(ACBrCTe).DACTE.PathPDF +
               OnlyNumber(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.Id) + 
			   '-procEventoCTe.pdf';
      Impresso := False;
      for j := 0 to (TACBrCTe(ACBrCTe).Conhecimentos.Count - 1) do
      begin
        if OnlyNumber(TACBrCTe(ACBrCTe).Conhecimentos.Items[j].CTe.infCTe.Id) = TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.chCTe then
        begin
          frmCTeDAEventoRL.SavePDF(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i],
            FLogo,
            sFile,
            FSistema,
            FUsuario,
            FMargemSuperior,
            FMargemInferior,
            FMargemEsquerda,
            FMargemDireita,
            TACBrCTe(ACBrCTe).Conhecimentos.Items[j].CTe);
          Impresso := True;
          Break;
        end;
      end;

      if Impresso = False then
      begin
        frmCTeDAEventoRL.SavePDF(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i],
          FLogo,
          sFile,
          FSistema,
          FUsuario,
          FMargemSuperior,
          FMargemInferior,
          FMargemEsquerda,
          FMargemDireita);
      end;
    end;
  end
  else
  begin
    for i := 0 to (TACBrCTe(ACBrCTe).EventoCTe.Evento.Count - 1) do
    begin
      sFile := TACBrCTe(ACBrCTe).DACTE.PathPDF +
               OnlyNumber(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.Id) +
               '-procEventoCTe.pdf';
      frmCTeDAEventoRL.SavePDF(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i],
        FLogo,
        sFile,
        FSistema,
        FUsuario,
        FMargemSuperior,
        FMargemInferior,
        FMargemEsquerda,
        FMargemDireita);
    end;
  end;

  FreeAndNil(frmCTeDAEventoRL);
end;

procedure TACBrCTeDACTeRL.ImprimirINUTILIZACAO(CTe: TCTe);
begin
  frmCTeDAInutRL := TfrmCTeDAInutRLRetrato.Create(Self);

  frmCTeDAInutRL.Imprimir(TACBrCTe(ACBrCTe),
                          FLogo, FNumCopias, FSistema, FUsuario,
                          FMostrarPreview, FMargemSuperior,
                          FMargemInferior, FMargemEsquerda,
                          FMargemDireita, FImpressora);

  FreeAndNil(frmCTeDAInutRL);
end;

procedure TACBrCTeDACTeRL.ImprimirINUTILIZACAOPDF(CTe: TCTe);
var
 NomeArq: String;
begin
  frmCTeDAInutRL := TfrmCTeDAInutRLRetrato.Create(Self);

  NomeArq := StringReplace(TACBrCTe(ACBrCTe).InutCTe.ID, 'ID', '', [rfIgnoreCase]);
  if NomeArq = '' then
    NomeArq := StringReplace(TACBrCTe(ACBrCTe).InutCTe.ID, 'ID', '', [rfIgnoreCase]);
  NomeArq := PathWithDelim(Self.PathPDF) + NomeArq + '-procInutCTe.pdf';

  frmCTeDAInutRL.SavePDF(TACBrCTe(ACBrCTe),
                         FLogo, NomeArq, FSistema, FUsuario,
                         FMargemSuperior, FMargemInferior,
                         FMargemEsquerda, FMargemDireita);

  FreeAndNil(frmCTeDAInutRL);
end;

end.
