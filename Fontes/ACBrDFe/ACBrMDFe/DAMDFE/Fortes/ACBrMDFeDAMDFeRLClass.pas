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

unit ACBrMDFeDAMDFeRLClass;

interface

uses
  Forms, SysUtils, Classes, ACBrMDFeDAMDFeClass, pmdfeMDFe;

type
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TACBrMDFeDAMDFeRL = class(TACBrMDFeDAMDFeClass)
  private
  protected
    FPrintDialog: Boolean;  
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImprimirDAMDFe(MDFe: TMDFe = nil); override;
    procedure ImprimirDAMDFePDF(MDFe: TMDFe = nil); override;
    procedure ImprimirEVENTO(MDFe: TMDFe = nil); override;
    procedure ImprimirEVENTOPDF(MDFe: TMDFe = nil); override;
  published
    property PrintDialog: Boolean read FPrintDialog write FPrintDialog;
end;

implementation

uses
  StrUtils, Dialogs, ACBrUtil, ACBrMDFe,
  ACBrMDFeDAMDFeRLRetrato, ACBrMDFeDAEventoRL, ACBrMDFeDAEventoRLRetrato;

var
  frmMDFeDAEventoRL: TfrmMDFeDAEventoRL;

constructor TACBrMDFeDAMDFeRL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrintDialog := True;
end;

destructor TACBrMDFeDAMDFeRL.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrMDFeDAMDFeRL.ImprimirDAMDFE(MDFe: TMDFe = nil);
var
  i: integer;
  frlDAMDFeRLRetrato: TfrlDAMDFeRLRetrato;
  sProt: string;
begin
  frlDAMDFeRLRetrato := TfrlDAMDFeRLRetrato.Create(Self);
  try
    sProt := TACBrMDFe(ACBrMDFe).DAMDFE.ProtocoloMDFe;
    frlDAMDFeRLRetrato.ProtocoloMDFe(sProt);

    if MDFe = nil then
    begin
      for i := 0 to TACBrMDFe(ACBrMDFe).Manifestos.Count - 1 do
      begin
        frlDAMDFeRLRetrato.Imprimir(Self,
          TACBrMDFe(ACBrMDFe).Manifestos.Items[i].MDFe
          , Logo
          , Email
          , ExpandirLogoMarca
          , Fax
          , NumCopias
          , Sistema
          , Site
          , Usuario
          , MostrarPreview
          , MargemSuperior
          , MargemInferior
          , MargemEsquerda
          , MargemDireita
          , Impressora
          , MDFeCancelada
          , MDFeEncerrado
          , PrintDialog   );
      end;
    end
    else
      frlDAMDFeRLRetrato.Imprimir(Self,
        MDFe
        , Logo
        , Email
        , ExpandirLogoMarca
        , Fax
        , NumCopias
        , Sistema
        , Site
        , Usuario
        , MostrarPreview
        , MargemSuperior
        , MargemInferior
        , MargemEsquerda
        , MargemDireita
        , Impressora
        , MDFeCancelada
        , MDFeEncerrado
        , PrintDialog   );
  finally
    frlDAMDFeRLRetrato.Free;
  end;
end;

procedure TACBrMDFeDAMDFeRL.ImprimirDAMDFEPDF(MDFe: TMDFe = nil);
var
  NomeArq: string;
  i: integer;
  frlDAMDFeRLRetrato: TfrlDAMDFeRLRetrato;
  sProt: string;
begin
  frlDAMDFeRLRetrato := TfrlDAMDFeRLRetrato.Create(Self);
  try
    sProt := TACBrMDFe(ACBrMDFe).DAMDFe.ProtocoloMDFe;
    frlDAMDFeRLRetrato.ProtocoloMDFe(sProt);

    if MDFe = nil then
    begin
      for i := 0 to TACBrMDFe(ACBrMDFe).Manifestos.Count - 1 do
      begin
        NomeArq := OnlyNumber(TACBrMDFe(ACBrMDFe).Manifestos.Items[i].MDFe.infMDFe.ID);
        NomeArq := PathWithDelim(Self.PathPDF) + NomeArq + '-mdfe.pdf';

        frlDAMDFeRLRetrato.SavePDF(Self,
          NomeArq
          , TACBrMDFe(ACBrMDFe).Manifestos.Items[i].MDFe
          , Logo
          , Email
          , ExpandirLogoMarca
          , Fax
          , NumCopias
          , Sistema
          , Site
          , Usuario
          , MargemSuperior
          , MargemInferior
          , MargemEsquerda
          , MargemDireita
          , MDFeCancelada
          , MDFeEncerrado);
      end;
    end
    else
    begin
      NomeArq := OnlyNumber(MDFe.infMDFe.ID);
      NomeArq := PathWithDelim(Self.PathPDF) + NomeArq + '-mdfe.pdf';

      frlDAMDFeRLRetrato.SavePDF(Self,
        NomeArq
        , MDFe
        , Logo
        , Email
        , ExpandirLogoMarca
        , Fax
        , NumCopias
        , Sistema
        , Site
        , Usuario
        , MargemSuperior
        , MargemInferior
        , MargemEsquerda
        , MargemDireita
        , MDFeCancelada
        , MDFeEncerrado);
    end;
  finally
    frlDAMDFeRLRetrato.Free;
  end;
end;

procedure TACBrMDFeDAMDFeRL.ImprimirEVENTO(MDFe: TMDFe);
var
  i, j: integer;
  Impresso: boolean;
begin
  frmMDFeDAEventoRL := TfrmMDFeDAEventoRLRetrato.Create(Self);
  try
    if TACBrMDFe(ACBrMDFe).Manifestos.Count > 0 then
    begin
      for i := 0 to (TACBrMDFe(ACBrMDFe).EventoMDFe.Evento.Count - 1) do
      begin
        Impresso := False;
        for j := 0 to (TACBrMDFe(ACBrMDFe).Manifestos.Count - 1) do
        begin
          if OnlyNumber(TACBrMDFe(ACBrMDFe).Manifestos.Items[j].MDFe.infMDFe.ID) = TACBrMDFe(ACBrMDFe).EventoMDFe.Evento.Items[i].InfEvento.chMDFe then
          begin
            frmMDFeDAEventoRL.Imprimir(Self,
              TACBrMDFe(ACBrMDFe).EventoMDFe.Evento.Items[i],
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
              TACBrMDFe(ACBrMDFe).Manifestos.Items[j].MDFe,
              PrintDialog);
            Impresso := True;
            Break;
          end;
        end;

        if Impresso = False then
        begin
          frmMDFeDAEventoRL.Imprimir(Self,
            TACBrMDFe(ACBrMDFe).EventoMDFe.Evento.Items[i],
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
            Nil,
            PrintDialog);
        end;
      end;
    end
    else
    begin
      for i := 0 to (TACBrMDFe(ACBrMDFe).EventoMDFe.Evento.Count - 1) do
      begin
        frmMDFeDAEventoRL.Imprimir(Self,
          TACBrMDFe(ACBrMDFe).EventoMDFe.Evento.Items[i],
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
          Nil,
          PrintDialog);
      end;
    end;
  finally
    FreeAndNil(frmMDFeDAEventoRL);
  end;
end;

procedure TACBrMDFeDAMDFeRL.ImprimirEVENTOPDF(MDFe: TMDFe);
var
  i, j: integer;
  sFile: string;
  Impresso: boolean;
begin
  frmMDFeDAEventoRL := TfrmMDFeDAEventoRLRetrato.Create(Self);
  try
    if TACBrMDFe(ACBrMDFe).Manifestos.Count > 0 then
    begin
      for i := 0 to (TACBrMDFe(ACBrMDFe).EventoMDFe.Evento.Count - 1) do
      begin
        sFile := TACBrMDFe(ACBrMDFe).DAMDFe.PathPDF +
                 OnlyNumber(TACBrMDFe(ACBrMDFe).EventoMDFe.Evento.Items[i].InfEvento.ID) +
                 '-procEventoMDFe.pdf';
        Impresso := False;

        for j := 0 to (TACBrMDFe(ACBrMDFe).Manifestos.Count - 1) do
        begin
          if OnlyNumber(TACBrMDFe(ACBrMDFe).Manifestos.Items[j].MDFe.infMDFe.ID) = TACBrMDFe(ACBrMDFe).EventoMDFe.Evento.Items[i].InfEvento.chMDFe then
          begin
            frmMDFeDAEventoRL.SavePDF(Self,
              TACBrMDFe(ACBrMDFe).EventoMDFe.Evento.Items[i],
              FLogo,
              sFile,
              FSistema,
              FUsuario,
              FMargemSuperior,
              FMargemInferior,
              FMargemEsquerda,
              FMargemDireita,
              TACBrMDFe(ACBrMDFe).Manifestos.Items[j].MDFe);
            Impresso := True;
            Break;
          end;
        end;

        if Impresso = False then
        begin
          frmMDFeDAEventoRL.SavePDF(Self,
            TACBrMDFe(ACBrMDFe).EventoMDFe.Evento.Items[i],
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
      for i := 0 to (TACBrMDFe(ACBrMDFe).EventoMDFe.Evento.Count - 1) do
      begin
        sFile := TACBrMDFe(ACBrMDFe).DAMDFe.PathPDF +
                 OnlyNumber(TACBrMDFe(ACBrMDFe).EventoMDFe.Evento.Items[i].InfEvento.ID) +
                 '-procEventoMDFe.pdf';

        frmMDFeDAEventoRL.SavePDF(Self,
        TACBrMDFe(ACBrMDFe).EventoMDFe.Evento.Items[i],
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
  finally
    FreeAndNil(frmMDFeDAEventoRL);
  end;
end;

end.
