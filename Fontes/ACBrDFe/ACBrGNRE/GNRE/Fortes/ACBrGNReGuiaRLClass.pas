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

unit ACBrGNReGuiaRLClass;

interface

uses
  Forms, SysUtils, Classes, ACBrGNREGuiaClass,pgnreGNRERetorno,RLTypes;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrGNREGuiaRL = class(TACBrGNREGuiaClass)
  private
  protected
    FPrintDialog: Boolean;  
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImprimirGuia(AGNRE: TGNRERetorno=nil); override;
    procedure ImprimirGuiaPDF(AGNRE: TGNRERetorno=nil); override;
  published
    property PrintDialog: Boolean read FPrintDialog write FPrintDialog;
end;

implementation

uses
  StrUtils, Dialogs, ACBrUtil, ACBrGNRE2, ACBrGNREGuiaRLRetrato;

constructor TACBrGNREGuiaRL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrintDialog := True;
end;

destructor TACBrGNREGuiaRL.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrGNREGuiaRL.ImprimirGuia(AGNRE: TGNRERetorno = nil);
var
  i: integer;
  frlGuiaRLRetrato: TfrlGuiaRLRetrato;
begin
  frlGuiaRLRetrato := TfrlGuiaRLRetrato.Create(Self);

  try
    frlGuiaRLRetrato.RLGNRe.PageSetup.PaperSize   :=fpA4;
    frlGuiaRLRetrato.RLGNRe.PageSetup.PaperHeight :=297.0;
    frlGuiaRLRetrato.RLGNRe.PageSetup.PaperWidth  :=210.0;

    if AGNRE = nil then
    begin
      for i:=0 to TACBrGNRE(ACBrGNRE).GuiasRetorno.Count -1 do
      begin
        frlGuiaRLRetrato.Imprimir(Self,
           TACBrGNRE(ACBrGNRE).GuiasRetorno.Items[i].GNRE,
           Email,
           Fax,
           NumCopias,
           Sistema,
           Site,
           Usuario,
           MostrarPreview,
           MargemSuperior,
           MargemInferior,
           MargemEsquerda,
           MargemDireita,
           Impressora,
           PrintDialog);
      end;
    end
    else
    begin
      frlGuiaRLRetrato.Imprimir(Self,
           AGNRE,
           Email,
           Fax,
           NumCopias,
           Sistema,
           Site,
           Usuario,
           MostrarPreview,
           MargemSuperior,
           MargemInferior,
           MargemEsquerda,
           MargemDireita,
           Impressora,
           PrintDialog);
    end;
  finally
    frlGuiaRLRetrato.Free;
  end;
end;

procedure TACBrGNReGuiaRL.ImprimirGuiaPDF(AGNRE: TGNRERetorno = nil);
var
  NomeArq: string;
  i: integer;
  frlGuiaRLRetrato: TfrlGuiaRLRetrato;
begin
  frlGuiaRLRetrato := TfrlGuiaRLRetrato.Create(Self);

  if AGNRE = nil then
  begin
    for i := 0 to TACBrGNRE(ACBrGNRE).GuiasRetorno.Count -1 do
    begin
      NomeArq:= OnlyNumber(TACBrGNRE(ACBrGNRE).GuiasRetorno.Items[i].GNRE.IdentificadorGuia);
      NomeArq:= PathWithDelim(Self.PathPDF) + NomeArq + '-guia.pdf';

      frlGuiaRLRetrato.SavePDF(Self,
          NomeArq,
          TACBrGNRE(ACBrGNRE).GuiasRetorno.Items[0].GNRE,
          Email,
          Fax,
          NumCopias,
          Sistema,
          Site,
          Usuario,
          MargemSuperior,
          MargemInferior,
          MargemEsquerda,
          MargemDireita);
    end;
  end
  else
  begin
    NomeArq:= OnlyNumber(AGNRE.IdentificadorGuia);
    NomeArq:= PathWithDelim(Self.PathPDF) + NomeArq + '-guia.pdf';

    frlGuiaRLRetrato.SavePDF(Self,
          NomeArq,
          AGNRE,
          Email,
          Fax,
          NumCopias,
          Sistema,
          Site,
          Usuario,
          MargemSuperior,
          MargemInferior,
          MargemEsquerda,
          MargemDireita);
  end;

  if frlGuiaRLRetrato.RLGNRe <> nil then
    frlGuiaRLRetrato.Free;
end;

end.
