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
  pcnConversao, pcteCTe, ACBrCTeDACTEClass, RLTypes;

type

  { TACBrCTeDACTeRL }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or
  pidiOSSimulator or  pidAndroid or
  pidLinux32 or pidiOSDevice
  {$IFDEF RTL300_UP}
  or pidiOSDevice32 or pidLinux64
  or pidWinNX32 or pidWinIoT32
  or pidiOSDevice64
  or pidOSX64 or pidLinux32Arm
  or pidLinux64Arm or pidAndroid64Arm
  {$ENDIF RTL300_UP})]
  {$ENDIF RTL230_UP}
  TACBrCTeDACTeRL = class(TACBrCTeDACTeClass)
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
  ACBrCTeDAInutRL, ACBrCTeDAInutRLRetrato,
  ACBrCTeDACTeRL, ACBrCTeDACTeRLRetrato, ACBrCTeDACTeRLRetratoA5,
  ACBrCTeDAEventoRL, ACBrCTeDAEventoRLRetrato;

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
  Conhecimentos: array of TCTe;
begin

  if CTe = nil then
  begin
    SetLength(Conhecimentos, TACBrCTe(ACBrCTe).Conhecimentos.Count);

    for i := 0 to TACBrCTe(ACBrCTe).Conhecimentos.Count - 1 do
    begin
      Conhecimentos[i] := TACBrCTe(ACBrCTe).Conhecimentos.Items[i].CTe;
    end;
  end
  else
  begin
    SetLength(Conhecimentos, 1);
    Conhecimentos[0] := CTe;
  end;

  case TamanhoPapel of
    tpA5: TfrmDACTeRLRetratoA5.Imprimir(Self, Conhecimentos);
    else TfrmDACTeRLRetrato.Imprimir(Self, Conhecimentos);
  end;
end;

procedure TACBrCTeDACTeRL.ImprimirDACTePDF(CTe: TCTe = nil);
var
  i: integer;
begin

  FPArquivoPDF := '';
  if CTe = nil then
  begin
    for i := 0 to TACBrCTe(ACBrCTe).Conhecimentos.Count - 1 do
    begin
      FPArquivoPDF := PathWithDelim(TACBrCTe(ACBrCTe).DACTE.PathPDF) +
          OnlyNumber(TACBrCTe(ACBrCTe).Conhecimentos.Items[i].CTe.infCTe.ID) + '-cte.pdf';

      TACBrCTe(ACBrCTE).Conhecimentos.Items[i].NomeArqPDF := FPArquivoPDF;
//      if i < TACBrCTe(ACBrCTe).Conhecimentos.Count - 1 then
//        FPArquivoPDF := FPArquivoPDF + sLinebreak;

      case TamanhoPapel of
        tpA5: TfrmDACTeRLRetratoA5.SalvarPDF(Self, TACBrCTe(ACBrCTe).Conhecimentos.Items[i].CTe, FPArquivoPDF);
        else TfrmDACTeRLRetrato.SalvarPDF(Self, TACBrCTe(ACBrCTe).Conhecimentos.Items[i].CTe, FPArquivoPDF);
      end;
    end;
  end
  else
  begin
    FPArquivoPDF := PathWithDelim(TACBrCTe(ACBrCTe).DACTE.PathPDF) +
                    OnlyNumber(CTe.infCTe.ID) + '-cte.pdf';

    case TamanhoPapel of
        tpA5: TfrmDACTeRLRetratoA5.SalvarPDF(Self, CTe, FPArquivoPDF);
        else TfrmDACTeRLRetrato.SalvarPDF(Self, CTe, FPArquivoPDF);
    end;
  end;
end;

procedure TACBrCTeDACTeRL.ImprimirEVENTO(CTe: TCTe);
var
  i, j: integer;
  Impresso: boolean;
begin
  if TACBrCTe(ACBrCTe).Conhecimentos.Count > 0 then
  begin
    for i := 0 to (TACBrCTe(ACBrCTe).EventoCTe.Evento.Count - 1) do
    begin
      Impresso := False;
      for j := 0 to (TACBrCTe(ACBrCTe).Conhecimentos.Count - 1) do
      begin
        if OnlyNumber(TACBrCTe(ACBrCTe).Conhecimentos.Items[j].CTe.infCTe.Id) = TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.chCTe then
        begin
          TfrmCTeDAEventoRLRetrato.Imprimir(Self, TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i],
            TACBrCTe(ACBrCTe).Conhecimentos.Items[j].CTe);
          Impresso := True;
          Break;
        end;
      end;

      if Impresso = False then
      begin
        TfrmCTeDAEventoRLRetrato.Imprimir(Self, TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i]);
      end;
    end;
  end
  else
  begin
    for i := 0 to (TACBrCTe(ACBrCTe).EventoCTe.Evento.Count - 1) do
    begin
      TfrmCTeDAEventoRLRetrato.Imprimir(Self, TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i], CTe);
    end;
  end;
end;

procedure TACBrCTeDACTeRL.ImprimirEVENTOPDF(CTe: TCTe);
var
  i, j: integer;
  Impresso: boolean;
begin
  if TACBrCTe(ACBrCTe).Conhecimentos.Count > 0 then
  begin
    for i := 0 to (TACBrCTe(ACBrCTe).EventoCTe.Evento.Count - 1) do
    begin
      FPArquivoPDF := TACBrCTe(ACBrCTe).DACTE.PathPDF +
               OnlyNumber(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.Id) + 
	       '-procEventoCTe.pdf';

      Impresso := False;
      for j := 0 to (TACBrCTe(ACBrCTe).Conhecimentos.Count - 1) do
      begin
        if OnlyNumber(TACBrCTe(ACBrCTe).Conhecimentos.Items[j].CTe.infCTe.Id) = TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.chCTe then
        begin
          TfrmCTeDAEventoRLRetrato.SalvarPDF(Self, TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i],
            FPArquivoPDF, TACBrCTe(ACBrCTe).Conhecimentos.Items[j].CTe);
          Impresso := True;
          Break;
        end;
      end;

      if Impresso = False then
      begin
        TfrmCTeDAEventoRLRetrato.SalvarPDF(Self, TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i], FPArquivoPDF);
      end;
    end;
  end
  else
  begin
    for i := 0 to (TACBrCTe(ACBrCTe).EventoCTe.Evento.Count - 1) do
    begin
      FPArquivoPDF := TACBrCTe(ACBrCTe).DACTE.PathPDF +
               OnlyNumber(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.Id) +
               '-procEventoCTe.pdf';
      TfrmCTeDAEventoRLRetrato.SalvarPDF(Self, TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i], FPArquivoPDF, CTe);
    end;
  end;
end;

procedure TACBrCTeDACTeRL.ImprimirINUTILIZACAO(CTe: TCTe);
begin
  TfrmCTeDAInutRLRetrato.Imprimir(Self, TACBrCTe(ACBrCTe).InutCTe, CTe);
end;

procedure TACBrCTeDACTeRL.ImprimirINUTILIZACAOPDF(CTe: TCTe);
begin
  FPArquivoPDF := StringReplace(TACBrCTe(ACBrCTe).InutCTe.ID, 'ID', '', [rfIgnoreCase]);
  FPArquivoPDF := PathWithDelim(Self.PathPDF) + FPArquivoPDF + '-procInutCTe.pdf';
  TfrmCTeDAInutRLRetrato.SalvarPDF(Self, TACBrCTe(ACBrCTe).InutCTe, FPArquivoPDF, CTe);
end;

end.
