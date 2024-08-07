{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrDCe.DACERLClass;

interface

{$H+}

uses
  SysUtils, 
  Classes, 
  ACBrBase,
  pcnConversao, 
  ACBrDCe.Classes,
  ACBrDCe.DACEClass,
  RLTypes;

type

  { TACBrDCeDACERL }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrDCeDACERL = class(TACBrDCeDACEClass)
  protected
     FPrintDialog: Boolean;
  public
    constructor Create(AOwner: TComponent); override;

    procedure ImprimirDACE(ADCe: TDCe = nil); override;
    procedure ImprimirDACEPDF(ADCe: TDCe = nil); override;
    procedure ImprimirDACEPDF(AStream: TStream; ADCe: TDCe = nil); override;

    procedure ImprimirEVENTO(ADCe: TDCe = nil); override;
    procedure ImprimirEVENTOPDF(ADCe: TDCe = nil); override;
    procedure ImprimirEVENTOPDF(AStream: TStream; ADCe: TDCe = nil); override;
  published
    property PrintDialog: Boolean read FPrintDialog write FPrintDialog;
  end;

implementation

uses
  StrUtils,   
  ACBrUtil.Base, 
  ACBrUtil.Strings, 
  ACBrUtil.FilesIO,
  ACBrDCe,
  ACBrDCe.EnvEvento,
  ACBrDCe.DACERL,
  ACBrDCe.DACERLRetrato,
  ACBrDCe.DAEventoRL,
  ACBrDCe.DAEventoRLRetrato;

constructor TACBrDCeDACERL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrintDialog := True;
end;

procedure TACBrDCeDACERL.ImprimirDACE(ADCe: TDCe = nil);
var
  i: integer;
  NotasFiscais: array of TDCe;
begin

  if ADCe = nil then
  begin
    SetLength(NotasFiscais, TACBrDCe(ACBrDCe).Declaracoes.Count);

    for i := 0 to TACBrDCe(ACBrDCe).Declaracoes.Count - 1 do
    begin
      NotasFiscais[i] := TACBrDCe(ACBrDCe).Declaracoes[i].DCe;
    end;
  end
  else
  begin
    SetLength(NotasFiscais, 1);
    NotasFiscais[0] := ADCe;
  end;

  TfrmDADCeRLRetrato.Imprimir(Self, NotasFiscais);
end;

procedure TACBrDCeDACERL.ImprimirDACEPDF(ADCe: TDCe = nil);
var
  i: integer;
  ArqPDF: String;

  function ImprimirDACEPDFTipo(ADCe: TDCe): String;
  begin
    Result := DefinirNomeArquivo(Self.PathPDF,
                                 OnlyNumber(ADCe.infDCe.ID) + '-DCe.pdf',
                                 Self.NomeDocumento);

    TfrmDADCeRLRetrato.SalvarPDF(Self, ADCe, Result);
  end;

begin
  FPArquivoPDF := '';

  if ADCe = nil then
  begin
    for i := 0 to TACBrDCe(ACBrDCe).Declaracoes.Count - 1 do
    begin
      ArqPDF := ImprimirDACEPDFTipo(TACBrDCe(ACBrDCe).Declaracoes[i].DCe);

      FPArquivoPDF := ArqPDF;

      TACBrDCe(ACBrDCe).Declaracoes[i].NomeArqPDF := FPArquivoPDF;
    end;
  end
  else
    FPArquivoPDF := ImprimirDACEPDFTipo(ADCe);
end;

procedure TACBrDCeDACERL.ImprimirDACEPDF(AStream: TStream; ADCe: TDCe = nil);
var
  i:integer;
  procedure StreamDANDCePDFTipo(ADCe: TDCe; const AStream: TStream);
  begin
    AStream.Size := 0;

    TfrmDADCeRLRetrato.SalvarPDF(Self, ADCe, AStream);
  end;
begin
  if not Assigned(AStream) then
    raise EACBrDCeException.Create('AStream precisa estar definido');

  if (ADCe = nil) then
  begin
    for i := 0 to (TACBrDCe(ACBrDCe).Declaracoes.Count - 1) do
      StreamDANDCePDFTipo(TACBrDCe(ACBrDCe).Declaracoes[i].DCe, AStream);
  end
  else
    StreamDANDCePDFTipo(ADCe, AStream);
end;

procedure TACBrDCeDACERL.ImprimirEVENTO(ADCe: TDCe);
var
  i, j: integer;
  Impresso: boolean;
begin
  if TACBrDCe(ACBrDCe).Declaracoes.Count > 0 then
  begin
    for i := 0 to (TACBrDCe(ACBrDCe).EventoDCe.Evento.Count - 1) do
    begin
      Impresso := False;
      for j := 0 to (TACBrDCe(ACBrDCe).Declaracoes.Count - 1) do
      begin
        if OnlyNumber(TACBrDCe(ACBrDCe).Declaracoes[j].DCe.infDCe.Id) = TACBrDCe(ACBrDCe).EventoDCe.Evento[i].InfEvento.chDCe then
        begin
          TfrmDCeDAEventoRLRetrato.Imprimir(Self, TACBrDCe(ACBrDCe).EventoDCe.Evento[i],
            TACBrDCe(ACBrDCe).Declaracoes[j].DCe);
          Impresso := True;
          Break;
        end;
      end;

      if not Impresso then
      begin
        TfrmDCeDAEventoRLRetrato.Imprimir(Self, TACBrDCe(ACBrDCe).EventoDCe.Evento[i]);
      end;
    end;
  end
  else
  begin
    for i := 0 to (TACBrDCe(ACBrDCe).EventoDCe.Evento.Count - 1) do
    begin
      TfrmDCeDAEventoRLRetrato.Imprimir(Self, TACBrDCe(ACBrDCe).EventoDCe.Evento[i], ADCe);
    end;
  end;
end;

procedure TACBrDCeDACERL.ImprimirEVENTOPDF(ADCe: TDCe);
var
  Impresso: Boolean;
  I, J: Integer;
  NumID, ArqPDF: String;

  function ImprimirEVENTOPDFTipo(EventoNFeItem: TInfEventoCollectionItem; ADCe: TDCe): String;
  begin
    Result := DefinirNomeArquivo(Self.PathPDF,
                                 OnlyNumber(EventoNFeItem.InfEvento.id) + '-procEventoDCe.pdf',
                                 Self.NomeDocumento);

    TfrmDCeDAEventoRLRetrato.SalvarPDF(Self, EventoNFeItem, Result, ADCe);
  end;

begin
  FPArquivoPDF := '';

  with TACBrDCe(ACBrDCe) do
  begin
    if (ADCe = nil) and (Declaracoes.Count > 0) then
    begin
      for i := 0 to (EventoDCe.Evento.Count - 1) do
      begin
        Impresso := False;
        ArqPDF := '';
        for j := 0 to (Declaracoes.Count - 1) do
        begin
          NumID := OnlyNumber(Declaracoes[j].DCe.infDCe.ID);
          if (NumID = OnlyNumber(EventoDCe.Evento[i].InfEvento.chDCe)) then
          begin
            ArqPDF := ImprimirEVENTOPDFTipo(EventoDCe.Evento[i], Declaracoes[j].DCe);
            Impresso := True;
            Break;
          end;
        end;

        if (not Impresso) then
          ArqPDF := ImprimirEVENTOPDFTipo(EventoDCe.Evento[i], nil);

        FPArquivoPDF := FPArquivoPDF + ArqPDF;
        if (i < (EventoDCe.Evento.Count - 1)) then
          FPArquivoPDF := FPArquivoPDF + sLinebreak;
      end;
    end
    else
    begin
      for i := 0 to (EventoDCe.Evento.Count - 1) do
      begin
        ArqPDF := ImprimirEVENTOPDFTipo(EventoDCe.Evento[i], ADCe);
        FPArquivoPDF := FPArquivoPDF + ArqPDF;
        if (i < (EventoDCe.Evento.Count - 1)) then
          FPArquivoPDF := FPArquivoPDF + sLinebreak;
      end;
    end;
  end;
end;

procedure TACBrDCeDACERL.ImprimirEVENTOPDF(AStream: TStream; ADCe: TDCe);
var
  Impresso: Boolean;
  I, J: Integer;
  NumID: String;
begin
  with TACBrDCe(ACBrDCe) do
  begin
    if (ADCe = nil) and (Declaracoes.Count > 0) then
    begin
      for i := 0 to (EventoDCe.Evento.Count - 1) do
      begin
        Impresso := False;
        for j := 0 to (Declaracoes.Count - 1) do
        begin
          NumID := OnlyNumber(Declaracoes[j].DCe.infDCe.ID);
          if (NumID = OnlyNumber(EventoDCe.Evento[i].InfEvento.chDCe)) then
          begin
            TfrmDCeDAEventoRLRetrato.SalvarPDF(Self, EventoDCe.Evento[i], AStream, Declaracoes[j].DCe);
            Impresso := True;
            Break;
          end;
        end;

        if not Impresso and (EventoDCe.Evento.Count > 0) then
          TfrmDCeDAEventoRLRetrato.SalvarPDF(Self, EventoDCe.Evento[0], AStream, nil);
      end;
    end
    else
    begin
      NumID := OnlyNumber(ADCe.infDCe.ID);
      Impresso := False;

      for i := 0 to (EventoDCe.Evento.Count - 1) do
      begin
        if (NumID = OnlyNumber(EventoDCe.Evento[i].InfEvento.chDCe)) then
        begin
          TfrmDCeDAEventoRLRetrato.SalvarPDF(Self, EventoDCe.Evento[i], AStream, ADCe);
          Impresso := True;
          Break;
        end;
      end;

      if not Impresso and (EventoDCe.Evento.Count > 0) then
        TfrmDCeDAEventoRLRetrato.SalvarPDF(Self, EventoDCe.Evento[0], AStream, nil);
    end;
  end;
end;

end.
