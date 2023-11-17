{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                              Mark dos Santos Gonçalves                       }
{                              Juliomar Marchetti                              }
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

unit ACBrCTeDACTeRLClass;

interface

{$H+}

uses
  SysUtils, 
  Classes, 
  ACBrBase,
  pcnConversao, 
  pcteCTe, 
  ACBrCTeDACTEClass, 
  RLTypes;

type

  { TACBrCTeDACTeRL }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrCTeDACTeRL = class(TACBrCTeDACTeClass)
  protected
     FPrintDialog: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ImprimirDACTe(ACTe: TCTe = nil); override;
    procedure ImprimirDACTePDF(ACTe: TCTe = nil); override;
    procedure ImprimirDACTePDF(AStream: TStream; ACTe: TCTe = nil); override;

    procedure ImprimirEVENTO(ACTe: TCTe = nil); override;
    procedure ImprimirEVENTOPDF(ACTe: TCTe = nil); override;
    procedure ImprimirEVENTOPDF(AStream: TStream; ACTe: TCTe = nil); override;

    procedure ImprimirINUTILIZACAO(ACTe: TCTe = nil); override;
    procedure ImprimirINUTILIZACAOPDF(ACTe: TCTe = nil); override;
  published
    property PrintDialog: Boolean read FPrintDialog write FPrintDialog;
  end;

implementation

uses
  StrUtils,   
  ACBrUtil.Base, 
  ACBrUtil.Strings, 
  ACBrUtil.FilesIO,
  ACBrCTe, 
  pcteEnvEventoCTe,
  ACBrCTeDAInutRL, 
  ACBrCTeDAInutRLRetrato,
  ACBrCTeDACTeRL, 
  ACBrCTeDACTeRLRetrato, 
  ACBrCTeDACTeRLRetratoA5,
  ACBrCTeDAEventoRL, 
  ACBrCTeDAEventoRLRetrato;

constructor TACBrCTeDACTeRL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrintDialog := True;
end;

procedure TACBrCTeDACTeRL.ImprimirDACTe(ACTe: TCTe = nil);
var
  i: integer;
  Conhecimentos: array of TCTe;
begin

  if ACTe = nil then
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
    Conhecimentos[0] := ACTe;
  end;

  case TamanhoPapel of
    tpA5: TfrmDACTeRLRetratoA5.Imprimir(Self, Conhecimentos);
    else TfrmDACTeRLRetrato.Imprimir(Self, Conhecimentos);
  end;
end;

procedure TACBrCTeDACTeRL.ImprimirDACTePDF(ACTe: TCTe = nil);
var
  i: integer;
  ArqPDF: String;

  function ImprimirDACTEPDFTipo(ACTe: TCTe): String;
  begin
    Result := DefinirNomeArquivo(Self.PathPDF,
                                 OnlyNumber(ACTe.infCTe.ID) + '-cte.pdf',
                                 Self.NomeDocumento);

    case TamanhoPapel of
      tpA5:
        TfrmDACTeRLRetratoA5.SalvarPDF(Self, ACTe, Result);
    else
      TfrmDACTeRLRetrato.SalvarPDF(Self, ACTe, Result);
    end;
  end;

begin
  FPArquivoPDF := '';

  if ACTe = nil then
  begin
    for i := 0 to TACBrCTe(ACBrCTe).Conhecimentos.Count - 1 do
    begin
      ArqPDF := ImprimirDACTEPDFTipo(TACBrCTe(ACBrCTe).Conhecimentos.Items[i].CTe);

      FPArquivoPDF := ArqPDF;

      TACBrCTe(ACBrCTE).Conhecimentos.Items[i].NomeArqPDF := FPArquivoPDF;
//      if i < TACBrCTe(ACBrCTe).Conhecimentos.Count - 1 then
//        FPArquivoPDF := FPArquivoPDF + sLinebreak;
    end;
  end
  else
    FPArquivoPDF := ImprimirDACTEPDFTipo(ACTe);
end;

procedure TACBrCTeDACTeRL.ImprimirDACTePDF(AStream: TStream; ACTe: TCTe = nil);
var
  i:integer;
  procedure StreamDANCTEPDFTipo(ACTe: TCTe; const AStream: TStream);
  begin
    AStream.Size := 0;
    //case Self.TipoDACTE of
      //tiPaisagem:
        //TfrlDANFeRLPaisagem.SalvarPDF(Self, ACTe, AStream);
    //else
      TfrmDACTeRLRetrato.SalvarPDF(Self, ACTe, AStream);
    //end;
  end;
begin
  if not Assigned(AStream) then
    raise EACBrCTeException.Create('AStream precisa estar definido');

  if (ACTe = nil) then
  begin
    for i := 0 to (TACBrCTe(ACBrCTE).Conhecimentos.Count - 1) do
      StreamDANCTEPDFTipo(TACBrCTe(ACBrCTE).Conhecimentos.Items[i].CTe, AStream);
  end
  else
    StreamDANCTEPDFTipo(ACTe, AStream);
end;

procedure TACBrCTeDACTeRL.ImprimirEVENTO(ACTe: TCTe);
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
      TfrmCTeDAEventoRLRetrato.Imprimir(Self, TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i], ACTe);
    end;
  end;
end;

procedure TACBrCTeDACTeRL.ImprimirEVENTOPDF(ACTe: TCTe);
var
  Impresso: Boolean;
  I, J: Integer;
  NumID, ArqPDF: String;

  function ImprimirEVENTOPDFTipo(EventoNFeItem: TInfEventoCollectionItem; ACTe: TCTe): String;
  begin
    Result := DefinirNomeArquivo(Self.PathPDF,
                                 OnlyNumber(EventoNFeItem.InfEvento.id) + '-procEventoCTe.pdf',
                                 Self.NomeDocumento);

    // TipoDANFE ainda não está sendo utilizado no momento
    TfrmCTeDAEventoRLRetrato.SalvarPDF(Self, EventoNFeItem, Result, ACTe);
  end;

begin
  FPArquivoPDF := '';

  with TACBrCTe(ACBrCTe) do
  begin
    if (ACTe = nil) and (Conhecimentos.Count > 0) then
    begin
      for i := 0 to (EventoCTe.Evento.Count - 1) do
      begin
        Impresso := False;
        ArqPDF := '';
        for j := 0 to (Conhecimentos.Count - 1) do
        begin
          NumID := OnlyNumber(Conhecimentos.Items[j].CTe.infCTe.ID);
          if (NumID = OnlyNumber(EventoCTe.Evento.Items[i].InfEvento.chCTe)) then
          begin
            ArqPDF := ImprimirEVENTOPDFTipo(EventoCTe.Evento.Items[i], Conhecimentos.Items[j].CTe);
            Impresso := True;
            Break;
          end;
        end;

        if (not Impresso) then
          ArqPDF := ImprimirEVENTOPDFTipo(EventoCTe.Evento.Items[i], nil);

        FPArquivoPDF := FPArquivoPDF + ArqPDF;
        if (i < (EventoCTe.Evento.Count - 1)) then
          FPArquivoPDF := FPArquivoPDF + sLinebreak;
      end;
    end
    else
    begin
      for i := 0 to (EventoCTe.Evento.Count - 1) do
      begin
        ArqPDF := ImprimirEVENTOPDFTipo(EventoCTe.Evento.Items[i], ACTe);
        FPArquivoPDF := FPArquivoPDF + ArqPDF;
        if (i < (EventoCTe.Evento.Count - 1)) then
          FPArquivoPDF := FPArquivoPDF + sLinebreak;
      end;
    end;
  end;
end;

procedure TACBrCTeDACTeRL.ImprimirEVENTOPDF(AStream: TStream; ACTe: TCTe);
var
  Impresso: Boolean;
  I, J: Integer;
  NumID: String;
begin
  with TACBrCTe(ACBrCTe) do
  begin
    if (ACTe = nil) and (Conhecimentos.Count > 0) then
    begin
      for i := 0 to (EventoCTe.Evento.Count - 1) do
      begin
        Impresso := False;
        for j := 0 to (Conhecimentos.Count - 1) do
        begin
          NumID := OnlyNumber(Conhecimentos.Items[j].CTe.infCTe.ID);
          if (NumID = OnlyNumber(EventoCTe.Evento.Items[i].InfEvento.chCTe)) then
          begin
            TfrmCTeDAEventoRLRetrato.SalvarPDF(Self, EventoCTe.Evento.Items[i], AStream, Conhecimentos.Items[j].CTe);
            Impresso := True;
            Break;
          end;
        end;

        if not Impresso and (EventoCTe.Evento.Count > 0) then
          TfrmCTeDAEventoRLRetrato.SalvarPDF(Self, EventoCTe.Evento.Items[0], AStream, nil);
      end;
    end
    else
    begin
      NumID := OnlyNumber(ACTe.infCTe.ID);
      Impresso := False;

      for i := 0 to (EventoCTe.Evento.Count - 1) do
      begin
        if (NumID = OnlyNumber(EventoCTe.Evento.Items[i].InfEvento.chCTe)) then
        begin
          TfrmCTeDAEventoRLRetrato.SalvarPDF(Self, EventoCTe.Evento.Items[i], AStream, ACTe);
          Impresso := True;
          Break;
        end;
      end;

      if not Impresso and (EventoCTe.Evento.Count > 0) then
        TfrmCTeDAEventoRLRetrato.SalvarPDF(Self, EventoCTe.Evento.Items[0], AStream, nil);
    end;
  end;
end;

procedure TACBrCTeDACTeRL.ImprimirINUTILIZACAO(ACTe: TCTe);
begin
  TfrmCTeDAInutRLRetrato.Imprimir(Self, TACBrCTe(ACBrCTe).InutCTe, ACTe);
end;

procedure TACBrCTeDACTeRL.ImprimirINUTILIZACAOPDF(ACTe: TCTe);
begin
  FPArquivoPDF := DefinirNomeArquivo(Self.PathPDF,
                                     OnlyNumber(TACBrCTe(ACBrCTe).InutCTe.ID) + '-procInutCTe.pdf',
                                     Self.NomeDocumento);

  TfrmCTeDAInutRLRetrato.SalvarPDF(Self, TACBrCTe(ACBrCTe).InutCTe, FPArquivoPDF, ACTe);
end;

end.
