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

unit ACBrNF3e.DANF3ERLClass;

interface

{$H+}

uses
  SysUtils, 
  Classes, 
  ACBrBase,
  pcnConversao, 
  ACBrNF3eClass,
  ACBrNF3eDANF3eClass,
  RLTypes;

type

  { TACBrNF3eDANF3eRL }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrNF3eDANF3eRL = class(TACBrNF3eDANF3eClass)
  protected
     FPrintDialog: Boolean;
  public
    constructor Create(AOwner: TComponent); override;

    procedure ImprimirDANF3e(ANF3e: TNF3e = nil); override;
    procedure ImprimirDANF3ePDF(ANF3e: TNF3e = nil); override;
//    procedure ImprimirDANF3ePDF(AStream: TStream; ANF3e: TNF3e = nil); override;

    procedure ImprimirEVENTO(ANF3e: TNF3e = nil); override;
    procedure ImprimirEVENTOPDF(ANF3e: TNF3e = nil); override;
//    procedure ImprimirEVENTOPDF(AStream: TStream; ANF3e: TNF3e = nil); override;
  published
    property PrintDialog: Boolean read FPrintDialog write FPrintDialog;
  end;

implementation

uses
  StrUtils,   
  ACBrUtil.Base, 
  ACBrUtil.Strings, 
  ACBrUtil.FilesIO,
  ACBrNF3e,
  ACBrNF3eEnvEvento,
  ACBrNF3e.DANF3ERL,
  ACBrNF3e.DANF3ERLRetrato,
  ACBrNF3e.DAEventoRL,
  ACBrNF3e.DAEventoRLRetrato;

constructor TACBrNF3eDANF3eRL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrintDialog := True;
end;

procedure TACBrNF3eDANF3eRL.ImprimirDANF3e(ANF3e: TNF3e = nil);
var
  i: integer;
  NotasFiscais: array of TNF3e;
begin

  if ANF3e = nil then
  begin
    SetLength(NotasFiscais, TACBrNF3e(ACBrNF3e).NotasFiscais.Count);

    for i := 0 to TACBrNF3e(ACBrNF3e).NotasFiscais.Count - 1 do
    begin
      NotasFiscais[i] := TACBrNF3e(ACBrNF3e).NotasFiscais.Items[i].NF3e;
    end;
  end
  else
  begin
    SetLength(NotasFiscais, 1);
    NotasFiscais[0] := ANF3e;
  end;

  TfrmDANF3eRLRetrato.Imprimir(Self, NotasFiscais);
end;

procedure TACBrNF3eDANF3eRL.ImprimirDANF3ePDF(ANF3e: TNF3e = nil);
var
  i: integer;
  ArqPDF: String;

  function ImprimirDANF3ePDFTipo(ANF3e: TNF3e): String;
  begin
    Result := DefinirNomeArquivo(Self.PathPDF,
                                 OnlyNumber(ANF3e.infNF3e.ID) + '-NF3e.pdf',
                                 Self.NomeDocumento);

    TfrmDANF3eRLRetrato.SalvarPDF(Self, ANF3e, Result);
  end;

begin
  FPArquivoPDF := '';

  if ANF3e = nil then
  begin
    for i := 0 to TACBrNF3e(ACBrNF3e).NotasFiscais.Count - 1 do
    begin
      ArqPDF := ImprimirDANF3ePDFTipo(TACBrNF3e(ACBrNF3e).NotasFiscais[i].NF3e);

      FPArquivoPDF := ArqPDF;

//      TACBrNF3e(ACBrNF3e).NotasFiscais[i].NomeArqPDF := FPArquivoPDF;
    end;
  end
  else
    FPArquivoPDF := ImprimirDANF3ePDFTipo(ANF3e);
end;
{
procedure TACBrNF3eDANF3eRL.ImprimirDANF3ePDF(AStream: TStream; ANF3e: TNF3e = nil);
var
  i:integer;
  procedure StreamDANNF3ePDFTipo(ANF3e: TNF3e; const AStream: TStream);
  begin
    AStream.Size := 0;

    TfrmDANF3eRLRetrato.SalvarPDF(Self, ANF3e, AStream);
  end;
begin
  if not Assigned(AStream) then
    raise EACBrNF3eException.Create('AStream precisa estar definido');

  if (ANF3e = nil) then
  begin
    for i := 0 to (TACBrNF3e(ACBrNF3e).NotasFiscais.Count - 1) do
      StreamDANNF3ePDFTipo(TACBrNF3e(ACBrNF3e).NotasFiscais.Items[i].NF3e, AStream);
  end
  else
    StreamDANNF3ePDFTipo(ANF3e, AStream);
end;
}
procedure TACBrNF3eDANF3eRL.ImprimirEVENTO(ANF3e: TNF3e);
var
  i, j: integer;
  Impresso: boolean;
begin
  if TACBrNF3e(ACBrNF3e).NotasFiscais.Count > 0 then
  begin
    for i := 0 to (TACBrNF3e(ACBrNF3e).EventoNF3e.Evento.Count - 1) do
    begin
      Impresso := False;
      for j := 0 to (TACBrNF3e(ACBrNF3e).NotasFiscais.Count - 1) do
      begin
        if OnlyNumber(TACBrNF3e(ACBrNF3e).NotasFiscais.Items[j].NF3e.infNF3e.Id) = TACBrNF3e(ACBrNF3e).EventoNF3e.Evento.Items[i].InfEvento.chNF3e then
        begin
          TfrmNF3eDAEventoRLRetrato.Imprimir(Self, TACBrNF3e(ACBrNF3e).EventoNF3e.Evento.Items[i],
            TACBrNF3e(ACBrNF3e).NotasFiscais.Items[j].NF3e);

          Impresso := True;
          Break;
        end;
      end;

      if not Impresso then
      begin
        TfrmNF3eDAEventoRLRetrato.Imprimir(Self, TACBrNF3e(ACBrNF3e).EventoNF3e.Evento.Items[i]);
      end;
    end;
  end
  else
  begin
    for i := 0 to (TACBrNF3e(ACBrNF3e).EventoNF3e.Evento.Count - 1) do
    begin
//      TfrmNF3eDAEventoRLRetrato.Imprimir(Self, TACBrNF3e(ACBrNF3e).EventoNF3e.Evento.Items[i], ANF3e);
    end;
  end;
end;

procedure TACBrNF3eDANF3eRL.ImprimirEVENTOPDF(ANF3e: TNF3e);
var
  Impresso: Boolean;
  I, J: Integer;
  NumID, ArqPDF: String;

  function ImprimirEVENTOPDFTipo(EventoNFeItem: TInfEventoCollectionItem; ANF3e: TNF3e): String;
  begin
    Result := DefinirNomeArquivo(Self.PathPDF,
                                 OnlyNumber(EventoNFeItem.InfEvento.id) + '-procEventoNF3e.pdf',
                                 Self.NomeDocumento);

    TfrmNF3eDAEventoRLRetrato.SalvarPDF(Self, EventoNFeItem, Result, ANF3e);
  end;

begin
  FPArquivoPDF := '';

  with TACBrNF3e(ACBrNF3e) do
  begin
    if (ANF3e = nil) and (NotasFiscais.Count > 0) then
    begin
      for i := 0 to (EventoNF3e.Evento.Count - 1) do
      begin
        Impresso := False;
        ArqPDF := '';
        for j := 0 to (NotasFiscais.Count - 1) do
        begin
          NumID := OnlyNumber(NotasFiscais.Items[j].NF3e.infNF3e.ID);
          if (NumID = OnlyNumber(EventoNF3e.Evento.Items[i].InfEvento.chNF3e)) then
          begin
            ArqPDF := ImprimirEVENTOPDFTipo(EventoNF3e.Evento.Items[i], NotasFiscais.Items[j].NF3e);
            Impresso := True;
            Break;
          end;
        end;

        if (not Impresso) then
          ArqPDF := ImprimirEVENTOPDFTipo(EventoNF3e.Evento.Items[i], nil);

        FPArquivoPDF := FPArquivoPDF + ArqPDF;
        if (i < (EventoNF3e.Evento.Count - 1)) then
          FPArquivoPDF := FPArquivoPDF + sLinebreak;
      end;
    end
    else
    begin
      for i := 0 to (EventoNF3e.Evento.Count - 1) do
      begin
        ArqPDF := ImprimirEVENTOPDFTipo(EventoNF3e.Evento.Items[i], ANF3e);
        FPArquivoPDF := FPArquivoPDF + ArqPDF;
        if (i < (EventoNF3e.Evento.Count - 1)) then
          FPArquivoPDF := FPArquivoPDF + sLinebreak;
      end;
    end;
  end;
end;
{
procedure TACBrNF3eDANF3eRL.ImprimirEVENTOPDF(AStream: TStream; ANF3e: TNF3e);
var
  Impresso: Boolean;
  I, J: Integer;
  NumID: String;
begin
  with TACBrNF3e(ACBrNF3e) do
  begin
    if (ANF3e = nil) and (NotasFiscais.Count > 0) then
    begin
      for i := 0 to (EventoNF3e.Evento.Count - 1) do
      begin
        Impresso := False;
        for j := 0 to (NotasFiscais.Count - 1) do
        begin
          NumID := OnlyNumber(NotasFiscais.Items[j].NF3e.infNF3e.ID);
          if (NumID = OnlyNumber(EventoNF3e.Evento.Items[i].InfEvento.chNF3e)) then
          begin
            TfrmNF3eDAEventoRLRetrato.SalvarPDF(Self, EventoNF3e.Evento.Items[i], AStream, NotasFiscais.Items[j].NF3e);
            Impresso := True;
            Break;
          end;
        end;

        if not Impresso and (EventoNF3e.Evento.Count > 0) then
          TfrmNF3eDAEventoRLRetrato.SalvarPDF(Self, EventoNF3e.Evento.Items[0], AStream, nil);
      end;
    end
    else
    begin
      NumID := OnlyNumber(ANF3e.infNF3e.ID);
      Impresso := False;

      for i := 0 to (EventoNF3e.Evento.Count - 1) do
      begin
        if (NumID = OnlyNumber(EventoNF3e.Evento.Items[i].InfEvento.chNF3e)) then
        begin
          TfrmNF3eDAEventoRLRetrato.SalvarPDF(Self, EventoNF3e.Evento.Items[i], AStream, ANF3e);
          Impresso := True;
          Break;
        end;
      end;

      if not Impresso and (EventoNF3e.Evento.Count > 0) then
        TfrmNF3eDAEventoRLRetrato.SalvarPDF(Self, EventoNF3e.Evento.Items[0], AStream, nil);
    end;
  end;
end;
}
end.
