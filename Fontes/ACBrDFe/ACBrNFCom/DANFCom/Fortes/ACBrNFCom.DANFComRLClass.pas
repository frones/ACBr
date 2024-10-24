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

unit ACBrNFCom.DANFComRLClass;

interface

{$H+}

uses
  SysUtils, 
  Classes, 
  ACBrBase,
  pcnConversao, 
  ACBrNFComClass,
  ACBrNFComDANFComClass,
  RLTypes;

type

  { TACBrNFComDANFComRL }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrNFComDANFComRL = class(TACBrNFComDANFComClass)
  protected
     FPrintDialog: Boolean;
  public
    constructor Create(AOwner: TComponent); override;

    procedure ImprimirDANFCom(ANFCom: TNFCom = nil); override;
    procedure ImprimirDANFComPDF(ANFCom: TNFCom = nil); override;
    procedure ImprimirDANFComPDF(AStream: TStream; ANFCom: TNFCom = nil); override;

    procedure ImprimirEVENTO(ANFCom: TNFCom = nil); override;
    procedure ImprimirEVENTOPDF(ANFCom: TNFCom = nil); override;
    procedure ImprimirEVENTOPDF(AStream: TStream; ANFCom: TNFCom = nil); override;
  published
    property PrintDialog: Boolean read FPrintDialog write FPrintDialog;
  end;

implementation

uses
  StrUtils,   
  ACBrUtil.Base, 
  ACBrUtil.Strings, 
  ACBrUtil.FilesIO,
  ACBrNFCom,
  ACBrNFComEnvEvento,
  ACBrNFCom.DANFComRL,
  ACBrNFCom.DANFComRLRetrato,
  ACBrNFCom.DAEventoRL,
  ACBrNFCom.DAEventoRLRetrato;

constructor TACBrNFComDANFComRL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPrintDialog := True;
end;

procedure TACBrNFComDANFComRL.ImprimirDANFCom(ANFCom: TNFCom = nil);
var
  i: integer;
  NotasFiscais: array of TNFCom;
begin
  if ANFCom = nil then
  begin
    SetLength(NotasFiscais, TACBrNFCom(ACBrNFCom).NotasFiscais.Count);

    for i := 0 to TACBrNFCom(ACBrNFCom).NotasFiscais.Count - 1 do
    begin
      NotasFiscais[i] := TACBrNFCom(ACBrNFCom).NotasFiscais.Items[i].NFCom;
    end;
  end
  else
  begin
    SetLength(NotasFiscais, 1);
    NotasFiscais[0] := ANFCom;
  end;

  TfrlDANFComRLRetrato.Imprimir(Self, NotasFiscais);
end;

procedure TACBrNFComDANFComRL.ImprimirDANFComPDF(ANFCom: TNFCom = nil);
var
  i: integer;
  ArqPDF: String;

  function ImprimirDANFComPDFTipo(ANFCom: TNFCom): String;
  begin
    Result := DefinirNomeArquivo(Self.PathPDF,
                                 OnlyNumber(ANFCom.infNFCom.ID) + '-NFCom.pdf',
                                 Self.NomeDocumento);

    TfrlDANFComRLRetrato.SalvarPDF(Self, ANFCom, Result);
  end;

begin
  FPArquivoPDF := '';

  if ANFCom = nil then
  begin
    for i := 0 to TACBrNFCom(ACBrNFCom).NotasFiscais.Count - 1 do
    begin
      ArqPDF := ImprimirDANFComPDFTipo(TACBrNFCom(ACBrNFCom).NotasFiscais.Items[i].NFCom);

      FPArquivoPDF := ArqPDF;

      TACBrNFCom(ACBrNFCom).NotasFiscais.Items[i].NomeArqPDF := FPArquivoPDF;
    end;
  end
  else
    FPArquivoPDF := ImprimirDANFComPDFTipo(ANFCom);
end;

procedure TACBrNFComDANFComRL.ImprimirDANFComPDF(AStream: TStream; ANFCom: TNFCom = nil);
var
  i:integer;

  procedure StreamDANNFComPDFTipo(ANFCom: TNFCom; const AStream: TStream);
  begin
    AStream.Size := 0;
    TfrlDANFComRLRetrato.SalvarPDF(Self, ANFCom, AStream);
  end;
begin
  if not Assigned(AStream) then
    raise EACBrNFComException.Create('AStream precisa estar definido');

  if (ANFCom = nil) then
  begin
    for i := 0 to (TACBrNFCom(ACBrNFCom).NotasFiscais.Count - 1) do
      StreamDANNFComPDFTipo(TACBrNFCom(ACBrNFCom).NotasFiscais.Items[i].NFCom, AStream);
  end
  else
    StreamDANNFComPDFTipo(ANFCom, AStream);
end;

procedure TACBrNFComDANFComRL.ImprimirEVENTO(ANFCom: TNFCom = nil);
var
  i, j: integer;
  Impresso: boolean;
begin
  if TACBrNFCom(ACBrNFCom).NotasFiscais.Count > 0 then
  begin
    for i := 0 to (TACBrNFCom(ACBrNFCom).EventoNFCom.Evento.Count - 1) do
    begin
      Impresso := False;

      for j := 0 to (TACBrNFCom(ACBrNFCom).NotasFiscais.Count - 1) do
      begin
        if OnlyNumber(TACBrNFCom(ACBrNFCom).NotasFiscais.Items[j].NFCom.infNFCom.Id) = TACBrNFCom(ACBrNFCom).EventoNFCom.Evento.Items[i].InfEvento.chNFCom then
        begin
          TfrmNFComDAEventoRLRetrato.Imprimir(Self, TACBrNFCom(ACBrNFCom).EventoNFCom.Evento.Items[i],
            TACBrNFCom(ACBrNFCom).NotasFiscais.Items[j].NFCom);
          Impresso := True;
          Break;
        end;
      end;

      if Impresso then
      begin
        TfrmNFComDAEventoRLRetrato.Imprimir(Self, TACBrNFCom(ACBrNFCom).EventoNFCom.Evento.Items[i]);
      end;
    end;
  end
  else
  begin
    for i := 0 to (TACBrNFCom(ACBrNFCom).EventoNFCom.Evento.Count - 1) do
    begin
      TfrmNFComDAEventoRLRetrato.Imprimir(Self, TACBrNFCom(ACBrNFCom).EventoNFCom.Evento.Items[i], ANFCom);
    end;
  end;
end;

procedure TACBrNFComDANFComRL.ImprimirEVENTOPDF(ANFCom: TNFCom = nil);
var
  Impresso: Boolean;
  I, J: Integer;
  NumID, ArqPDF: String;

  function ImprimirEVENTOPDFTipo(EventoNFeItem: TInfEventoCollectionItem; ANFCom: TNFCom): String;
  begin
    Result := DefinirNomeArquivo(Self.PathPDF,
                                 OnlyNumber(EventoNFeItem.InfEvento.id) + '-procEventoNFCom.pdf',
                                 Self.NomeDocumento);

    TfrmNFComDAEventoRLRetrato.SalvarPDF(Self, EventoNFeItem, Result, ANFCom);
  end;

begin
  FPArquivoPDF := '';

  with TACBrNFCom(ACBrNFCom) do
  begin
    if (ANFCom = nil) and (NotasFiscais.Count > 0) then
    begin
      for i := 0 to (EventoNFCom.Evento.Count - 1) do
      begin
        Impresso := False;
        ArqPDF := '';

        for j := 0 to (NotasFiscais.Count - 1) do
        begin
          NumID := OnlyNumber(NotasFiscais.Items[j].NFCom.infNFCom.ID);
          if (NumID = OnlyNumber(EventoNFCom.Evento.Items[i].InfEvento.chNFCom)) then
          begin
            ArqPDF := ImprimirEVENTOPDFTipo(EventoNFCom.Evento.Items[i], NotasFiscais.Items[j].NFCom);
            Impresso := True;
            Break;
          end;
        end;

        if (not Impresso) then
          ArqPDF := ImprimirEVENTOPDFTipo(EventoNFCom.Evento.Items[i], nil);

        FPArquivoPDF := FPArquivoPDF + ArqPDF;

        if (i < (EventoNFCom.Evento.Count - 1)) then
          FPArquivoPDF := FPArquivoPDF + sLinebreak;
      end;
    end
    else
    begin
      for i := 0 to (EventoNFCom.Evento.Count - 1) do
      begin
        ArqPDF := ImprimirEVENTOPDFTipo(EventoNFCom.Evento.Items[i], ANFCom);
        FPArquivoPDF := FPArquivoPDF + ArqPDF;

        if (i < (EventoNFCom.Evento.Count - 1)) then
          FPArquivoPDF := FPArquivoPDF + sLinebreak;
      end;
    end;
  end;
end;

procedure TACBrNFComDANFComRL.ImprimirEVENTOPDF(AStream: TStream; ANFCom: TNFCom = nil);
var
  Impresso: Boolean;
  I, J: Integer;
  NumID: String;
begin
  with TACBrNFCom(ACBrNFCom) do
  begin
    if (ANFCom = nil) and (NotasFiscais.Count > 0) then
    begin
      for i := 0 to (EventoNFCom.Evento.Count - 1) do
      begin
        Impresso := False;

        for j := 0 to (NotasFiscais.Count - 1) do
        begin
          NumID := OnlyNumber(NotasFiscais.Items[j].NFCom.infNFCom.ID);

          if (NumID = OnlyNumber(EventoNFCom.Evento.Items[i].InfEvento.chNFCom)) then
          begin
            TfrmNFComDAEventoRLRetrato.SalvarPDF(Self, EventoNFCom.Evento.Items[i], AStream, NotasFiscais.Items[j].NFCom);
            Impresso := True;
            Break;
          end;
        end;

        if not Impresso and (EventoNFCom.Evento.Count > 0) then
          TfrmNFComDAEventoRLRetrato.SalvarPDF(Self, EventoNFCom.Evento.Items[0], AStream, nil);
      end;
    end
    else
    begin
      NumID := OnlyNumber(ANFCom.infNFCom.ID);
      Impresso := False;

      for i := 0 to (EventoNFCom.Evento.Count - 1) do
      begin
        if (NumID = OnlyNumber(EventoNFCom.Evento.Items[i].InfEvento.chNFCom)) then
        begin
          TfrmNFComDAEventoRLRetrato.SalvarPDF(Self, EventoNFCom.Evento.Items[i], AStream, ANFCom);
          Impresso := True;
          Break;
        end;
      end;

      if not Impresso and (EventoNFCom.Evento.Count > 0) then
        TfrmNFComDAEventoRLRetrato.SalvarPDF(Self, EventoNFCom.Evento.Items[0], AStream, nil);
    end;
  end;
end;

end.
