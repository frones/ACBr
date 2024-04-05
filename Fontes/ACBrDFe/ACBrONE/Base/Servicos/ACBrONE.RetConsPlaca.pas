{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
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

unit ACBrONE.RetConsPlaca;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase,
  ACBrXmlBase,
  ACBrXmlDocument;

type
  TinfMDFeCollectionItem = class(TObject)
  private
    FchMDFe: string;

  public
    property chMDFe: string read FchMDFe write FchMDFe;
  end;

  TinfMDFeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfMDFeCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfMDFeCollectionItem);
  public
    function New: TinfMDFeCollectionItem;
    property Items[Index: Integer]: TinfMDFeCollectionItem read GetItem write SetItem; default;
  end;

  TRetConsPlaca = class(TObject)
  private
    Fversao: string;
    FtpAmb: TACBrTipoAmbiente;
    FverAplic: string;
    FcStat: Integer;
    FxMotivo: string;
    FdhResp: TDateTime;
    FNSU: string;
    FleituraComp: string;
    FinfMDFe: TinfMDFeCollection;
    FXmlRetorno: string;

    procedure SetinfMDFe(const Value: TinfMDFeCollection);
    procedure Ler_retOneConsPorPlaca(ANode: TACBrXmlNode);
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: boolean;

    property versao: string           read Fversao      write Fversao;
    property tpAmb: TACBrTipoAmbiente read FtpAmb       write FtpAmb;
    property verAplic: string         read FverAplic    write FverAplic;
    property cStat: Integer           read FcStat       write FcStat;
    property xMotivo: string          read FxMotivo     write FxMotivo;
    property dhResp: TDateTime        read FdhResp      write FdhResp;
    property NSU: string              read FNSU         write FNSU;
    property leituraComp: string      read FleituraComp write FleituraComp;
    property infMDFe: TinfMDFeCollection read FinfMDFe  write SetinfMDFe;

    property XmlRetorno: string read FXmlRetorno write FXmlRetorno;
  end;

implementation

uses
  synacode,
  ACBrUtil.FilesIO;

{ TRetConsPlaca }

constructor TRetConsPlaca.Create;
begin
  inherited Create;

  FinfMDFe := TinfMDFeCollection.Create();
end;

destructor TRetConsPlaca.Destroy;
begin
  FinfMDFe.Free;

  inherited;
end;

function TRetConsPlaca.LerXml: boolean;
var
  Document: TACBrXmlDocument;
  ANode: TACBrXmlNode;
  ok: Boolean;
begin
  Document := TACBrXmlDocument.Create;

  try
    Document.LoadFromXml(XmlRetorno);

    ANode := Document.Root;

    if ANode <> nil then
    begin
      versao := ObterConteudoTag(ANode.Attributes.Items['versao']);
      tpAmb := StrToTipoAmbiente(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpAmb'), tcStr));
      verAplic := ObterConteudoTag(ANode.Childrens.FindAnyNs('verAplic'), tcStr);
      cStat := ObterConteudoTag(ANode.Childrens.FindAnyNs('cStat'), tcInt);
      xMotivo := ObterConteudoTag(ANode.Childrens.FindAnyNs('xMotivo'), tcStr);
      dhResp := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhResp'), tcDatHor);

      Ler_retOneConsPorPlaca(ANode.Childrens.FindAnyNs('retOneConsPorPlaca'));
    end;
  finally
    Result := True;
    FreeAndNil(Document);
  end;
end;

procedure TRetConsPlaca.Ler_retOneConsPorPlaca(ANode: TACBrXmlNode);
var
  ANodeAux: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  auxStr: AnsiString;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  ANodeAux := ANode.Childrens.FindAnyNs('loteDistLeitura');

  if ANodeAux <> nil then
  begin
    ANodeAux := ANodeAux.Childrens.FindAnyNs('leituraCompactada');

    if ANodeAux <> nil then
    begin
      NSU := ObterConteudoTag(ANodeAux.Attributes.Items['NSU']);
      auxStr := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('leituraComp'), tcStr);

      if auxStr <> '' then
        leituraComp := UnZip(DecodeBase64(auxStr));

      ANodes := ANodeAux.Childrens.FindAllAnyNs('infMDFe');

      for i := 0 to Length(ANodes) - 1 do
      begin
        FinfMDFe.New;
        FinfMDFe[i].chMDFe := ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('chMDFe'), tcStr);
      end;
    end;
  end;
end;

procedure TRetConsPlaca.SetinfMDFe(const Value: TinfMDFeCollection);
begin
  FinfMDFe := Value;
end;

{ TinfMDFeCollection }

function TinfMDFeCollection.GetItem(Index: Integer): TinfMDFeCollectionItem;
begin
  Result := TinfMDFeCollectionItem(inherited Items[Index]);
end;

function TinfMDFeCollection.New: TinfMDFeCollectionItem;
begin
  Result := TinfMDFeCollectionItem.Create;
  Add(Result);
end;

procedure TinfMDFeCollection.SetItem(Index: Integer;
  Value: TinfMDFeCollectionItem);
begin
  inherited Items[Index] := Value;
end;

end.

