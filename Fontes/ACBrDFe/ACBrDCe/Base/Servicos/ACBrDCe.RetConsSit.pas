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

unit ACBrDCe.RetConsSit;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase, ACBrXmlBase,
  ACBrDFeComum.Proc,
  ACBrDCe.RetEnvEvento;

type

  TRetEventoDCeCollectionItem = class(TObject)
  private
    FRetEventoDCe: TRetEventoDCe;
  public
    constructor Create;
    destructor Destroy; override;
    property RetEventoDCe: TRetEventoDCe read FRetEventoDCe write FRetEventoDCe;
  end;

  TRetEventoDCeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRetEventoDCeCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetEventoDCeCollectionItem);
  public
    function New: TRetEventoDCeCollectionItem;
    property Items[Index: Integer]: TRetEventoDCeCollectionItem read GetItem write SetItem; default;
  end;

  TRetConsSitDCe = class(TObject)
  private
    Fversao: string;
    FtpAmb: TACBrTipoAmbiente;
    FverAplic: string;
    FcStat: Integer;
    FxMotivo: string;
    FcUF: Integer;
    FdhRecbto: TDateTime;
    FchDCe: string;
    FprotDCe: TProcDFe;
    FprocEventoDCe: TRetEventoDCeCollection;
    FnRec: string;
    FXMLprotDCe: string;

    FXmlRetorno: string;
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: Boolean;

    property versao: string read Fversao write Fversao;
    property tpAmb: TACBrTipoAmbiente read FtpAmb write FtpAmb;
    property verAplic: string read FverAplic write FverAplic;
    property cStat: Integer read FcStat write FcStat;
    property xMotivo: string read FxMotivo write FxMotivo;
    property cUF: Integer read FcUF write FcUF;
    property dhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property chDCe: string read FchDCe write FchDCe;
    property protDCe: TProcDFe read FprotDCe write FprotDCe;
    property procEventoDCe: TRetEventoDCeCollection read FprocEventoDCe write FprocEventoDCe;
    property nRec: string read FnRec write FnRec;
    property XMLprotDCe: string read FXMLprotDCe write FXMLprotDCe;

    property XmlRetorno: string read FXmlRetorno write FXmlRetorno;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrXmlDocument;

{ TRetEventoCollection }

function TRetEventoDCeCollection.GetItem(Index: Integer): TRetEventoDCeCollectionItem;
begin
  Result := TRetEventoDCeCollectionItem(inherited Items[Index]);
end;

procedure TRetEventoDCeCollection.SetItem(Index: Integer;
  Value: TRetEventoDCeCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TRetEventoCollectionItem }

constructor TRetEventoDCeCollectionItem.Create;
begin
  inherited Create;

  FRetEventoDCe := TRetEventoDCe.Create;
end;

destructor TRetEventoDCeCollectionItem.Destroy;
begin
  FRetEventoDCe.Free;

  inherited;
end;

function TRetEventoDCeCollection.New: TRetEventoDCeCollectionItem;
begin
  Result := TRetEventoDCeCollectionItem.Create;
  Self.Add(Result);
end;

{ TRetConsSitDCe }

constructor TRetConsSitDCe.Create;
begin
  inherited Create;

  FprotDCe := TProcDFe.Create('', '', '');
end;

destructor TRetConsSitDCe.Destroy;
begin
  FprotDCe.Free;

  if Assigned(procEventoDCe) then
    procEventoDCe.Free;

  inherited;
end;

function TRetConsSitDCe.LerXml: Boolean;
var
  Document: TACBrXmlDocument;
  ANode, ANodeAux: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  ok: Boolean;
  i: Integer;
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
      xMotivo := ACBrStr(ObterConteudoTag(ANode.Childrens.FindAnyNs('xMotivo'), tcStr));
      cUF := ObterConteudoTag(ANode.Childrens.FindAnyNs('cUF'), tcInt);
      nRec := ObterConteudoTag(ANode.Childrens.FindAnyNs('nRec'), tcStr);
      dhRecbto := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhRecbto'), tcDatHor);
      chDCe := ObterConteudoTag(ANode.Childrens.FindAnyNs('chDCe'), tcStr);

      case cStat of
        100, // Autorizado o Uso da DCe
        101, // Cancelamento de DCe homologado
        102, // Substituição da DCe homologado
        150: // Autorizado o Uso da DCe, autorização fora de prazo
          begin
            ANodeAux := ANode.Childrens.FindAnyNs('protDCe');

            if ANodeAux <> nil then
            begin
              // A propriedade XMLprotDCe contem o XML que traz o resultado do
              // processamento da DCe.
              XMLprotDCe := ANodeAux.OuterXml;

              ANodeAux := ANodeAux.Childrens.FindAnyNs('infProt');

              if ANodeAux <> nil then
              begin
       //         protDCe.Id := ObterConteudoTag(ANodeAux.Attributes.Items['Id']);
                protDCe.tpAmb := StrToTipoAmbiente(ok, ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('tpAmb'), tcStr));
                protDCe.verAplic := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('verAplic'), tcStr);
                protDCe.chDFe := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('chDCe'), tcStr);
                protDCe.dhRecbto := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('dhRecbto'), tcDatHor);
                protDCe.nProt := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('nProt'), tcStr);
                protDCe.digVal := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('digVal'), tcStr);
                protDCe.cStat := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('cStat'), tcInt);
                protDCe.xMotivo := ACBrStr(ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('xMotivo'), tcStr));

                ANodeAux := ANodeAux.Childrens.FindAnyNs('infFisco');

                if ANodeAux <> nil then
                begin
                  protDCe.cMsg := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('cMsg'), tcInt);
                  protDCe.xMsg := ACBrStr(ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('xMsg'), tcStr));
                end;
              end;
            end;
          end;
      end;

      if Assigned(procEventoDCe) then
        procEventoDCe.Free;

      procEventoDCe := TRetEventoDCeCollection.Create;

      try
        ANodeArray := ANode.Childrens.FindAllAnyNs('procEventoDCe');

        if Assigned(ANodeArray) then
        begin
          for i := Low(ANodeArray) to High(ANodeArray) do
          begin
            AnodeAux := ANodeArray[i];

            procEventoDCe.New;
            procEventoDCe.Items[i].RetEventoDCe.XmlRetorno := AnodeAux.OuterXml;
            procEventoDCe.Items[i].RetEventoDCe.LerXml;
          end;
        end;

        Result := True;
      except
        Result := False;
      end;
    end;

    FreeAndNil(Document);
    Result := True;
  except
    Result := False;
  end;
end;

end.

