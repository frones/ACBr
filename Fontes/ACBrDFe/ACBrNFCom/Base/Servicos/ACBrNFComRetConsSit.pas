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

unit ACBrNFComRetConsSit;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase, ACBrXmlBase,
  ACbrNFComProc, ACBrNFComRetEnvEvento;

type

  TRetEventoNFComCollectionItem = class(TObject)
  private
    FRetEventoNFCom: TRetEventoNFCom;
  public
    constructor Create;
    destructor Destroy; override;
    property RetEventoNFCom: TRetEventoNFCom read FRetEventoNFCom write FRetEventoNFCom;
  end;

  TRetEventoNFComCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRetEventoNFComCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetEventoNFComCollectionItem);
  public
    function New: TRetEventoNFComCollectionItem;
    property Items[Index: Integer]: TRetEventoNFComCollectionItem read GetItem write SetItem; default;
  end;

  TRetConsSitNFCom = class(TObject)
  private
    Fversao: string;
    FtpAmb: TACBrTipoAmbiente;
    FverAplic: string;
    FcStat: Integer;
    FxMotivo: string;
    FcUF: Integer;
    FdhRecbto: TDateTime;
    FchNFCom: string;
    FprotNFCom: TProcNFCom;
    FprocEventoNFCom: TRetEventoNFComCollection;
    FnRec: string;
    FXMLprotNFCom: string;

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
    property chNFCom: string read FchNFCom write FchNFCom;
    property protNFCom: TProcNFCom read FprotNFCom write FprotNFCom;
    property procEventoNFCom: TRetEventoNFComCollection read FprocEventoNFCom write FprocEventoNFCom;
    property nRec: string read FnRec write FnRec;
    property XMLprotNFCom: string read FXMLprotNFCom write FXMLprotNFCom;

    property XmlRetorno: string read FXmlRetorno write FXmlRetorno;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrXmlDocument;

{ TRetEventoCollection }

function TRetEventoNFComCollection.GetItem(Index: Integer): TRetEventoNFComCollectionItem;
begin
  Result := TRetEventoNFComCollectionItem(inherited Items[Index]);
end;

procedure TRetEventoNFComCollection.SetItem(Index: Integer;
  Value: TRetEventoNFComCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TRetEventoCollectionItem }

constructor TRetEventoNFComCollectionItem.Create;
begin
  inherited Create;

  FRetEventoNFCom := TRetEventoNFCom.Create;
end;

destructor TRetEventoNFComCollectionItem.Destroy;
begin
  FRetEventoNFCom.Free;

  inherited;
end;

function TRetEventoNFComCollection.New: TRetEventoNFComCollectionItem;
begin
  Result := TRetEventoNFComCollectionItem.Create;
  Self.Add(Result);
end;

{ TRetConsSitNFCom }

constructor TRetConsSitNFCom.Create;
begin
  inherited Create;

  FprotNFCom := TProcNFCom.create;
end;

destructor TRetConsSitNFCom.Destroy;
begin
  FprotNFCom.Free;

  if Assigned(procEventoNFCom) then
    procEventoNFCom.Free;

  inherited;
end;

function TRetConsSitNFCom.LerXml: Boolean;
var
  Document: TACBrXmlDocument;
  ANode, ANodeAux: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  ok: Boolean;
  i: Integer;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      Result := True;

      Document.LoadFromXml(XmlRetorno);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        versao := ObterConteudoTag(ANode.Attributes.Items['versao']);
        tpAmb := StrToTipoAmbiente(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpAmb'), tcStr));
        verAplic := ObterConteudoTag(ANode.Childrens.FindAnyNs('verAplic'), tcStr);
        cStat := ObterConteudoTag(ANode.Childrens.FindAnyNs('cStat'), tcInt);
        xMotivo := ObterConteudoTag(ANode.Childrens.FindAnyNs('xMotivo'), tcStr);
        cUF := ObterConteudoTag(ANode.Childrens.FindAnyNs('cUF'), tcInt);
        nRec := ObterConteudoTag(ANode.Childrens.FindAnyNs('nRec'), tcStr);
        dhRecbto := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhRecbto'), tcDatHor);
        chNFCom := ObterConteudoTag(ANode.Childrens.FindAnyNs('chNFCom'), tcStr);

        case cStat of
          100, // Autorizado o Uso da NFCom
          101, // Cancelamento de NFCom homologado
          102, // Substituição da NFCom homologado
          150: // Autorizado o Uso da NFCom, autorização fora de prazo
            begin
              ANodeAux := ANode.Childrens.FindAnyNs('protNFCom');

              if ANodeAux <> nil then
              begin
                // A propriedade XMLprotNFCom contem o XML que traz o resultado do
                // processamento da NFCom.
                XMLprotNFCom := ANodeAux.OuterXml;

                ANodeAux := ANodeAux.Childrens.FindAnyNs('infProt');

                if ANodeAux <> nil then
                begin
                  protNFCom.Id := ObterConteudoTag(ANodeAux.Attributes.Items['Id']);
                  protNFCom.tpAmb := StrToTipoAmbiente(ok, ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('tpAmb'), tcStr));
                  protNFCom.verAplic := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('verAplic'), tcStr);
                  protNFCom.chNFCom := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('chNFCom'), tcStr);
                  protNFCom.dhRecbto := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('dhRecbto'), tcDatHor);
                  protNFCom.nProt := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('nProt'), tcStr);
                  protNFCom.digVal := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('digVal'), tcStr);
                  protNFCom.cStat := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('cStat'), tcInt);
                  protNFCom.xMotivo := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('xMotivo'), tcStr);

                  ANodeAux := ANodeAux.Childrens.FindAnyNs('infFisco');

                  if ANodeAux <> nil then
                  begin
                    protNFCom.cMsg := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('cMsg'), tcInt);
                    protNFCom.xMsg := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('xMsg'), tcStr);
                  end;
                end;
              end;
            end;
        end;

        if Assigned(procEventoNFCom) then
          procEventoNFCom.Free;

        procEventoNFCom := TRetEventoNFComCollection.Create;

        try
          ANodeArray := ANode.Childrens.FindAllAnyNs('procEventoNFCom');

          if Assigned(ANodeArray) then
          begin
            for i := Low(ANodeArray) to High(ANodeArray) do
            begin
              AnodeAux := ANodeArray[i];

              procEventoNFCom.New;
              procEventoNFCom.Items[i].RetEventoNFCom.XmlRetorno := AnodeAux.OuterXml;
              procEventoNFCom.Items[i].RetEventoNFCom.LerXml;
            end;
          end;
        except
          Result := False;
        end;
      end;
    except
      Result := False;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

end.

