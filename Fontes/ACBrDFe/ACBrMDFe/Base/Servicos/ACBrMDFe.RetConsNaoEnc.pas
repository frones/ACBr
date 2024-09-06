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

unit ACBrMDFe.RetConsNaoEnc;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  pcnConversao,
  ACBrBase, ACBrXmlBase;

type

  TRetInfMDFeCollectionItem = class(TObject)
  private
    FchMDFe: String;
    FnProt: String;
  public
    property chMDFe: String read FchMDFe write FchMDFe;
    property nProt: String  read FnProt  write FnProt;
  end;

  TRetInfMDFeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRetInfMDFeCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetInfMDFeCollectionItem);
  public
    function Add: TRetInfMDFeCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetInfMDFeCollectionItem;
    property Items[Index: Integer]: TRetInfMDFeCollectionItem read GetItem write SetItem; default;
  end;

  TRetConsMDFeNaoEnc = class(TObject)
  private
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FcUF: Integer;
    FInfMDFe: TRetInfMDFeCollection;

    FXmlRetorno: string;
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: Boolean;

    property versao: String                 read Fversao   write Fversao;
    property tpAmb: TpcnTipoAmbiente        read FtpAmb    write FtpAmb;
    property verAplic: String               read FverAplic write FverAplic;
    property cStat: Integer                 read FcStat    write FcStat;
    property xMotivo: String                read FxMotivo  write FxMotivo;
    property cUF: Integer                   read FcUF      write FcUF;
    property InfMDFe: TRetInfMDFeCollection read FInfMDFe  write FInfMDFe;

    property XmlRetorno: string read FXmlRetorno write FXmlRetorno;
  end;

implementation

uses
  ACBrXmlDocument;

{ TRetConsMDFeNaoEnc }

constructor TRetConsMDFeNaoEnc.Create;
begin
  inherited Create;

end;

destructor TRetConsMDFeNaoEnc.Destroy;
begin
  if Assigned(InfMDFe) then
    InfMDFe.Free;

  inherited;
end;

function TRetConsMDFeNaoEnc.LerXml: boolean;
var
  Document: TACBrXmlDocument;
  ANode, ANodeAux: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  ok: Boolean;
  i: Integer;
  Item: TRetInfMDFeCollectionItem;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if XmlRetorno = '' then Exit;

      Document.LoadFromXml(XmlRetorno);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        versao := ObterConteudoTag(ANode.Attributes.Items['versao']);
        verAplic := ObterConteudoTag(ANode.Childrens.FindAnyNs('verAplic'), tcStr);
        tpAmb := StrToTpAmb(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpAmb'), tcStr));
        cUF := ObterConteudoTag(ANode.Childrens.FindAnyNs('cUF'), tcInt);
        cStat := ObterConteudoTag(ANode.Childrens.FindAnyNs('cStat'), tcInt);
        xMotivo := ObterConteudoTag(ANode.Childrens.FindAnyNs('xMotivo'), tcStr);

        if Assigned(InfMDFe) then
          InfMDFe.Free;

        InfMDFe := TRetInfMDFeCollection.Create;

        ANodes := ANode.Childrens.FindAll('infMDFe');
        for i := 0 to Length(ANodes) - 1 do
        begin
          AnodeAux := ANodes[i];

          Item := InfMDFe.New;

          Item.chMDFe := ObterConteudoTag(AnodeAux.Childrens.FindAnyNs('chMDFe'), tcStr);
          Item.nProt := ObterConteudoTag(AnodeAux.Childrens.FindAnyNs('nProt'), tcStr);
        end;
      end;

      Result := True;
    except
      Result := False;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

{ TRetInfMDFeCollection }

function TRetInfMDFeCollection.Add: TRetInfMDFeCollectionItem;
begin
  Result := Self.New;
end;

function TRetInfMDFeCollection.GetItem(Index: Integer): TRetInfMDFeCollectionItem;
begin
  Result := TRetInfMDFeCollectionItem(inherited Items[Index]);
end;

procedure TRetInfMDFeCollection.SetItem(Index: Integer;
  Value: TRetInfMDFeCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TRetInfMDFeCollection.New: TRetInfMDFeCollectionItem;
begin
  Result := TRetInfMDFeCollectionItem.Create;
  Self.Add(Result);
end;

end.

