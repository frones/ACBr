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

unit ACBrNFe.RetConsSit;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  pcnConversao,
  pcnNFeConsts,
  ACBrBase, ACBrXmlBase,
  ACBrDFeComum.Proc,
  ACBrNFe.RetEnvEvento;

type

  TRetCancNFe = class(TObject)
  private
    Fversao: string;
    FtpAmb: TpcnTipoAmbiente;
    FdhRecbto: TDateTime;
    FcStat: Integer;
    FcUF: Integer;
    FchNFE: string;
    FverAplic: string;
    FnProt: string;
    FxMotivo: string;
  public
    property versao: string          read Fversao   write Fversao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb    write FtpAmb;
    property verAplic: string        read FverAplic write FverAplic;
    property cStat: Integer          read FcStat    write FcStat;
    property xMotivo: string         read FxMotivo  write FxMotivo;
    property cUF: Integer            read FcUF      write FcUF;
    property chNFE: string           read FchNFE    write FchNFE;
    property dhRecbto: TDateTime     read FdhRecbto write FdhRecbto;
    property nProt: string           read FnProt    write FnProt;
  end;

  TRetEventoNFeCollectionItem = class(TObject)
  private
    FRetEventoNFe: TRetEventoNFe;
  public
    constructor Create;
    destructor Destroy; override;
    property RetEventoNFe: TRetEventoNFe read FRetEventoNFe write FRetEventoNFe;
  end;

  TRetEventoNFeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRetEventoNFeCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetEventoNFeCollectionItem);
  public
    function Add: TRetEventoNFeCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetEventoNFeCollectionItem;
    property Items[Index: Integer]: TRetEventoNFeCollectionItem read GetItem write SetItem; default;
  end;

  TRetConsSitNFe = class(TObject)
  private
    Fversao: string;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: string;
    FcStat: Integer;
    FxMotivo: string;
    FcUF: Integer;
    FdhRecbto: TDateTime;
    FchNFe: string;
    FprotNFe: TProcDFe;
    FretCancNFe: TRetCancNFe;
    FnRec: string;  // Consta no Retorno da NFC-e
    FXMLprotNFe: string;

    FprocEventoNFe: TRetEventoNFeCollection;

    FXmlRetorno: string;
  public
    constructor Create(const Versao: string);
    destructor Destroy; override;

    function LerXml: Boolean;

    property versao: string          read Fversao     write Fversao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb      write FtpAmb;
    property verAplic: string        read FverAplic   write FverAplic;
    property cStat: Integer          read FcStat      write FcStat;
    property xMotivo: string         read FxMotivo    write FxMotivo;
    property cUF: Integer            read FcUF        write FcUF;
    property dhRecbto: TDateTime     read FdhRecbto   write FdhRecbto;
    property chNfe: string           read FchNfe      write FchNfe;
    property protNFe: TProcDFe       read FprotNFe    write FprotNFe;
    property retCancNFe: TRetCancNFe read FretCancNFe write FretCancNFe;
    property nRec: string            read FnRec       write FnRec;
    property XMLprotNFe: string      read FXMLprotNFe write FXMLprotNFe;

    property procEventoNFe: TRetEventoNFeCollection read FprocEventoNFe write FprocEventoNFe;

    property XmlRetorno: string read FXmlRetorno write FXmlRetorno;
  end;

implementation

uses
  ACBrXmlDocument;

{ TRetConsSitNFe }

constructor TRetConsSitNFe.Create(const Versao: string);
begin
  inherited Create;

  FprotNFe    := TProcDFe.Create(Versao, NAME_SPACE, 'NFe');
  FretCancNFe := TRetCancNFe.Create;
end;

destructor TRetConsSitNFe.Destroy;
begin
  FprotNFe.Free;
  FretCancNFe.Free;

  if Assigned(procEventoNFe) then
    procEventoNFe.Free;

  inherited;
end;

function TRetConsSitNFe.LerXml: Boolean;
var
  Document: TACBrXmlDocument;
  ANode, ANodeAux: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  ok: Boolean;
  i: Integer;
  Item : TRetEventoNFeCollectionItem;
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
        nRec := ObterConteudoTag(ANode.Childrens.FindAnyNs('nRec'), tcStr);
        cStat := ObterConteudoTag(ANode.Childrens.FindAnyNs('cStat'), tcInt);
        xMotivo := ObterConteudoTag(ANode.Childrens.FindAnyNs('xMotivo'), tcStr);
        dhRecbto := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhRecbto'), tcDatHor);
        chNFe := ObterConteudoTag(ANode.Childrens.FindAnyNs('chNFe'), tcStr);

        case cStat of
          100, 101, 104, 110, 150, 151, 155, 301, 302, 303:
            begin
              ANodeAux := ANode.Childrens.FindAnyNs('protNFe');

              if ANodeAux <> nil then
              begin
                // A propriedade XMLprotNFe contem o XML que traz o resultado do
                // processamento da NF-e.
                XMLprotNFe := ANodeAux.OuterXml;

                ANodeAux := ANodeAux.Childrens.FindAnyNs('infProt');

                if ANodeAux <> nil then
                begin
                  protNFe.tpAmb := StrToTipoAmbiente(ok, ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('tpAmb'), tcStr));
                  protNFe.verAplic := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('verAplic'), tcStr);
                  protNFe.chDFe := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('chNFe'), tcStr);
                  protNFe.dhRecbto := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('dhRecbto'), tcDatHor);
                  protNFe.nProt := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('nProt'), tcStr);
                  protNFe.digVal := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('digVal'), tcStr);
                  protNFe.cStat := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('cStat'), tcInt);
                  protNFe.xMotivo := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('xMotivo'), tcStr);
                  protNFe.cMsg := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('cMsg'), tcInt);
                  protNFe.xMsg := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('xMsg'), tcStr);
                end;
              end;
            end;
        end;

        retCancNFe.cStat := 0;

        if cStat in [101, 151, 155] then
        begin
          ANodeAux := ANode.Childrens.FindAnyNs('infCanc');

          if ANodeAux <> nil then
          begin
            retCancNFe.tpAmb := StrToTpAmb(ok, ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('tpAmb'), tcStr));
            retCancNFe.verAplic := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('verAplic'), tcStr);
            retCancNFe.cStat := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('cStat'), tcInt);
            retCancNFe.xMotivo := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('xMotivo'), tcStr);
            retCancNFe.cUF := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('cUF'), tcInt);
            retCancNFe.chNFe := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('chNFe'), tcStr);
            retCancNFe.dhRecbto := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('dhRecbto'), tcDatHor);
            retCancNFe.nProt := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('nProt'), tcStr);
          end;
        end;

        if Assigned(procEventoNFe) then
          procEventoNFe.Free;

        procEventoNFe := TRetEventoNFeCollection.Create;

        try
          ANodeArray := ANode.Childrens.FindAllAnyNs('procEventoNFe');

          if Assigned(ANodeArray) then
          begin
            for i := Low(ANodeArray) to High(ANodeArray) do
            begin
              AnodeAux := ANodeArray[i];

              Item := procEventoNFe.New;

              Item.RetEventoNFe.XmlRetorno := AnodeAux.OuterXml;
              Item.RetEventoNFe.XML := AnodeAux.OuterXml;
              Item.RetEventoNFe.LerXml;
            end;
          end;
        finally
          Result := True;
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

{ TRetEventoCollection }

function TRetEventoNFeCollection.Add: TRetEventoNFeCollectionItem;
begin
  Result := Self.New;
end;

function TRetEventoNFeCollection.GetItem(Index: Integer): TRetEventoNFeCollectionItem;
begin
  Result := TRetEventoNFeCollectionItem(inherited Items[Index]);
end;

procedure TRetEventoNFeCollection.SetItem(Index: Integer;
  Value: TRetEventoNFeCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TRetEventoCollectionItem }

constructor TRetEventoNFeCollectionItem.Create;
begin
  inherited Create;

  FRetEventoNFe := TRetEventoNFe.Create;
end;

destructor TRetEventoNFeCollectionItem.Destroy;
begin
  FRetEventoNFe.Free;

  inherited;
end;

function TRetEventoNFeCollection.New: TRetEventoNFeCollectionItem;
begin
  Result := TRetEventoNFeCollectionItem.Create;
  Self.Add(Result);
end;

end.

