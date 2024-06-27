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

unit ACBrCTe.RetConsSit;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  pcnConversao,
  pcteConsts,
  ACBrBase, ACBrXmlBase,
//  pcnProcCTe,
  ACBrDFeComum.Proc,
  ACBrCTe.RetEnvEvento;

type

  TRetCancCTe = class(TObject)
  private
    Fversao: string;
    FtpAmb: TpcnTipoAmbiente;
    FdhRecbto: TDateTime;
    FcStat: Integer;
    FcUF: Integer;
    FchCTe: string;
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
    property chCTe: string           read FchCTe    write FchCTe;
    property dhRecbto: TDateTime     read FdhRecbto write FdhRecbto;
    property nProt: string           read FnProt    write FnProt;
  end;

  TRetEventoCTeCollectionItem = class(TObject)
  private
    FRetEventoCTe: TRetEventoCTe;
  public
    constructor Create;
    destructor Destroy; override;
    property RetEventoCTe: TRetEventoCTe read FRetEventoCTe write FRetEventoCTe;
  end;

  TRetEventoCTeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRetEventoCTeCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetEventoCTeCollectionItem);
  public
    function Add: TRetEventoCTeCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetEventoCTeCollectionItem;
    property Items[Index: Integer]: TRetEventoCTeCollectionItem read GetItem write SetItem; default;
  end;

  TRetConsSitCTe = class(TObject)
  private
    Fversao: string;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: string;
    FcStat: Integer;
    FxMotivo: string;
    FcUF: Integer;
    FdhRecbto: TDateTime;
    FchCTe: string;
    FprotCTe: TProcDFe;
    FretCancCTe: TRetCancCTe;
    FnRec: string;
    FXMLprotCTe: string;

    FprocEventoCTe: TRetEventoCTeCollection;

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
    property chCTe: string           read FchCTe      write FchCTe;
    property protCTe: TProcDFe       read FprotCTe    write FprotCTe;
    property retCancCTe: TRetCancCTe read FretCancCTe write FretCancCTe;
    property nRec: string            read FnRec       write FnRec;
    property XMLprotCTe: string      read FXMLprotCTe write FXMLprotCTe;

    property procEventoCTe: TRetEventoCTeCollection read FprocEventoCTe write FprocEventoCTe;

    property XmlRetorno: string read FXmlRetorno write FXmlRetorno;
  end;

implementation

uses
  ACBrXmlDocument;

{ TRetConsSitCTe }

constructor TRetConsSitCTe.Create(const Versao: string);
begin
  inherited Create;

  FprotCTe    := TProcDFe.Create(Versao, NAME_SPACE_CTE, 'CTe');
  FretCancCTe := TRetCancCTe.Create;
end;

destructor TRetConsSitCTe.Destroy;
begin
  FprotCTe.Free;
  FretCancCTe.Free;

  if Assigned(procEventoCTe) then
    procEventoCTe.Free;

  inherited;
end;

function TRetConsSitCTe.LerXml: Boolean;
var
  Document: TACBrXmlDocument;
  ANode, ANodeAux: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  ok: Boolean;
  i: Integer;
  Item : TRetEventoCTeCollectionItem;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      Result := False;

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
        chCTe := ObterConteudoTag(ANode.Childrens.FindAnyNs('chCTe'), tcStr);

        case cStat of
          100, 101, 104, 110, 150, 151, 155, 301, 302, 303:
            begin
              ANodeAux := ANode.Childrens.FindAnyNs('protCTe');

              if ANodeAux <> nil then
              begin
                // A propriedade XMLprotCTe contem o XML que traz o resultado do
                // processamento da NF-e.
                XMLprotCTe := ANodeAux.OuterXml;

                ANodeAux := ANodeAux.Childrens.FindAnyNs('infProt');

                if ANodeAux <> nil then
                begin
                  protCTe.tpAmb := StrToTipoAmbiente(ok, ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('tpAmb'), tcStr));
                  protCTe.verAplic := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('verAplic'), tcStr);
                  protCTe.chDFe := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('chCTe'), tcStr);
                  protCTe.dhRecbto := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('dhRecbto'), tcDatHor);
                  protCTe.nProt := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('nProt'), tcStr);
                  protCTe.digVal := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('digVal'), tcStr);
                  protCTe.cStat := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('cStat'), tcInt);
                  protCTe.xMotivo := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('xMotivo'), tcStr);

                  ANodeAux := ANodeAux.Childrens.FindAnyNs('infFisco');

                  if ANodeAux <> nil then
                  begin
                    protCTe.cMsg := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('cMsg'), tcInt);
                    protCTe.xMsg := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('xMsg'), tcStr);
                  end;
                end;
              end;
            end;
        end;

        retCancCTe.cStat := 0;

        if cStat in [101, 151, 155] then
        begin
          ANodeAux := ANode.Childrens.FindAnyNs('infCanc');

          if ANodeAux <> nil then
          begin
            retCancCTe.tpAmb := StrToTpAmb(ok, ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('tpAmb'), tcStr));
            retCancCTe.verAplic := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('verAplic'), tcStr);
            retCancCTe.cStat := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('cStat'), tcInt);
            retCancCTe.xMotivo := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('xMotivo'), tcStr);
            retCancCTe.cUF := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('cUF'), tcInt);
            retCancCTe.chCTe := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('chCTe'), tcStr);
            retCancCTe.dhRecbto := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('dhRecbto'), tcDatHor);
            retCancCTe.nProt := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('nProt'), tcStr);
          end;
        end;

        if Assigned(procEventoCTe) then
          procEventoCTe.Free;

        procEventoCTe := TRetEventoCTeCollection.Create;

        try
          ANodeArray := ANode.Childrens.FindAllAnyNs('procEventoCTe');

          if Assigned(ANodeArray) then
          begin
            for i := Low(ANodeArray) to High(ANodeArray) do
            begin
              AnodeAux := ANodeArray[i];

              Item := procEventoCTe.New;

              Item.RetEventoCTe.XmlRetorno := AnodeAux.OuterXml;
              Item.RetEventoCTe.XML := AnodeAux.OuterXml;
              Item.RetEventoCTe.LerXml;
            end;
          end;
        finally
          Result := True;
        end;
      end;
    except
      Result := False;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

{ TRetEventoCollection }

function TRetEventoCTeCollection.Add: TRetEventoCTeCollectionItem;
begin
  Result := Self.New;
end;

function TRetEventoCTeCollection.GetItem(Index: Integer): TRetEventoCTeCollectionItem;
begin
  Result := TRetEventoCTeCollectionItem(inherited Items[Index]);
end;

procedure TRetEventoCTeCollection.SetItem(Index: Integer;
  Value: TRetEventoCTeCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TRetEventoCollectionItem }

constructor TRetEventoCTeCollectionItem.Create;
begin
  inherited Create;
  FRetEventoCTe := TRetEventoCTe.Create;
end;

destructor TRetEventoCTeCollectionItem.Destroy;
begin
  FRetEventoCTe.Free;
  inherited;
end;

function TRetEventoCTeCollection.New: TRetEventoCTeCollectionItem;
begin
  Result := TRetEventoCTeCollectionItem.Create;
  Self.Add(Result);
end;

end.

