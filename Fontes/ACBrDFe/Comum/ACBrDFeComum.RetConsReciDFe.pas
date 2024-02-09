{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit ACBrDFeComum.RetConsReciDFe;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase, ACBrXmlBase;

type

  TProtDFeCollectionItem = class(TObject)
  private
    FId: string;
    FtpAmb: TACBrTipoAmbiente;
    FverAplic: string;
    FchDFe: string;
    FdhRecbto: TDateTime;
    FnProt: string;
    FdigVal: string;
    FcStat: Integer;
    FxMotivo: string;
    FXMLprotDFe: string;
    FcMsg : Integer;
    FxMsg : string;
  public
    property Id: string              read FId         write FId;
    property tpAmb: TACBrTipoAmbiente read FtpAmb      write FtpAmb;
    property verAplic: string        read FverAplic   write FverAplic;
    property chDFe: string           read FchDFe      write FchDFe;
    property dhRecbto: TDateTime     read FdhRecbto   write FdhRecbto;
    property nProt: string           read FnProt      write FnProt;
    property digVal: string          read FdigVal     write FdigVal;
    property cStat: Integer          read FcStat      write FcStat;
    property xMotivo: string         read FxMotivo    write FxMotivo;
    property XMLprotDFe: string      read FXMLprotDFe write FXMLprotDFe;
    property cMSg: Integer           read FcMsg       write FcMsg;
    property xMsg: string            read FxMsg       write FxMsg;
  end;

  TProtDFeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TProtDFeCollectionItem;
    procedure SetItem(Index: Integer; Value: TProtDFeCollectionItem);
  public
    function New: TProtDFeCollectionItem;
    property Items[Index: Integer]: TProtDFeCollectionItem read GetItem write SetItem; default;
  end;

  TRetConsReciDFe = class(TObject)
  private
    Fversao: string;
    FtpAmb: TACBrTipoAmbiente;
    FverAplic: string;
    FnRec: string;
    FcStat: Integer;
    FxMotivo: string;
    FcUF: Integer;
    FcMsg: Integer;
    FxMsg: string;
    FProtDFe: TProtDFeCollection;
    FtagGrupoMsg: string;

    FXmlRetorno: string;

    procedure SetProtDFe(const Value: TProtDFeCollection);
  public
    constructor Create(const AtagGrupoMsg: string);
    destructor Destroy; override;

    function LerXML: Boolean;

    property versao: string              read Fversao   write Fversao;
    property tpAmb: TACBrTipoAmbiente     read FtpAmb    write FtpAmb;
    property verAplic: string            read FverAplic write FverAplic;
    property nRec: string                read FnRec     write FnRec;
    property cStat: Integer              read FcStat    write FcStat;
    property xMotivo: string             read FxMotivo  write FxMotivo;
    property cUF: Integer                read FcUF      write FcUF;
    property cMsg: Integer               read FcMsg     write FcMsg;
    property xMsg: string                read FxMsg     write FxMsg;
    property ProtDFe: TProtDFeCollection read FProtDFe  write SetProtDFe;

    property XmlRetorno: string read FXmlRetorno write FXmlRetorno;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrXmlDocument;

{ TRetConsReciDFe }

constructor TRetConsReciDFe.Create(const AtagGrupoMsg: string);
begin
  inherited Create;

  FProtDFe := TProtDFeCollection.Create();

  FtagGrupoMsg := AtagGrupoMsg;
end;

destructor TRetConsReciDFe.Destroy;
begin
  FProtDFe.Free;

  inherited;
end;

procedure TRetConsReciDFe.SetProtDFe(const Value: TProtDFeCollection);
begin
  FProtDFe.Assign(Value);
end;

{ TProtDFeCollection }

function TProtDFeCollection.GetItem(Index: Integer): TProtDFeCollectionItem;
begin
  Result := TProtDFeCollectionItem(inherited Items[Index]);
end;

procedure TProtDFeCollection.SetItem(Index: Integer; Value: TProtDFeCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TRetConsReciDFe.LerXML: Boolean;
var
  Document: TACBrXmlDocument;
  ANode, AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
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
      tpAmb := StrToTipoAmbiente(ok, ObterConteudoTag(Anode.Childrens.FindAnyNs('tpAmb'), tcStr));
      verAplic := ObterConteudoTag(ANode.Childrens.FindAnyNs('verAplic'), tcStr);
      nRec := ObterConteudoTag(ANode.Childrens.FindAnyNs('nRec'), tcStr);
      cStat := ObterConteudoTag(ANode.Childrens.FindAnyNs('cStat'), tcInt);
      xMotivo := ACBrStr(ObterConteudoTag(ANode.Childrens.FindAnyNs('xMotivo'), tcStr));
      cUF := ObterConteudoTag(Anode.Childrens.FindAnyNs('cUF'), tcInt);
      cMsg := ObterConteudoTag(Anode.Childrens.FindAnyNs('cMsg'), tcInt);
      xMsg := ACBrStr(ObterConteudoTag(ANode.Childrens.FindAnyNs('xMsg'), tcStr));

      ANodes := ANode.Childrens.FindAllAnyNs('prot' + FtagGrupoMsg);

      ProtDFe.Clear;

      for i := 0 to Length(ANodes) - 1 do
      begin
        ProtDFe.New;
        with ProtDFe[i] do
        begin
          // A propriedade XMLprotDFe contem o XML que traz o resultado do
          // processamento da NF-e.
          XMLprotDFe := ANodes[i].OuterXml;

          AuxNode := ANodes[i].Childrens.FindAnyNs('infProt');

          if AuxNode <> nil then
          begin
            Id := ObterConteudoTag(AuxNode.Attributes.Items['Id']);
            tpAmb := StrToTipoAmbiente(ok, ObterConteudoTag(AuxNode.Childrens.FindAnyNs('tpAmb'), tcStr));
            verAplic := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('verAplic'), tcStr);
            chDFe := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('chDFe'), tcStr);
            dhRecbto := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('dhRecbto'), tcDatHor);
            nProt := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nProt'), tcStr);
            digVal := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('digVal'), tcStr);
            cStat := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cStat'), tcInt);
            xMotivo := ACBrStr(ObterConteudoTag(AuxNode.Childrens.FindAnyNs('xMotivo'), tcStr));
            cMsg := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cMsg'), tcInt);
            xMsg := ACBrStr(ObterConteudoTag(AuxNode.Childrens.FindAnyNs('xMsg'), tcStr));
          end;
        end;
      end;
    end;

    FreeAndNil(Document);
    Result := True;
  except
    Result := False;
  end;
end;

function TProtDFeCollection.New: TProtDFeCollectionItem;
begin
  Result := TProtDFeCollectionItem.Create;
  Add(Result);
end;

end.

