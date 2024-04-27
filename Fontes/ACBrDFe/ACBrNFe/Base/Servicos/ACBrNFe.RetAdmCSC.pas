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

unit ACBrNFe.RetAdmCSC;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase,
  pcnConversao,
  ACBrXmlBase;

type

  TRetdadosCscCollectionItem = class(TObject)
  private
    FidCsc: Integer;
    FcodigoCsc: string;
  public
    property idCsc: Integer    read FidCsc     write FidCsc;
    property codigoCsc: string read FcodigoCsc write FcodigoCsc;
  end;

  TRetdadosCscCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRetdadosCscCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetdadosCscCollectionItem);
  public
    function Add: TRetdadosCscCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetdadosCscCollectionItem;
    property Items[Index: Integer]: TRetdadosCscCollectionItem read GetItem write SetItem; default;
  end;

  TRetAdmCSCNFCe = class(TObject)
  private
    Fversao: string;
    FtpAmb: TpcnTipoAmbiente;
    FindOP: TpcnIndOperacao;
    FcStat: Integer;
    FxMotivo: string;
    FdadosCsc: TRetdadosCscCollection;
    FXmlRetorno: string;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;

    property versao: string                   read Fversao   write Fversao;
    property tpAmb: TpcnTipoAmbiente          read FtpAmb    write FtpAmb;
    property indOP: TpcnIndOperacao           read FindOP    write FindOP;
    property cStat: Integer                   read FcStat    write FcStat;
    property xMotivo: string                  read FxMotivo  write FxMotivo;
    property dadosCsc: TRetdadosCscCollection read FdadosCsc write FdadosCsc;

    property XmlRetorno: string read FXmlRetorno write FXmlRetorno;
  end;

implementation

uses
  ACBrXmlDocument;

{ TRetdadosCscCollection }

function TRetdadosCscCollection.Add: TRetdadosCscCollectionItem;
begin
  Result := Self.New;
end;

function TRetdadosCscCollection.GetItem(
  Index: Integer): TRetdadosCscCollectionItem;
begin
  Result := TRetdadosCscCollectionItem(inherited Items[Index]);
end;

procedure TRetdadosCscCollection.SetItem(Index: Integer;
  Value: TRetdadosCscCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TRetdadosCscCollection.New: TRetdadosCscCollectionItem;
begin
  Result := TRetdadosCscCollectionItem.Create;
  Self.Add(Result);
end;

{ TRetAdmCSCNFCe }

constructor TRetAdmCSCNFCe.Create;
begin
  inherited Create;

  FdadosCsc := TRetdadosCscCollection.Create;
end;

destructor TRetAdmCSCNFCe.Destroy;
begin
  FdadosCsc.Free;

  inherited;
end;

function TRetAdmCSCNFCe.LerXml: Boolean;
var
  Document: TACBrXmlDocument;
  ANode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  ok: Boolean;
  i: Integer;
  Item: TRetdadosCscCollectionItem;
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
        tpAmb := StrToTpAmb(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpAmb'), tcStr));
        indOp := StrToIndOperacao(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('indOp'), tcStr));
        cStat := ObterConteudoTag(ANode.Childrens.FindAnyNs('cStat'), tcInt);
        xMotivo := ObterConteudoTag(ANode.Childrens.FindAnyNs('xMotivo'), tcStr);

        ANodes := ANode.Childrens.FindAllAnyNs('dadosCsc');

        dadosCsc.Clear;

        for i := 0 to Length(ANodes) - 1 do
        begin
          Item := dadosCsc.New;

          Item.idCsc     := ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('idCsc'), tcInt);
          Item.codigoCsc := ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('codigoCsc'), tcStr);
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

end.

